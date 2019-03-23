{ profiling, haddocks }:

self: super:

with rec {
  inherit (super) lib;

  hlib = super.haskell.lib;

  disableTemplateHaskellOverlay = (
    import ./disable-template-haskell.nix {
      haskellLib = hlib;
      inherit (super) fetchFromGitHub;
    });
  
  # This function removes any cruft not relevant to our Haskell builds.
  #
  # If something irrelevant to our build is not removed by this function, and
  # you modify that file, Nix will rebuild the derivation even though nothing
  # that would affect the output has changed.
  #
  # The `excludePred` argument is a function that can be used to filter out more
  # files on a package-by-package basis.
  # The `includePred` argument is a function that can be used to include files
  # that this function would normally filter out.
  clean = (
    { path,
      excludePred ? (name: type: false),
      includePred ? (name: type: false)
    }:
    if lib.canCleanSource path
    then lib.cleanSourceWith {
           filter = name: type: (includePred name type) || !(
             with rec {
               baseName     = baseNameOf (toString name);
               isFile       = (type == "regular");
               isLink       = (type == "symlink");
               isDir        = (type == "directory");
               isUnknown    = (type == "unknown");
               isNamed      = str: (baseName == str);
               hasExtension = ext: (lib.hasSuffix ext baseName);
               beginsWith   = pre: (lib.hasPrefix pre baseName);
               matches      = regex: (builtins.match regex baseName != null);
             };

             lib.any (lib.all (x: x)) [
               # Each element of this list is a list of booleans, which should be
               # thought of as a "predicate" on paths; the predicate is true if the
               # list is composed entirely of true values.
               #
               # If any of these predicates is true, then the path will not be
               # included in the source used by the Nix build.
               #
               # Remember to use parentheses around elements of a list;
               # `[ f x ]`   is a heterogeneous list with two elements,
               # `[ (f x) ]` is a homogeneous list with one element.
               # Knowing the difference might save your life.
               [ (excludePred name type) ]
               [ isUnknown ]
               [ isDir (isNamed "dist") ]
               [ isDir (isNamed "dist-newstyle") ]
               [ isDir (isNamed  "run") ]
               [ (isFile || isLink) (hasExtension ".nix") ]
               [ (beginsWith ".ghc") ]
               [ (hasExtension ".sh") ]
               [ (hasExtension ".txt") ]
             ]);
           src = lib.cleanSource path;
         }
    else path);
    
  mainReflexOverlay = hself: hsuper: {
    callC2N = (
      { name,
        path                  ? (throw "callC2N requires path argument!"),
        rawPath               ? (clean { inherit path; }),
        relativePath          ? null,
        args                  ? {},
        apply                 ? [],
        extraCabal2nixOptions ? []
      }:

      with rec {
        filter = p: type: (
          (super.lib.hasSuffix "${name}.cabal" p)
          || (baseNameOf p == "package.yaml"));
        expr = hsuper.haskellSrc2nix {
          inherit name;
          extraCabal2nixOptions = self.lib.concatStringsSep " " (
            (if relativePath == null then [] else ["--subpath" relativePath])
            ++ extraCabal2nixOptions);
          src = if super.lib.canCleanSource rawPath
                then super.lib.cleanSourceWith { src = rawPath; inherit filter; }
                else rawPath;
        };
        compose = f: g: x: f (g x);
        composeList = x: lib.foldl' compose lib.id x;
      };

      composeList apply
      (hlib.overrideCabal
       (hself.callPackage expr args)
       (orig: { src = rawPath; })));
       
    trasa = hself.callC2N {
      name = "trasa";
      path = ../trasa;
      apply = [ ]
        ++ ( if profiling
             then [ hlib.enableLibraryProfiling hlib.enableExecutableProfiling ]
             else [ hlib.disableLibraryProfiling hlib.disableExecutableProfiling ]
           )
        ++ ( if haddocks
             then [ hlib.doHaddock ]
             else [ hlib.dontHaddock ]
           ); 
    };
    trasa-client = hself.callC2N {
      name = "trasa-client";
      path = ../trasa-client;
      apply = [ ]
        ++ ( if profiling
             then [ hlib.enableLibraryProfiling hlib.enableExecutableProfiling ]
             else [ hlib.disableLibraryProfiling hlib.disableExecutableProfiling ]
           )
        ++ ( if haddocks
             then [ hlib.doHaddock ]
             else [ hlib.dontHaddock ]
           );   
    };
    trasa-server = hself.callC2N {
      name = "trasa-server";
      path = ../trasa-server;
      apply = [ ]
        ++ ( if profiling
             then [ hlib.enableLibraryProfiling hlib.enableExecutableProfiling ]
             else [ hlib.disableLibraryProfiling hlib.disableExecutableProfiling ]
           )
        ++ ( if haddocks
             then [ hlib.doHaddock ]
             else [ hlib.dontHaddock ]
           );   
    };
    trasa-reflex = hself.callC2N {
      name = "trasa-reflex";
      path = ../trasa-reflex;
      apply = [ ]
        ++ ( if profiling
             then [ hlib.enableLibraryProfiling hlib.enableExecutableProfiling ]
             else [ hlib.disableLibraryProfiling hlib.disableExecutableProfiling ]
           )
        ++ ( if haddocks
             then [ hlib.doHaddock ]
             else [ hlib.dontHaddock ]
           ); 
    };
    trasa-th = hself.callC2N {
      name = "trasa-th";
      path = ../trasa-th;
      apply = [ ]
        ++ ( if profiling
             then [ hlib.enableLibraryProfiling hlib.enableExecutableProfiling ]
             else [ hlib.disableLibraryProfiling hlib.disableExecutableProfiling ]
           )
        ++ ( if haddocks
             then [ hlib.doHaddock ]
             else [ hlib.dontHaddock ]
           );
    };
    trasa-tutorial = hself.callC2N {
      name = "trasa-tutorial";
      path = ../trasa-tutorial;
      apply = [ ]
        ++ ( if profiling
             then [ hlib.enableLibraryProfiling hlib.enableExecutableProfiling ]
             else [ hlib.disableLibraryProfiling hlib.disableExecutableProfiling ]
           )
        ++ ( if haddocks
             then [ hlib.doHaddock ]
             else [ hlib.dontHaddock ]
           );
    };

    reflex = hself.callC2N {
      name = "reflex";
      rawPath = super.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex";
        rev = "a083a7664a1bcc881e1b9b2bb95413503a96dea4";
        sha256 = "05221dicjckpq43alp64h3sq1bnxv6wm1qax8awisablr6q8kqy3";
      };
      apply = [ 
        hlib.doJailbreak
        hlib.dontCheck
        hlib.dontBenchmark
        (drv: hlib.disableCabalFlag drv "use-template-haskell") 
      ];
    };

    reflex-dom-core = hself.callC2N {
      name = "reflex-dom-core";
      rawPath = super.fetchFromGitHub {
        owner = "chessai";
        repo = "reflex-dom";
        rev = "f5bd9df60f41651a63b80e0b863be31b72475fd3";
        sha256 = "1nvpp77ba7xwrs8nmrvcmc6v1knyqxjsznw8n7b8mqf4xbcqf9kk";
      };
      relativePath = "reflex-dom-core";
      apply = [ hlib.dontCheck hlib.dontBenchmark hlib.doJailbreak ];
    };

    reflex-dom = hself.callC2N {
      name = "reflex-dom";
      rawPath = super.fetchFromGitHub {
        owner = "chessai";
        repo = "reflex-dom";
        rev = "f5bd9df60f41651a63b80e0b863be31b72475fd3";
        sha256 = "1nvpp77ba7xwrs8nmrvcmc6v1knyqxjsznw8n7b8mqf4xbcqf9kk";
      };
      relativePath = "reflex-dom";
      apply = [ hlib.dontCheck hlib.dontBenchmark hlib.doJailbreak ];
    };

    jsaddle-webkit2gtk = hself.callC2N {
      name = "jsaddle-webkit2gtk";
      rawPath = super.fetchFromGitHub {
        owner = "chessai";
        repo = "jsaddle";
        rev = "77f07643d92486c8d02abac41db25efc0047b227";
        sha256 = "19d7nqdqj2cphiz65vsfk2xwlim5z0ch6p2l03h6nia66l06y7yr";
      };
      relativePath = "jsaddle-webkit2gtk";
      apply = [ hlib.dontCheck hlib.dontBenchmark hlib.doJailbreak ];
    };

    haskell-gi-overloading = hsuper.haskell-gi-overloading_0_0;

    chrome-test-utils = hself.callC2N {
      name = "chrome-test-utils";
      rawPath = super.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-dom";
        rev = "646a98aa0caa9d392f6a1e3bc929851883f081a1";
        sha256 = "0259f0df8dyfrlrv5lxj49n0phpwxly0lqjz35s7chcdxv3m1fz7";
      };
      relativePath = "chrome-test-utils";
      apply = [ hlib.dontCheck hlib.dontBenchmark ];
    };

    # examples
    backend = hself.callC2N {
      name = "backend";
      path = ../example/backend;
      apply = [ ];
    };

    frontend = hself.callC2N {
      name = "frontend";
      path = ../example/frontend;
      apply = [ ];
    };

    common = hself.callC2N {
      name = "common";
      path = ../example/common;
      apply = [ ];
    };

    # broken doctests/tests
    comonad           = hlib.disableCabalFlag hsuper.comonad "test-doctests";
    semigroupoids     = hlib.disableCabalFlag hsuper.semigroupoids "doctests";
    lens              = hlib.disableCabalFlag hsuper.lens "test-doctests";
    distributive      = hlib.dontCheck (hlib.disableCabalFlag hsuper.distributive "test-doctests");

    http-types        = hlib.dontCheck hsuper.http-types;
    silently          = hlib.dontCheck hsuper.silently;
    unliftio          = hlib.dontCheck hsuper.unliftio;
    conduit           = hlib.dontCheck hsuper.conduit;
    yaml              = hlib.dontCheck hsuper.yaml;
    extra             = hlib.dontCheck hsuper.extra;
    half              = hlib.dontCheck hsuper.half;
    iproute           = hlib.dontCheck hsuper.iproute;
    aeson-compat      = hlib.dontCheck hsuper.aeson-compat;
    tzdata            = hlib.dontCheck hsuper.tzdata;
    tz                = hlib.dontCheck hsuper.tz;
    time-exts         = hlib.dontCheck hsuper.time-exts;
    double-conversion = hlib.dontCheck hsuper.double-conversion;

  };

  composeOverlayList = lib.foldl' lib.composeExtensions (_: _: {});

  reflexOverlay = composeOverlayList [
    disableTemplateHaskellOverlay
    mainReflexOverlay
  ];

};

{
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc844 = (super.haskell.packages.ghc844.override {
        overrides = super.lib.composeExtensions
          (super.haskell.packageOverrides or (self: super: {}))
          reflexOverlay;
      });
      ghc863 = (super.haskell.packages.ghc863.override {
        overrides = super.lib.composeExtensions
          (super.haskell.packageOverrides or (self: super: {}))
          reflexOverlay;
      });
      ghcjs86 = (super.haskell.packages.ghcjs86.override {
        overrides = super.lib.composeExtensions
          (super.haskell.packageOverrides or (self: super: {}))
          reflexOverlay;
      });
    };
  };
}
