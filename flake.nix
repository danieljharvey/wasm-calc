{
  description = "wasm-calc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          # current compiler version, ideally, we'll put everything here
          # eventually

          compilerVersion = "ghc965";

          # fix things
          haskell = pkgs.haskell // {
            packages = pkgs.haskell.packages // {
              "${compilerVersion}" =
                pkgs.haskell.packages."${compilerVersion}".override {
                  overrides = self: super: {
                    # try and remove cycle
                    cabal-fmt = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.overrideCabal super.cabal-fmt (drv: {
                      enableSeparateBinOutput = false;
                    }));
                    # try and remove cycle
                    ormolu = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.overrideCabal super.ormolu (drv: {
                      enableSeparateBinOutput = false;
                    }));

                    diagnose = (pkgs.haskell.lib.doJailbreak
                      (pkgs.haskell.lib.appendConfigureFlags (pkgs.haskell.lib.markUnbroken super.diagnose) [ "-f +megaparsec-compat" "-f parsec-compat" ])).overrideAttrs
                      (oldAttrs: rec {
                        buildInputs = [ super.megaparsec ];
                      });


                    # allow slippery version bounds and don't run tests
                    wasm = pkgs.haskell.lib.markUnbroken (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak super.wasm));
                  };

                };
            };
          };

          haskellPackages = haskell.packages.${compilerVersion};

          jailbreakUnbreak = pkg:
            pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

          packageName = "wasm-calc";

          makeCalc = name: dir:
            pkgs.haskell.lib.overrideCabal
              (haskellPackages.callCabal2nix name dir {
                # Dependency overrides go here
              })
              (old:
                {
                  # copy the client into the package
                  testToolDepends = [ pkgs.nodejs ];
                });

          wasm-calc = makeCalc "wasm-calc" ./wasm-calc;
          wasm-calc2 = makeCalc "wasm-calc2" ./wasm-calc2;
          wasm-calc3 = makeCalc "wasm-calc3" ./wasm-calc3;
          wasm-calc4 = makeCalc "wasm-calc4" ./wasm-calc4;
          wasm-calc5 = makeCalc "wasm-calc5" ./wasm-calc5;
          wasm-calc6 = makeCalc "wasm-calc6" ./wasm-calc6;
          wasm-calc7 = makeCalc "wasm-calc7" ./wasm-calc7;
          wasm-calc8 = makeCalc "wasm-calc8" ./wasm-calc8;
          wasm-calc9 = makeCalc "wasm-calc9" ./wasm-calc9;
          wasm-calc10 = makeCalc "wasm-calc10" ./wasm-calc10;

        in
        {
          packages =
            {
              inherit wasm-calc wasm-calc2 wasm-calc3
                wasm-calc4 wasm-calc5 wasm-calc6
                wasm-calc7 wasm-calc8 wasm-calc9 wasm-calc10;
            };

          # sneaky way to 'build all'
          checks = {
            inherit wasm-calc wasm-calc2 wasm-calc3
              wasm-calc4 wasm-calc5 wasm-calc6
              wasm-calc7 wasm-calc8 wasm-calc9
              wasm-calc10;
          };

          defaultPackage = self.packages.${system}.wasm-calc9;

          devShell = pkgs.mkShell {

            buildInputs = with haskellPackages; [
              ghc
              hlint
              ormolu
              #haskell-language-server
              pkgs.ghciwatch
              cabal-fmt
              cabal-install

              # test tooling
              pkgs.nixpkgs-fmt
              pkgs.git
              pkgs.nodejs
              pkgs.watchexec
              pkgs.nodePackages_latest.serve
            ];

            inputsFrom = builtins.attrValues self.packages.${system};
          };
        });
}
