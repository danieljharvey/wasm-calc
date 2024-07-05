{
  description = "wasm-calc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
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
                  (pkgs.haskell.lib.appendConfigureFlags (pkgs.haskell.lib.markUnbroken super.diagnose) ["-f +megaparsec-compat" "-f parsec-compat"])).overrideAttrs
                    (oldAttrs: rec {
        buildInputs = [super.megaparsec];
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
      in
      {
        packages.wasm-calc9 =
          pkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix "wasm-calc9" ./wasm-calc9 {
            # Dependency overrides go here

             })               (old:
             {
                # copy the client into the package
               testToolDepends = [pkgs.git pkgs.nodejs];
              });

        defaultPackage = self.packages.${system}.wasm-calc9;

        devShell = pkgs.mkShell {
          nativeBuildInputs = with haskellPackages; [ pkgs.git pkgs.nodejs ];

          buildInputs = with haskellPackages; [
            ghc
            hlint
            ormolu
            #haskell-language-server
            pkgs.ghciwatch
            cabal-fmt
            cabal-install
            # test tooling
            pkgs.git
            pkgs.nodejs
            pkgs.watchexec
            pkgs.nodePackages_latest.serve
            #pkgs.wabt
            #pkgs.emscripten
            #pkgs.wasmtime
          ];

          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
