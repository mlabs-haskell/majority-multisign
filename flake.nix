{
  description = "majority-multisign";

  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2105";
  inputs.plutus.url = "github:input-output-hk/plutus"; # used for libsodium-vrf

  outputs = { self, nixpkgs, haskell-nix, plutus }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };

      projectFor = system:
        let
          deferPluginErrors = true;
          pkgs = nixpkgsFor system;

          fakeSrc = pkgs.runCommand "real-source" { } ''
            cp -rT ${self} $out
            chmod u+w $out/cabal.project
            cat $out/cabal-haskell.nix.project >> $out/cabal.project
          '';

          sources = import ./nix/sources.nix { };
        in (nixpkgsFor system).haskell-nix.cabalProject' {
          src = fakeSrc.outPath;
          compiler-nix-name = "ghc8107";
          cabalProjectFileName = "cabal.project";
          modules = [{
            packages = {
              marlowe.flags.defer-plugin-errors = deferPluginErrors;
              plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
              plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
              plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
              cardano-crypto-praos.components.library.pkgconfig =
                nixpkgs.lib.mkForce
                [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig =
                nixpkgs.lib.mkForce
                [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              cardano-wallet-core.components.library.build-tools = [
                (import plutus {
                  inherit system;
                }).pkgs.buildPackages.buildPackages.gitMinimal
              ];
              cardano-config.components.library.build-tools = [
                (import plutus {
                  inherit system;
                }).pkgs.buildPackages.buildPackages.gitMinimal
              ];
            };
          }];
          shell = {
            withHoogle = true;

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs =
              [ pkgs.cabal-install pkgs.hlint pkgs.haskellPackages.fourmolu ];

            additional = ps: with ps; [
              tasty-plutus
              plutus-extra
              plutus-context-builder
              quickcheck-plutus-instances
              plutus-laws
              plutus-list
              plutus-golden

              plutarch

              base-deriving-via
              cardano-addresses
              cardano-addresses-cli
              cardano-binary
              cardano-crypto
              cardano-crypto-class
              cardano-crypto-praos
              cardano-crypto-wrapper
              cardano-ledger-alonzo
              cardano-ledger-byron
              cardano-ledger-core
              cardano-ledger-pretty
              cardano-ledger-shelley
              cardano-ledger-shelley-ma
              cardano-prelude
              cardano-slotting
              flat
              freer-extras
              goblins
              measures
              orphans-deriving-via
              playground-common
              plutus-contract
              plutus-chain-index
              plutus-core
              plutus-ledger
              plutus-ledger-api
              plutus-pab
              plutus-playground-server
              plutus-tx
              plutus-tx-plugin
              plutus-use-cases
              prettyprinter-configurable
              quickcheck-dynamic
              Win32-network
              word-array
            ];
          };
          sha256map = pkgs.lib.foldr (data: tab:
            with data;
            tab // {
              "https://github.com/${owner}/${repo}"."${rev}" = sha256;
              "https://github.com/${owner}/${repo}.git"."${rev}" = sha256;
            }) { } (pkgs.lib.attrValues sources);
        };
    in {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      # this could be done automatically, but would reduce readability
      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system: self.flake.${system}.checks);
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test" {
          nativeBuildInputs = builtins.attrValues self.checks.${system};
        } "touch $out");
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}