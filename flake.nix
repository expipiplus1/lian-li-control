{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        unextendedHaskellPackages = pkgs.haskell.packages.ghc94;
        haskellPackages = unextendedHaskellPackages.extend (self: super:
          with pkgs.haskell.lib.compose;
          (pkgs.lib.mapAttrs (n: markUnbroken) {
            bindings-libusb = addBuildDepends [ pkgs.pkg-config pkgs.libusb1 ]
              super.bindings-libusb;
            hid = doJailbreak super.hid;
          }));

        packageName = "lian-li-pump-control";
      in rec {
        packages.${packageName} = with pkgs.haskell.lib.compose;
          overrideCabal (d: ({
            postInstall = ''
              mkdir -p $out/lib/udev/rules.d/
              cp ${self}/*.rules $out/lib/udev/rules.d/
            '';
          })) (haskellPackages.callCabal2nix packageName self { });
        packages.default = with pkgs.haskell.lib.compose;
          (haskellPackages.generateOptparseApplicativeCompletions
            [ packageName ] packages.${packageName});

        devShell = haskellPackages.shellFor {
          packages = p: [ packages.${packageName} ];
          withHoogle = true;
          buildInputs = with unextendedHaskellPackages; [
            # hls puts an old fourmolu in scope :/, so make sure a desired one
            # gets in there first
            # fourmolu_0_13_1_0
            haskell-language-server
            cabal-install
          ];
        };

        nixosModules.default = { lib, config }:
          with lib;
          let cfg = config.services.hardware.lian-li-pump-control;
          in {
            options.services.hardware.lian-li-pump-control = {
              enable = mkEnableOption (mdDoc "Set Lian-Li pump settings");

              package = mkOption {
                type = types.package;
                default = packages.default;
                description =
                  mdDoc "Set version of lian-li-pump-control package to use.";
              };
            };
            config = mkIf cfg.enable {
              environment.systemPackages = [ cfg.package ];
              services.udev.packages = [ cfg.package ];

              systemd.services.openrgb = {
                description = "OpenRGB server daemon";
                wantedBy = [
                  "multi-user.target"
                  "suspend.target"
                  "hibernate.target"
                  "hybrid-sleep.target"
                  "suspend-then-hibernate.target"
                ];
                serviceConfig = {
                  Type = "oneshot";
                  ExecStart =
                    "${cfg.package}/bin/lian-li-pump-control --get-speed";
                };
              };
            };
          };
      });
}
