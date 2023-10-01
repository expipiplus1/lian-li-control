{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      d = flake-utils.lib.eachDefaultSystem (system:
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

          packageName = "lian-li-control";
        in rec {
          packages.${packageName} = with pkgs.haskell.lib.compose;
            overrideCabal (d: ({
              postInstall = ''
                mkdir -p $out/lib/udev/rules.d/
                cp ${self}/*.rules $out/lib/udev/rules.d/
              '';
            })) (haskellPackages.callCabal2nix packageName self { });
          packages.default = with pkgs.haskell.lib.compose;
            (haskellPackages.generateOptparseApplicativeCompletions [
              "lian-li-fan-control"
              "lian-li-pump-control"
            ] packages.${packageName});

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
        });
    in d // {
      nixosModules.fan = { lib, config, system, pkgs, ... }:
        with lib;
        let cfg = config.services.hardware.lian-li-fan-control;
        in {
          options.services.hardware.lian-li-fan-control = {
            enable = mkEnableOption (mdDoc "Set Lian-Li fan settings");

            package = mkOption {
              type = types.package;
              default = d.packages.${pkgs.stdenv.hostPlatform.system}.default;
              description =
                mdDoc "Set version of lian-li-control package to use.";
            };

            speed = mkOption {
              type = types.nullOr (types.either (types.enum [ "pwm" ])
                (types.ints.between 0 100));
              default = null;
              description =
                mdDoc ''Desired fan RPM, or "pwm" to track the pwm input'';
            };

            color = mkOption {
              type = types.nullOr (types.enum [ "sync" "nosync" ]);
              default = null;
              description = mdDoc
                ''"sync" to track the rgb input or "nosync" to listen on USB'';
            };
          };
          config = mkIf cfg.enable {
            environment.systemPackages = [ cfg.package ];
            services.udev.packages = [ cfg.package ];

            systemd.services.lian-li-fan-control = {
              description = "Set lian-li fan settings";
              wantedBy = [
                "multi-user.target"
                "suspend.target"
                "hibernate.target"
                "hybrid-sleep.target"
                "suspend-then-hibernate.target"
              ];
              serviceConfig = {
                Type = "oneshot";
                ExecStart = "${cfg.package}/bin/lian-li-fan-control ${
                    if cfg.speed == null then
                      ""
                    else
                      "--set-speed ${builtins.toString cfg.speed}"
                  } ${
                    if cfg.color == null then
                      ""
                    else
                      "--set-color ${builtins.toString cfg.color}"
                  }";
              };
            };
          };
        };
      nixosModules.pump = { lib, config, system, pkgs, ... }:
        with lib;
        let cfg = config.services.hardware.lian-li-pump-control;
        in {
          options.services.hardware.lian-li-pump-control = {
            enable = mkEnableOption (mdDoc "Set Lian-Li pump settings");

            package = mkOption {
              type = types.package;
              default = d.packages.${pkgs.stdenv.hostPlatform.system}.default;
              description =
                mdDoc "Set version of lian-li-control package to use.";
            };

            speed = mkOption {
              type = types.nullOr (types.either (types.enum [ "pwm" ])
                (types.ints.between 2200 4200));
              default = null;
              description =
                mdDoc ''Desired pump RPM, or "pwm" to track the pwm input'';
            };

            color = mkOption {
              type = types.nullOr (types.either (types.enum [ "sync" ])
                (types.strMatching "[0-9a-fA-F]{6}:[0-9a-fA-F]{6}"));
              default = null;
              description = mdDoc ''
                Desired pump inner and outer color (rrggbb:rrggbb), or "sync" to track the rgb input'';
            };
          };
          config = mkIf cfg.enable {
            environment.systemPackages = [ cfg.package ];
            services.udev.packages = [ cfg.package ];

            systemd.services.lian-li-pump-control = {
              description = "Set lian-li pump settings";
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
                  "${cfg.package}/bin/lian-li-pump-control --get-speed ${
                    if cfg.speed == null then
                      ""
                    else
                      "--set-speed ${builtins.toString cfg.speed}"
                  } ${
                    if cfg.color == null then
                      ""
                    else
                      "--set-color ${builtins.toString cfg.color}"
                  }";
              };
            };
          };
        };
    };
}
