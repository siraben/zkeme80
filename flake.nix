{
  description = "zkeme80";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  };
  outputs = { self, nixpkgs, utils, flake-compat }:
    utils.lib.eachDefaultSystem (system:
      with import nixpkgs { inherit system; }; rec {
        packages = rec {
          default = pkgs.writeShellScriptBin "runit" ''
            exec ${pkgs.bash}/bin/bash ${./debug/run-tilem.sh} \
              ${./debug/tilem-keybindings.ini} \
              ${pkgs.tilem}/bin/tilem2 -r ${zkeme80}/zkeme80.rom
          '';
          zkeme80 = runCommand "zkeme80.rom" { buildInputs = [ guile guile-json ]; } ''
            cp -r ${./.}/src/* .
            chmod -R +w .
            guile --no-auto-compile -c '(use-modules (ice-9 format)) (load "zkeme80.scm") (make-rom+map "zkeme80.rom" "zkeme80.ram-labelmap.json")'
            mkdir $out
            cp zkeme80.rom zkeme80.ram-labelmap.json $out/
          '';
        };
        defaultPackage = self.packages.${system}.default;

        devShells.default = mkShell {
          buildInputs = [ guile guile-json python3 imagemagick ];
        };
      }
    );

}
