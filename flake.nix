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
            exec ${pkgs.tilem}/bin/tilem2 -r ${zkeme80}/zkeme80.rom
          '';
          zkeme80 = runCommand "zkeme80.rom" { buildInputs = [ guile ]; } ''
            cp -r ${./.}/src/* .
            chmod -R +w .
            echo '(begin (load "zkeme80.scm") (make-rom "zkeme80.rom"))' | guile
            mkdir $out
            cp zkeme80.rom $out/
          '';
        };
        defaultPackage = self.packages.${system}.default;
      }
    );

}
