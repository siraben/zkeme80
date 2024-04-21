{
  description = "zkeme80";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, utils, flake-compat }:
    utils.lib.eachSystem (with utils.lib.system; [ x86_64-linux aarch64-linux x86_64-darwin ]) (system:
      with import nixpkgs { inherit system; }; rec {
        packages = rec {
          default = self.packages.${system}.runit;
          zkeme80 = runCommand "zkeme80.rom" { buildInputs = [ guile ]; } ''
            cp -r ${./.}/src/* .
            chmod -R +w .
            echo '(begin (load "zkeme80.scm") (make-rom "zkeme80.rom"))' | guile
            mkdir $out
            cp zkeme80.rom $out/
          '';
          runit = writeScript "runit" ''
            #!/usr/bin/env sh
            ${tilem}/bin/tilem2 -r ${zkeme80}/zkeme80.rom
          '';
        };
        defaultPackage = self.packages.${system}.default;
      }
    );

}
