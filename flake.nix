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

  outputs = { self, nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      with import nixpkgs { inherit system; }; rec {
        zkeme80 = runCommand "zkeme80.rom" { buildInputs = [ guile ]; } ''
            cp -r ${./.}/src/* .
            chmod -R +w .
            echo '(begin (load "zkeme80.scm") (make-rom "zkeme80.rom"))' | guile
            mkdir $out
            cp zkeme80.rom $out/
          '';
        defaultPackage = writeScript "runit" ''
            #!/usr/bin/env sh
            ${tilem}/bin/tilem2 -r ${zkeme80}/zkeme80.rom
          '';
        defaultApp = {
          type = "app";
          program = "${defaultPackage}";
        };
      }
    );
}
