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
          zkeme80 = runCommand "zkeme80.rom" { nativeBuildInputs = [ guile python3 gnumake ]; } ''
            cp -r ${./.}/src ${./.}/tests .
            cp ${./.}/Makefile ${./.}/build.scm ${./.}/0A.key .
            chmod -R +w .
            make test-build test-modules build
            mkdir $out
            cp zkeme80.rom zkeme80.ram-labelmap.json $out/
          '';
        };
        defaultPackage = self.packages.${system}.default;

        devShells.default = mkShell {
          buildInputs = [ guile knightos-mktiupgrade python3 imagemagick ];
        };
      }
    );

}
