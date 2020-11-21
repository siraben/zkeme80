{ sources ? import nix/sources.nix
, pkgs ? import sources.nixpkgs { }
}:
with pkgs;

rec {
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
}
