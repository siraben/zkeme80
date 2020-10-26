{ pkgs ? import
    (builtins.fetchTarball {
      name = "nixos-unstable-2020-10-21";
      url = "https://github.com/nixos/nixpkgs/archive/5d0e2dedd5594cdf3ebbda8bc61310e227235a9c.tar.gz";
      sha256 = "1npc0viqm99gbj1q6vfqj4j4gz8jg5s6c6iiv3j01z2dbimwqyab";
    })
    { }
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
