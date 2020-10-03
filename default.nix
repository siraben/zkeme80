with import (builtins.fetchTarball {
  name = "nixos-unstable-2018-11-07";
  url = "https://github.com/nixos/nixpkgs/archive/6141939d6e0a77c84905efd560c03c3032164ef1.tar.gz";
  sha256 = "1nz2z71qvjna8ki5jq4kl6pnl716hj66a0gs49l18q24pj2kbjwh";
}) {};

rec {
  tilem = stdenv.mkDerivation {
    name = "tilem";
    src = fetchurl {
      url = "https://sourceforge.net/projects/tilem/files/tilem/2.0/tilem-2.0.tar.bz2/download";
      name = "tilem-2.0.tar.bz2";
      sha256 = "1ba38xzhp3yf21ip3cgql6jzy49jc34sfnjsl4syxyrd81d269zw";
    };
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ glib gnome2.gtk libticonv libtifiles libticables libticalcs ];
  };
  tilibs = fetchurl {
    url = "https://www.ticalc.org/pub/unix/tilibs.tar.gz";
    sha256 = "07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg";
  };
  tilibs2 = runCommand "tilibs2" {} ''
    mkdir $out
    cd $out
    unpackFile ${tilibs}
    mv -vi tilibs2/* .
    rmdir tilibs2
  '';
  libticonv = stdenv.mkDerivation {
    name = "libticonv";
    src = "${tilibs2}/libticonv-1.1.5.tar.bz2";
    nativeBuildInputs = [ autoreconfHook pkgconfig ];
    buildInputs = [ glib ];
    configureFlags = [ "--enable-iconv" ];
  };
  libticables = stdenv.mkDerivation {
    name = "libticables";
    src = "${tilibs2}/libticables2-1.3.5.tar.bz2";
    nativeBuildInputs = [ autoreconfHook pkgconfig ];
    buildInputs = [ glib libusb1 ];
    configureFlags = [ "--enable-libusb10" ];
  };
  libticalcs = stdenv.mkDerivation {
    name = "libticalcs";
    src = "${tilibs2}/libticalcs2-1.1.9.tar.bz2";
    nativeBuildInputs = [ autoreconfHook pkgconfig ];
    buildInputs = [ glib libticables libticonv libtifiles lzma bzip2 ]
                  ++ lib.optionals stdenv.isLinux [ acl ]
                  ++ lib.optionals stdenv.isDarwin [ darwin.libobjc ];
  };
  libtifiles = stdenv.mkDerivation {
    name = "libtifiles";
    src = "${tilibs2}/libtifiles2-1.1.7.tar.bz2";
    nativeBuildInputs = [ autoreconfHook pkgconfig ];
    buildInputs = [ glib libticonv libarchive lzma bzip2 ];
  };
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
