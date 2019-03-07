with import <nixpkgs> {};

lib.fix (self: {
  tilem = stdenv.mkDerivation {
    name = "tilem";
    src = fetchurl {
      url = "https://sourceforge.net/projects/tilem/files/tilem/2.0/tilem-2.0.tar.bz2/download";
      name = "tilem-2.0.tar.bz2";
      sha256 = "1ba38xzhp3yf21ip3cgql6jzy49jc34sfnjsl4syxyrd81d269zw";
    };
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ glib gnome2.gtk self.libticonv self.libtifiles self.libticables self.libticalcs ];
  };
  tilibs = fetchurl {
    url = "https://www.ticalc.org/pub/unix/tilibs.tar.gz";
    sha256 = "07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg";
  };
  tilibs2 = runCommand "tilibs2" {} ''
    mkdir $out
    cd $out
    unpackFile ${self.tilibs}
    mv -vi tilibs2/* .
    rmdir tilibs2
  '';
  libticonv = stdenv.mkDerivation {
    name = "libticonv";
    src = "${self.tilibs2}/libticonv-1.1.5.tar.bz2";
    nativeBuildInputs = [ autoreconfHook pkgconfig ];
    buildInputs = [ glib ];
    configureFlags = [ "--enable-iconv" ];
  };
  libticables = stdenv.mkDerivation {
    name = "libticables";
    src = "${self.tilibs2}/libticables2-1.3.5.tar.bz2";
    nativeBuildInputs = [ autoreconfHook pkgconfig ];
    buildInputs = [ glib libusb1 ];
    configureFlags = [ "--enable-libusb10" ];
  };
  libticalcs = stdenv.mkDerivation {
    name = "libticalcs";
    src = "${self.tilibs2}/libticalcs2-1.1.9.tar.bz2";
    nativeBuildInputs = [ autoreconfHook pkgconfig ];
    buildInputs = [ glib self.libticables self.libticonv self.libtifiles acl lzma bzip2 ];
  };
  libtifiles = stdenv.mkDerivation {
    name = "libtifiles";
    src = "${self.tilibs2}/libtifiles2-1.1.7.tar.bz2";
    nativeBuildInputs = [ autoreconfHook pkgconfig ];
    buildInputs = [ glib self.libticonv libarchive lzma bzip2 ];
  };

  zkeme80 = runCommand "zkeme80.rom" { buildInputs = [ guile ]; } ''
    cp -r ${./.}/src/* .
    chmod -R +w .
    echo '(begin (load "zkeme80.scm") (make-rom "zkeme80.rom"))' | guile
    mkdir $out
    cp zkeme80.rom $out/
  '';
  runit = writeScript "runit" ''
    #!/bin/sh
    ${self.tilem}/bin/tilem2 -r ${self.zkeme80}/zkeme80.rom
  '';
})
