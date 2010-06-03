Information on building 3rd-party libraries needed for
Mac OS X GRacket.

Get these packages (or newer, if compatible):
 pkg-config-0.23.tar.gz
 libpng-1.4.0.tar.gz 
 pixman-0.17.14.tar.gz
 cairo-1.9.6.tar.gz
 gettext-0.17.tar.gz
 glib-2.22.4.tar.gz
 pango-1.28.0.tar.gz
 libjpeg62 (maybe in binary form)

Patches:
 cairo/src/cairo-quartz-font.c:656:
    if (width < 1) width = 1;
    if (height < 1) height = 1;
 glib/glib/gconvert.c:54:
   #if !(defined(__APPLE__) && defined(__LP64__)) && !defined(USE_LIBICONV_GNU) && defined (_LIBICONV_H) 
 pango/pango/modules.c:573:
   // read_modules ();

Configures (where <dest> is some temporary area):
  pkg-config: --prefix=<dest>
  libpng: --prefix=<dest>
  pixman: --prefix=<dest>
  Cairo: PATH=<dest>/bin --disable-xlib --disable-ft --disable-fc --prefix=<dest>
  gettext: --prefix=<dest>
  glib: PATH=<dest>/bin CFLAGS=-I<dest>/include LDFLAGS=-L<dest>/lib --prefix=<dest>
  Pango: PATH=<dest>/bin --without-x --with-included-modules=yes --with-dynamic-modules=no

 Note: PATH above ensures that pkg-config binaries are used to find
 things in <dest> rather than some other area, such as /opt/local.

Install:
  racket install-libs.rkt <dest>/lib
   * using `racket' for the target installation
   * do not include a trailing slash
   * double-check installed libraries to ensure that they do not
     have <dest> in their shared-library paths
