Information on building 3rd-party libraries needed for Mac OS X GRacket.

Get these packages (or newer, if compatible):
 pkg-config-0.23.tar.gz
 libpng-1.4.0.tar.gz 
 pixman-0.17.14.tar.gz
 cairo-1.9.6.tar.gz
 gettext-0.17.tar.gz
 glib-2.22.4.tar.gz
 pango-1.28.0.tar.gz
 libjpeg62 (maybe in binary form)

 PSMTabBarControl, probably from "maccode.googlecode.com",
                   and handled differently

Patches:
 cairo/src/cairo-quartz-font.c:656:
    if (width < 1) width = 1;
    if (height < 1) height = 1;
 glib/glib/gconvert.c:54: change to
   #if !(defined(__APPLE__) && defined(__LP64__)) && !defined(USE_LIBICONV_GNU) && defined (_LIBICONV_H) 
 pango/pango/modules.c:573: change to
   // read_modules ();
 pango/modules/basic/basic-atsui.c:60: add
   if (!glyph) { glyph = PANGO_GET_UNKNOWN_GLYPH(glyph); }
 pango/pangocairo-atsuifont.c:141: add
  metrics->underline_position = -metrics->underline_position;
  pango_quantize_line_geometry (&metrics->underline_thickness,
                                &metrics->underline_position);
  metrics->underline_position = -(metrics->underline_position 
                                  + metrics->underline_thickness);

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

XCode:
 Build PSMTabBarControl. You only need the Framework target, and
 in Release mode.

Install:
  racket install-libs.rkt <dest>/lib
   * using `racket' for the target installation
   * do not include a trailing slash
   * double-check installed libraries to ensure that they do not
     have <dest> in their shared-library paths

  Also copy "PSMTabBarControl.framework" into the installation's "lib"
  directory. You can flatten all the auto-version soft links (moving
  "PSMTabBarControl" and "Resources" to immediately inside
  "PSMTabBarControl), and you can use `ditto' to prune the binary to
  just the platform that you're using.

--------------------------------------------------

DESTDIR=
WORKDIR=
ARCHDIR=

cd "$WORKDIR"
tar zxf "$ARCHDIR"pkg-config-0.23.tar.gz
tar zxf "$ARCHDIR"libpng-1.4.0.tar.gz
tar zxf "$ARCHDIR"pixman-0.17.14.tar.gz
tar zxf "$ARCHDIR"cairo-1.9.6.tar.gz
tar zxf "$ARCHDIR"gettext-0.17.tar.gz
tar zxf "$ARCHDIR"glib-2.22.4.tar.gz
tar zxf "$ARCHDIR"pango-1.28.0.tar.gz
cd pkg-config-0.23/
./configure --prefix="$DESTDIR"
make
make install
cd ../libpng-1.4.0/
./configure --prefix="$DESTDIR"
make
make install
cd ..
cd pixman-0.17.14/
./configure --prefix="$DESTDIR"
make
make install
cd ../cairo-1.9.6/
env PATH="$DESTDIR"/bin:"$PATH" ./configure --disable-xlib --disable-ft --disable-fc --prefix="$DESTDIR"
make
make install
cd ../gettext-0.17/
./configure --prefix="$DESTDIR"
make
make install
cd ../glib
cd ../glib-2.22.4/
env PATH="$DESTDIR"/bin:"$PATH" CFLAGS=-I"$DESTDIR"/include LDFLAGS=-L"$DESTDIR"/lib ./configure --prefix="$DESTDIR"
make
make install
cd ../pango-1.28.0/
env PATH="$DESTDIR"/bin:"$PATH" ./configure --without-x --with-included-modules=yes --with-dynamic-modules=no --prefix="$DESTDIR"
make
make install
