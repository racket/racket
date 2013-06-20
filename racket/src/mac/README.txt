Information on building 3rd-party libraries needed for Mac OS X GRacket.

Get these packages (or newer, if compatible):
 pkg-config-0.25.tar.gz  [PowerPC: pkg-config-0.23.tar.gz]
 libpng-1.5.7.tar.gz 
 pixman-0.24.2.tar.gz    [PowerPC: pixman-0.21.6.tar.gz]
 cairo-1.10.2.tar.gz
 gettext-0.18.1.1.tar.gz [PowerPC: gettext-0.17.tar.gz]
 libffi-3.0.10.tar.gz    [PowerPC: skip]
 glib-2.31.14.tar.gz     [PowerPC: glib-2.22.4.tar.gz]
 pango-1.29.5.tar.gz     [PowerPC: pango-1.28.0.tar.gz]
 gmp-5.0.5.tar.gz
 mpfr-3.1.1.tar.gz
 libjpeg62 (maybe in binary form)

 PSMTabBarControl, probably from
                      https://github.com/dorianj/PSMTabBarControl
                      [PowerPC: maccode.googlecode.com]
                   and handled differently; note that the version
                   at from maccode has a bug on dealloc() and
                   uses methods that are now deprecated


Patches:
 cairo/src/cairo-path-fixed.c:1295: [from Cairo repo, 3/18/11]
   /* This check is valid because the current implementation of
     * _cairo_path_fixed_is_box () only accepts rectangles like:
     * move,line,line,line[,line|close[,close|move]]. */
    buf = cairo_path_head (path);
    if (buf->num_ops > 4)
	return TRUE;
 glib/glib/gconvert.c:61: comment out #error near
   #if defined(USE_LIBICONV_GNU) && !defined (_LIBICONV_H)
 pango/pango/modules.c:573: change to
   // read_modules ();
 pango/modules/basic/basic-atsui.c,
   pango/pangocairo-atsuifont.c,
   pango/pangoatsui.h,
   pango/pangoatsui.c:
     apply "atsui.patch" (32-bit only)
 pango/modules/basic/basic-coretext.c,
   pango/pangocairo-coretextfont.c:
     apply "coretext.patch" (64-bit only)
 gettext/gettext-tools/gnulib-lib/stpncpy.c:28: may need to comment out
     // # define __stpncpy stpncpy
 PowerPC: PSMTabBarControl/PSMTabBarControl.m:216: change to
     // copy _cells because removing a cell
     // can modify the array (which is not allowed)
     NSArray *copyOfCells = [NSArray arrayWithArray: _cells];
     NSEnumerator *enumerator = [copyOfCells objectEnumerator];
 gcc-4.0: gmp/gmp.h:424: __gnu_inline__ => __weak__

Configures (where <dest> is some temporary area):
  pkg-config: --prefix=<dest>
  libpng: --prefix=<dest>
  pixman: --prefix=<dest>
  Cairo: PATH=<dest>/bin:... --disable-xlib --disable-ft --disable-fc --prefix=<dest>
  gettext: --prefix=<dest>
  libffi: --prefix=<dest>
  glib: PATH=<dest>/bin:... CFLAGS=-I<dest>/include LDFLAGS=-L<dest>/lib --prefix=<dest>
   [PowerPC: after configure, set "allow_undefined_flag" to "" in "libtool";
             add "-lresolv" to link command for "libgio"]
  Pango: PATH=<dest>/bin:... --without-x --with-included-modules=yes --with-dynamic-modules=no --prefix=<dest>
   [PowerPC: same as glib for "libtool"; use PATH for `make', too]
  gmp: --prefix=<dest>
  mpfr: CFLAGS=-I<dest>/include LDFLAGS=-L<dest>/lib --prefix=<dest>

 To support 10.4, add
  CC=gcc-4.0 
  CPPFLAGS="-isysroot /Developer/SDKs/MacOSX10.4u.sdk -mmacosx-version-min=10.4"
  LDFLAGS="-isysroot /Developer/SDKs/MacOSX10.4u.sdk -mmacosx-version-min=10.4"
 for all packages.

 Note: PATH above ensures that pkg-config binaries are used to find
 things in <dest> rather than some other area, such as /opt/local.

 Be sure to use two-level namespaces for all libraries. While
 two-level namespaces are the default on Mac OS X, some packages turn
 on flat namespaces, at least on PowerPC. Use `otol -vh' and look for
 "TWOLEVEL" in the output to check that a library uses two-level
 namespaces.

XCode:
 Build PSMTabBarControl. You only need the Framework target, and
 in Release mode (which is "Build for Archiving" in Xcode 4.5).
 Use `ditto' to reduce the framework to one architecture.
 Add back the "LICENSE.txt" file.

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
# This script isn't right, because the versions are out of date
# and libffi isn't included, but maybe it clarifies some of
# the instructions above

DESTDIR=
WORKDIR=
ARCHDIR=

cd "$WORKDIR"
tar zxf "$ARCHDIR"pkg-config-0.23.tar.gz
tar zxf "$ARCHDIR"libpng-1.4.0.tar.gz
tar zxf "$ARCHDIR"pixman-0.21.6.tar.gz
tar zxf "$ARCHDIR"cairo-1.10.2.tar.gz
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
cd pixman-0.21.6/
./configure --prefix="$DESTDIR"
make
make install
cd ../cairo-1.10.2/
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
