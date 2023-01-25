#lang scribble/doc
@(require scribble/manual
          racket/cmdline
          racket/match
          "private/utils.rkt")

@(define mode
   (let ([mode 'docs])
     (command-line
      #:argv (for/list ([arg (in-vector (current-command-line-arguments))]
                        ;; hack: with `raco setup`, might be e.g. #("setup")
                        #:when (member arg '("--repo-root-dir" "--repo-src-dir")))
               arg)
      #:usage-help
      "Use with “scribble ++arg” to generate license files for the source repository,"
      "rather than the license page for the installed documentation."
      #:once-any
      [("--repo-root-dir") "Generate “LICENSE.txt” for the Git repository root."
       (set! mode 'root)]
      [("--repo-src-dir") "Generate “LICENSE.txt” for the “racket/src” directory."
       (set! mode 'src)]
      #:args ()
      mode)))

@(define-syntax-rule (if-repo for-repo for-docs)
   ;; For text that should be different in a LICENSE.txt file
   ;; in the source tree than in installed documentation.
   (if (eq? 'docs mode)
       for-docs
       for-repo))
@(define-syntax-rule (when-repo arg ...)
   (if-repo (list arg ...) null))
@(define-syntax-rule (unless-repo arg ...)
   (if-repo null (list arg ...)))

@(define (src-filepath . args)
   (match mode
     ['root
      (apply filepath "racket/src/" args)]
     ['src
      (apply filepath args)]
     ['doc
      (error 'src-filepath "not supported in 'doc mode")]))

@; ---------------------------------------------------------------------------------------------------

@unless-repo[@main-page['license]]

Racket is distributed under the MIT license and the Apache License,
version 2.0, at your option. However, the Racket runtime system includes
components distributed under other licenses. In short:

@itemize[

 @item{
  The Racket @tech[#:indirect? #t #:doc '(lib "scribblings/reference/reference.scrbl")]{CS}
  runtime system embeds Chez Scheme, which is
  distributed under the Apache License, version 2.0.
  @when-repo{This runtime system is built from code in
   @src-filepath{cs} and @src-filepath{ChezScheme}.}
 }

 @item{
  The Racket @tech[#:indirect? #t #:doc '(lib "scribblings/reference/reference.scrbl")]{BC}
  runtime system includes code distributed under the
  GNU Lesser General Public License, either version 3 of the license,
  or (at your option) any later version.
  @when-repo{This runtime system is built from code in @src-filepath{bc}.}
 }

 ]

Except for Windows executables that are created with the ``embed DLLs''
option, the runtime system remains separate as a shared library or
additional executable, which means that it is dynamically linked and
can be replaced with a modified variant by users.

@(for/list ([abbrev (in-list '("APACHE" "MIT" "LGPL"))]
            [name (in-list '("Apache License, version 2.0"
                             "MIT license"
                             "GNU Lesser General Public License, version 3"))])
   @para{
 See @if-repo[@elem{the file @src-filepath{LICENSE-@|abbrev|.txt}}
              @elem{
                @filepath{LICENSE-@|abbrev|.txt} in your Racket installation's
                @filepath{share} directory}]
 for the full text of the @|name|.
 })

The source code of Racket is available at
@url{https://github.com/racket/racket} and
@url{https://download.racket-lang.org/releases}.

The Racket runtime system includes or extends
several components which have their own licenses.

The following are used in all Racket executables:

@itemize[
 @item{
  SHA-224 and SHA-256 implementation from Mbed TLS @(linebreak)
  Copyright © 2006--2015, ARM Limited, All Rights Reserved @(linebreak)
  Mbed TLS is licensed under the Apache License, version 2.0.
  @when-repo[(linebreak)]{Code from Mbed TLS can be found in
   @src-filepath{rktio/rktio_sha2.c} and @src-filepath{zuo/zuo.c}.}
 }
 @item{
  ZLib @(linebreak)
  Copyright © 1995--2022 Jean-loup Gailly and Mark Adler @(linebreak)
  Zlib is distributed under a liberal license: see @url{https://zlib.net}. @(linebreak)
  Code translated from earlier versions of Zlib can be found in
  @(nonbreaking
    (cond
      [(eq? 'root mode)
       @filepath{racket/collects/file/{gzip,gunzip}.rkt}]
      ;; no special case for 'src because the relative path will be
      ;; different in the repository than in a source distribution.
      [else
       ;; cf. path->relative-string/library
       @filepath{<collects>/file/{gzip,gunzip}.rkt}])).
  Racket CS executables also use Zlib via Chez Scheme@when-repo{:
   a copy of Zlib can be found in @src-filepath{ChezScheme/zlib}}.
  Additionally, on some platforms, Zlib is packaged with the support
  libraries for @racketmodname[racket/draw #:indirect].
 }
 @item{
  Terminal support from Chez Scheme @(linebreak)
  Both Racket BC and Racket CS use primitive terminal support code from
  Chez Scheme's expression editor, which is based on a command-line editor for
  Scheme developed from 1989 through 1994 by C@._ David Boyer.
  @when-repo{This code can be found in @src-filepath{ChezScheme/c/expeditor.c}.} @;
  The expression editor, like the rest of Chez Scheme, is licensed
  under the Apache License, version 2.0.
 }
  @item{
  Startup path support from LLVM @(linebreak)
  The implementation of the C API function
  @seclink[#:indirect? #t #:doc '(lib "scribblings/inside/inside.scrbl") "cs-self-exe"]{
   @racketplainfont{racket_get_self_exe_path}} in Racket CS and related internal
  functions in Racket BC includes code from the LLVM Project, which is licensed
  under the Apache License, version 2.0, with
  @hyperlink["https://spdx.org/licenses/LLVM-exception.html"]{LLVM exceptions}.
  @when-repo{Code adapted from the LLVM Project can be found in @src-filepath{start/self_exe.inc}.}
 }
 ]

The following are used in all Racket executables for Windows:

@itemize[
 @item{
  MemoryModule @(linebreak)
  Copyright © 2004--2015 by Joachim Bauch / mail@"@"joachim-bauch.de
  @url{https://www.joachim-bauch.de} @(linebreak)
  MemoryModule is licensed under the Mozilla Public License, version 2.0:
  see @url{https://www.mozilla.org/en-US/MPL/2.0/} for the full text of the license.
  @(if-repo
    @elem{Code from MemoryModule can be found in @src-filepath{start/MemoryModule.{c,h}}.}
    @elem{The source code of MemoryModule is included with the source code of Racket.})
 }
 ]

The following are used only in Racket BC executables:

@itemize[
 @item{
  libscheme @(linebreak)
  Copyright © 1994 Brent Benson @(linebreak)
  All rights reserved. @(linebreak)
  See the file @if-repo[
 @src-filepath{LICENSE-libscheme.txt}
 @elem{@filepath{LICENSE-libscheme.txt} in your Racket installation's
    @filepath{share} directory}]
  for the full text of the libscheme license.
  @when-repo{Code from libscheme can be found in @src-filepath{bc/src}.}
 }
 @item{
  GNU Lightning @(linebreak)
  Copyright © 1994, 1995, 1996, 1999, 2000, 2001, 2002, 2011
  Free Software Foundation, Inc. @(linebreak)
  GNU Lightning is distributed under the GNU Lesser General
  Public License, version 3, or (at your option) any later version. @(linebreak)
  @(if-repo
    @elem{A fork of GNU Lightning can be found in @src-filepath{bc/src/lightning}.}
    @elem{
   The source code of Racket's variant of GNU Lightning is included
   with the source code of Racket.
   })
 }
 @item{
  GNU MP Library @(linebreak)
  Copyright © 1991, 1992, 1993, 1994, 1996, 1999, 2000, 2007
  Free Software Foundation, Inc. @(linebreak)
  GMP is distributed under the GNU Lesser General
  Public License, version 3, or (at your option) any later version. @(linebreak)
  @(if-repo
    @elem{Code from GMP can be found in @src-filepath{bc/src/gmp}.}
    @elem{Source code from GMP is included with the source code of Racket.})
 }
 @item{
  libunwind @(linebreak)
  Copyright © 2003--2005 Hewlett-Packard Development Company, L.P. @(linebreak)
  libunwind is distributed under the MIT license.
  @when-repo[(linebreak)]{Code from libunwind can be found in @src-filepath{bc/src/unwind}.}
 }
 @item{
  libffi @(linebreak)
  Copyright © 1996--2019 Anthony Green, Red Hat, Inc and others. @(linebreak)
  libffi is distributed under the MIT license.
  @when-repo[(linebreak)]{A copy of libffi can be found in @src-filepath{bc/foreign/libffi}.}
 }
 @item{
  random.inc @(linebreak)
  Based on @filepath{random.c} from FreeBSD 2.2. @(linebreak)
  Copyright © 1983, 1993 The Regents of the University of California. @(linebreak)
  This code is distributed under a three-clause BSD license.
  @when-repo[(linebreak)]{Code from FreeBSD 2.2 can be found in @src-filepath{bc/src/random.inc}.}
 }
 ]

In addition to the Racket runtime system, the default mode
of building Racket will install some packages, which are
distributed under their own licenses.

The Racket code for all packages in the main Racket
distribution is primarily under the MIT license and the
Apache License, version 2.0, at your option, but some
packages contain Racket code under other permissive licenses.

Some components of Racket are implemented using additional
shared libraries. For some platforms and installation modes
(especially for Windows and Mac OS), these libraries are
distributed in platform-specific Racket packages; in other
cases, they may be included with the operating system or
installable via the system's package manager. In all cases,
these libraries are dynamically loaded and can be replaced
with modified variants by users. In particular:

@itemize[
 @item{
  Even the ``minimal Racket'' distribution may include shared libraries
  for @seclink[#:indirect? #t #:doc '(lib "openssl/openssl.scrbl") "top"]{
   OpenSSL}, @seclink[#:indirect? #t #:doc '(lib "db/scribblings/db.scrbl") "sqlite3-requirements"]{
   SQLite}, and/or @seclink[#:indirect? #t #:doc '(lib "readline/readline.scrbl") "top"]{
   Editline}, depending on the platform.

  The Windows distribution also includes GNU libiconv, which is
  distributed under the GNU Lesser General Public License,
  either version 3 of the license, or (at your option) any
  later version. See the @tech[#:indirect? #t #:doc '(lib "scribblings/reference/reference.scrbl")]{
   byte converter} documentation for further details.
 }
 @item{
  @seclink[#:indirect? #t #:doc '(lib "scribblings/draw/draw.scrbl") "libs"]{
   The Racket Drawing Toolkit} is implemented using Cairo,
  Pango, HarfBuzz, FriBidi, FreeType, FontConfig, and Glib.
  Some of these libraries are distributed under the
  GNU Lesser General Public License: see the applicable
  platform-specific Racket packages for details.
 }
 @item{
  @seclink[#:indirect? #t #:doc '(lib "scribblings/gui/gui.scrbl") "libs"]{
   The Racket Graphical Interface Toolkit} is primarily
  implemented using @racketmodname[racket/draw #:indirect] and
  the native GUI framework for each platform, but can be configured to always
  use GTK, instead. @;{TODO: When do we use ATK? We package it for Windows and Mac OS.}
  On Mac OS, @racketmodname[racket/gui #:indirect] also uses the libraries
  MMTabBarView and/or PSMTabBarControl, which are distributed under a three-clause
  BSD license.
 }
 @item{
  The @racketmodname[math/bigfloat #:indirect] library uses GNU MP and MPFR,
  which are distributed under the GNU Lesser General Public License, either
  version 3 of the license, or (at your option) any later version.
 }
 @item{
  Files generated by
  @seclink[#:indirect? #t #:doc '(lib "scribblings/scribble/scribble.scrbl") "top"]{
   Scribble}'s LaTeX renderer use LaTeX packages with various licenses,
  such as the LaTeX Project Public License. Some of these are distributed
  with the Racket package @racket["scribble-lib"]; others may be installed
  as part of your TeX distribution.
 }
 ]

For further details about the licenses for these libraries,
see the @filepath{LICENSE.txt} files included with the
applicable platform-specific packages.

Similarly, your Racket distribution or installation may have been configured
with additional packages, including third-party packages, which could use
other licenses: see the documentation or @filepath{LICENSE.txt} file included
with each package for information about the applicable licenses.

@when-repo{
 The Racket source distribution also includes build scripts
 generated by GNU Autoconf which are under various licenses,
 including the GNU General Public License with Autoconf exception;
 however, these files are not installed with Racket.
}

@(if (eq? 'root mode)
     @list{
 Finally, this Git repository also contains (in the @racket["racket-benchmarks"]
 package) the following benchmarks based on third-party code which are not part of
 the standard Racket distribution:

 @itemlist[
 @item{
   psyntax (Portable implementation of syntax-case) @(linebreak)
   By R@._ Kent Dybvig, Oscar Waddell, Bob Hieb, and Carl Bruggeman @(linebreak)
   psyntax was extracted from Chez Scheme, which is distributed under the
   Apache License, version 2.0; see also the file header for the original
   permissive license.
   Code from psyntax can be found in
   @filepath{pkgs/racket-benchmarks/tests/racket/benchmarks/common/psyntax.sch}.
  }
 @item{
   SCM @(linebreak)
   Copyright (C) 1991, 1993, 1994, 1995 Free Software Foundation, Inc. @(linebreak)
   SCM is distributed under the GNU Lesser General
   Public License, version 3, or (at your option) any later version. @(linebreak)
   Code from SCM can be found in
   @filepath{pkgs/racket-benchmarks/tests/racket/benchmarks/shootout/pidigits1.rkt}.
  }
 ]}
     null)
