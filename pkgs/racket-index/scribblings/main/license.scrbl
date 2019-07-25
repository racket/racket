#lang scribble/doc
@(require scribble/manual "private/utils.rkt")

@(define (copyright . strs) (apply verbatim #:indent 2 strs))

@main-page['license]

Racket is distributed under the MIT license and the Apache version 2.0
license, at your option. However, the Racket runtime system includes
components distributed under other licenses. In short:

@itemize[

 @item{The traditional Racket runtime system includes code distributed
       under the GNU Lesser General Public License, version 3.}

 @item{The Racket on Chez Scheme runtime system embeds Chez Scheme, which
       is distributed under the Apache version 2.0 license.}

]

Except for Windows executables that are created with the ``embed DLLs''
option, the runtime system remains separate as a shared library or
additional executable, which means that it is dynamically linked and
can be replaced with a modified variant by users.

See @filepath{LICENSE-LGPL.txt} in your Racket installation's
@filepath{share} directory for the full text of the GNU Lesser General
Public License.

See @filepath{LICENSE-APACHE.txt} in your Racket installation's
@filepath{share} directory for the full text of the Apache version 2.0
license.

See @filepath{LICENSE-MIT.txt} in your Racket installation's
@filepath{share} directory for the full text of the MIT license.

Racket software includes or extends the following copyrighted material:

@copyright{
  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
}

@copyright{
  Collector C++ extension by Jesse Hull and John Ellis
  Copyright (c) 1994 Xerox Corporation
  All rights reserved.
}

@copyright{
  GNU MP Library
  Copyright (c) 1991, 1992, 1993, 1994, 1996, 1999, 2000, 2007
  Free Software Foundation, Inc.
}

@copyright{
  GNU lightning
  Copyright (c) 1994, 1995, 1996, 1999, 2000, 2001, 2002, 2011
  Free Software Foundation, Inc.
}

@copyright{
  libunwind
  Copyright (c) 2003-2005 Hewlett-Packard Development Company, L.P.
}

@copyright{
  MemoryModule
  Copyright (c) 2004-2015 by Joachim Bauch / mail@"@"joachim-bauch.de
  http://www.joachim-bauch.de
}

@copyright{
  SHA-224 and SHA-256 implementation from mbed TLS
  Copyright (c) 2006-2015, ARM Limited, All Rights Reserved
}

See also other @filepath{LICENSE.txt} files in your distribution or
packages.
