#lang scribble/doc
@(require scribble/manual "private/utils.rkt")

@(define (copyright . strs) (apply verbatim #:indent 2 strs))

@main-page['license]

Racket software and documentation is distributed under the GNU Lesser
General Public License (LGPL).  This means

@itemize[

 @item{You can link Racket software into
       proprietary applications, provided you follow the specific
       rules stated in the LGPL.}

 @item{You can modify Racket software. If you distribute a modified
       version, you must distribute it under the terms of the LGPL,
       which in particular means that you must release the source code
       for the modified software.}

]

See @filepath{doc/release-notes/COPYING.txt} in your Racket installation
for more information.

@copyright{
  Racket
  Copyright (c) 2010-2013 PLT Design Inc.
}

Racket software includes or extends the following copyrighted material:

@copyright{
  DrScheme
  Copyright (c) 1995-2003 PLT
  Copyright (c) 2004-2010 PLT Design Inc.
  All rights reserved.
}

@copyright{
  MrEd
  Copyright (c) 1995-2003 PLT
  Copyright (c) 2004-2010 PLT Design Inc.
  All rights reserved.
}

@copyright{
  MzScheme
  Copyright (c) 1995-2003 PLT
  Copyright (c) 2004-2010 PLT Design Inc.
  All rights reserved.
}

@copyright{
  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
}

@copyright{
  Conservative garbage collector
  Copyright (c) 1988, 1989 Hans-J. Boehm, Alan J. Demers
  Copyright (c) 1991-1996 Xerox Corporation
  Copyright (c) 1996-1999 Silicon Graphics
  Copyright (c) 1999-2005 Hewlett-Packard Development Company, L.P.
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
  GNU MPFR Library
  Copyright (c) 2000-2012
  Free Software Foundation, Inc.
  Contributed by the AriC and Caramel projects, INRIA.
}

@copyright{
  GNU lightning
  Copyright (c) 1994, 1995, 1996, 1999, 2000, 2001, 2002
  Free Software Foundation, Inc.
}

@copyright{
  libunwind
  Copyright (c) 2003-2005 Hewlett-Packard Development Company, L.P.
}

@copyright{
  PSMTabBarControl
  John Pannell, Robert Payne, Adam Strzelecki, Dorian Johnson
  https://github.com/dorianj/PSMTabBarControl
}

@copyright{
  Clanbomber icon and Stop icons
  Everaldo Coelho
  http://www.everaldo.com/
}

@copyright{
  Save icon
  David Vignoni
  http://icon-king.com/
}
