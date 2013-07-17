#lang at-exp racket/base
(require racket/format)

(provide make-readme)

(define (make-readme config)
  @~a{
      The Racket Programming Language
      ===============================

      This is Racket...

      More Information
      ----------------
     
      Visit us at
         http://racket-lang.org/ 
      for more Racket resources.
     
     
      License
      -------
     
      Racket
      Copyright (c) 2010-2013 PLT Design Inc.
     
      Racket is distributed under the GNU Lesser General Public License
      (LGPL).  This means that you can link Racket into proprietary
      applications, provided you follow the rules stated in the LGPL.  You can
      also modify Racket; if you distribute a modified version, you must
      distribute it under the terms of the LGPL, which in particular means
      that you must release the source code for the modified software.  See
      lib/COPYING_LESSER.txt for more information.})

(define macosx-notes
  @~a{Install by dragging the enclosing Racket folder to your Applications folder
      --- or wherever you like. You can move the Racket folder at any time, but do not
      move applications or other files within the folder. If you want to use the
      Racket command-line programs, then (optionally) add the path of the "bin"
      subdirectory to your PATH environment variable.})

(define drracket-more-info
  @~a{For Racket documentation, use DrRacket's `Help' menu, run the `Racket
      Documentation' application (Windows or Mac OS X), or run `raco docs'
      from a command line.})
