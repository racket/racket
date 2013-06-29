#lang setup/infotab

(define collection 'multi)

(define deps '("base"
               "compatibility-lib"
               "draw-lib"
               "errortrace-lib"
               "macro-debugger-text-lib"
               "parser-tools-lib"
               "pconvert-lib"
               "pict-lib"
               "profile-lib"
               "sandbox-lib"
               "scribble-lib"
               "slideshow-lib"
               "snip-lib"
               "string-constants-lib"
               "typed-racket-lib"
               "unstable-contract-lib"
               "wxme-lib"
               "gui-lib"
               "racket-index"
               "racket-doc"
               "html-lib"
               "images"
               "icons"
               "typed-racket-more"
               "trace"
               "macro-debugger"
               "srfi-lib"
               "srfi-doc"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))
