#lang setup/infotab

(define collection 'multi)

(define deps '("draw-lib"
               "snip-lib"
               "wxme-lib"
               "pict-lib"
               "scribble-lib"
               "string-constants-lib"
               "unstable-list-lib" ; for class-iop
               "unstable-options-lib"
               ("gui-i386-macosx" #:platform "i386-macosx")
               ("gui-x86_64-macosx" #:platform "x86_64-macosx")
               ("gui-win32-i386" #:platform "win32\\i386")
               ("gui-win32-x86_64" #:platform "win32\\x86_64")))
