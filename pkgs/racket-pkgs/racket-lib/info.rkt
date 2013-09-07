#lang info

(define collection 'multi)

(define deps
  '(("racket-win32-i386" #:platform "win32\\i386")
    ("racket-win32-x86_64" #:platform "win32\\x86_64")
    ("db-ppc-macosx" #:platform "ppc-macosx")
    ("db-win32-i386" #:platform "win32\\i386")
    ("db-win32-x86_64" #:platform "win32\\x86_64")
    ("com-win32-i386" #:platform "win32\\i386")
    ("com-win32-x86_64" #:platform "win32\\x86_64")))

(define pkg-desc "Combines platform-specific native libraries that are useful for base Racket")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))
