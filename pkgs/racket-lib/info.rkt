#lang info

(define collection 'multi)

(define deps
  '(("racket-win32-i386-2" #:platform "win32\\i386")
    ("racket-win32-x86_64-2" #:platform "win32\\x86_64")
    ("racket-x86_64-linux-natipkg-2" #:platform "x86_64-linux-natipkg")
    ("racket-x86_64-macosx-2" #:platform "x86_64-macosx")
    ("racket-i386-macosx-2" #:platform "i386-macosx")
    ("racket-ppc-macosx-2" #:platform "ppc-macosx")
    ("db-ppc-macosx" #:platform "ppc-macosx")
    ("db-win32-i386" #:platform "win32\\i386")
    ("db-win32-x86_64" #:platform "win32\\x86_64")
    ("db-x86_64-linux-natipkg" #:platform "x86_64-linux-natipkg")
    ("com-win32-i386" #:platform "win32\\i386")
    ("com-win32-x86_64" #:platform "win32\\x86_64")))

(define pkg-desc "Combines platform-specific native libraries that are useful for base Racket")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))
