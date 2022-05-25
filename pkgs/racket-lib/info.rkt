#lang info

(define collection 'multi)

(define deps
  '("base"
    ("racket-win32-i386-3" #:platform "win32\\i386")
    ("racket-win32-x86_64-3" #:platform "win32\\x86_64")
    ("racket-win32-arm64-3" #:platform "win32\\arm64")
    ("racket-x86_64-linux-natipkg-3" #:platform "x86_64-linux-natipkg")
    ("racket-x86_64-macosx-3" #:platform "x86_64-macosx")
    ("racket-i386-macosx-3" #:platform "i386-macosx")
    ("racket-ppc-macosx-3" #:platform "ppc-macosx")
    ("racket-aarch64-macosx-3" #:platform "aarch64-macosx")
    ("db-ppc-macosx" #:platform "ppc-macosx")
    ("db-win32-i386" #:platform "win32\\i386")
    ("db-win32-x86_64" #:platform "win32\\x86_64")
    ("db-win32-arm64" #:platform "win32\\arm64")
    ("db-x86_64-linux-natipkg" #:platform "x86_64-linux-natipkg")
    ("com-win32-i386" #:platform "win32\\i386")
    ("com-win32-x86_64" #:platform "win32\\x86_64")))

(define pkg-desc "Combines platform-specific native libraries that are useful for base Racket")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))

(define license
  '(Apache-2.0 OR MIT))
