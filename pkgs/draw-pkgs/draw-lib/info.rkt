#lang info

(define collection 'multi)

(define deps
  '("base"
    ("draw-i386-macosx-2" #:platform "i386-macosx")
    ("draw-x86_64-macosx-2" #:platform "x86_64-macosx")
    ("draw-ppc-macosx-2" #:platform "ppc-macosx")
    ("draw-win32-i386-2" #:platform "win32\\i386")
    ("draw-win32-x86_64-2" #:platform "win32\\x86_64")))

(define pkg-desc "implementation (no documentation) part of \"draw\"")

(define pkg-authors '(mflatt))

(define version "1.1")
