#lang racket/base
(require racket/cmdline)

(provide build-command-line
         m32? win? mac? ppc?
         archives-dir)

(define m32? 'unknown)
(define win? 'unknown)
(define mac? 'unknown)
(define ppc? #f)

(define archives-dir (current-directory))

(define-syntax-rule (build-command-line c ...)
  (let ()
    (define m32? 'unknown)
    (define win? 'unknown)
    (define mac? 'unknown)
    (define ppc? (regexp-match? #rx"ppc" (system-library-subpath #f)))
    (define archives-dir (current-directory))
    (begin0
     (command-line
      #:once-any
      [("--win") "build Windows via MinGW"
       (set! win? #t)]
      [("--mac") "build for/on Mac OS X"
       (set! win? #f)]
      #:once-any
      [("--m32") "build 32-bit mode x86/PowerPC"
       (set! m32? #t)]
      [("--m64") "build 64-bit mode x86_64"
       (set! m32? #f)]
      [("--mppc") "build 32-bit mode PowerPC"
       (set! m32? #t)
       (set! ppc? #t)]
      #:once-each
      [("--archives") dir "Find archives in <dir>"
       (set! archives-dir dir)]
      c ...)
     (when (eq? win? 'unknown)
       (error 'build "please pick `--win` or `--mac`"))
     (when (eq? m32? 'unknown)
       (error 'build "please pick `--m32` or `--m64`"))
     (when (and ppc? (not m32?))
       (error 'build "cannot use `--m64` on PowerPC"))
     (set! mac? (not win?))
     (install! m32? win? mac? (and mac? ppc?) archives-dir))))

(define (install! -m32? -win? -mac? -ppc? -archives-dir)
  (set! m32? -m32?)
  (set! win? -win?)
  (set! mac? -mac?)
  (set! ppc? -ppc?)
  (set! archives-dir -archives-dir))
