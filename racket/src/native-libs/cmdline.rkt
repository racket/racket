#lang racket/base
(require racket/cmdline)

(provide build-command-line
         m32? win? mac? linux? ppc? aarch64?
         archives-dirs)

(define m32? 'unknown)
(define win? 'unknown)
(define linux? #f)
(define mac? 'unknown)
(define ppc? #f)
(define aarch64? #f)

(define archives-dirs #f)

(define-syntax-rule (build-command-line c ...)
  (let ()
    (define m32? 'unknown)
    (define win? 'unknown)
    (define mac? 'unknown)
    (define linux? #f)
    (define ppc? (regexp-match? #rx"ppc" (system-library-subpath #f)))
    (define aarch64? (eq? 'aarch64 (system-type 'arch)))
    (define archives-dirs #f)
    (begin0
     (command-line
      #:once-any
      [("--win") "build Windows via MinGW"
       (set! win? #t)]
      [("--mac") "build for/on Mac OS"
       (set! win? #f)]
      [("--linux") "build for/on Linux"
       (set! win? #f)
       (set! linux? #t)]
      #:once-any
      [("--m32") "build 32-bit mode x86/PowerPC"
       (set! m32? #t)]
      [("--m64") "build 64-bit mode x86_64/AArch64"
       (set! m32? #f)]
      [("--mppc") "build 32-bit mode PowerPC"
       (set! m32? #t)
       (set! ppc? #t)]
      [("--maarch64") "build 64-bit mode AArch64"
       (set! m32? #f)
       (set! aarch64? #t)]
      #:multi
      [("--archives") dir "Find archives in <dir>"
       (set! archives-dirs (cons dir (or archives-dirs null)))]
      #:once-each
      c ...)
     (when (eq? win? 'unknown)
       (error 'build "please pick `--win`, `--mac`, or `--linux`"))
     (when (eq? m32? 'unknown)
       (error 'build "please pick `--m32`, `--m64`, `--mppc`, or `--maarch64`"))
     (when (and ppc? (not m32?))
       (error 'build "cannot use `--m64` on PowerPC"))
     (set! mac? (not (or win? linux?)))
     (install! m32? win? mac? linux? (and mac? ppc?) aarch64?
               (reverse (or archives-dirs
                            (list (current-directory))))))))

(define (install! -m32? -win? -mac? -linux? -ppc? -aarch64? -archives-dirs)
  (set! m32? -m32?)
  (set! win? -win?)
  (set! mac? -mac?)
  (set! linux? -linux?)
  (set! ppc? -ppc?)
  (set! aarch64? -aarch64?)
  (set! archives-dirs -archives-dirs))
