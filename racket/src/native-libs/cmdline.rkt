#lang racket/base
(require racket/cmdline)

(provide build-command-line
         win? mac? linux?
         i386? x86_64? ppc? aarch64?
         archives-dirs)

(define win? #f)
(define linux? #f)
(define mac? #f)

(define i386? #f)
(define x86_64? #f)
(define ppc? #f)
(define aarch64? #f)

(define archives-dirs #f)

(define-syntax-rule (build-command-line c ...)
  (let ()
    (define win? 'unknown)
    (define mac? #f)
    (define linux? #f)
    (define i386? 'unknown)
    (define x86_64? #f)
    (define ppc? #f)
    (define aarch64? #f)
    (define archives-dirs #f)
    (begin0
     (command-line
      #:once-any
      [("--win") "build Windows via MinGW"
       (set! win? #t)]
      [("--mac") "build for/on Mac OS"
       (set! win? #f)
       (set! mac? #t)]
      [("--linux") "build for/on Linux"
       (set! win? #f)
       (set! linux? #t)]
      #:once-any
      [("--i386") "build i386"
       (set! i386? #t)]
      [("--x86_64") "build x86_64"
       (set! i386? #f)
       (set! x86_64? #t)]
      [("--ppc") "build 32-bit PowerPC"
       (set! i386? #f)
       (set! ppc? #t)]
      [("--aarch64") "build AArch64/Arm64"
       (set! i386? #f)
       (set! aarch64? #t)]
      #:multi
      [("--archives") dir "Find archives in <dir>"
       (set! archives-dirs (cons dir (or archives-dirs null)))]
      #:once-each
      c ...)
     (when (eq? win? 'unknown)
       (error 'build "please pick `--win`, `--mac`, or `--linux`"))
     (when (eq? i386? 'unknown)
       (error 'build "please pick `--i386`, `--x86_64`, `--ppc`, or `--aarch64`"))
     (install! win? mac? linux? i386? x86_64? ppc? aarch64?
               (reverse (or archives-dirs
                            (list (current-directory))))))))

(define (install! -win? -mac? -linux? -i386? -x86_64? -ppc? -aarch64? -archives-dirs)
  (set! win? -win?)
  (set! mac? -mac?)
  (set! linux? -linux?)
  (set! i386? -i386?)
  (set! x86_64? -x86_64?)
  (set! ppc? -ppc?)
  (set! aarch64? -aarch64?)
  (set! archives-dirs -archives-dirs))
