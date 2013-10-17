#!/bin/sh
#|
exec racket -qu "$0" ${1+"$@"}
|#

;; Benchmark harness for the shootout benchmarks
;; Mostly taken from the common benchmark harness

(module auto scheme/base
  (require (for-syntax scheme/base)
           mzlib/process
           "../common/cmdline.rkt"
           mzlib/list
           mzlib/compile
           mzlib/inflate
           mzlib/date
           mzlib/port
           mzlib/file
           dynext/file
           syntax/toplevel
           scheme/runtime-path
           compiler/find-exe)

  ;; Implementaton-specific control functions ------------------------------

  (define (bytes->number b)
    (string->number (bytes->string/latin-1 b)))

  (define ((run-mk script) bm)
    (when (file-exists? (symbol->string bm))
      (delete-file (symbol->string bm)))
    (parameterize ([current-command-line-arguments (vector (symbol->string bm))])
      (namespace-require 'scheme)
      (load script)))

  (define (mk-racket bm)
    (unless (directory-exists? "compiled")
      (make-directory "compiled"))
    (parameterize ([current-namespace (make-base-namespace)]
                   [read-accept-reader #t])
      (let ([name (format "~a.rkt" bm)])
        (compile-file name
                      "compiled/current-bm_rkt.zo"))))

  (define (clean-up-zo bm)
    (when (file-exists? "compiled/current-bm_rkt.zo")
      (delete-file "compiled/current-bm_rkt.zo")))

  (define (mk-typed-racket-non-optimizing bm)
    (unless (directory-exists? "typed/compiled")
      (make-directory "typed/compiled"))
    (parameterize ([current-namespace (make-base-namespace)]
                   [read-accept-reader #t])
      (let ([name (format "~a-non-optimizing.rkt" bm)])
        (compile-file (format "typed/~a" name)
                      "typed/compiled/current-bm_rkt.zo"))))
  (define (mk-typed-racket bm)
    (unless (directory-exists? "typed/compiled")
      (make-directory "typed/compiled"))
    (parameterize ([current-namespace (make-base-namespace)]
                   [read-accept-reader #t])
      (let ([name (format "~a-optimizing.rkt" bm)])
        (compile-file (format "typed/~a" name)
                      "typed/compiled/current-bm_rkt.zo"))))

  (define (clean-up-typed bm)
    (when (file-exists? "typed/compiled/current-bm_rkt.zo")
      (delete-file "typed/compiled/current-bm_rkt.zo")))

  (define (extract-racket-times bm str)
    (let ([m (regexp-match #rx#"cpu time: ([0-9]+) real time: ([0-9]+) gc time: ([0-9]+)" str)])
      (map bytes->number (cdr m))))

  ;; Table of implementatons and benchmarks ------------------------------

  (define-struct impl (name setup make run extract-result clean-up skips))

  (define impls
    (list
     (make-impl 'racket
                void
                mk-racket
                (lambda (bm)
                  (system* (find-exe) "run.rkt" bm "racket"))
                extract-racket-times
                clean-up-zo
                '())
     (make-impl 'typed-racket-non-optimizing
                void
                mk-typed-racket-non-optimizing
                (lambda (bm)
                  (system* (find-exe) "run.rkt" bm "typed-racket-non-optimizing"))
                extract-racket-times
                clean-up-typed
                '())
     (make-impl 'typed-racket
                void
                mk-typed-racket
                (lambda (bm)
                  (system* (find-exe) "run.rkt" bm "typed-racket"))
                extract-racket-times
                clean-up-typed
                '())
     ))

  (define benchmarks
    '(ackermann
      ary
      binarytrees
      chameneos
      cheapconcurrency
      echo
      except
      fannkuch
      fannkuch-redux
      fasta
      fibo
      hash
      hash2
      heapsort
      hello
      k-nucleotide
      lists
      mandelbrot
      mandelbrot-generic
      matrix
      meteor
      moments
      nbody
      nbody-generic
      nbody-vec
      nbody-vec-generic
      nestedloop
      nothing
      nsieve
      nsievebits
      partialsums
      pidigits
      pidigits1
      random
      recursive
      regexmatch
      regexpdna
      reversecomplement
      reversefile
      sieve
      spectralnorm
      spectralnorm-generic
      strcat
      sumcol
      ;; thread-ring ; calls exit, so won't display running time
      wc
      wordfreq))

  (define without-input-benchmarks
    '(spellcheck))

  (define (run-benchmark impl bm)
    (let ([i (ormap (lambda (i)
                      (and (eq? impl (impl-name i))
                           i))
                    impls)])
      (if (memq bm (impl-skips i))
          (rprintf "[~a ~a ~s #f]\n" impl bm '(#f #f #f))
          (begin
            ((impl-setup i) bm)
            (let ([start (current-inexact-milliseconds)])
              ((impl-make i) bm)
              (let ([end (current-inexact-milliseconds)])
                (let loop ([n num-iterations])
                  (unless (zero? n)
                    (let ([out (open-output-bytes)])
                      (unless (parameterize ([current-output-port out]
                                             [current-error-port out])
                                ((impl-run i) 
                                 (if (symbol? bm)
                                   (format "~a" bm)
                                   bm)))
                        (error 'auto "~a\nrun failed ~a" (get-output-bytes out) bm))
                      (rprintf "[~a ~a ~s ~a]\n"
                               impl
                               bm
                               ((impl-extract-result i) bm (get-output-bytes out))
                               (inexact->exact (round (- end start)))))
                    (loop (sub1 n)))))
              ((impl-clean-up i) bm))))
      (flush-output)))

  ;; Extract command-line arguments --------------------

  (define-values (actual-benchmarks-to-run 
                  actual-implementations-to-run 
                  num-iterations)
    (process-command-line benchmarks
                          '()
                          (map impl-name impls) '() ; no obsolete implementations here
                          3))

  (define-runtime-path bm-directory ".")
  
  ;; Run benchmarks -------------------------------

  #;(rprintf "; ~a\n" (date->string (seconds->date (current-seconds)) #t))

  (parameterize ([current-directory bm-directory])
    (for-each (lambda (impl)
                (map (lambda (bm)
                       (run-benchmark impl bm))
                     actual-benchmarks-to-run))
              actual-implementations-to-run)))
