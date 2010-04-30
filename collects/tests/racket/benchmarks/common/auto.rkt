#!/bin/sh
#|
exec racket -qu "$0" ${1+"$@"}
|#

;; See "tabulate.rkt" for information on the output format

(module auto scheme/base
  (require (for-syntax scheme/base)
           mzlib/process
           "cmdline.rkt"
           mzlib/list
           mzlib/compile
           mzlib/inflate
           mzlib/date
           mzlib/port
           mzlib/file
           dynext/file
           syntax/toplevel
           scheme/runtime-path)

  ;; Implementaton-specific control functions ------------------------------

  (define (bytes->number b)
    (string->number (bytes->string/latin-1 b)))

  (define ((run-mk script) bm)
    (when (file-exists? (symbol->string bm))
      (delete-file (symbol->string bm)))
    (parameterize ([current-command-line-arguments (vector (symbol->string bm))])
      (namespace-require 'scheme)
      (load script)))

  (define (clean-up-bin bm)
    (delete-file (symbol->string bm)))

  (define (clean-up-o1 bm)
    (delete-file (format "~a.o1" bm)))

  (define (mk-racket bm) (void))
  #;
  (define (mk-racket bm)
    (unless (directory-exists? "compiled")
      (make-directory "compiled"))
    (parameterize ([current-namespace (make-base-namespace)]
                   [read-accept-reader #t])
      (let ([name (format "~a.rkt" bm)])
        (compile-file name
                      (build-path "compiled" (path-add-suffix name #".zo"))))))

  (define (clean-up-zo bm)
    (when (directory-exists? "compiled")
      (delete-directory/files "compiled")))

  (define (clean-up-nothing bm)
    (void))

  (define (mk-plt-r5rs bm)
    (with-output-to-file (format "~a.scm" bm)
      #:exists 'replace
      (lambda ()
        (printf "(load \"r5rs-wrap.rkt\")\n(load \"~a.sch\")\n" bm)))
    ;; To get compilation time:
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (namespace-require 'r5rs)
      (with-input-from-file (format "~a.sch" bm)
        (lambda ()
          (let loop ()
            (let ([e (read-syntax)])
              (unless (eof-object? e)
                (eval-compile-time-part-of-top-level/compile 
                 (namespace-syntax-introduce e))
                (loop))))))))

  (define (clean-up-plt-r5rs bm)
    (let ([f (format "~s.scm" bm)])
      (when (file-exists? f)
        (delete-file f))))

  (define (mk-racket-tl bm)
    ;; To get compilation time:
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'scheme/base)
      (eval '(define null #f)) ; for dynamic.sch
      (compile-file (format "~a.sch" bm))))

  (define (setup-larceny bm)
    (setup-sps bm "(larceny benchmarking)"))

  (define (mk-larceny bm)
    (parameterize ([current-input-port 
		    (open-input-string
		     (format (string-append
			      "(import (larceny compiler))\n"
			      "(compile-library \"~a.sls\")\n")
			     bm))]
		   [current-output-port (open-output-bytes)])
      (system "larceny -err5rs")
      ;; Make sure compiled version is used:
      (delete-file (format "~a.sls" bm))))

  (define (clean-up-fasl bm)
    (clean-up-sps bm)
    (delete-file (format "~a.slfasl" bm)))

  (define (mk-mzc bm)
    (parameterize ([current-output-port (open-output-bytes)])
      (system (format "mzc ~a.rkt" bm))))

  (define (clean-up-extension bm)
    (delete-file (append-extension-suffix (symbol->string bm))))

  (define (run-exe bm)
    (system (symbol->string bm)))

  (define (run-exe/time bm)
    (system (format "time ~a" bm)))

  (define (run-gambit-exe bm)
    (system (format "gsi -:d-,m10000 ~a.o1" bm)))

  (define (run-larceny bm)
    (system "larceny -r6rs -program prog.sps -path ."))

  (define (setup-sps bm lib)
    (with-output-to-file "prog.sps"
      #:exists 'truncate
      (lambda ()
        (printf "(import (~a))\n" bm)
        (printf "(bm-!-go)\n")))
    (with-output-to-file (format "~a.sls" bm)
      #:exists 'truncate
      (lambda ()
        (printf "(library (~a)\n" bm)
        (printf " (export bm-!-go)\n")
        (printf " (import (rnrs) (rnrs mutable-pairs) (rnrs mutable-strings) (rnrs r5rs) (rnrs eval) ~a)\n" lib)
        (printf " (define (bm-!-go) 'ok)\n")
        (call-with-input-file (format "~a.sch" bm)
          (lambda (in)
            (copy-port in (current-output-port))))
        (printf ")\n"))))

  (define (clean-up-sps bm)
    (delete-file "prog.sps")
    (let ([f (format "~a.sls" bm)])
      (when (file-exists? f)
        (delete-file f))))

  (define (setup-ikarus bm)
    (setup-sps bm "(ikarus)")
    (system "rm -rf ~/.ikarus"))
  
  (define (mk-ikarus bm)
    (system "ikarus --compile-dependencies prog.sps"))

  (define (run-ikarus bm)
    (system "ikarus --r6rs-script prog.sps"))

  (define (clean-up-ikarus bm)
    (clean-up-sps bm)
    (system "rm -rf ~/.ikarus"))

  (define (run-scheme48 bm)
    (parameterize ([current-input-port
                    (open-input-string
                     (format
                      ",bench on\n,open time bitwise\n,load \"scheme48-prelude.sch\"\n,load \"~a.sch\"\n,exit\n"
                      bm))])
      (system "scheme48 -h 20000000")))

  (define (extract-scheme48-times bm str)
    (let ([m (regexp-match #rx#"cpu time: ([0-9]+) real time: ([0-9]+)" str)]
          ;; `time' result is 10s of milliseconds? OS ticks, maybe?
          [msec/tick 10])
      (list (bytes->number (cadr m))
            (bytes->number (caddr m))
            0)))

  (define (mk-mit bm)
    (with-output-to-file (format "~a.scm" bm)
      #:exists 'truncate
      (lambda ()
        (printf "(declare (usual-integrations))\n")
        (call-with-input-file "mit-prelude.sch" 
          (lambda (in) (copy-port in (current-output-port))))
        (call-with-input-file (format "~a.sch" bm)
          (lambda (in) (copy-port in (current-output-port))))))
    (parameterize ([current-input-port 
                    (open-input-string
                     (format "(cf \"~a\")\n" bm))]
                   [current-output-port (open-output-nowhere)])
      (system "mit-scheme")))

  (define (run-mit bm)
    (parameterize ([current-input-port 
                    (open-input-string
                     (format "(load \"~a\")\n(exit)\ny\n" bm))])
      (system "mit-scheme --heap 12000")))

  (define (clean-up-mit bm)
    (delete-file (format "~a.com" bm))
    (delete-file (format "~a.ext" bm))
    (delete-file (format "~a.bci" bm))
    (delete-file (format "~a.bin" bm))
    (delete-file (format "~a.scm" bm)))

  (define (extract-mit-times bm str)
    (let ([m (regexp-match #rx#"cpu: ([0-9]+) real: ([0-9]+) gc: ([0-9]+)" str)]
          ;; `time' result is 10s of milliseconds? OS ticks, maybe?
          [msec/tick 10])
      (list (bytes->number (cadr m))
            (bytes->number (caddr m))
            (bytes->number (cadddr m)))))

  (define (run-petite bm)
    (parameterize ([current-input-port
                    (open-input-string
                     (format
                      "(load \"petite-prelude.sch\")\n(load \"~a.sch\")\n(exit)\n"
                      bm))])
      (system "petite")))

  (define (extract-petite-times bm str)
    (let ([m (regexp-match #rx#"([0-9]+) ms elapsed cpu time(?:, including ([0-9]+) ms collecting)?[ \n]* ([0-9]+) ms elapsed real time" str)])
      (list (bytes->number (cadr m))
            (bytes->number (cadddr m))
            (if (caddr m) (bytes->number (caddr m)) 0))))

  (define (run-guile bm)
    (parameterize ([current-input-port
                    (open-input-string
                     (format
                      "(load \"guile-prelude.sch\")\n(load \"~a.sch\")\n"
                      bm))])
      (system "guile")))

  (define (extract-guile-times bm str)
    (let ([m (regexp-match #rx#"user: ([0-9]+) system: ([0-9]+) real: ([0-9]+) gc: ([0-9]+)" str)]
          ;; `time' result is 10s of milliseconds? OS ticks, maybe?
          [msec/tick 10])
      (list (+ (bytes->number (cadr m))
               (bytes->number (caddr m)))
            (bytes->number (cadddr m))
            (bytes->number (cadddr (cdr m))))))

  (define (extract-times bm str)
    str)

  (define (extract-gambit-times bm str)
    (let ([m (regexp-match (byte-regexp
                            (bytes-append
                             #"([0-9]+) ms real.*[^0-9]"
                             #"([0-9]+) ms cpu.*"
                             #"(?:no collections|collections? accounting for ([0-9]+) ms.*)"))
                           str)])
      (map bytes->number
           (list (caddr m)
                 (cadr m)
                 (or (cadddr m) #"0")))))

  (define (extract-racket-times bm str)
    (let ([m (regexp-match #rx#"cpu time: ([0-9]+) real time: ([0-9]+) gc time: ([0-9]+)" str)])
      (map bytes->number (cdr m))))

  (define (extract-bigloo-times bm str)
    (let ([m (regexp-match #rx#"real: ([0-9]+) sys: ([0-9]+) user: ([0-9]+)" str)]
          ;; `time' result is 10s of milliseconds? OS ticks, maybe?
          [msec/tick 10])
      (list (* msec/tick (+ (bytes->number (caddr m))
                            (bytes->number (cadddr m))))
            (* msec/tick (bytes->number (cadr m)))
            0)))

  (define (extract-larceny-times bm str)
    (let ([m (regexp-match #rx#"Elapsed time...: ([0-9]+) ms.*Elapsed GC time: ([0-9]+) ms" str)])
      (list (bytes->number (cadr m))
	    #f
	    (bytes->number (caddr m)))))

  (define (extract-chicken-times bm str)
    (let ([m (regexp-match #rx#"([0-9.]+) seconds.*[^0-9.]([0-9.]+) seconds" str)])
      (list (* 1000 (string->number (format "#e~a" (cadr m))))
            #f
            (* 1000 (string->number (format "#e~a" (caddr m)))))))

  (define (extract-time-times bm str)
    (let ([m (regexp-match #rx#"real[ \t]+([0-9m.]+)s.*user[ \t]+([0-9m.]+)s.sys[ \t]+([0-9m.]+)s." str)]
          [ms->milliseconds (lambda (s)
                              (let ([m (regexp-match "([0-9]+)m([0-9.]+)" s)])
                                (+ (* 60000 (string->number (format "~a" (cadr m))))
                                   (* 1000 (string->number (format "#e~a" (caddr m)))))))])
      (let ([real (ms->milliseconds (cadr m))]
            [user (ms->milliseconds (caddr m))]
            [sys (ms->milliseconds (cadddr m))])
        (list (+ user sys) real #f))))

  (define (extract-ikarus-times bm str)
    (let ([m (regexp-match (bytes-append
                            #"([0-9]*) ms elapsed cpu time, including ([0-9]*) ms collecting\n"
                            #"[ \t]*([0-9]*) ms elapsed real time")
                           str)])
      (if m
          (list (string->number (bytes->string/utf-8 (cadr m)))
                (string->number (bytes->string/utf-8 (cadddr m)))
                (string->number (bytes->string/utf-8 (caddr m))))
          (list #f #f #f))))


  ;; Table of implementatons and benchmarks ------------------------------

  (define-struct impl (name setup make run extract-result clean-up skips))

  (define mutable-pair-progs '(conform
                               destruct
                               dynamic
                               lattice
                               maze
                               peval
                               scheme
                               sort1))

  (define impls
    (list
     (make-impl 'racket
                void
                mk-racket
                (lambda (bm)
                  (system (format "racket -u ~a.rkt" bm)))
                extract-racket-times
                clean-up-zo
                mutable-pair-progs)
     (make-impl 'mz-old
                void
                mk-racket
                (lambda (bm)
                  (system (format "mz-old -u ~a.rkt" bm)))
                extract-racket-times
                clean-up-zo
                mutable-pair-progs)
     (make-impl 'racketcgc
                void
                mk-racket
                (lambda (bm)
                  (system (format "racketcgc -u ~a.rkt" bm)))
                extract-racket-times
                clean-up-zo
                mutable-pair-progs)
     (make-impl 'racket3m
                void
                mk-racket
                (lambda (bm)
                  (system (format "racket3m -u ~a.rkt" bm)))
                extract-racket-times
                clean-up-zo
                mutable-pair-progs)
     (make-impl 'plt-r5rs
                void
                mk-plt-r5rs
                (lambda (bm)
                  (system (format "plt-r5rs ~a.scm" bm)))
                extract-racket-times
                clean-up-plt-r5rs
                null)
     (make-impl 'mzc
                void
                mk-mzc
                (lambda (bm)
                  (system (format "racket -mvqee '(load-extension \"~a\")' '(require ~a)'" 
                                  (append-extension-suffix (symbol->string bm))
                                  bm)))
                extract-racket-times
                clean-up-extension
                (append '(takr takr2)
                        mutable-pair-progs))
     (make-impl 'racket-j
                void
                mk-racket
                (lambda (bm)
                  (system (format "racket -jqu ~a.rkt" bm)))
                extract-racket-times
                clean-up-zo
                mutable-pair-progs)
     (make-impl 'racketcgc-j
                void
                mk-racket
                (lambda (bm)
                  (system (format "racketcgc -jqu ~a.rkt" bm)))
                extract-racket-times
                clean-up-zo
                mutable-pair-progs)
     (make-impl 'racketcgc-tl
                void
                mk-racket-tl
                (lambda (bm)
                  (system (format "racketcgc -qr compiled/~a.zo" bm)))
                extract-racket-times
                clean-up-zo
                (append '(nucleic2)
                        mutable-pair-progs))
     (make-impl 'chicken
                void
                (run-mk "mk-chicken.rkt")
                run-exe
                extract-chicken-times
                clean-up-bin
                '(scheme2 takr2))
     (make-impl 'bigloo
                void
                (run-mk "mk-bigloo.rkt")
                run-exe
                extract-bigloo-times
                clean-up-bin
                '(cpstack takr2))
     (make-impl 'gambit
                void
                (run-mk "mk-gambit.rkt")
                run-gambit-exe
                extract-gambit-times
                clean-up-o1
                '(nucleic2))
     (make-impl 'larceny
                setup-larceny
                mk-larceny
                run-larceny
                extract-larceny-times
                clean-up-fasl
                '())
     (make-impl 'ikarus
                setup-ikarus
                mk-ikarus
                run-ikarus
                extract-ikarus-times
                clean-up-ikarus
                '(takr))
     (make-impl 'mit
                void
                mk-mit
                run-mit
                extract-mit-times
                clean-up-mit
                '(nucleic2 puzzle takr2))
     (make-impl 'scheme48
                void
                void
                run-scheme48
                extract-scheme48-times
                void
                '())
     (make-impl 'petite
                void
                void
                run-petite
                extract-petite-times
                void
                '())
     (make-impl 'guile
                void
                void
                run-guile
                extract-guile-times
                void
                '(ctak))
))

  (define obsolte-impls '(racket3m racketcgc racket-j racketcgc-j racketcgc-tl mzc mz-old))

  (define benchmarks
    '(conform
      cpstack
      ctak
      deriv
      dderiv
      destruct
      div
      dynamic
      dynamic2
      earley
      fft
      graphs
      lattice
      lattice2
      maze
      maze2
      mazefun
      nboyer
      nestedloop
      nfa
      nothing
      nqueens
      nucleic2
      paraffins
      peval
      puzzle
      sboyer
      scheme
      scheme2
      sort1
      tak
      takl
      takr
      takr2
      triangle))

  (define extra-benchmarks
    '(kanren
      psyntax))

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
                                ((impl-run i) bm))
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
                          extra-benchmarks
                          (map impl-name impls) obsolte-impls
                          3))

  (define-runtime-path bm-directory ".")
  
  ;; Benchmark-specific setup --------------------

  (parameterize ([current-directory bm-directory])
    (when (memq 'dynamic actual-benchmarks-to-run)
      (unless (file-exists? "dynamic-input.txt")
        (gunzip "dynamic-input.txt.gz"))))

  ;; Run benchmarks -------------------------------

  (rprintf "; ~a\n" (date->string (seconds->date (current-seconds)) #t))

  (parameterize ([current-directory bm-directory])
    (for-each (lambda (impl)
                (map (lambda (bm)
                       (run-benchmark impl bm))
                     actual-benchmarks-to-run))
              actual-implementations-to-run)))
