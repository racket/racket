#!/bin/sh
#|
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module auto scheme/base
  (require (for-syntax scheme/base)
           mzlib/process
           "cmdline.ss"
           mzlib/list
           mzlib/compile
           mzlib/inflate
           mzlib/date
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
      (load script)))

  (define (clean-up-bin bm)
    (delete-file (symbol->string bm)))

  (define (clean-up-o1 bm)
    (delete-file (format "~a.o1" bm)))

  (define (mk-mzscheme bm)
    ;; To get compilation time:
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'scheme/base)
      (load (format "~a.ss" bm))))

  (define (clean-up-nothing bm)
    (void))

  (define (mk-plt-r5rs bm)
    (with-output-to-file (format "~a.scm" bm)
      #:exists 'replace
      (lambda ()
        (printf "(load \"r5rs-wrap.ss\")\n(load \"~a.sch\")\n" bm)))
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

  (define (mk-mzscheme-tl bm)
    ;; To get compilation time:
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'scheme/base)
      (eval '(define null #f)) ; for dynamic.sch
      (compile-file (format "~a.sch" bm))))

  (define (clean-up-zo bm)
    (delete-file (build-path "compiled" (format "~a.zo" bm))))

  (define (mk-larceny bm)
    (parameterize ([current-input-port 
		    (open-input-string
		     (format (string-append
			      "(compiler-switches 'fast-safe)\n"
			      "(compile-file \"~a.sch\")\n")
			     bm))]
		   [current-output-port (open-output-bytes)])
      (system "larceny")))

  (define (clean-up-fasl bm)
    (delete-file (format "~a.fasl" bm)))

  (define (mk-mzc bm)
    (parameterize ([current-output-port (open-output-bytes)])
      (system (format "mzc ~a.ss" bm))))

  (define (clean-up-extension bm)
    (delete-file (append-extension-suffix (symbol->string bm))))

  (define (run-exe bm)
    (system (symbol->string bm)))

  (define (run-exe/time bm)
    (system (format "time ~a" bm)))

  (define (run-gambit-exe bm)
    (system (format "gsi -:d-,m10000 ~a.o1" bm)))

  (define (run-larceny bm)
    (parameterize ([current-input-port (open-input-string
                                        (format "(load \"~a.fasl\")\n"
                                                bm))])
      (system "larceny")))

  (define (mk-ikarus bm)
    (void))

  (define (run-ikarus bm)
    (system (format "ikarus ~a.sch < /dev/null" bm)))

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

  (define (extract-mzscheme-times bm str)
    (let ([m (regexp-match #rx#"cpu time: ([0-9]+) real time: ([0-9]+) gc time: ([0-9]+)" str)])
      (map bytes->number (cdr m))))

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
      (list (string->number (bytes->string/utf-8 (cadr m)))
            (string->number (bytes->string/utf-8 (cadddr m)))
            (string->number (bytes->string/utf-8 (caddr m))))))


  ;; Table of implementatons and benchmarks ------------------------------

  (define-struct impl (name make run extract-result clean-up skips))

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
     (make-impl 'mzscheme
                mk-mzscheme
                (lambda (bm)
                  (system (format "mzscheme -u ~a.ss" bm)))
                extract-mzscheme-times
                clean-up-nothing
                mutable-pair-progs)
     (make-impl 'mz-old
                mk-mzscheme
                (lambda (bm)
                  (system (format "mz-old -u ~a.ss" bm)))
                extract-mzscheme-times
                clean-up-nothing
                mutable-pair-progs)
     (make-impl 'mzschemecgc
                mk-mzscheme
                (lambda (bm)
                  (system (format "mzschemecgc -u ~a.ss" bm)))
                extract-mzscheme-times
                clean-up-nothing
                mutable-pair-progs)
     (make-impl 'mzscheme3m
                mk-mzscheme
                (lambda (bm)
                  (system (format "mzscheme3m -u ~a.ss" bm)))
                extract-mzscheme-times
                clean-up-nothing
                mutable-pair-progs)
     (make-impl 'plt-r5rs
                mk-plt-r5rs
                (lambda (bm)
                  (system (format "plt-r5rs ~a.scm" bm)))
                extract-mzscheme-times
                clean-up-plt-r5rs
                null)
     (make-impl 'mzc
                mk-mzc
                (lambda (bm)
                  (system (format "mzscheme -mvqee '(load-extension \"~a\")' '(require ~a)'" 
                                  (append-extension-suffix (symbol->string bm))
                                  bm)))
                extract-mzscheme-times
                clean-up-extension
                (append '(takr takr2)
                        mutable-pair-progs))
     (make-impl 'mzscheme-j
                mk-mzscheme
                (lambda (bm)
                  (system (format "mzscheme -jqu ~a.ss" bm)))
                extract-mzscheme-times
                clean-up-nothing
                mutable-pair-progs)
     (make-impl 'mzschemecgc-j
                mk-mzscheme
                (lambda (bm)
                  (system (format "mzschemecgc -jqu ~a.ss" bm)))
                extract-mzscheme-times
                clean-up-nothing
                mutable-pair-progs)
     (make-impl 'mzschemecgc-tl
                mk-mzscheme-tl
                (lambda (bm)
                  (system (format "mzschemecgc -qr compiled/~a.zo" bm)))
                extract-mzscheme-times
                clean-up-zo
                (append '(nucleic2)
                        mutable-pair-progs))
     (make-impl 'chicken
                (run-mk "mk-chicken.ss")
                run-exe
                extract-chicken-times
                clean-up-bin
                '(nucleic2))
     (make-impl 'bigloo
                (run-mk "mk-bigloo.ss")
                run-exe/time
                extract-time-times
                clean-up-bin
                '(cpstack maze maze2 puzzle triangle))
     (make-impl 'gambit
                (run-mk "mk-gambit.ss")
                run-gambit-exe
                extract-gambit-times
                clean-up-o1
                '(nucleic2))
     (make-impl 'larceny
                mk-larceny
                run-larceny
                extract-larceny-times
                clean-up-fasl
                '(maze maze2))
     (make-impl 'ikarus
                mk-ikarus
                run-ikarus
                extract-ikarus-times
                clean-up-nothing
                '(fft))))

  (define obsolte-impls '(mzscheme mzscheme-j mzschemecgc-tl mzc mz-old))

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

  (define (run-benchmark impl bm)
    (let ([i (ormap (lambda (i)
                      (and (eq? impl (impl-name i))
                           i))
                    impls)])
      (if (memq bm (impl-skips i))
          (rprintf "[~a ~a ~s #f]\n" impl bm '(#f #f #f))
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
            ((impl-clean-up i) bm)))
      (flush-output)))

  ;; Extract command-line arguments --------------------

  (define-values (actual-benchmarks-to-run 
                  actual-implementations-to-run 
                  num-iterations)
    (process-command-line benchmarks
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
