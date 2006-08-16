#!/bin/sh
#|
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module auto mzscheme
  (require (lib "process.ss")
           (lib "cmdline.ss")
           (lib "list.ss")
           (lib "compile.ss")
           (lib "file.ss" "dynext"))

  (define (bytes->number b)
    (string->number (bytes->string/latin-1 b)))

  (define ((run-mk script) bm)
    (when (file-exists? (symbol->string bm))
      (delete-file (symbol->string bm)))
    (parameterize ([current-command-line-arguments (vector (symbol->string bm))])
      (load script)))

  (define (clean-up-bin bm)
    (delete-file (symbol->string bm)))

  (define (mk-mzscheme bm)
    ;; To get compilation time:
    (parameterize ([current-namespace (make-namespace)])
      (load (format "~a.ss" bm))))

  (define (clean-up-nothing bm)
    (void))

  (define (mk-mzscheme-tl bm)
    ;; To get compilation time:
    (parameterize ([current-namespace (make-namespace)])
      (namespace-require 'mzscheme)
      (namespace-transformer-require 'mzscheme)
      (eval '(define null #f)) ; for dynamic.sch
      (compile-file (format "~a.sch" bm))))

  (define (clean-up-zo bm)
    (delete-file (build-path "compiled" (format "~a.zo" bm))))

  (define (mk-larceny bm)
    (parameterize ([current-input-port (open-input-string
                                        (format "(compile-file \"~a.sch\")\n"
                                                bm))]
		   [current-output-port (open-output-bytes)])
      (system "larceny")))

  (define (clean-up-fasl bm)
    (delete-file (build-path "compiled" (format "~a.fasl"))))

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
    (system (format "~a -:d-" bm)))

  (define (run-larceny bm)
    (parameterize ([current-input-port (open-input-string
                                        (format "(load \"~a.fasl\")\n"
                                                bm))])
      (system "larceny")))

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

  (define-struct impl (name make run extract-result clean-up skips))

  (define impls
    (list
     (make-impl 'mzscheme
                mk-mzscheme
                (lambda (bm)
                  (system (format "mzscheme -qu ~a.ss" bm)))
                extract-mzscheme-times
                clean-up-nothing
                '())
     (make-impl 'mzscheme3m
                mk-mzscheme
                (lambda (bm)
                  (system (format "mzscheme3m -qu ~a.ss" bm)))
                extract-mzscheme-times
                clean-up-nothing
                '())
     (make-impl 'mzc
                mk-mzc
                (lambda (bm)
                  (system (format "mzscheme -mvqee '(load-extension \"~a\")' '(require ~a)'" 
                                  (append-extension-suffix (symbol->string bm))
                                  bm)))
                extract-mzscheme-times
                clean-up-extension
                '(takr))
     (make-impl 'mzscheme-j
                mk-mzscheme
                (lambda (bm)
                  (system (format "mzscheme -jqu ~a.ss" bm)))
                extract-mzscheme-times
                clean-up-nothing
                '())
     (make-impl 'mzscheme3m-tl
                mk-mzscheme-tl
                (lambda (bm)
                  (system (format "mzscheme3m -qr compiled/~a.zo" bm)))
                extract-mzscheme-times
                clean-up-zo
                '(nucleic2))
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
                '(cpstack ctak puzzle triangle))
     (make-impl 'gambit
                (run-mk "mk-gambit.ss")
                run-gambit-exe
                extract-gambit-times
                clean-up-bin
                '(nucleic2))
     (make-impl 'larceny
                mk-larceny
                run-larceny
                extract-larceny-times
                clean-up-fasl
                '())))

  (define obsolte-impls '(mzscheme mzscheme-j mzscheme3m-tl mzc))

  (define benchmarks
    '(conform
      cpstack
      ctak
      deriv
      dderiv
      destruct
      div
      dynamic
      earley
      fft
      nboyer
      nestedloop
      nfa
      nucleic2
      puzzle
      sboyer
      sort1
      tak
      takl
      takr
      triangle))

  (define num-iterations 3)

  (define (run-benchmark impl bm)
    (let ([i (ormap (lambda (i)
                      (and (eq? impl (impl-name i))
                           i))
                    impls)])
      (if (memq bm (impl-skips i))
          (printf "[~a ~a ~s 0]\n" impl bm '(#f #f #f))
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
                    (printf "[~a ~a ~s ~a]\n"
                            impl
                            bm
                            ((impl-extract-result i) bm (get-output-bytes out))
                            (inexact->exact (round (- end start)))))
                  (loop (sub1 n)))))))
      ((impl-clean-up i) bm)))

  (define no-implementations (map (lambda (s)
                                    (cons (string->symbol (format "no-~a" s))
                                          s))
                                  (map impl-name impls)))
  (define no-benchmarks (map (lambda (s)
                               (cons (string->symbol (format "no-~a" s))
                                     s))
                             benchmarks))

  (define run-benchmarks #f)
  (define run-implementations #f)

  (define default-benchmarks benchmarks)
  (define default-implementations (remq* obsolte-impls
                                         (map impl-name impls)))

  (define args
    (command-line
     "auto"
     (current-command-line-arguments)
     (once-each
      [("-n" "--iters") n "set number of run iterations"
       (let ([v (string->number n)])
         (unless (and (number? v)
                      (exact? v)
                      (positive? v))
           (error 'auto "bad interation count: ~a" n))
         (set! num-iterations v))])
     (args impl-or-benchmark impl-or-benchmark)))

  (for-each (lambda (arg)
              (let ([s (string->symbol arg)])
                (cond
                 [(memq s (map impl-name impls))
                  (set! run-implementations
                        (append (or run-implementations null)
                                (list s)))]
                 [(assq s no-implementations)
                  => (lambda (a)
                       (set! run-implementations
                             (remq (cdr a)
                                   (or run-implementations default-implementations))))]
                 [(memq s benchmarks)
                  (set! run-benchmarks
                        (append (or run-benchmarks null)
                                (list s)))]
                 [(assq s no-benchmarks)
                  => (lambda (a)
                       (set! run-benchmarks
                             (remq (cdr a)
                                   (or run-benchmarks default-benchmarks))))]
                 [else
                  (error 'auto "mysterious argument: ~a" arg)])))
            args)

  (map (lambda (impl)
         (map (lambda (bm)
                (run-benchmark impl bm))
              (or run-benchmarks
                  benchmarks)))
       (or run-implementations
           default-implementations)))
