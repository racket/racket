
(module cmdline mzscheme
  (require mzlib/process
           mzlib/cmdline
           mzlib/list
           mzlib/compile
           mzlib/inflate
           mzlib/date
           dynext/file)

  (provide process-command-line
           rprintf)

  (define current-output-file (make-parameter #f))
  
  (define (process-command-line benchmarks 
                                extra-benchmarks
                                implementations non-default-implementations
                                num-iterations)
    
    (define no-implementations (map (lambda (s)
                                      (cons (string->symbol (format "no-~a" s))
                                            s))
                                    implementations))
    (define no-benchmarks (map (lambda (s)
                                 (cons (string->symbol (format "no-~a" s))
                                       s))
                               benchmarks))

    (define run-benchmarks #f)
    (define run-implementations #f)

    (define default-benchmarks benchmarks)
    (define default-implementations (remq* non-default-implementations implementations))

    ;; Extract command-line arguments --------------------

    (define args
      (command-line
       "auto"
       (current-command-line-arguments)
       (once-each
        [("--show") "show implementations and benchmarks"
         (printf "Implementations:\n")
         (for-each (lambda (impl)
                     (printf " ~a\n" impl))
                   default-implementations)
         (for-each (lambda (impl)
                     (printf " ~a [skipped by default]\n" impl))
                   non-default-implementations)
         (printf "Benchmarks:\n")
         (for-each (lambda (bm)
                     (printf " ~a\n" bm))
                   benchmarks)
         (for-each (lambda (bm)
                     (printf " ~a (not run by default)\n" bm))
                   extra-benchmarks)
         (exit 0)]
        [("-o" "--out") filename "append output to <filename>"
         (current-output-file filename)]
        [("-n" "--iters") n "set number of run iterations"
         (let ([v (string->number n)])
           (unless (and (number? v)
                        (exact? v)
                        (positive? v))
             (error 'auto "bad interation count: ~a" n))
           (set! num-iterations v))])
       (args impl-or-benchmark impl-or-benchmark)))

    ;; Process arguments ------------------------------

    (let loop ([args args])
      (unless (null? args)
        (let ([arg (car args)])
          (let ([s (string->symbol arg)])
            (cond
             [(memq s implementations)
              (set! run-implementations
                    (append (or run-implementations null)
                            (list s)))
              (loop (cdr args))]
             [(assq s no-implementations)
              => (lambda (a)
                   (set! run-implementations
                         (remq (cdr a)
                               (or run-implementations default-implementations)))
                   (loop (cdr args)))]
             [(or (memq s benchmarks)
                  (memq s extra-benchmarks))
              =>
              (lambda (l)
                (let* ([...? (and (pair? (cdr args))
                                  (equal? (cadr args) "..."))]
                       [new (if ...?
                                l
                                (list s))])
                  (set! run-benchmarks
                        (append (or run-benchmarks null)
                                new))
                  (loop ((if ...? cddr cdr) args))))]
             [(assq s no-benchmarks)
              => (lambda (a)
                   (set! run-benchmarks
                         (remq (cdr a)
                               (or run-benchmarks default-benchmarks)))
                   (loop (cdr args)))]
             [else
              (error 'auto "mysterious argument: ~a" arg)])))))

    (values (or run-benchmarks
                benchmarks)
            (or run-implementations
                default-implementations)
            num-iterations))

  (define (rprintf . args)
    (apply printf args)
    (when (current-output-file)
      (with-output-to-file (current-output-file)
        (lambda ()
          (apply printf args))
        'append))))
