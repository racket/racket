
(module trace mzscheme
  (require (lib "lex.ss" "parser-tools")
           (lib "class.ss"))
  (require "deriv.ss"
           "deriv-parser.ss"
           "deriv-tokens.ss"
           "reductions.ss"
           "hide.ss"
           "hiding-policies.ss")
  
  (provide trace-verbose?
           trace
           trace/result
           trace+reductions
           current-expand-observe
           (all-from "reductions.ss"))

  (define current-expand-observe
    (dynamic-require '#%expobs 'current-expand-observe))

  (define trace-verbose? (make-parameter #f))

  ;; trace : syntax -> Derivation
  (define (trace stx)
    (let-values ([(result tracer) (expand+tracer stx)])
      (parse-derivation tracer)))

  ;; trace/result : syntax -> (values syntax/exn Derivation)
  (define (trace/result stx)
    (let-values ([(result tracer) (expand+tracer stx)])
      (values result
              (parse-derivation tracer))))

  ;; trace+reductions : syntax -> ReductionSequence
  (define (trace+reductions stx)
    (reductions (trace stx)))

  ;; expand+tracer : syntax/sexpr -> (values syntax/exn (-> event))
  (define (expand+tracer sexpr)
    (let* ([s (make-semaphore 1)]
           [head (cons #f #f)]
           [tail head]
           [pos 0])
      (define (add! x)
        (semaphore-wait s)
        (set-car! tail x) 
        (set-cdr! tail (cons #f #f))
        (set! tail (cdr tail))
        (semaphore-post s))
      (define get
        (let ([head head])
          (lambda ()
            (semaphore-wait s)
            (let ([result (car head)])
              (set! head (cdr head))
              (semaphore-post s)
              result))))
      (parameterize ((current-expand-observe
                      (lambda (sig val)
                        (add! (cons sig val)))))
        (let ([result
               (with-handlers ([(lambda (exn) #t)
                                (lambda (exn)
                                  (add! (cons 'error exn))
                                  exn)])
                 (expand sexpr))])
          (add! (cons 'EOF pos))
          (values result
                  (lambda ()
                    (let* ([sig+val (get)]
                           [sig (car sig+val)]
                           [val (cdr sig+val)]
                           [t (tokenize sig val pos)])
                      (when (trace-verbose?)
                            (printf "~s: ~s~n" pos (token-name (position-token-token t))))
                      (set! pos (add1 pos))
                      t)))))))

  )
