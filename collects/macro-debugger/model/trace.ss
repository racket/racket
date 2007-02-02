
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
    (let-values ([(result tracer) (expand+tracer stx expand)])
      (parse-derivation tracer)))

  ;; trace/result : syntax -> (values syntax/exn Derivation)
  (define (trace/result stx)
    (let-values ([(result tracer) (expand+tracer stx expand)])
      (values result
              (parse-derivation tracer))))

  ;; trace+reductions : syntax -> ReductionSequence
  (define (trace+reductions stx)
    (reductions (trace stx)))

  ;; expand+tracer : syntax/sexpr (syntax -> A) -> (values A/exn (-> event))
  (define (expand+tracer sexpr expander)
    (let* ([events null]
           [pos 0])
      (define (add! x)
        (set! events (cons x events)))
      (parameterize ((current-expand-observe
                      (let ([c 0])
                        (lambda (sig val)
                          (set! c (add1 c))
                          (add! (cons sig val))))))
        (let ([result
               (with-handlers ([(lambda (exn) #t)
                                (lambda (exn)
                                  (add! (cons 'error exn))
                                  exn)])
                 (expander sexpr))])
          (add! (cons 'EOF pos))
          (values result
                  (let ([events (reverse events)])
                    (lambda ()
                      (define sig+val (car events))
                      (set! events (cdr events))
                      (let* ([sig (car sig+val)]
                             [val (cdr sig+val)]
                             [t (tokenize sig val pos)])
                        (when (trace-verbose?)
                          (printf "~s: ~s~n" pos
                                  (token-name (position-token-token t))))
                        (set! pos (add1 pos))
                        t))))))))

  )
