
#lang scheme/base
(require scheme/promise
         parser-tools/lex
         "deriv.ss"
         "deriv-parser.ss"
         "deriv-tokens.ss")

(provide trace
         trace*
         trace/result
         trace-verbose?
         events->token-generator
         current-expand-observe

         trace-macro-limit
         trace-limit-handler)

(define current-expand-observe
  (dynamic-require ''#%expobs 'current-expand-observe))

(define trace-verbose? (make-parameter #f))

;; trace : stx -> Deriv
(define (trace stx)
  (let-values ([(result events derivp) (trace* stx expand)])
    (force derivp)))

;; trace/result : stx -> stx/exn Deriv
(define (trace/result stx)
  (let-values ([(result events derivp) (trace* stx expand)])
    (values result
            (force derivp))))

;; trace* : stx (stx -> stx) -> stx/exn (list-of event) (promise-of Deriv)
(define (trace* stx expander)
  (let-values ([(result events) (expand/events stx expander)])
    (values result
            events
            (delay (parse-derivation
                    (events->token-generator events))))))

;; events->token-generator : (list-of event) -> (-> token)
(define (events->token-generator events)
  (let ([pos 0])
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
        t))))

(define trace-macro-limit (make-parameter #f))
(define trace-limit-handler (make-parameter #f))

;; expand/events : stx (stx -> stx) -> stx/exn (list-of event)
(define (expand/events sexpr expander)
  (define events null)
  (define counter 0)
  (define (add! x y)
    (set! events (cons (cons x y) events)))
  (define add!/check
    (let ([limit (trace-macro-limit)]
          [handler (trace-limit-handler)])
      (if (and limit handler (exact-positive-integer? limit))
          (lambda (x y)
            (add! x y)
            (when (= x 8) ;; enter-macro
              (set! counter (add1 counter))
              (when (= counter limit)
                (set! limit (handler counter)))))
          add!)))
  (parameterize ((current-expand-observe add!/check))
    (let ([result
           (with-handlers ([(lambda (exn) #t)
                            (lambda (exn)
                              (add! 'error exn)
                              exn)])
             (expander sexpr))])
      (add! 'EOF #f)
      (values result
              (reverse events)))))
