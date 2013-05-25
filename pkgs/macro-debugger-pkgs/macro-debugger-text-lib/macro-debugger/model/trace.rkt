#lang racket/base
(require racket/promise
         racket/list
         syntax/modcode
         syntax/modresolve
         parser-tools/lex
         "deriv-parser.rkt"
         "deriv-tokens.rkt")

(provide trace
         trace*
         trace-module
         trace*-module
         trace/result
         trace-verbose?
         events->token-generator
         current-expand-observe
         expand/compile-time-evals

         trace-macro-limit
         trace-limit-handler)

(define current-expand-observe
  (dynamic-require ''#%expobs 'current-expand-observe))

(define trace-verbose? (make-parameter #f))

;; trace : stx -> Deriv
(define (trace stx [expander expand/compile-time-evals])
  (let-values ([(result events derivp) (trace* stx expander)])
    (force derivp)))

;; trace-module : module-path -> Deriv
(define (trace-module module-path)
  (let-values ([(result events derivp) (trace*-module module-path)])
    (force derivp)))

;; trace/result : stx -> stx/exn Deriv
(define (trace/result stx [expander expand/compile-time-evals])
  (let-values ([(result events derivp) (trace* stx expander)])
    (values result
            (force derivp))))

;; trace* : stx (stx -> stx) -> stx/exn (list-of event) (promise-of Deriv)
(define (trace* stx [expander expand/compile-time-evals])
  (let-values ([(result events) (expand/events stx expander)])
    (values result
            events
            (delay (parse-derivation
                    (events->token-generator events))))))

;; trace*-module : module-path -> stx/exn (listof event) (promiseof Deriv)
(define (trace*-module module-path)
  (get-module-code (resolve-module-path module-path #f)
                   #:choose (lambda _ 'src)
                   #:compile (lambda (stx)
                               (trace* stx expand))))

;; events->token-generator : (list-of event) -> (-> token)
(define (events->token-generator events)
  (let ([pos 1])
    (lambda ()
      (define sig+val (car events))
      (set! events (cdr events))
      (let* ([sig (car sig+val)]
             [val (cdr sig+val)]
             [t (tokenize sig val pos)])
        (when (trace-verbose?)
          (printf "~s: ~s\n" pos
                  (token-name (position-token-token t))))
        (set! pos (add1 pos))
        t))))

(define trace-macro-limit (make-parameter +inf.0))
(define trace-limit-handler (make-parameter #f))

;; expand/events : stx (stx -> stx) -> stx/exn (list-of event)
(define (expand/events sexpr expander)
  (define events null)
  ;; Problem: jumps within expansion (eg, macro catches error thrown from within
  ;; call to 'local-expand') can result in ill-formed event stream.
  ;; In general, not possible to detect jump endpoints, but we can at least isolate
  ;; the bad parts by watching for mismatched bracketing events
  ;; (eg, macro-{pre,post}-transform).
  (define counter 0)        ;; = (length events)
  (define macro-stack null) ;; (listof (cons (U stx 'local-bind) nat))
  (define (add! x y)
    (set! counter (add1 counter))
    (set! events (cons (cons (signal->symbol x) y) events)))
  (define add!/check
    (let ([limit (trace-macro-limit)]
          [handler (trace-limit-handler)]
          [limit-counter 0]
          [last-local-value-id #f])
      (lambda (x y)
        (add! x y)
        (case x
          ((8) ;; enter-macro
           (set! limit-counter (add1 limit-counter))
           (when (>= limit-counter limit)
             (set! limit (handler limit-counter))))
          ((21) ;; macro-pre-transform
           (let ([rec (cons y counter)])
             (set! macro-stack (cons rec macro-stack))))
          ((22) ;; macro-post-transform
           (cond [(and (pair? macro-stack)
                       (eq? (car (car macro-stack)) (cdr y)))
                  (set! macro-stack (cdr macro-stack))]
                 [else ;; Jumped!
                  (let loop ([ms macro-stack])
                    (let ([top (car ms)])
                      (cond [(eq? (car top) (cdr y))
                             (let* ([reset-to (cdr top)]
                                    [len (- counter reset-to 1)]
                                    [pfx (take (cdr events) len)]
                                    [sfx (drop (cdr events) len)])
                               (set! macro-stack (cdr ms))
                               (set! events sfx)
                               (set! counter (cdr top))
                               (add! 'local-mess (reverse pfx))
                               (add! 'macro-post-transform y))]
                            [else (loop (cdr ms))])))]))
          ((143) ;; local-bind
           (let ([rec (cons 'local-bind counter)])
             (set! macro-stack (cons rec macro-stack))))
          ((160) ;; exit-local-bind
           (let ([top (car macro-stack)])
             (cond [(eq? (car top) 'local-bind)
                    (set! macro-stack (cdr macro-stack))]
                   [else ;; Jumped!
                    (error 'trace "internal error: cannot handle catch within bind")])))
          ((153) ;; local-value
           (set! last-local-value-id y))
          ((154) ;; local-value-result
           (add! 'local-value-binding
                 (and y (identifier-binding last-local-value-id)))
           (set! last-local-value-id #f))))))
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

(require syntax/stx
         syntax/kerncase)

(define (emit sig [val #f])
  ((current-expand-observe) sig val))

(define (expand/compile-time-evals stx)
  (define (expand/cte stx)
    (define _ (emit 'visit stx))
    (define e1 (expand-syntax-to-top-form stx))
    (define e2
      (syntax-case e1 (begin)
        [(begin expr ...)
         (begin
           (emit 'top-begin e1)
           (with-syntax ([(expr ...) 
                          ;;left-to-right part of this map is important:
                          (map (lambda (e)
                                 (emit 'next)
                                 (expand/cte e))
                               (syntax->list #'(expr ...)))]
                         [beg (stx-car e1)])
             (datum->syntax e1 (syntax-e (syntax (beg expr ...))) e1 e1)))]
        [else
         (begin
           (emit 'top-non-begin)
           (let ([e (expand-syntax e1)])
             ;; Must set to void to avoid catching DrRacket's annotations...
             (parameterize ((current-expand-observe void))
               (eval-compile-time-part e))
             e))]))
    (emit 'return e2)
    e2)
  (emit 'start)
  (expand/cte (namespace-syntax-introduce (datum->syntax #f stx))))

;; eval-compile-time-part : syntax boolean -> void
;; compiles the syntax it receives as an argument and evaluates the compile-time part of it.
;; pre: there are no top-level begins in stx.
(define (eval-compile-time-part stx)
  (define (eval/compile stx)
    (eval (compile-syntax stx)))
  (kernel-syntax-case stx #f
    [(#%require req ...)
     (for ([req (syntax->list #'(req ...))])
       (namespace-require/expansion-time (syntax->datum req)))]
    [(module . _)
     (eval/compile stx)]
    [(define-syntaxes . _)
     (eval/compile stx)]
    [(begin-for-syntax . _)
     (eval/compile stx)]
    [(define-values (id ...) . _)
     (with-syntax ([defvals (stx-car stx)]
                   [undefined (letrec ([x x]) x)])
       (for ([id (syntax->list #'(id ...))])
         (with-syntax ([id id])
           (eval/compile #'(defvals (id) undefined)))))
     ;; Following doesn't work (namespace mismatch)
     ;; (eval/compile #'(define-values (id ...) (let ([id #f] ...) (values id ...))))
     ]
    [_else
     (void)]))
