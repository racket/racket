#lang racket/base
(require (prefix-in pl- '#%place)
         '#%boot
         (only-in '#%paramz parameterization-key make-custodian-from-main)
         (only-in '#%futures processor-count)
         '#%place-struct
         racket/fixnum
         racket/flonum
         racket/vector

         (for-syntax racket/base
                     racket/syntax))

(provide dynamic-place
         place-sleep
         place-wait 
         place-kill
         place-break
         place-channel
         place-channel-put
         place-channel-get
         place-channel?
         place?
         place-message-allowed?
         place-channel-put/get
         processor-count
         place
         (rename-out [pl-place-enabled? place-enabled?])
         place-dead-evt)

(define-struct TH-place (th ch cust) 
  #:property prop:evt (lambda (x) (TH-place-channel-in (TH-place-ch x))))

(define (place-channel-put/get ch msg)
  (place-channel-put ch msg)
  (place-channel-get ch))

(define (make-th-async-channel)
  (define ch (make-channel))
  (values 
    (thread 
      (lambda ()
        (let loop ()
          (let ([v (thread-receive)])
            (channel-put ch v)
            (loop)))))
    ch))
  
(define (th-dynamic-place mod funcname)
  (unless (or (path-string? mod) (resolved-module-path? mod))
    (raise-type-error 'place "resolved-module-path? or path-string?" 0 mod funcname))
  (unless (symbol? funcname)
    (raise-type-error 'place "symbol?" 1 mod funcname))
  (define-values (pch cch) (th-place-channel))
  (define cust (make-custodian-from-main))
  (define th (thread 
              (lambda ()
                (with-continuation-mark
                    parameterization-key
                    orig-paramz
                  (parameterize ([current-namespace (make-base-namespace)]
                                 [current-custodian cust])
                    ((dynamic-require mod funcname) cch))))))
  (TH-place th pch cust))

(define (th-place-sleep n) (sleep n))
(define (th-place-wait pl) (thread-wait (TH-place-th pl)) 0)
(define (th-place-kill pl) (custodian-shutdown-all (TH-place-cust pl)))
(define (th-place-break pl) (break-thread (TH-place-th pl)))
(define (th-place-dead-evt pl) (thread-dead-evt (TH-place-th pl)))
(define (th-place-channel)
  (define-values (as ar) (make-th-async-channel))
  (define-values (bs br) (make-th-async-channel))
  (define pch (TH-place-channel ar bs))
  (define cch (TH-place-channel br as))
  (values pch cch))
 
(define (deep-copy x)
  (define (dcw o)
    (cond 
      [(ormap (lambda (x) (x o)) (list number? char? boolean? null? void? string? symbol? TH-place-channel?)) o]
      [(cond
        [(path? o) (path->bytes o)]
        [(bytes? o) (if (pl-place-shared? o) o (bytes-copy o))]
        [(fxvector? o) (if (pl-place-shared? o) o (fxvector-copy o))]
        [(flvector? o) (if (pl-place-shared? o) o (flvector-copy o))]
        [else #f])
        => values]
      [(TH-place? o) (dcw (TH-place-ch o))]
      [(pair? o) (cons (dcw (car o)) (dcw (cdr o)))]
      [(vector? o) (vector-map! dcw (vector-copy o))]
      [(struct? o)
        (define key (prefab-struct-key o))
        (when (not key)
          (error "Must be a prefab struct"))
        (apply make-prefab-struct 
               key 
               (map dcw (cdr (vector->list (struct->vector o)))))]
      [else (raise-mismatch-error 'place-channel-put "cannot transmit a message containing value: " o)]))

  (dcw x))


(define (th-place-channel-put pl msg)
  (define th  
    (cond
      [(TH-place? pl) (TH-place-channel-out (TH-place-ch pl))]
      [(TH-place-channel? pl) (TH-place-channel-out pl)]
      [else (raise-type-error 'place-channel-put "expect a place? or place-channel?" pl)]))
  (void (thread-send th (deep-copy msg) #f)))

(define (th-place-channel-get pl)
  (channel-get
    (cond
      [(TH-place? pl) (TH-place-channel-in (TH-place-ch pl))]
      [(TH-place-channel? pl) (TH-place-channel-in pl)]
      [else (raise-type-error 'place-channel-get "expect a place? or place-channel?" pl)])))

(define (th-place-channel? pl)
  (or (TH-place? pl)
      (TH-place-channel? pl)))

(define (th-place-message-allowed? pl)
  #t)

(define-syntax-rule (define-pl x p t) (define x (if (pl-place-enabled?) p t)))

(define-pl dynamic-place      pl-dynamic-place      th-dynamic-place)
(define-pl place-sleep        pl-place-sleep        th-place-sleep)
(define-pl place-wait         pl-place-wait         th-place-wait)
(define-pl place-kill         pl-place-kill         th-place-kill)
(define-pl place-break        pl-place-break        th-place-break)
(define-pl place-channel      pl-place-channel      th-place-channel)
(define-pl place-channel-put  pl-place-channel-put  th-place-channel-put)
(define-pl place-channel-get  pl-place-channel-get  th-place-channel-get)
(define-pl place-channel?     pl-place-channel?     th-place-channel?)
(define-pl place?             pl-place?             TH-place?)
(define-pl place-message-allowed? pl-place-message-allowed? th-place-message-allowed?)
(define-pl place-dead-evt     pl-place-dead-evt     th-place-dead-evt)

(define-syntax-rule (define-syntax-case (N a ...) b ...)
  (define-syntax (N stx)
    (syntax-case stx ()
      [(_ a ...) b ...])))

(define-for-syntax (gen-create-place stx)
 (syntax-case stx ()
   [(_ ch body ...)
     (with-syntax ([interal-def-name
                    (syntax-local-lift-expression #'(lambda (ch) body ...))]
                   [funcname (datum->syntax stx (generate-temporary #'place/anon))])
      (syntax-local-lift-provide #'(rename interal-def-name funcname))
      #'(let ([module-path (resolved-module-path-name
              (variable-reference->resolved-module-path
               (#%variable-reference)))])
       (dynamic-place module-path (quote funcname))))]))

(define-syntax (place stx)
  (gen-create-place stx))
