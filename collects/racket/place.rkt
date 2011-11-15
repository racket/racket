#lang racket/base
(require (prefix-in pl- '#%place)
         '#%boot
         (only-in '#%paramz parameterization-key make-custodian-from-main)
         (only-in '#%futures processor-count)
         '#%place-struct
         racket/fixnum
         racket/flonum
         racket/vector
         mzlib/private/streams

         (for-syntax racket/base
                     racket/syntax))

(provide dynamic-place
         dynamic-place*
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
         place*
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

(define (pump-place p pin pout perr in out err)
  (cond
    [(pl-place-enabled?)
      (define-values (t-in t-out t-err) (pump-ports (place-dead-evt p) pin pout perr in out err))
      (pl-place-pumper-threads p (vector t-in t-out t-err))]
    [else (void)]))

(define (dynamic-place module-path function)
  (start-place 'dynamic-place module-path function 
               #f (current-output-port) (current-error-port)))

(define (dynamic-place* module-path
                        function
                        #:in [in #f]
                        #:out [out (current-output-port)]
                        #:err [err (current-error-port)])
  (start-place* 'dynamic-place* module-path function in out err))

(define (start-place who module-path function in out err)
  (define-values (p i o e) (start-place* who
                                         module-path 
                                         function 
                                         in
                                         out
                                         err))
  (close-output-port i)
  p)

(define (start-place* who module-path function in out err)
  ;; Duplicate checks in that are in the primitive `pl-dynamic-place', 
  ;; unfortunately, but we want these checks before we start making
  ;; stream-pumping threads, etc.
  (unless (or (module-path? module-path) (path? module-path))
    (raise-type-error who "module-path or path" module-path))
  (unless (symbol? function)
    (raise-type-error who "symbol" function))
  (unless (or (not in) (input-port? in))
    (raise-type-error who "input-port or #f" in))
  (unless (or (not out) (output-port? out))
    (raise-type-error who "output-port or #f" out))
  (unless (or (not err) (output-port? err) (eq? err 'stdout))
    (raise-type-error who "output-port, #f, or 'stdout" err))
  (when (and (pair? module-path) (eq? (car module-path) 'quote)
             (not (module-predefined? module-path)))
    (raise-mismatch-error who "not a filesystem or predefined module-path: " module-path))
  (when (and (input-port? in) (port-closed? in))
    (raise-mismatch-error who "input port is closed: " in))
  (when (and (output-port? out) (port-closed? out))
    (raise-mismatch-error who "output port is closed: " out))
  (when (and (output-port? err) (port-closed? err))
    (raise-mismatch-error who "error port is closed: " err))
  (cond
    [(pl-place-enabled?)
      (define-values (p pin pout perr)
        (pl-dynamic-place module-path
                          function
                          (if-stream-in  who in)
                          (if-stream-out who out)
                          (if-stream-out who err)))

      (pump-place p pin pout perr in out err)
      (values p 
              (and (not in) pin)
              (and (not out) pout)
              (and (not err) perr))]

    [else
      (define-values (inr  inw ) (if in  (values #f #f) (make-pipe)))
      (define-values (outr outw) (if out (values #f #f) (make-pipe)))
      (define-values (errr errw) (if err (values #f #f) (make-pipe)))

      (parameterize ([current-input-port  (or in  inr)]
                     [current-output-port (or out outw)]
                     [current-error-port  (or err errw)])
        (values (th-dynamic-place module-path function)
                (and (not in ) inw )
                (and (not out) outr)
                (and (not err) errr)))]))

(define-for-syntax (place-form _in _out _err _start-place-func stx orig-stx)
  (syntax-case stx ()
    [(who ch body1 body ...)
     (if (eq? (syntax-local-context) 'module-begin)
         ;; when a `place' form is the only thing in a module mody:
         #`(begin #,stx)
         ;; normal case:
         (begin
           (unless (syntax-transforming-module-expression?)
             (raise-syntax-error #f "can only be used in a module" stx))
           (unless (identifier? #'ch)
             (raise-syntax-error #f "expected an identifier" stx #'ch))
           (with-syntax ([internal-def-name
                          (syntax-local-lift-expression #'(lambda (ch) body1 body ...))]
                         [func-name (generate-temporary #'place/anon)]
                         [in _in]
                         [out _out]
                         [err _err]
                         [start-place-func _start-place-func])
             (syntax-local-lift-provide #'(rename internal-def-name func-name))
             #'(place/proc (#%variable-reference) 'func-name 'who start-place-func in out err))))]
     [(_ ch)
      (raise-syntax-error #f "expected at least one body expression" orig-stx)]))

(define-syntax (place stx)
  (place-form #'#f #'(current-output-port) #'(current-error-port) #'start-place stx stx))

(define-syntax (place* stx)
  (syntax-case stx ()
    [(pf #:in in #:out out #:err err ch body ...) (place-form #'in #'out #'err  #'start-place* #'(pf ch body ...) stx)]
    [(pf #:in in #:out out ch body ...)           (place-form #'in #'out #'#f   #'start-place* #'(pf ch body ...) stx)]
    [(pf #:out out #:err err ch body ...)         (place-form #'#f #'out #'err  #'start-place* #'(pf ch body ...) stx)]
    [(pf #:in in #:err err ch body ...)           (place-form #'in #'#f  #'err  #'start-place* #'(pf ch body ...) stx)]
    [(pf #:in in ch body ...)                     (place-form #'in #'#f  #'#f   #'start-place* #'(pf ch body ...) stx)]
    [(pf #:out out ch body ...)                   (place-form #'#f #'out #'#f   #'start-place* #'(pf ch body ...) stx)]
    [(pf #:err err ch body ...)                   (place-form #'#f #'#f  #'err  #'start-place* #'(pf ch body ...) stx)]
    [(pf ch body ...)                             (place-form #'#f #'#f  #'#f   #'start-place* #'(pf ch body ...) stx)]))

(define (place/proc vr func-name who start-place-func in out err)
  (define name
    (resolved-module-path-name
     (variable-reference->resolved-module-path
      vr)))
  (when (and (symbol? name)
             (not (module-predefined? `(quote ,name))))
     (error who "the enclosing module's resolved name is not a path or predefined"))
  (start-place-func who (if (symbol? name) `(quote ,name) name) func-name in out err))
