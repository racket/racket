#lang racket/base
(require (prefix-in pl- '#%place)
         (only-in '#%futures processor-count)
         racket/place/private/th-place
         racket/place/private/prop
         racket/private/streams
         (for-syntax racket/base racket/syntax))

;; This module is mostly re-exported by `racket/place/dynamic`

(provide (protect-out dynamic-place
                      dynamic-place*
                      start-place
                      start-place*)
         place-wait
         place-kill
         (rename-out [place-break/opt place-break])
         place-channel
         place-channel-put
         place-channel-get
         place-channel?
         place?
         place-message-allowed?
         place-channel-put/get
         processor-count
         (rename-out [pl-place-enabled? place-enabled?])
         place-dead-evt
         place-location?
         prop:place-location)

(define-syntax (define-pl-func stx)
  (syntax-case stx ()
    [(_ func p args ...)
     (with-syntax [(func-sym #'(quote func))
                   (pl-func (format-id #'here "pl-~a" #'func))
                   (th-func (format-id #'here "th-~a" #'func))]
       #'(define (func p args ...)
           (cond
             [(prop:place? p) ((prop:place-ref p) func-sym p args ...)]
             [(pl-place-enabled?) (pl-func p args ...)]
             [else (th-func p args ...)])))]))

(define (place-channel-put/get ch msg)
  (place-channel-put ch msg)
  (place-channel-get ch))

(define place-channel (if (pl-place-enabled?)  pl-place-channel th-place-channel))

(define-pl-func place-wait p)
(define-pl-func place-kill p)
(define-pl-func place-break p kind)
(define-pl-func place-channel-put p msg)
(define-pl-func place-channel-get p)
(define-pl-func place-channel? p)
(define-pl-func place? p)
(define-pl-func place-message-allowed? p)
(define-pl-func place-dead-evt p)

(define-values (prop:place-location place-location? place-location-ref)
  (make-struct-type-property 'place-location
                             (lambda (v info)
                               (unless (and (procedure? v)
                                            (procedure-arity-includes? v 4))
                                 (raise-argument-error 'guard-for-prop:place-location
                                                       "(procedure-arity-includes/c 4)"
                                                       v))
                               v)))

(define place-break/opt
  (let ([place-break (lambda (p [kind #f]) (place-break p kind))])
    place-break))

(define (pump-place p pin pout perr in out err)
  (cond
    [(pl-place-enabled?)
      (define-values (t-in t-out t-err) (pump-ports (place-dead-evt p) pin pout perr in out err))
      (pl-place-pumper-threads p (vector t-in t-out t-err))]
    [else (void)]))

(define (dynamic-place module-path function #:at [node #f] #:named [named #f])
  (cond
    [node
     (unless (place-location? node)
       (raise-argument-error 'dynamic-place "(or/c place-location? #f)" node))
     ((place-location-ref node) node module-path function named)]
    [else
      (start-place 'dynamic-place module-path function
                   #f (current-output-port) (current-error-port))]))

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
    (raise-argument-error who "(or/c module-path? path?)" module-path))
  (unless (symbol? function)
    (raise-argument-error who "symbol?" function))
  (unless (or (not in) (input-port? in))
    (raise-argument-error who "(or/c input-port? #f)" in))
  (unless (or (not out) (output-port? out))
    (raise-argument-error who "(or/c output-port? #f)" out))
  (unless (or (not err) (output-port? err) (eq? err 'stdout))
    (raise-argument-error who "(or/c output-port? #f 'stdout)" err))
  (when (and (pair? module-path) (eq? (car module-path) 'quote)
             (not (module-predefined? module-path)))
    (raise-arguments-error who "not a filesystem or predefined module path" 
                           "module path" module-path))
  (when (and (input-port? in) (port-closed? in))
    (raise-arguments-error who "input port is closed" "port" in))
  (when (and (output-port? out) (port-closed? out))
    (raise-arguments-error who "output port is closed" "port" out))
  (when (and (output-port? err) (port-closed? err))
    (raise-arguments-error who "error port is closed" "port" err))
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
