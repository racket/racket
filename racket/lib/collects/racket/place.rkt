#lang racket/base
(require (prefix-in pl- '#%place)
         '#%boot
         (only-in '#%paramz parameterization-key make-custodian-from-main)
         (only-in '#%futures processor-count)
         '#%place-struct
         racket/fixnum
         racket/flonum
         racket/vector
         racket/place/private/th-place
         racket/place/private/prop
         racket/private/streams
         racket/lazy-require


         (for-syntax racket/base
                     racket/syntax))

(provide dynamic-place
         dynamic-place*
         place-sleep
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
         place
         place*
         (rename-out [pl-place-enabled? place-enabled?])
         place-dead-evt
         )

(define-syntax (define-pl-func stx)
  (syntax-case stx ()
    [(_ func p args ...)
     (with-syntax [(func-sym #'(quote func))
                   (pl-func  (string->symbol (string-append "pl-" (symbol->string (syntax->datum #'func)))))
                   (th-func  (string->symbol (string-append "th-" (symbol->string (syntax->datum #'func)))))]
       #'(define (func p args ...)
           (cond
             [(prop:place? p) ((prop:place-ref p) func-sym p args ...)]
             [(pl-place-enabled?) (pl-func p args ...)]
             [else (th-func p args ...)])))]))

(lazy-require [racket/place/distributed (supervise-place-at)])

(define (place-channel-put/get ch msg)
  (place-channel-put ch msg)
  (place-channel-get ch))

(define place-channel (if (pl-place-enabled?)  pl-place-channel th-place-channel))

(define-pl-func place-sleep p)
(define-pl-func place-wait p)
(define-pl-func place-kill p)
(define-pl-func place-break p kind)
(define-pl-func place-channel-put p msg)
(define-pl-func place-channel-get p)
(define-pl-func place-channel? p)
(define-pl-func place? p)
(define-pl-func place-message-allowed? p)
(define-pl-func place-dead-evt p)

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
      (supervise-place-at node module-path function #:named named)]
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


(define-for-syntax (modpath->string modpath)
  (cond
    [(equal? modpath #f)
     (number->string (current-inexact-milliseconds))]
    [else
      (define name (resolved-module-path-name modpath))
      (cond
        [(symbol? name) (symbol->string name)]
        [(path? name) (path->string name)])]))

(define-for-syntax (place-form _in _out _err _start-place-func stx orig-stx)
  (syntax-case stx ()
    [(who ch body1 body ...)
     (if (eq? (syntax-local-context) 'module-begin)
         ;; when a `place' form is the only thing in a module mody:
         #`(begin #,stx)
         ;; normal case:
         (let ()
           (unless (syntax-transforming-module-expression?)
             (raise-syntax-error #f "can only be used in a module" stx))
           (unless (identifier? #'ch)
             (raise-syntax-error #f "expected an identifier" stx #'ch))
           (define func-name-stx
             (datum->syntax stx
               (string->symbol
                 (string-append "place/anon"
                                (modpath->string (current-module-declare-name))))))
           (with-syntax ([internal-def-name
                          (syntax-local-lift-expression #'(lambda (ch) body1 body ...))]
                         [func-name (generate-temporary func-name-stx)]
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
