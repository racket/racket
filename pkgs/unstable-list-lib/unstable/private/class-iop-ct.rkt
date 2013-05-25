#lang racket/base
;; owner: ryanc
(require (for-template racket/base
                       racket/class)
         syntax/parse
         syntax/stx)

(provide static-interface?
         make-static-interface
         static-interface-dynamic
         static-interface-members

         make-checked-binding
         checked-binding?
         checked-binding-dynamic
         checked-binding-iface

         checked-binding
         static-interface

         interface-expander?
         make-interface-expander
         interface-expander-proc

         interface-expander
         method-entry)


(define-struct static-interface (dynamic members)
  #:omit-define-syntaxes
  #:property prop:procedure
             (lambda (self stx)
               (syntax-case stx ()
                 [(ifname . args)
                  (datum->syntax stx (cons #'(#%expression ifname) #'args) stx)]
                 [ifname
                  (identifier? #'ifname)
                  (static-interface-dynamic self)])))

(define-struct raw-checked-binding (dynamic iface)
  #:omit-define-syntaxes
  #:property prop:procedure
             (lambda (self stx)
               (syntax-case stx (set!)
                 [(set! var expr)
                  #`(let ([newval expr])
                      (unless (is-a? newval #,(static-interface-dynamic
                                               (raw-checked-binding-iface self)))
                        (error 'check "interface check failed on: ~e" newval))
                      (set! #,(raw-checked-binding-dynamic self) newval))]
                 [(var . args)
                  (datum->syntax stx (cons #'(#%expression var) #'args) stx)]
                 [var
                  (identifier? #'var)
                  (raw-checked-binding-dynamic self)]
                 [else
                  (raise-syntax-error #f "oops" stx)])))

(define (make-checked-binding dynamic iface)
  (make-set!-transformer
   (make-raw-checked-binding dynamic iface)))

(define (checked-binding? x)
  (and (set!-transformer? x)
       (raw-checked-binding? (set!-transformer-procedure x))))

(define (checked-binding-dynamic x)
  (raw-checked-binding-dynamic (set!-transformer-procedure x)))

(define (checked-binding-iface x)
  (raw-checked-binding-iface (set!-transformer-procedure x)))


(define-struct interface-expander (proc)
  #:omit-define-syntaxes)


;; Syntax

(define-syntax-class static-interface
  (pattern x
           #:declare x (static static-interface? 'static-interface)
           #:attr value (attribute x.value)))

(define-syntax-class checked-binding
  (pattern x
           #:declare x (static checked-binding? 'checked-binding)
           #:attr value (attribute x.value)))

(define-syntax-class interface-expander
  (pattern x
           #:declare x (static interface-expander? 'interface-expander)
           #:attr value (attribute x.value)))

(define-syntax-class method-entry
  (pattern m:id
           #:with (method ...) #'(m))
  (pattern (macro:interface-expander . args)
           #:with (method ...)
                  (with-syntax ([((m ...) ...)
                                 (for/list ([m (stx->list
                                                ((interface-expander-proc (attribute macro.value))
                                                 #'(macro . args)))])
                                   (syntax-parse m
                                     [m:method-entry #'(m.method ...)]))])
                    #'(m ... ...))))
