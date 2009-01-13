#lang scheme/base
(require scheme/class
         (for-syntax scheme/base
                     macro-debugger/stxclass/stxclass
                     "class-ct.ss"))
(provide define-interface
         define-interface/dynamic

         send:
         send*:
         send/apply:

         define:
         lambda:
         init:
         init-private:)

;; Configuration
(define-for-syntax warn-on-dynamic-interfaces? #f)
(define-for-syntax warn-on-dynamic-object-check-generation? #f)
(define-for-syntax define-dotted-names #f)

;; define-interface SYNTAX
;; (define-interface NAME (IDENTIFIER ...))
;; Defines NAME as an interface.
(define-syntax (define-interface stx)
  (syntax-parse stx
    [(_ name:id (mname:id ...))
     #'(define-interface/dynamic name
         (let ([name (interface () mname ...)]) name)
         (mname ...))]))

;; define-interface/dynamic SYNTAX
;; (define-interface/dynamic NAME EXPR (IDENTIFIER ...))
;; Defines NAME as a static interface containing the names listed.
;; The EXPR is used as the dynamic componenent of the interface, and 
;; it should contain a superset of the names listed.
(define-syntax (define-interface/dynamic stx)
  (syntax-parse stx
    [(_ name:id dynamic-interface:expr (mname:id ...))
     (with-syntax ([(dynamic-name) (generate-temporaries #'(name))])
       #'(begin (define dynamic-name
                  (let ([dynamic-name dynamic-interface])
                    (for-each 
                     (lambda (m)
                       (unless (method-in-interface? m dynamic-name)
                         (error 'name "dynamic interface missing method '~s'" m)))
                     '(mname ...))
                    dynamic-name))
                (define-syntax name
                  (make-static-interface #'dynamic-name '(mname ...)))))]))

;; Helper

(begin-for-syntax
  (define (check-method-in-interface for-whom method si)
    (unless (member (syntax-e method) (static-interface-members si))
      (raise-syntax-error for-whom
                          "method not in static interface"
                          method))))

;; Checked send

(define-syntax (send: stx)
  (syntax-parse stx
    [(send: obj:expr iface:static-interface method:id . args)
     (begin (check-method-in-interface 'send: #'method #'iface.value)
            (syntax/loc stx
              (send (check-object<:interface send: obj iface)
                    method . args)))]))

(define-syntax (send*: stx)
  (syntax-parse stx
    [(send*: obj:expr iface:static-interface (method:id . args) ...)
     (begin (for ([method (syntax->list #'(method ...))])
              (check-method-in-interface 'send*: method #'iface.value))
            (syntax/loc stx 
              (send* (check-object<:interface send*: obj iface)
                (method . args) ...)))]))

(define-syntax (send/apply: stx)
  (syntax-parse stx
    [(send/apply: obj:expr iface:static-interface method:id . args)
     (begin (check-method-in-interface 'send/apply: #'method #'iface.value)
            (syntax/loc stx 
              (send/apply (check-object<:interface send/apply obj iface)
                          method . args)))]))

;;

;; check-object<:interface SYNTAX
(define-syntax (check-object<:interface stx)
  (syntax-parse stx
    [(_ for-whom obj:checked-binding iface:static-interface)
     (if (eq? (checked-binding-iface #'obj.value) #'iface.value)
         #'obj
         (syntax/loc stx
           (check-object<:interface for-whom
                                    (#%expression obj)
                                    (#%expression iface))))]
    [(_ for-whom obj:expr iface:expr)
     (begin
       (when warn-on-dynamic-object-check-generation?
         (printf "dynamic object check: ~s,~s~n"
                 (syntax-source #'obj)
                 (syntax-line #'obj)))
       #'(dynamic:check-object<:interface 'for-whom obj iface))]))

(define (dynamic:check-object<:interface for-whom obj iface)
  (unless (is-a? obj iface)
    (error for-whom "interface check failed on: ~e" obj))
  obj)

;;

(define-syntax (define: stx)
  (syntax-parse stx
    [(_ name:id iface:static-interface expr)
     (let ([si #'iface.value])
       (with-syntax ([(name-internal) (generate-temporaries #'(name))]
                     [(method ...) (static-interface-members si)]
                     [(name.method ...)
                      (map (lambda (m)
                             (datum->syntax #'name
                                            (string->symbol (format "~a.~a" (syntax-e #'name) m))))
                           (static-interface-members si))])
         #`(begin (define name-internal
                    (check-object<:interface define: expr iface))
                  (define-syntax name
                    (make-checked-binding
                     #'name-internal
                     (syntax-local-value #'iface)))
                  #,(if define-dotted-names
                        #'(begin
                            (define-syntax name.method
                              (syntax-rules ()
                                [(name.method . args)
                                 (send: name iface method . args)]))
                            ...)
                        #'(begin)))))]
    [(_ (f:id . args) . body)
     #'(define f (lambda: args . body))]))

(define-syntax (lambda: stx)
  ;; FIXME: rewrite as stxclass
  (define (arg->define stx temp)
    (syntax-case stx ()
      [(arg : iface) 
       (and (identifier? #'arg)
            (eq? ': (syntax-e #':)))
       #`(define: arg iface #,temp)]
      [arg
       (identifier? #'arg)
       #`(define-syntax arg (make-rename-transformer #'#,temp))]))
  (syntax-parse stx
    [(_ (arg ...) . body)
     (let ([temporaries (generate-temporaries #'(arg ...))])
       (with-syntax ([(temp ...) temporaries]
                     [(checked-definition ...)
                      (map arg->define
                           (syntax->list #'(arg ...))
                           temporaries)])
         #'(lambda (temp ...)
             (let ()
               checked-definition ...
               (let () . body)))))]))


;; FIXME: unsafe due to mutation
(define-syntax (init-field: stx)
  (syntax-parse stx
    [(_ (name:id iface:static-interface) ...)
     #'(begin (init1: init-field name iface) ...)]))

(define-syntax (init: stx)
  (syntax-parse stx
    [(_ (name:id iface:static-interface) ...)
     #'(begin (init1: init name iface) ...)]))

(define-syntax (init1: stx)
  (syntax-parse stx
    [(_ init name:id iface:static-interface)
     (with-syntax ([(name-internal) (generate-temporaries #'(name))])
       #'(begin (init (name name-internal))
                (void (check-object<:interface init: name-internal iface))
                (define-syntax name 
                  (make-checked-binding
                   #'name-internal
                   (syntax-local-value #'iface)))))]))

(define-syntax (init-private stx)
  (syntax-parse stx
    [(init-private form ...)
     #'(begin (init-private1 form) ...)]))

(define-syntax (init-private1 stx)
  (syntax-parse stx
    [(init-private1 id:id)
     (with-syntax ([(id-internal) (generate-temporaries #'(id))])
       #'(begin (init (id-internal id))
                (define id id-internal)))]))

(define-syntax (init-private: stx)
  (syntax-parse stx
    [(_ (name:id iface:static-interface) ...)
     #'(begin (init-private1: name iface) ...)]))

(define-syntax (init-private1: stx)
  (syntax-parse stx
    [(_ name:id iface:static-interface)
     (with-syntax ([(id-internal) (generate-temporaries #'(id))])
       #'(begin (init (id-internal name))
                (define: name iface id-internal)))]))
