(module generic-interfaces "pre-base.rkt"

  ;; Defines (forgeries of) generic interfaces that correspond to struct
  ;; properties that come from racket/base.
  ;; Since racket/base can't depend on racket/generics, we can't use
  ;; `define-generics' to build these generic interfaces. Thus we must
  ;; forge them.

  (#%require (for-syntax '#%kernel))

  (#%provide gen:equal+hash gen:custom-write)

  (define-values (prop:gen:equal+hash equal+hash? gen:equal+hash-acc)
    (make-struct-type-property
     'prop:gen:equal+hash
     (lambda (v si)
       (if (and (vector? v)
                (= 3 (vector-length v))
                (procedure? (vector-ref v 0))
                (procedure-arity-includes? (vector-ref v 0) 3)
                (procedure? (vector-ref v 1))
                (procedure-arity-includes? (vector-ref v 1) 2)
                (procedure? (vector-ref v 2))
                (procedure-arity-includes? (vector-ref v 2) 2))
           v
           (raise-type-error 'guard-for-prop:gen:equal+hash
                             "vector of three procedures (arities 3, 2, 2)"
                             v)))
     (list (cons prop:equal+hash vector->list))))

  (define-syntax gen:equal+hash
    (list (quote-syntax prop:gen:equal+hash)
          (quote-syntax equal-proc)
          (quote-syntax hash-proc)
          (quote-syntax hash2-proc)))


  (define-values (prop:gen:custom-write gen:custom-write? gen:custom-write-acc)
    (make-struct-type-property
     'prop:gen:custom-write
     (lambda (v si)
       (if (and (vector? v)
                (= 1 (vector-length v))
                (procedure? (vector-ref v 0))
                (procedure-arity-includes? (vector-ref v 0) 3))
           v
           (raise-type-error 'guard-for-prop:gen:custom-write
                             "vector of one procedure (arity 3)"
                             v)))
     (list (cons prop:custom-write (lambda (v) (vector-ref v 0))))))

  (define-syntax gen:custom-write
    (list (quote-syntax prop:gen:custom-write)
          (quote-syntax write-proc)))

  )
