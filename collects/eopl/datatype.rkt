;; NOTE: datatypes are currently transparent, for the sake of EoPL's
;; use of `equal?'

#lang racket/base

(require (for-syntax racket/base "private/utils.rkt"))

(define-syntax (define-datatype stx)
  (syntax-case stx ()
    [(_ name pred-name
        (variant-name (field-name field-pred) ...)
        ...)
     (let ([variant-names (syntax->list #'(variant-name ...))])
       ;; More syntax checks...
       (unless (identifier? #'name)
         (raise-syntax-error
          #f "expected an identifier for the datatype name" stx #'name))
       (unless (identifier? #'pred-name)
         (raise-syntax-error
          #f "expected an identifier for the predicate name" stx #'pred-name))
       (for ([vt     (in-list variant-names)]
             [fields (in-list (syntax->list #'((field-name ...) ...)))])
         (unless (identifier? vt)
           (raise-syntax-error
            'cases "expected an identifier for the variant name" stx vt))
         (for ([field (in-list (syntax->list fields))])
           (unless (identifier? field)
             (raise-syntax-error
              'cases "expected an identifier for the field name" stx field))))
       ;; Count the fields for each variant:
       (with-syntax ([(variant-field-count ...)
                      (for/list ([x (in-list (syntax->list
                                              #'((field-name ...) ...)))])
                        (datum->syntax (quote-syntax here)
                                       (length (syntax->list x))
                                       #f))]
                     [(variant? ...)
                      (for/list ([vn (in-list variant-names)])
                        (datum->syntax
                         vn
                         (string->uninterned-symbol
                          (format "~a?" (syntax-e vn)))))]
                     [(variant-accessor ...)
                      (for/list ([vn (in-list variant-names)])
                        (datum->syntax
                         vn
                         (string->uninterned-symbol
                          (format "~a-accessor" (syntax-e vn)))))]
                     [(variant-mutator ...)
                      (generate-temporaries variant-names)]
                     [(make-variant ...)
                      (generate-temporaries variant-names)]
                     [(struct:variant ...)
                      (generate-temporaries variant-names)]
                     [(make-variant-name ...)
                      (for/list ([vn (in-list variant-names)])
                        (datum->syntax
                         vn
                         (string->symbol
                          (format "make-~a" (syntax-e vn)))))])
         #'(begin
             (define-syntax name
               ;; Note: we're back to the transformer environment, here.
               ;; Also, this isn't a transformer function, so any direct
               ;;  use of the name will trigger a syntax error. The name
               ;;  can be found by `syntax-local-value', though.
               (let ([cert (syntax-local-certifier #t)])
                 (make-dt (cert #'pred-name)
                          (list (make-vt (cert #'variant-name)
                                         (cert #'variant?)
                                         (cert #'variant-accessor)
                                         variant-field-count)
                                ...))))
             ;; Bind the predicate and selector functions:
             (define-values (pred-name
                             variant-name ...
                             variant? ...
                             variant-accessor ...)
               ;; Create a new structure for the datatype (using the
               ;; datatype name in `struct', so it prints nicely).
               (let-values ([(struct:x make-x x? acc mut)
                             (make-struct-type 'name #f 0 0 #f null (make-inspector))])
                 (let-values ([(struct:variant make-variant variant?
                                variant-accessor variant-mutator)
                               (make-struct-type 'variant-name struct:x variant-field-count 0
                                                 #f
                                                 null
                                                 (make-inspector))]
                              ...)
                   ;; User-available functions:
                   (values
                    x? ;; The datatype predicate
                    ;; Create the constructor functions:
                    (let* ([vname 'variant-name]
                           [variant-name
                            (lambda (field-name ...)
                              (unless (field-pred field-name)
                                (error vname "bad value for ~a field: ~e"
                                       'field-name field-name))
                              ...
                              (make-variant field-name ...))])
                      variant-name)
                    ...
                    variant? ...
                    variant-accessor ...))))
             ;; Compatibility bindings
             (define-values (make-variant-name ...) (values variant-name ...)))))]
    [(_ name pred-name variant ...)
     ;; Must be a bad variant...
     (for ([variant (in-list (syntax->list #'(variant ...)))])
       (syntax-case variant ()
         [(variant-name field ...)
          (let ([name #'variant-name])
            (unless (identifier? name)
              (raise-syntax-error
               #f "expected an identifier for the variant name" stx name))
            ;; Must be a bad field:
            (for ([field (in-list (syntax->list #'(field ...)))])
              (syntax-case field ()
                [(field-name field-pred)
                 (let ([name #'field-name])
                   (unless (identifier? name)
                     (raise-syntax-error
                      #f "expected an identifier for the field name" stx name)))]
                [_else
                 (raise-syntax-error
                  #f "expected a field name followed by a predicate expression, all in parentheses" stx field)])))]
         [_else
          (raise-syntax-error
           #f "expected a variant name followed by a sequence of field declarations, all in parentheses" stx variant)]))]
    [(_ name)
     (raise-syntax-error
      #f "missing predicate name and variant clauses" stx)]))

(define-syntax (cases stx)
  (syntax-case stx ()
    [(_ datatype expr
        clause
        ...)
     ;; Get datatype information:
     (let ([dt (and (identifier? #'datatype)
                    (syntax-local-value #'datatype (lambda () #f)))])
       (unless (dt? dt)
         (raise-syntax-error 'cases "not a datatype name" stx #'datatype))

       ;; Parse clauses:
       (define-values (vts field-idss bodys else-body)
         (let loop ([clauses (syntax->list #'(clause ...))]
                    [saw-cases null])
           (if (null? clauses)
             (values null null null #f)
             (let ([clause (car clauses)])
               (syntax-case* clause ()
                             (lambda (a b)
                               (and (eq? (syntax-e b) 'else)
                                    (not (identifier-binding b))))
                 [(variant (field-id ...) body0 body1 ...)
                  (let* ([variant #'variant]
                         [vt (ormap (lambda (dtv)
                                      (define vt-name (vt-name-stx dtv))
                                      (and (free-identifier=? variant vt-name)
                                           dtv))
                                    (dt-variants dt))]
                         [orig-variant (and vt (vt-name-stx vt))])
                    (unless orig-variant
                      (raise-syntax-error
                       #f
                       (format "not a variant of `~a'"
                               (syntax->datum #'datatype))
                       stx
                       variant))

                    (let ([field-ids (syntax->list #'(field-id ...))])
                      (for ([fid (in-list field-ids)])
                        (unless (identifier? fid)
                          (raise-syntax-error
                           #f "expected an identifier for a field" stx fid)))
                      (let ([dtv (variant-assq variant (dt-variants dt))])
                        (unless (= (length field-ids) (vt-field-count dtv))
                          (raise-syntax-error
                           #f
                           (format
                            "variant case `~a' for `~a' has wrong field count (expected ~a, found ~a)"
                            (syntax->datum variant)
                            (syntax->datum #'datatype)
                            (vt-field-count dtv)
                            (length field-ids))
                           stx
                           clause)))

                      ;; Check for duplicate local field ids:
                      (let ([dup (check-duplicate-identifier field-ids)])
                        (when dup
                          (raise-syntax-error
                           #f "duplicate field identifier" stx dup)))

                      ;; Check for redundant case:
                      (when (memq orig-variant saw-cases)
                        (raise-syntax-error #f "duplicate case" stx clause))

                      ;; This clause is ok:
                      (let-values ([(vts idss bodys else)
                                    (loop (cdr clauses) (cons orig-variant saw-cases))])
                        (values (cons vt vts)
                                (cons field-ids idss)
                                (cons #'(begin body0 body1 ...) bodys)
                                else))))]
                 [(else body0 body1 ...)
                  (begin
                    (unless (null? (cdr clauses))
                      (raise-syntax-error
                       #f "else clause must be last" stx clause))
                    (values null null null #'(begin body0 body1 ...)))]
                 [_else (raise-syntax-error #f "bad clause" stx clause)])))))
       ;; Missing any variants?
       (unless (or else-body (= (length vts) (length (dt-variants dt))))
         (define here (map vt-name-stx vts))
         (define missing
           (let loop ([l (dt-variants dt)])
             (cond [(null? l) ""]
                   [(ormap (lambda (i) (free-identifier=? (vt-name-stx (car l)) i)) here)
                    (loop (cdr l))]
                   [else (format " ~a~a"
                                 (syntax-e (vt-name-stx (car l)))
                                 (loop (cdr l)))])))
         (raise-syntax-error
          #f
          (format "missing cases for the following variants:~a" missing)
          stx))

       ;; Create the result:
       (with-syntax ([pred (dt-pred-stx dt)]
                     [(variant? ...) (map vt-predicate-stx vts)]
                     [((field-extraction ...) ...)
                      (for/list ([vt (in-list vts)])
                        (with-syntax ([accessor (vt-accessor-stx vt)])
                          (let loop ([n 0])
                            (if (= n (vt-field-count vt))
                              null
                              (cons #`(accessor v #,n)
                                    (loop (add1 n)))))))]
                     [((field-id ...) ...) field-idss]
                     [(body ...) bodys]
                     [else-body (or else-body
                                    #'(error 'cases "no variant case matched"))])
         #'(let ([v expr])
             (if (not (pred v))
               (error 'cases "not a ~a: ~s" (quote datatype) v)
               (cond
                 [(variant? v)
                  (let ([field-id field-extraction] ...)
                    body)]
                 ...
                 [else else-body])))))]))

(define-syntax (provide-datatype stx)
  (syntax-case stx ()
    [(_ datatype)
     (let ([dt (syntax-local-value #'datatype (lambda () #f))])
       (unless (dt? dt)
         (raise-syntax-error #f "not a datatype name" stx #'datatype))
       (with-syntax ([pred (dt-pred-stx dt)]
                     [(orig-variant ...) (map vt-name-stx (dt-variants dt))])
         #'(provide datatype pred orig-variant ...)))]))

(provide define-datatype cases provide-datatype)
