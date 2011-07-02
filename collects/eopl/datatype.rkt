;; NOTE: datatypes are currently transparent, for the sake of EoPL's
;; use of `equal?'

#lang mzscheme

(require mzlib/pconvert-prop)
(require-for-syntax "private/utils.rkt")

(define-syntax define-datatype
  (lambda (stx)
    (syntax-case stx ()
      [(_ name pred-name
          (variant-name (field-name field-pred) ...)
          ...)
       (let ([variant-names (syntax->list (syntax (variant-name ...)))])
         ;; More syntax checks...
         (unless (identifier? (syntax name))
           (raise-syntax-error #f
                               "expected an identifier for the datatype name"
                               stx (syntax name)))
         (unless (identifier? (syntax pred-name))
           (raise-syntax-error #f
                               "expected an identifier for the predicate name"
                               stx (syntax pred-name)))
         (for-each (lambda (vt fields)
                     (unless (identifier? vt)
                       (raise-syntax-error
                        'cases
                        "expected an identifier for the variant name"
                        stx vt))
                     (for-each (lambda (field)
                                 (unless (identifier? field)
                                   (raise-syntax-error
                                    'cases
                                    "expected an identifier for the field name"
                                    stx field)))
                               (syntax->list fields)))
                   variant-names
                   (syntax->list (syntax ((field-name ...) ...))))
         ;; Count the fields for each variant:
         (with-syntax ([(variant-field-count ...)
                        (map (lambda (n)
                               (datum->syntax-object (quote-syntax here) n #f))
                             (map length
                                  (map
                                   syntax->list
                                   (syntax->list
                                    (syntax ((field-name ...) ...))))))]
                       [(variant? ...)
                        (map (lambda (vn)
                               (datum->syntax-object
                                vn
                                (string->uninterned-symbol
                                 (format "~a?" (syntax-e vn)))))
                             variant-names)]
                       [(variant-accessor ...)
                        (map (lambda (vn)
                               (datum->syntax-object
                                vn
                                (string->uninterned-symbol
                                 (format "~a-accessor" (syntax-e vn)))))
                             variant-names)]
                       [(variant-mutator ...)
                        (generate-temporaries variant-names)]
                       [(make-variant ...)
                        (generate-temporaries variant-names)]
                       [(struct:variant ...)
                        (generate-temporaries variant-names)]
                       [(make-variant-name ...)
                        (map (lambda (vn)
                               (datum->syntax-object
                                vn
                                (string->symbol
                                 (format "make-~a" (syntax-e vn)))))
                             variant-names)])
           (syntax
             (begin
               (define-syntax name
                 ;; Note: we're back to the transformer environment, here.
                 ;; Also, this isn't a transformer function, so any direct
                 ;;  use of the name will trigger a syntax error. The name
                 ;;  can be found by `syntax-local-value', though.
                 (let ([cert (syntax-local-certifier #t)])
                   (make-dt (cert (syntax pred-name))
                            (list
                             (make-vt (cert (syntax variant-name))
                                      (cert (syntax variant?))
                                      (cert (syntax variant-accessor))
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
                                                   `((,prop:print-convert-constructor-name . variant-name))
                                                   (make-inspector))]
                                ...)
                     ;; User-available functions:
                     (values
                      x? ;; The datatype predicate
                      ;; Create the constructor functions:
                      (let ([vname (quote variant-name)])
                        (let ([variant-name
                               (lambda (field-name ...)
                                 (unless (field-pred field-name)
                                   (error vname
                                          "bad value for ~a field: ~e"
                                          (quote field-name)
                                          field-name))
                                 ...
                                 (make-variant field-name ...))])
                          variant-name))
                      ...
                      variant? ...
                      variant-accessor ...))))
               ;; Compatibility bindings
               (define-values (make-variant-name ...) (values variant-name ...))))))]
      [(_ name pred-name variant ...)
       ;; Must be a bad variant...
       (for-each (lambda (variant)
                   (syntax-case variant ()
                     [(variant-name field ...)
                      (let ([name (syntax variant-name)])
                        (unless (identifier? name)
                          (raise-syntax-error
                           #f
                           "expected an identifier for the variant name"
                           stx
                           name))
                        ;; Must be a bad field:
                        (for-each (lambda (field)
                                    (syntax-case field ()
                                      [(field-name field-pred)
                                       (let ([name (syntax field-name)])
                                         (unless (identifier? name)
                                           (raise-syntax-error
                                            #f
                                            "expected an identifier for the field name"
                                            stx
                                            name)))]
                                      [_else
                                       (raise-syntax-error
                                        #f
                                        "expected a field name followed by a predicate expression, all in parentheses"
                                        stx
                                        field)]))
                                  (syntax->list (syntax (field ...)))))]
                     [_else
                      (raise-syntax-error
                       #f
                       "expected a variant name followed by a sequence of field declarations, all in parentheses"
                       stx
                       variant)]))
                 (syntax->list (syntax (variant ...))))]
      [(_ name)
       (raise-syntax-error
        #f
        "missing predicate name and variant clauses"
        stx)])))

(define-syntax cases
  (lambda (stx)
    (syntax-case stx ()
      [(_ datatype expr
          clause
          ...)
       ;; Get datatype information:
       (let ([dt (and (identifier? (syntax datatype))
                      (syntax-local-value (syntax datatype) (lambda () #f)))])
         (unless (dt? dt)
           (raise-syntax-error
            'cases
            "not a datatype name"
            stx
            (syntax datatype)))

         ;; Parse clauses:
         (let-values ([(vts field-idss bodys else-body)
                       (let loop ([clauses (syntax->list (syntax (clause ...)))][saw-cases null])
                         (cond
                           [(null? clauses)
                            (values null null null #f)]
                           [else
                            (let ([clause (car clauses)])
                              (syntax-case* clause (else)
                                            (lambda (a b)
                                              (and (eq? (syntax-e b) 'else)
                                                   (not (identifier-binding b))))
                                [(variant (field-id ...) body0 body1 ...)
                                 (let* ([variant (syntax variant)]
                                        [vt
                                         (ormap (lambda (dtv)
                                                  (let ([vt-name (vt-name-stx dtv)])
                                                    (and (module-identifier=? variant vt-name)
                                                         dtv)))
                                                (dt-variants dt))]
                                        [orig-variant (and vt (vt-name-stx vt))])
                                   (unless orig-variant
                                     (raise-syntax-error
                                      #f
                                      (format "not a variant of `~a'"
                                              (syntax-object->datum (syntax datatype)))
                                      stx
                                      variant))

                                   (let ([field-ids (syntax->list (syntax (field-id ...)))])
                                     (for-each (lambda (fid)
                                                 (unless (identifier? fid)
                                                   (raise-syntax-error
                                                    #f
                                                    "expected an identifier for a field"
                                                    stx
                                                    fid)))
                                               field-ids)
                                     (let ([dtv (variant-assq variant (dt-variants dt))])
                                       (unless (= (length field-ids)
                                                  (vt-field-count dtv))
                                         (raise-syntax-error
                                          #f
                                          (format
                                           "variant case `~a' for `~a' has wrong field count (expected ~a, found ~a)"
                                           (syntax-object->datum variant)
                                           (syntax-object->datum (syntax datatype))
                                           (vt-field-count dtv)
                                           (length field-ids))
                                          stx
                                          clause)))

                                     ;; Check for duplicate local field ids:
                                     (let ([dup (check-duplicate-identifier field-ids)])
                                       (when dup
                                         (raise-syntax-error
                                          #f
                                          "duplicate field identifier"
                                          stx
                                          dup)))

                                     ;; Check for redundant case:
                                     (when (memq orig-variant saw-cases)
                                       (raise-syntax-error
                                        #f
                                        "duplicate case"
                                        stx
                                        clause))

                                     ;; This clause is ok:
                                     (let-values ([(vts idss bodys else)
                                                   (loop (cdr clauses) (cons orig-variant saw-cases))])
                                       (values (cons vt vts)
                                               (cons field-ids idss)
                                               (cons (syntax (begin body0 body1 ...)) bodys)
                                               else))))]
                                [(else body0 body1 ...)
                                 (begin
                                   (unless (null? (cdr clauses))
                                     (raise-syntax-error
                                      #f
                                      "else clause must be last"
                                      stx
                                      clause))
                                   (values null null null (syntax (begin body0 body1 ...))))]
                                [_else (raise-syntax-error
                                        #f
                                        "bad clause"
                                        stx
                                        clause)]))]))])

           ;; Missing any variants?
           (unless (or else-body
                       (= (length vts) (length (dt-variants dt))))
             (let* ([here (map vt-name-stx vts)]
                    [missing (let loop ([l (dt-variants dt)])
                               (cond
                                 [(null? l) ""]
                                 [(ormap (lambda (i) (module-identifier=? (vt-name-stx (car l)) i)) here)
                                  (loop (cdr l))]
                                 [else
                                  (format " ~a~a"
                                          (syntax-e (vt-name-stx (car l)))
                                          (loop (cdr l)))]))])
               (raise-syntax-error
                #f
                (format "missing cases for the following variants:~a" missing)
                stx)))

           ;; Create the result:
           (with-syntax ([pred (dt-pred-stx dt)]
                         [(variant? ...) (map vt-predicate-stx vts)]
                         [((field-extraction ...) ...)
                          (map (lambda (vt)
                                 (with-syntax ([accessor (vt-accessor-stx vt)])
                                   (let loop ([n 0])
                                     (if (= n (vt-field-count vt))
                                       null
                                       (cons (with-syntax ([n n])
                                               (syntax (accessor v n)))
                                             (loop (add1 n)))))))
                               vts)]
                         [((field-id ...) ...) field-idss]
                         [(body ...) bodys]
                         [else-body (or else-body
                                        (syntax
                                          (error 'cases "no variant case matched")))])
             (syntax
               (let ([v expr])
                 (if (not (pred v))
                   (error 'cases "not a ~a: ~s" (quote datatype) v)
                   (cond
                     [(variant? v)
                      (let ([field-id field-extraction] ...)
                        body)]
                     ...
                     [else else-body])))))))])))

(define-syntax provide-datatype
  (lambda (stx)
    (syntax-case stx ()
      [(_ datatype)
       (let ([dt (syntax-local-value (syntax datatype) (lambda () #f))])
         (unless (dt? dt)
           (raise-syntax-error
            #f
            "not a datatype name"
            stx
            (syntax datatype)))
         (with-syntax ([pred (dt-pred-stx dt)]
                       [(orig-variant ...)
                        (map vt-name-stx (dt-variants dt))])
           (syntax
             (provide datatype
                      pred
                      orig-variant ...))))])))

(provide define-datatype cases provide-datatype)
