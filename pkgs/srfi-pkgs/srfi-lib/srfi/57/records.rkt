; SRFI 57
; Zhu Chongkai   mrmathematica@yahoo.com
; Based on original author's syntax-case implementation
(module records mzscheme

  (provide define-record-type
           define-record-scheme
           record-update
           record-update!
           record-compose
           show)

  (require            (prefix s9: srfi/9))
  (require-for-syntax (prefix s1: srfi/1))

  (require-for-syntax "registry.rkt")

  (define-syntax define-record-type
    (syntax-rules ()
      ((define-record-type . body)
       (parse-declaration #f . body))))

  (define-syntax define-record-scheme
    (syntax-rules ()
      ((define-record-scheme . body)
       (parse-declaration #t . body))))

  (define-syntax parse-declaration
    (syntax-rules ()
      ((parse-declaration is-scheme? (name super ...) (constructor pos-label ...) predicate field-clause ...)
       (build-record (constructor pos-label ...)  #f (super ...) (field-clause ...) name predicate is-scheme?))
      ((parse-declaration is-scheme? (name super ...) constructor predicate field-clause ...)
       (build-record (constructor)  #t (super ...) (field-clause ...) name predicate is-scheme?))
      ((parse-declaration is-scheme? (name super ...) constructor-clause)
       (parse-declaration is-scheme? (name super ...) constructor-clause #f))
      ((parse-declaration is-scheme? (name super ...))
       (parse-declaration is-scheme? (name super ...) #f #f))
      ((parse-declaration is-scheme? name . rest)
       (parse-declaration is-scheme? (name) . rest))))

  (define-syntax record-update!
    (lambda (stx)
      (syntax-case stx ()
        ((_ record name (label exp) ...)
         (with-syntax (((setter ...)
                        (map (lambda (label)
                               (lookup-setter #`name label))
                             (syntax->list #`(label ...)))))
           #`(let ((r record))
               (setter r exp)
               ...
               r))))))

  (define-syntax record-update
    (lambda (stx)
      (syntax-case stx ()
        ((_ record name (label exp) ...)
         (if (lookup-scheme? #`name)
             (with-syntax ((copier (lookup-copier #`name)))
               #`(let ((new (copier record)))
                   (record-update! new name (label exp) ...)))
             #`(record-compose (name record) (name (label exp) ...)))))))


  (define-syntax record-compose
    (lambda (stx)
      (syntax-case stx ()
        ((record-compose (export-name (label exp) ...))
         #`(export-name (label exp) ...))
        ((record-compose (import-name record) import ... (export-name (label exp) ...))
         (with-syntax
             (((copy-label ...)
               (s1:lset-intersection free-identifier=?
                                     (lookup-labels #`export-name)
                                     (s1:lset-difference free-identifier=?
                                                         (lookup-labels #`import-name)
                                                         (syntax->list #`(label ...))))))
           (with-syntax (((getter ...)
                          (s1:map (lambda (label)
                                    (lookup-getter #`import-name label))
                                  (syntax->list #`(copy-label ...)))))
             #`(let ((r record))
                 (record-compose import ...
                                 (export-name (copy-label (getter r))
                                              ...
                                              (label exp)
                                              ...)))))))))

  (define-syntax build-record
    (let ()

      (define (build-record stx)
        (syntax-case stx ()
          ((build-record (constructor pos-label ...)
                         default-order?
                         (super ...)
                         ((field-label . accessors) ...)
                         name
                         predicate
                         is-scheme?)
           (with-syntax
               (((label ...)
                 (s1:delete-duplicates (s1:fold-right append
                                                      (syntax->list #`(pos-label ... field-label ...))
                                                      (map lookup-labels
                                                           (syntax->list #`(super ...))))
                                       free-identifier=?))
                ((super ...)
                 (s1:delete-duplicates (s1:fold-right append
                                                      '()
                                                      (map lookup-supers
                                                           (syntax->list #`(super ...))))
                                       free-identifier=?)))
             (with-syntax
                 (((pos-label ...)

                   (if (syntax-e #`default-order?)
                       #`(label ...)
                       #`(pos-label ...)))

                  (((field-label getter setter) ...)

                   (append (map augment-field
                                (syntax->list #`((field-label . accessors) ...)))
                           (map (lambda (label)
                                  (maybe-generate #`name `(,label getter setter)))
                                (s1:lset-difference free-identifier=?
                                                    (syntax->list #`(label ...))
                                                    (syntax->list #`(field-label ...)))))))

               (with-syntax ((supers         #`(super ...))
                             ((pos-temp ...) (generate-temporaries #`(pos-label ...)))
                             ((constructor predicate maker copier)
                              (maybe-generate #`name `(,#`constructor ,#`predicate maker copier))))
                 (begin
                   (register #`name (make-entry #`name
                                                (syntax-e #`is-scheme?)
                                                #`predicate
                                                (syntax->list #`(super ... name))
                                                (syntax->list #`(label ...))
                                                (syntax->list #`(pos-label ...))
                                                (map syntax->list
                                                     (syntax->list #`((field-label getter setter) ...)))
                                                #`copier))

                   (if (syntax-e #`is-scheme?)

                       #`(begin
                           (define-generic (predicate x) (lambda (x) #f))
                           (define-generic (getter x))
                           ...
                           (define-generic (setter x v))
                           ...
                           (define-generic (copier x)))

                       #`(begin
                           (s9:define-record-type internal-name
                                                  (maker field-label ...)
                                                  predicate
                                                  (field-label getter setter) ...)

                           (define constructor
                             (lambda (pos-temp ...)
                               (populate maker (field-label ...) (pos-label pos-temp) ...)))

                           (extend-predicates supers predicate)
                           (extend-accessors supers field-label predicate getter setter)
                           ...

                           (define (copier x)
                             (maker (getter x) ...))
                           (extend-copiers supers copier predicate)

                           (define-method (show (r predicate))
                             (list 'name
                                   (list 'field-label (getter r))
                                   ...))

                           (define-syntax name
                             (syntax-rules ()
                               ((name . bindings) (populate maker (field-label ...) . bindings))))

                           ))))))))) ; build-record

      (define (maybe-generate context maybe-identifiers)
        (map (lambda (elem)
               (if (identifier? elem)
                   elem
                   (datum->syntax-object context (if (symbol? elem)
                                                     (gensym elem)
                                                     (gensym)))))
             maybe-identifiers))

      (define (augment-field clause)
        (syntax-case clause ()
          ((label)               `(,#`label ,@(maybe-generate #`label `(   getter    setter))))
          ((label getter)        `(,#`label ,@(maybe-generate #`label `(,#`getter    setter))))
          ((label getter setter) `(,#`label ,@(maybe-generate #`label `(,#`getter ,#`setter))))))

      build-record))

  (define-syntax extend-predicates
    (lambda (stx)
      (syntax-case stx ()
        ((extend-predicates (super ...) new-type)
         (with-syntax (((predicate ...) (map lookup-predicate
                                             (syntax->list #`(super ...)))))
           #`(begin
               (define-method predicate (new-type) (x) any?)
               ...))))))

  (define-syntax extend-copiers
    (lambda (stx)
      (syntax-case stx ()
        ((extend-copiers (super ...) copy new-type)
         (with-syntax (((copier ...) (map lookup-copier
                                          (syntax->list #`(super ...)))))
           #`(begin
               (define-method copier (new-type) (x)  copy)
               ...))))))

  (define-syntax extend-accessors
    (lambda (stx)
      (syntax-case stx ()
        ((extend-accessors (super ...) label new-type selector modifier)
         (with-syntax (((getter ...) (s1:filter (lambda (id)
                                                  (not (eqv? id #f)))
                                                (map (lambda (super)
                                                       (lookup-getter super #`label))
                                                     (syntax->list #`(super ...)))))
                       ((setter ...) (s1:filter (lambda (id)
                                                  (not (eqv? id #f)))
                                                (map (lambda (super)
                                                       (lookup-setter super #`label))
                                                     (syntax->list #`(super ...))))))
           #`(begin
               (define-method getter (new-type) (x) selector)
               ...
               (define-method setter (new-type any?) (x v) modifier)
               ...))))))

  (define-syntax populate
    (lambda (stx)

      (define (order ordering bindings default)
        (if (null? (s1:lset-difference free-identifier=?
                                       (map car bindings)
                                       ordering))
            (map (lambda (label)
                   (cond ((s1:assoc label bindings free-identifier=?) => (lambda (x) x))
                         (else `(,label ,default))))
                 ordering)
            (raise-syntax-error #f "Illegal labels in" stx)))

      (syntax-case stx ()
        ((populate maker labels . bindings)
         (with-syntax ((((label exp) ...) (order (syntax->list #`labels)
                                                 (map syntax->list
                                                      (syntax->list #'bindings))
                                                 #`'<undefined>)))
           #`(maker exp ...))))))


  ; Simple generic functions suitable for our disjoint base record types:

  (define-syntax define-generic
    (syntax-rules ()
      ((define-generic (name arg ...))
       (define-generic (name arg ...)
         (lambda (arg ...) (error "Inapplicable method:" 'name
                                  "Arguments:" (show arg) ... ))))
      ((define-generic (name arg ...) proc)
       (define name (make-generic (arg ...) proc)))))

  (define-syntax define-method
    (syntax-rules ()
      ((define-method (generic (arg pred?) ...) . body)
       (define-method generic (pred? ...) (arg ...) (lambda (arg ...) . body)))
      ((define-method generic (pred? ...) (arg ...) procedure)
       (let ((next ((generic) 'get-proc))
             (proc procedure))
         (((generic) 'set-proc)
          (lambda (arg ...)
            (if (and (pred? arg) ...)
                (proc arg ...)
                (next arg ...))))))))

  (define-syntax make-generic
    (syntax-rules ()
      ((make-generic (arg arg+ ...) default-proc)
       (let ((proc default-proc))
         (case-lambda
           ((arg arg+ ...)
            (proc arg arg+ ...))
           (()
            (lambda (msg)
              (case msg
                ((get-proc) proc)
                ((set-proc) (lambda (new)
                              (set! proc new)))))))))))

  (define-generic (show x)
    (lambda (x) x))

  (define (any? x) #t)

) ; records
