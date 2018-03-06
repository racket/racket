#lang racket/base
(require (for-syntax racket/base
                     racket/provide-transform))

(provide struct*
         struct*-copy
         struct*-out)

;; The `struct*` form is like `struct`, but a field can be have a `*`
;; before it or not: the fields without `*` are moved into a nested
;; structure (and cannot be mutable), and the ones with `*` are kept
;; immediate. This distinction is useful is `struct*-copy` is used
;; often to asjust some fields and not others in a relatively larger
;; struct.

;; Example:
#;
(struct* fish (* weight
                 color
                 name))
;; Makes a `fish` struct where `struct*-copy` is used
;; frequently to change `weight`, but not `color` or
;; `name` --- so `color` and `name` will be represented
;; together in an inner structure that is referenced
;; though one field in the outer structure.

;; Currently doesn't support:
;;  * Subtypes deeper than one

(begin-for-syntax
  (struct struct*-shape (constructor
                         parent
                         outer-name inner-name outer-name-inner
                         all-fields ; including parent fields
                         outer-fields inner-fields mutators)
    #:property prop:procedure (lambda (shape stx)
                                (with-syntax ([make-id (struct*-shape-constructor shape)])
                                  (syntax-case stx ()
                                    [(id arg ...)
                                     (syntax/loc stx (make-id arg ...))]
                                    [else
                                     (syntax/loc stx make-id)])))))

(define-syntax (struct* stx)
  (let-values ([(name parent-name fields options)
                (syntax-case stx ()
                  [(_ name parent-name (field ...) options ...)
                   (values #'name #'parent-name #'(field ...) #'(options ...))]
                  [(_ name (field ...) options ...)
                   (values #'name #f #'(field ...) #'(options ...))])])
    (define parent-shape (and parent-name
                              (syntax-local-value parent-name (lambda () #f))))
    (when parent-name
      (check-struct* parent-shape stx parent-name))
    (with-syntax ([((outer-field ...) (inner-field ...)) (split-star-fields fields)]
                  [outer-name (make-id name '/outer)]
                  [inner-name (make-id name '/inner)]
                  [(option ...) options])
      (with-syntax ([(outer-field-name ...) (map field-id (syntax->list #'(outer-field ...)))]
                    [(inner-field-name ...) (map field-id (syntax->list #'(inner-field ...)))]
                    [(outer-parent-name ...)
                     (if parent-name
                         (list (struct*-shape-outer-name parent-shape))
                         null)]
                    [(inner-parent-name ...)
                     (if parent-name
                         (list (struct*-shape-inner-name parent-shape))
                         null)]
                    [(chain-field ...) (if parent-name
                                           '()
                                           (list (datum->syntax name 'inner)))]
                    [(every-field ...) (append (if parent-shape
                                                   (struct*-shape-all-fields parent-shape)
                                                   null)
                                               (extract-all-fields fields))]
                    [(parent-outer-field ...) (if parent-shape
                                                  (struct*-shape-outer-fields parent-shape)
                                                  null)]
                    [(parent-inner-field ...) (if parent-shape
                                                  (struct*-shape-inner-fields parent-shape)
                                                  null)]
                    [(name-outer-field ...) (make-accessor-ids name #'(outer-field ...))]
                    [(set-name-outer-field! ...) (make-mutator-ids name #'(outer-field ...))]
                    [(name-inner-field ...) (make-accessor-ids name #'(inner-field ...))]
                    [(outer-name-outer-field ...) (make-accessor-ids #'outer-name #'(outer-field ...))]
                    [(set-outer-name-outer-field! ...) (make-mutator-ids #'outer-name #'(outer-field ...))]
                    [(inner-name-inner-field ...) (make-accessor-ids #'inner-name #'(inner-field ...))]
                    [outer-name-inner (if parent-shape
                                          (struct*-shape-outer-name-inner parent-shape)
                                          (make-id #'outer-name '-inner))]
                    [parent-name parent-name]
                    [name name]
                    [make-name (make-id name '/make)]
                    [name? (make-id name '?)]
                    [outer-name? (make-id #'outer-name '?)]
                    [quote-parent-syntax (if parent-shape
                                             #'quote-syntax
                                             #'quote)])
        #`(begin
            (struct outer-name outer-parent-name ... (chain-field ... outer-field ...)
                    option ...
              #:reflection-name 'name
              #:authentic)
            (struct inner-name inner-parent-name ... (inner-field ...)
              #:authentic)
            (define-syntax name (struct*-shape
                                 (quote-syntax make-name)
                                 (quote-parent-syntax parent-name)
                                 (quote-syntax outer-name)
                                 (quote-syntax inner-name)
                                 (quote-syntax outer-name-inner)
                                 '(every-field ...)
                                 '(outer-field-name ...)
                                 '(inner-field-name ...)
                                 '(set-name-outer-field! ...)))
            (define (name? v) (outer-name? v))
            (define (make-name every-field ...)
              (outer-name (inner-name parent-inner-field ... inner-field-name ...)
                          parent-outer-field ... outer-field-name ...))
            (define (name-outer-field v) (outer-name-outer-field v)) ...
            (define (set-name-outer-field! v f) (set-outer-name-outer-field! v f)) ...
            (define (name-inner-field v) (inner-name-inner-field (outer-name-inner v))) ...)))))          

;; ----------------------------------------

(define-syntax (struct*-copy stx)
  (syntax-case stx ()
    [(_ name expr binding ...)
     (identifier? #'name)
     (let ([shape (syntax-local-value #'name (lambda () #f))])
       (check-struct* shape stx #'name)
       (with-syntax ([outer-name (struct*-shape-outer-name shape)]
                     [inner-name (struct*-shape-inner-name shape)]
                     [((outer-binding ...) (inner-binding ...))
                      (split-star-bindings #'(binding ...)
                                           shape
                                           stx)]
                     [(inner-place ...) (if (struct*-shape-parent shape)
                                            `(#:parent ,(struct*-shape-outer-name
                                                         (syntax-local-value
                                                          (struct*-shape-parent shape))))
                                            '())]
                     [outer-name-inner (struct*-shape-outer-name-inner shape)]
                     [inner (datum->syntax (struct*-shape-outer-name-inner shape) 'inner)])
         #`(let ([v expr])
             (struct-copy outer-name v
                          outer-binding ...
                          [inner inner-place ...
                                 (struct-copy/maybe inner-name (outer-name-inner v)
                                                    inner-binding ...)]))))]))

(define-syntax struct-copy/maybe
  (syntax-rules ()
    [(struct-copy/maybe struct val) val]
    [(struct-copy/maybe struct val binding ...)
     (struct-copy struct val binding ...)]))

;; ----------------------------------------

(define-syntax struct*-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     (syntax-case stx ()
       [(_ name)
        (begin
          (syntax-local-lift-module-end-declaration #'(provide-struct* name))
          #'(combine-out))]))))

(define-syntax (provide-struct* stx)
  (syntax-case stx ()
    [(_ name)
     (let ()
       (define shape (syntax-local-value #'name (lambda () #f)))
       (check-struct* shape stx #'name)
       (with-syntax ([name? (make-id #'name '?)]
                     [(name-field ...)
                      (for/list ([field (in-list (append
                                                  (struct*-shape-outer-fields shape)
                                                  (struct*-shape-inner-fields shape)))])
                        (make-id #'name (string->symbol (format "-~a" field))))]
                     [(mutator ...)
                      (for/list ([mutator (in-list (struct*-shape-mutators shape))])
                        (datum->syntax #'name mutator))])
         #'(provide name name? name-field ... mutator ...)))]))

;; ----------------------------------------

(define-for-syntax (check-struct* shape stx id)
  (unless (struct*-shape? shape)
    (raise-syntax-error #f "not a struct* binding" stx id)))

(define-for-syntax (make-id base sym)
  (datum->syntax base (string->symbol (format "~a~a" (syntax-e base) sym)) base))

(define-for-syntax (make-accessor-ids name fields)
  (for/list ([f (in-list (syntax->list fields))])
    (define id (field-id f))
    (datum->syntax id (string->symbol (format "~a-~a" (syntax-e name) (syntax-e id))))))

(define-for-syntax (make-mutator-ids name fields)
  (for/list ([f (in-list (syntax->list fields))]
             #:when (syntax-case f ()
                      [(_ #:mutable) #t]
                      [_ #f]))
    (define id (field-id f))
    (datum->syntax id (string->symbol (format "set-~a-~a!" (syntax-e name) (syntax-e id))))))

(define-for-syntax (field-id f)
  (syntax-case f ()
    [(id . _) #'id]
    [id #'id]))

(define-for-syntax (extract-all-fields fields)
  (let loop ([fields (syntax->list fields)])
    (cond
     [(null? fields) null]
     [(eq? '* (syntax-e (car fields)))
      (cons (field-id (cadr fields)) (loop (cddr fields)))]
     [else
      (cons (field-id (car fields)) (loop (cdr fields)))])))

(define-for-syntax (split-star-fields fields)
  (let loop ([fields (syntax->list fields)] [accum-outer null] [accum-inner null])
    (cond
     [(null? fields) (list (reverse accum-outer) (reverse accum-inner))]
     [(eq? '* (syntax-e (car fields)))
      (loop (cddr fields) (cons (cadr fields) accum-outer) accum-inner)]
     [else
      (loop (cdr fields) accum-outer (cons (car fields) accum-inner))])))

(define-for-syntax (split-star-bindings bindings shape stx)
  (let loop ([bindings (syntax->list bindings)] [accum-outer null] [accum-inner null])
    (cond
     [(null? bindings) (list (reverse accum-outer) (reverse accum-inner))]
     [else
      (define binding (car bindings))
      (define (outer-in-shape? shape id)
        (memq (syntax-e id) (struct*-shape-outer-fields shape)))
      (define-values (new-binding outer?)
        (syntax-case binding ()
          [[id val]
           (begin
             (define outer? (outer-in-shape? shape #'id))
             (with-syntax ([id (datum->syntax (if outer?
                                                  (struct*-shape-outer-name shape)
                                                  (struct*-shape-outer-name shape))
                                              (syntax-e #'id))])
               (values (syntax/loc stx [id val]) outer?)))]
          [[id #:parent parent val]
           (begin
             (unless (and (struct*-shape-parent shape)
                          (free-identifier=? #'parent
                                             (struct*-shape-parent shape)))
               (raise-syntax-error #f "bad parent name"
                                   stx #'parent))
             (define parent-shape (syntax-local-value #'parent #f))
             (define outer? (outer-in-shape? parent-shape #'id))
             (define parent-name (if outer?
                                     (struct*-shape-outer-name parent-shape)
                                     (struct*-shape-inner-name parent-shape)))
             (with-syntax ([parent-name parent-name]
                           [id (datum->syntax parent-name (syntax-e #'id))])
               (values (syntax/loc binding [id #:parent parent-name val])
                       outer?)))]))
      (if outer?
          (loop (cdr bindings) (cons new-binding accum-outer) accum-inner)
          (loop (cdr bindings) accum-outer (cons new-binding accum-inner)))])))
