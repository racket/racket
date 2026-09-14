
;; Every Racket struct type should be a record subtype of `|#%racket-base-rtd|`
;; instead of just `#!base-rtd`. That gives us room to record key properties
;; like `prop:procedure` for faster access.

;; When creating a nongenerative Racket structure type, the extra
;; field values must be faslable values. In the case of Racket
;; structure-type properties, that's not always possible, so properties
;; can be recorded on the uid, instead. For the key properties,
;; choose a faslable representation so that they can always be fast.

(define |#%racket-base-rtd|
  (make-record-type-descriptor
   'struct
   #!base-rtd
   '#{struct icw1nrrg1rjuf16733snprjoz-0}
   #f ; sealed?
   #t ; opaque?
   '#((immutable procedure)
      (immutable arity)
      (immutable props) ; #f => properties attached via uid
      (immutable insp))))

(define |#%racket-type-base-rtd|
  (make-record-type-descriptor
   'struct-type
   #!base-rtd
   '#{struct-type icw1nrrg1rjuf16733snprjoz-1}
   #f ; sealed?
   #t ; opaque?
   '#((immutable authenticity))))

(define NUMBER-OF-BASE-RTD-FIELDS (#%$record-type-field-count #!base-rtd))
(define NUMBER-OF-RACKET-BASE-RTD-FIELDS (#%$record-type-field-count |#%racket-base-rtd|))

(define (racket-rtd? rtd)
  (#%record? rtd |#%racket-base-rtd|))

(define racket-rtd-procedure (record-accessor |#%racket-base-rtd| 0))
(define racket-rtd-arity (record-accessor |#%racket-base-rtd| 1))
(define racket-rtd-props (record-accessor |#%racket-base-rtd| 2))
(define racket-rtd-insp (record-accessor |#%racket-base-rtd| 3))

(define racket-type-rtd-authenticity (record-accessor |#%racket-type-base-rtd| 0))

(define-syntax (define-racket-record-type stx)
  (syntax-case stx (fields nongenerative)
    [(_ name parent
        [fields (kind field-name) ...]
        [nongenerative . uid]
        [key val]
        ...)
     (let ([option (lambda (a-key default)
                     (let loop ([keys (datum (key ...))]
                                [vals #'(val ...)])
                       (cond
                         [(null? keys) default]
                         [(eq? (car keys) a-key) (car vals)]
                         [else (loop (cdr keys) (cdr vals))])))])
       (#%for-each (lambda (key)
                     (unless (#%memq (#%syntax->datum key) '(sealed
                                                             constructor
                                                             predicate
                                                             procedure
                                                             procedure-arity))
                       (errorf "bad record keyword ~s" key)))
                   #'(key ...))
       (with-syntax ([sealed? (option 'sealed #'#f)]
                     [make-name (option 'constructor #'#f)]
                     [name? (option 'predicate #'#f)]
                     [proc (option 'procedure #'#f)]
                     [arity (option 'procedure-arity #'#f)])
         (let ([mk (lambda args
                     (#%datum->syntax
                      (let loop ([args args])
                        (if (string? (car args))
                            (loop (cdr args))
                            (car args)))
                      (string->symbol
                       (#%apply #%string-append
                                (let loop ([args args])
                                  (if (null? args)
                                      '()
                                      (cons
                                       (if (string? (car args))
                                           (car args)
                                           (#%symbol->string (#%syntax->datum (car args))))
                                       (loop (cdr args)))))))))])
           (with-syntax ([(name-field ...)
                          (#%map (lambda (fld) (mk #'name "-" fld)) #'(field-name ...))]
                         [(field-idx ...) (let loop ([fields #'(field-name ...)] [i 0])
                                            (if (null? fields)
                                                '()
                                                (cons i (loop (cdr fields) (add1 i)))))]
                         [(set-name-field! ...)
                          (#%filter
                           #%values
                           (#%map (lambda (fld kind)
                                    (and (eq? kind 'mutable)
                                         (mk "set-" #'name "-" fld "!")))
                                  #'(field-name ...)
                                  (datum (kind ...))))]
                         [(set-field-idx ...) (let loop ([fields (datum (kind ...))] [i 0])
                                                (if (null? fields)
                                                    '()
                                                    (let ([r (loop (cdr fields) (add1 i))])
                                                      (if (eq? (car fields) 'mutable)
                                                          (cons i r)
                                                          r))))]
                         [rtd:name (mk "rtd:" #'name)]
                         [rcd:name (mk "rcd:" #'name)])
             #`(begin
                 (define rtd:name
                   (#%$make-record-type-descriptor
                    |#%racket-base-rtd|
                    'name
                    #,(if (datum parent)
                          (mk "rtd:" #'parent)
                          #''#f)
                    '#,(cond
                         [(null? (#%syntax->datum #'uid))
                          (#%datum->syntax #'name ((current-generate-id) (datum name)))]
                         [else (car (#%syntax->datum #'uid))])
                    sealed?
                    #f     ; opaque?
                    '#((kind field-name)
                       ...)
                    'define-racket-record-type
                    proc   ; procedure
                    arity  ; arity
                    #f     ; props
                    none)) ; insp
                 (define rcd:name (make-record-constructor-descriptor rtd:name
                                                                      #,(if (datum parent)
                                                                            (mk "rcd:" #'parent)
                                                                            #''#f)
                                                                      #f))
                 (define #,(if (datum make-name)
                               #'make-name
                               (mk "make-" #'name))
                   (record-constructor rcd:name))
                 (define #,(if (datum name?) #'name? (mk #'name "?")) (record-predicate rtd:name))
                 (define name-field (record-accessor rtd:name field-idx))
                 ...
                 (define set-name-field! (record-mutator rtd:name set-field-idx))
                 ...)))))]
    [(_ name
        [fields . flds]
        . more)
     #'(define-racket-record-type name #f
         [fields . flds]
         . more)]
    [(_ name parent
        [fields . flds]
        . more)
     #'(define-racket-record-type name parent
         [fields . flds]
         [nongenerative]
         . more)]))
