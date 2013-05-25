#lang racket/base
(require racket/struct-info
         (for-syntax racket/base)
         (for-template racket/base))

(provide build-struct-names
         build-struct-generation
         build-struct-expand-info
         struct-declaration-info?
         extract-struct-info
         
         generate-struct-declaration
         generate-delayed-struct-declaration)

;; build-struct-names : id (list-of id) bool bool -> (list-of id)
(define build-struct-names
  (lambda (name-stx fields omit-sel? omit-set? [srcloc-stx #f])
    (let ([name (symbol->string (syntax-e name-stx))]
          [fields (map symbol->string (map syntax-e fields))]
          [+ string-append])
      (map (lambda (s)
             (datum->syntax name-stx (string->symbol s) srcloc-stx))
           (append
            (list 
             (+ "struct:" name)
             (+ "make-" name)
             (+ name "?"))
            (let loop ([l fields])
              (if (null? l)
                  null
                  (append
                   (if omit-sel?
                       null
                       (list (+ name "-" (car l))))
                   (if omit-set?
                       null
                       (list (+ "set-" name "-" (car l) "!")))
                   (loop (cdr l))))))))))

(define build-struct-generation
  (lambda (name-stx fields omit-sel? omit-set? [super-type #f] [prop-value-list #'null]
                    [immutable-positions #'null] [mk-rec-prop-list (lambda (struct: make- ? acc mut) #'null)])
    (let ([names (build-struct-names name-stx fields omit-sel? omit-set?)])
      (build-struct-generation* names name-stx fields omit-sel? omit-set? super-type prop-value-list
                                immutable-positions mk-rec-prop-list))))

(define build-struct-generation*
  (lambda (names name fields omit-sel? omit-set? [super-type #f] [prop-value-list #'null]
                 [immutable-positions #'null] [mk-rec-prop-list (lambda (struct: make- ? acc mut) #'null)])
    (let ([num-fields (length fields)]
          [acc/mut-makers (let loop ([l fields][n 0])
                            (if (null? l)
                                null
                                (let ([mk-one
                                       (lambda (acc?)
                                         (list
                                          `(,(if acc?
                                                 'frp:make-struct-field-accessor
                                                 'frp:make-struct-field-mutator)
                                            ,(if acc? 'acc 'mut)
                                            ,n ',(car l))))])
                                  (append
                                   (if omit-sel?
                                       null
                                       (mk-one #t))
                                   (if omit-set?
                                       null
                                       (mk-one #f))
                                   (loop (cdr l) (add1 n))))))]
          [extra-props (mk-rec-prop-list 'struct: 'make- '? 'acc 'mut)])
      `(let-values ([(struct: make- ? acc mut)
                     (frp:make-struct-type ',name ,super-type ,num-fields 0 #f ,prop-value-list #f #f ,immutable-positions)])
         (values struct:
                 make-
                 ?
                 ,@acc/mut-makers)))))

(define build-struct-expand-info
  (lambda (name-stx fields omit-sel? omit-set? base-name base-getters base-setters)
    (let* ([names (build-struct-names name-stx fields omit-sel? omit-set?)])
      (build-struct-expand-info* names name-stx fields omit-sel? omit-set? base-name base-getters base-setters))))

(define build-struct-expand-info*
  (lambda (names name-stx fields omit-sel? omit-set? base-name base-getters base-setters)
    (let* ([flds (cdddr names)]
           [every-other (lambda (l)
                          (let loop ([l l])
                            (cond
                              [(null? l) null]
                              [(null? (cdr l)) (list (car l))]
                              [else (cons (car l) (loop (cddr l)))])))]
           [add-#f (lambda (omit? base)
                     (if omit?
                         (when (let loop ([l base])
                                 (cond
                                   [(null? l) #t]
                                   [(not (car l)) #f]
                                   [else (loop (cdr l))]))
                           (append base '(#f)))
                         base))]
           [cert-f (gensym)]
           [qs (lambda (x) (if (eq? x #t)
                               x
                               (and x `(,cert-f (quote-syntax ,x)))))])
      `(let ([,cert-f (syntax-local-certifier #t)])
         (list
          ,(qs (car names))
          ,(qs (cadr names))
          ,(qs (caddr names))
          (list
           ,@(reverse (if omit-sel?
                          null
                          (map qs (if omit-set? flds (every-other flds)))))
           ,@(map qs (add-#f omit-sel? base-getters)))
          (list
           ,@(reverse (if omit-set?
                          null
                          (map qs (if omit-sel?
                                      flds
                                      (every-other (if (null? flds)
                                                       null
                                                       (cdr flds)))))))
           ,@(map qs (add-#f omit-set? base-setters)))
          ,(qs base-name))))))

(define (struct-declaration-info? x)
  (struct-info? x))

;; ----------------------------------------

(define struct-info-type-id car)
(define struct-info-constructor-id cadr)
(define struct-info-predicate-id caddr)
(define struct-info-accessor-ids cadddr)
(define struct-info-mutator-ids (lambda (x) (list-ref x 4)))

(define (get-stx-info orig-stx super-id defined-names gen-expr?)
  ;; Looks up super info, if needed, and builds compile-time info for the
  ;; new struct; called by all three forms, but does only half the work
  ;; if `defined-names' is #f.
  ;; If `expr?' is #t, then generate an expression to build the info,
  ;; otherwise build the info directly.
  (let* ([cert-f (gensym)]
         [qs (if gen-expr? (lambda (x) #`(#,cert-f (quote-syntax #,x))) values)]
         [every-other (lambda (l)
                        (let loop ([l l][r null])
                          (cond
                            [(null? l) r]
                            [(null? (cdr l)) (cons (car l) r)]
                            [else (loop (cddr l) (cons (car l) r))])))]
         [super-si (and super-id 
                        (syntax-local-value super-id (lambda () #f)))]
         [super-info (and super-si
                          (struct-declaration-info? super-si)
                          (extract-struct-info super-si))])
    (when super-id 
      ;; Did we get valid super-info ?
      (when (or (not (struct-declaration-info? super-si))
                (not (struct-info-type-id super-info)))
        (raise-syntax-error
         #f
         (if (struct-declaration-info? super-si)
             "parent struct information does not include a type for subtyping"
             (format "parent struct type not defined~a"
                     (if super-info
                         (format " (~a does not name struct type information)"
                                 (syntax-e super-id))
                         "")))
         orig-stx
         super-id)))
    ;; Generate the results:
    (values
     super-info
     (if defined-names
         (let-values ([(initial-gets initial-sets)
                       (if super-info
                           (values (map qs (struct-info-accessor-ids super-info))
                                   (map qs (struct-info-mutator-ids super-info)))
                           (values null null))]
                      [(fields) (cdddr defined-names)]
                      [(wrap) (if gen-expr? (lambda (x) #`(list #,@x)) values)])
           #`(let ([#,cert-f (syntax-local-certifier #t)])
               (make-struct-info
                (lambda ()
                  #,(wrap
                     (list (qs (car defined-names))
                           (qs (cadr defined-names))
                           (qs (caddr defined-names))
                           (wrap
                            (apply
                             list
                             (append (map qs (every-other fields)) 
                                     initial-gets)))
                           (wrap
                            (apply
                             list
                             (append (map qs (if (null? fields) 
                                                 null 
                                                 (every-other (cdr fields)))) 
                                     initial-sets)))
                           (if super-id
                               (qs super-id)
                               #t)))))))
         #f))))

(define (make-core make-make-struct-type orig-stx defined-names super-info name field-names)
  #`(let-values ([(type maker pred access mutate)
                  #,(make-make-struct-type orig-stx name defined-names super-info)])
      (values type maker pred
              #,@(let loop ([field-names field-names][n 0])
                   (if (null? field-names)
                       null
                       (list* #`(make-struct-field-accessor access #,n '#,(car field-names))
                              #`(make-struct-field-mutator mutate #,n '#,(car field-names))
                              (loop (cdr field-names) (add1 n))))))))

(define (generate-struct-declaration orig-stx
                                     name super-id field-names 
                                     context 
                                     make-make-struct-type
                                     continue-macro-id continue-data)
  (let ([defined-names (build-struct-names name field-names #f #f name)]
        [delay? (and (not (memq context '(module top-level expression)))
                     super-id)])
    (let-values ([(super-info stx-info) 
                  (if delay?
                      (values #f #f)
                      (get-stx-info orig-stx super-id defined-names #t))])
      (let ([result
             #`(begin
                 (define-values
                   #,defined-names
                   #,(if delay?
                         #`(begin0 ;; the `begin0' guarantees that it's an expression
                             (#,continue-macro-id #,orig-stx #,name #,super-id 
                                                  #,defined-names #,field-names 
                                                  #,continue-data))
                         (make-core make-make-struct-type orig-stx defined-names super-info name field-names)))
                 (define-syntaxes (#,name)
                   #,(if delay?
                         #`(let-values ([(super-info stx-info) 
                                         (get-stx-info (quote-syntax ,orig-stx)
                                                       (quote-syntax ,super-id)
                                                       (list #,@(map (lambda (x) 
                                                                       #`(quote-syntax #,x))
                                                                     defined-names))
                                                       #f
                                                       values)])
                             stx-info)
                         stx-info)))])
        (if super-id
            (syntax-property result 
                             'disappeared-use 
                             (syntax-local-introduce super-id))
            result)))))

(define (generate-delayed-struct-declaration stx make-make-make-struct-type)
  (syntax-case stx ()
    [(_ orig-stx name super-id defined-names field-names continue-data)
     (let-values ([(super-info stx-info) (get-stx-info #'orig-stx #'super-id #f #f)])
       (make-core (make-make-make-struct-type #'continue-data)
                  #'orig-stx
                  (syntax->list #'defined-names)
                  super-info
                  #'name
                  (syntax->list #'field-names)))]))
