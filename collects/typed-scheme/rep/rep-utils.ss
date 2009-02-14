#lang scheme/base
(require "../utils/utils.ss")

(require mzlib/struct 
         mzlib/plt-match
         syntax/boundmap
         "free-variance.ss"
         "interning.ss"
         mzlib/etc
         (for-syntax 
          scheme/base
          syntax/struct
          syntax/stx
	  (utils utils)))

(provide == dt de print-type* print-effect* Type Type? Effect Effect? defintern hash-id Type-seq Effect-seq Type-key)



;; hash table for defining folds over types
(define-values-for-syntax (type-name-ht effect-name-ht)
  (values (make-hasheq) (make-hasheq)))

(provide (for-syntax type-name-ht effect-name-ht))


;; all types are Type?
(define-struct/printer Type (seq key) (lambda (a b c) ((unbox print-type*) a b c)))

(define-struct/printer Effect (seq key) (lambda (a b c) ((unbox print-effect*) a b c)))





;; type/effect definition macro

(define-for-syntax type-rec-id #'type-rec-id)
(define-for-syntax effect-rec-id #'effect-rec-id)
(define-for-syntax fold-target #'fold-target)

(provide (for-syntax type-rec-id effect-rec-id fold-target))

(define-syntaxes (dt de)
  (let ()
    (define (parse-opts opts stx)
      (let loop ([provide? #t] [intern? #f] [frees #t] [fold-rhs #f] [key '(#f)] [opts opts])
        (cond 
          [(null? opts) (values provide? intern? frees fold-rhs key)]
          [(eq? '#:no-provide (syntax-e (stx-car opts)))
           (loop #f intern? frees fold-rhs key (cdr opts))]
          [(eq? '#:no-frees (syntax-e (stx-car opts)))
           (loop #f intern? #f fold-rhs key (cdr opts))]
          [(not (and (stx-pair? opts) (stx-pair? (stx-car opts))))
           (raise-syntax-error #f "bad options" stx)]
          [(eq? '#:intern (syntax-e (stx-car (car opts))))
           (loop provide? (stx-car (stx-cdr (car opts))) frees fold-rhs key (cdr opts))]
          [(eq? '#:frees (syntax-e (stx-car (car opts))))
           (loop provide? intern? (stx-cdr (car opts)) fold-rhs key (cdr opts))]
          [(eq? '#:fold-rhs (syntax-e (stx-car (car opts))))
           (loop provide? intern? frees (stx-cdr (car opts)) key (cdr opts))]
	  [(eq? '#:key (syntax-e (stx-car (car opts))))
	   (loop provide? intern? frees fold-rhs (stx-cdr (car opts)) (cdr opts))]
          [else (raise-syntax-error #f "bad options" stx)])))
    (define (mk par ht-stx)        
      (lambda (stx)          
        (syntax-case stx ()
          [(dform nm flds . opts)
           (let*-values ([(provide? intern? frees fold-rhs key-expr) (parse-opts (syntax->list #'opts) #'opts)]
                         [(kw) (string->keyword (symbol->string (syntax-e #'nm)))])
             (with-syntax* 
                 ([ex (id #'nm #'nm ":")]
                  [kw-stx kw]
		  [(key-expr) key-expr]
                  [parent par]
                  [(s:ty maker pred acc ...) (build-struct-names #'nm (syntax->list #'flds) #f #t #'nm)]
                  [(flds* ...) #'flds]
                  [*maker (id #'nm "*" #'nm)]
                  [**maker (id #'nm "**" #'nm)]
                  [ht-stx ht-stx]
                  [bfs-fold-rhs (cond [(and fold-rhs (eq? (syntax-e (stx-car fold-rhs)) '#:base))
                                       #`(lambda (tr er) #,fold-target)]
                                      [(and fold-rhs (stx-pair? fold-rhs))
                                       (with-syntax ([fr (stx-car fold-rhs)])
                                         #'(lambda (tr er) 
                                             #'fr))]
                                      [fold-rhs (raise-syntax-error fold-rhs "something went wrong")]
                                      [else #'(lambda (type-rec-id effect-rec-id) 
						#;
                                                (printf "about to call ~a with ~a args~n"
							'*maker
							(length '(flds* ...)))
						#`(*maker (#,type-rec-id flds*) ...))])]
                  [provides (if provide? 
                                #`(begin 
                                    (provide ex pred acc ...)
                                    (provide (rename-out [*maker maker])))
                                #'(begin))]
                  [intern (cond 
                            [(syntax? intern?)
                             #`(defintern (**maker key . flds) maker #,intern?)]                            
                            [(null? (syntax-e #'flds))
                             #'(defintern (**maker key . flds) maker #f)]
                            [(stx-null? (stx-cdr #'flds)) #'(defintern (**maker key . flds) maker . flds)]
                            [else #'(defintern (**maker key . flds) maker (list . flds))])]
                  [frees (cond
                           [(not frees) #'(begin)]
                           ;; we know that this has no free vars
                           [(and (pair? frees) (syntax? (car frees)) (not (syntax-e (car frees))))
                            (syntax/loc stx
                              (define (*maker . flds)
                                (define v (**maker key-expr . flds)) 
                                (unless-in-table 
                                 var-table v
                                 (hash-set! var-table v empty-hash-table)
                                 (hash-set! index-table v empty-hash-table))
                                v))]
                           ;; we provided an expression each for calculating the free vars and free idxs
                           ;; this should really be 2 expressions, one for each kind
                           [(and (pair? frees) (pair? (cdr frees)))
                            (quasisyntax/loc
                                stx
                              (define (*maker . flds)				
                                (define v (**maker key-expr . flds))
                                #,
                                (quasisyntax/loc (car frees)
                                  (unless-in-table 
                                   var-table v
                                   (hash-set! var-table v #,(car frees))
                                   (hash-set! index-table v #,(cadr frees))))
                                v))]                                       
                           [else 
                            (let 
                                ([combiner 
                                  (lambda (f flds)
                                    (syntax-case flds ()
                                      [() #'empty-hash-table]
                                      [(e) #`(#,f e)]
                                      [(e ...) #`(combine-frees (list (#,f e) ...))]))])
                              (quasisyntax/loc stx
                                (define (*maker . flds)
                                  (define v (**maker key-expr . flds))
                                  (unless-in-table 
                                   var-table v
                                   (define fvs #,(combiner #'free-vars* #'flds))
                                   (define fis #,(combiner #'free-idxs* #'flds))
                                   (hash-set! var-table v fvs)
                                   (hash-set! index-table v fis))
                                  v)))])])
               #`(begin
                   (define-struct (nm parent) flds #:inspector #f)
                   (define-match-expander ex
                     (lambda (s)
                       (... 
                        (syntax-case s ()
                          [(__ . fs) 
                           (with-syntax ([flds** (syntax/loc s (_ _ . fs))])
                             (quasisyntax/loc s (struct nm flds**)))]))))
                   (begin-for-syntax
                     (hash-set! ht-stx 'kw-stx (list #'ex #'flds bfs-fold-rhs #'#,stx)))
                   intern
                   provides
                   frees)))])))
    (values (mk #'Type #'type-name-ht) (mk #'Effect #'effect-name-ht))))

