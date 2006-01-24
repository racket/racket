(module translate-class-utils mzscheme 

  (require (lib "list.ss" "srfi" "1")
           (only (lib "list.ss") quicksort)
           (lib "plt-match.ss")
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss"
           "../tools/general.ss"
           "../typechecker/type-utils.ss"
           "translate-expression.ss"
           "translate-utils.ss")
  
  (define-struct comp:export   (stx type binds) #f)
  (define-struct comp:exp-bind (old new method?) #f)

  (provide translate-class-exports translate-subclass-exports)
  (define (translate-class-exports exports)
    (let ([exports (filter-exports (generate-exports exports))])
      (map (lambda (e)
             (translate-export #f e))
           exports)))
  
  (define (translate-subclass-exports super-types exports)
    (let ([exports (filter-exports (generate-exports exports))])
      (map (lambda (e)
             (if (ormap (lambda (t)
                          (<:_P t (comp:export-type e)))
                        super-types)
                 (translate-export #t e)
                 (translate-export #f e)))
           exports)))
  

  
  (define (generate-super-exports type-entry comp-binds)
    (let loop ([super-types     (tenv:type-supers type-entry)]
               [super-comp-exps '()])
      (if (null? super-types)
          super-comp-exps
          (let ([super-entry (get-type-entry (car super-types))])
            (let loop2 ([super-members    (append (tenv:type-members super-entry)
                                                  (tenv:type-inherited super-entry))]
                        [super-comp-binds '()])
              (if (null? super-members)
                  (loop (cdr super-types)
                        (cons (make-comp:export #f (car super-types) super-comp-binds)
                              (append (generate-super-exports super-entry comp-binds)
                                      super-comp-exps)))
                  (let ([matched (find (lambda (eb)
                                         (tenv-key=? (tenv:member-name (car super-members))
                                                     (comp:exp-bind-new eb)))
                                       comp-binds)])
                    (loop2 (cdr super-members)
                           (cons matched super-comp-binds)))))))))
                                                                                     
  
  (define (generate-exports exports)
    (let loop ([exports   exports]
               [comp-exps '()])
      (if (null? exports)
          comp-exps
          (let* ([export     (car exports)]
                 [type-entry (get-type-entry (ast:export-type export))])
            (let loop2 ([exp-binds  (ast:export-members export)]
                        [members    (append (tenv:type-members type-entry)
                                            (tenv:type-inherited type-entry))]
                        [comp-binds '()])
              (if (null? exp-binds)
                  (let ([super-exports (generate-super-exports type-entry comp-binds)])
                    (loop (cdr exports)
                          (cons (make-comp:export (ast-syntax export)
                                                  (ast:export-type export)
                                                  comp-binds)
                                (append super-exports comp-exps))))
                  (let-values ([(matched non-matches) (partition-first (lambda (m)
                                                                         (tenv-key=? (ast:export/member-external (car exp-binds))
                                                                                     (tenv:member-name m)))
                                                                       members)])
                    (loop2 (cdr exp-binds)
                           non-matches
                           (cons (make-comp:exp-bind (ast:export/member-internal (car exp-binds))
                                                     (ast:export/member-external (car exp-binds))
                                                     (ast:type:method? (tenv:member-type matched)))
                                 comp-binds)))))))))
  
  (define (sort-binds export)
    (quicksort (comp:export-binds export)
               (lambda (b1 b2)
                 (tenv-key<? (comp:exp-bind-new b1)
                             (comp:exp-bind-new b2)))))
               
  
  (define (check-exports exports)
    (let* ([main-export (car exports)]
           [main-export-binds (sort-binds main-export)])
      (let loop ([exports (cdr exports)])
        (if (null? exports)
            (void)
            (let loop2 ([binds-1 main-export-binds]
                        [binds-2 (sort-binds (car exports))])
              ;; if one's empty, both must be since we passed the typechecker
              (cond
                [(null? binds-1)
                 (loop (cdr exports))]
                [(tenv-key=? (comp:exp-bind-old (car binds-1))
                             (comp:exp-bind-old (car binds-2)))
                 (loop2 (cdr binds-1) (cdr binds-2))]
                [else
                 (raise-read-error-with-stx
                  (format "Different local names exported for member ~a of type ~a: ~a here and ~a elsewhere"
                          (printable-type (comp:export-type main-export))
                          (printable-key (comp:exp-bind-new (car binds-1)))
                          (printable-key (comp:exp-bind-old (car binds-1)))
                          (printable-key (comp:exp-bind-old (car binds-2))))
                  (comp:exp-bind-old (car binds-1)))]))))))
        
  (define (filter-exports exports)
    (let loop ([exports   exports]
               [kept-exps '()])
      (if (null? exports)
          kept-exps
          (let-values ([(matches non-matches) (partition (lambda (exp)
                                                           (type-equal?
                                                                        (comp:export-type (car exports))
                                                                        (comp:export-type exp)))
                                                         exports)])
            (check-exports matches)
            (let ([exp-with-stx (find comp:export-stx (cons (car exports) matches))])
              (if exp-with-stx
                  (loop non-matches (cons exp-with-stx kept-exps))
                  (loop non-matches (cons (car exports) kept-exps))))))))
                                      
  (define (translate-export in-super? export)
    (cons 'begin
          (map (lambda (b)
                 (translate-exp-bind in-super? (comp:export-type export) b))
               (comp:export-binds export))))
  
  (define (translate-exp-bind in-super? type binding)
    (let ([right-defn (if in-super? 'define/override 'define/public)])
      (match binding
        [(struct comp:exp-bind (old-name new-name #t))
         (at #f `(,right-defn (,(translate-method-name type new-name) arg-tuple)
                   ,(translate-static-method old-name 'arg-tuple)))]
        [(struct comp:exp-bind (old-name new-name #f))
         (at #f `(begin
                   (,right-defn (,(translate-field-getter-name type new-name) args)
                     ,(translate-static-field-getter old-name))
                   (,right-defn (,(translate-field-setter-name type new-name) set-arg)
                     ,(translate-static-field-setter old-name 'set-arg))))])))

  (provide translate-super-new translate-inits translate-member)
  (define (translate-super-new super-new)
    (at (ast-syntax super-new)
        (cons 'super-new (map (lambda (a)
                                (list (at-ctxt (ast:named/arg-name a))
                                      (translate-expression (ast:named/arg-actual a))))
                              (ast:super-new-args super-new)))))
  
  (define (translate-inits inits)
    (cons 'init (map (lambda (i)
                       (at-ctxt (ast:formal-name i)))
                     inits)))
  
  (define (mangle-init-name name)
    (at name (string->symbol (string-append "init-" (symbol->string (syntax-e name))))))
  
  (define (translate-member member)
    (match member
      [(struct ast:class/member:field/formal (stx name type value))
       (if value
           (at stx`(begin (init ([,(mangle-init-name name) ,(at-ctxt name)]
                                 ,(translate-expression value)))
                          (define ,(at-ctxt name) ,(mangle-init-name name))))
           (at stx `(begin (init ([,(mangle-init-name name) ,(at-ctxt name)]))
                           (define ,(at-ctxt name) ,(mangle-init-name name)))))]
      [(struct ast:class/member:field (stx name type value))
       (at stx `(define ,(at-ctxt name) ,(translate-expression value)))]
      [(struct ast:class/member:method (stx name type formals body))
       (translate-function stx name formals (translate-expression body))]))
  
  
  )
