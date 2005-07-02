(module translate-class-utils mzscheme 

  (require (lib "list.ss" "srfi" "1")
           (lib "plt-match.ss")
           "../../ast.ss"
           "../../tenv.ss"
           "../../utils.ss"
           "../typechecker/type-utils.ss"
           "translate-expression.ss"
           "translate-utils.ss")
  
  (define-struct comp:export   (stx type binds) #f)
  (define-struct comp:exp-bind (old new method?) #f)

  (provide translate-class-exports translate-subclass-exports)
  (define (translate-class-exports tenv exports)
    (let ([exports (filter-exports tenv (generate-exports tenv exports))])
      (map (lambda (e)
             (translate-export tenv #f #f e))
           exports)))
  
  (define (translate-subclass-exports tenv super-types arg-type exports)
    (let ([exports (filter-exports tenv (generate-exports tenv exports))])
      (map (lambda (e)
             (if (ormap (lambda (t)
                          (<:_P tenv t (comp:export-type e)))
                        super-types)
                 (translate-export tenv #t arg-type e)
                 (translate-export tenv #f arg-type e)))
           exports)))
  

  
  (define (generate-super-exports tenv type-entry comp-binds)
    (let loop ([super-types     (tenv:type-supers type-entry)]
               [super-comp-exps '()])
      (if (null? super-types)
          super-comp-exps
          (let ([super-entry (get-type-entry tenv (car super-types))])
            (let loop2 ([super-members    (append (tenv:type-members super-entry)
                                                  (tenv:type-inherited super-entry))]
                        [super-comp-binds '()])
              (if (null? super-members)
                  (loop (cdr super-types)
                        (cons (make-comp:export #f (car super-types) super-comp-binds)
                              (append (generate-super-exports tenv super-entry comp-binds)
                                      super-comp-exps)))
                  (let ([matched (find (lambda (eb)
                                         (tenv-key=? (tenv:member-name (car super-members))
                                                     (comp:exp-bind-new eb)))
                                       comp-binds)])
                    (loop2 (cdr super-members)
                           (cons matched super-comp-binds)))))))))
                                                                                     
  
  (define (generate-exports tenv exports)
    (let loop ([exports   exports]
               [comp-exps '()])
      (if (null? exports)
          comp-exps
          (let* ([export     (car exports)]
                 [type-entry (get-type-entry tenv (honu:export-type export))])
            (let loop2 ([exp-binds  (honu:export-binds export)]
                        [members    (append (tenv:type-members type-entry)
                                            (tenv:type-inherited type-entry))]
                        [comp-binds '()])
              (if (null? exp-binds)
                  (let ([super-exports (generate-super-exports tenv type-entry comp-binds)])
                    (loop (cdr exports)
                          (cons (make-comp:export (honu:ast-stx export)
                                                  (honu:export-type export)
                                                  comp-binds)
                                (append super-exports comp-exps))))
                  (let-values ([(matched non-matches) (partition-first (lambda (m)
                                                                         (tenv-key=? (honu:exp-bind-new (car exp-binds))
                                                                                     (tenv:member-name m)))
                                                                       members)])
                    (loop2 (cdr exp-binds)
                           non-matches
                           (cons (make-comp:exp-bind (honu:exp-bind-old (car exp-binds))
                                                     (honu:exp-bind-new (car exp-binds))
                                                     (honu:type-disp? (tenv:member-type matched)))
                                 comp-binds)))))))))
              
  (define (filter-exports tenv exports)
    (let loop ([exports   exports]
               [kept-exps '()])
      (if (null? exports)
          kept-exps
          (let-values ([(matches non-matches) (partition (lambda (exp)
                                                           (type-equal? tenv
                                                                        (comp:export-type (car exports))
                                                                        (comp:export-type exp)))
                                                         exports)])
            (let ([exp-with-stx (find comp:export-stx (cons (car exports) matches))])
              (if exp-with-stx
                  (loop non-matches (cons exp-with-stx kept-exps))
                  (loop non-matches (cons (car exports) kept-exps))))))))
                                      
  (define (translate-export tenv in-super? arg-type export)
    (cons 'begin
          (map (lambda (b)
                 (translate-exp-bind tenv in-super? arg-type (comp:export-type export) b))
               (comp:export-binds export))))
  
  (define (translate-exp-bind tenv in-super? arg-type type binding)
    (let ([right-defn (if in-super? 'define/override 'define/public)])
      (match binding
        [(struct comp:exp-bind (old-name new-name #t))
         (at #f `(,right-defn (,(translate-method-name type new-name) args)
                   ,(translate-static-method tenv arg-type old-name 'args)))]
        [(struct comp:exp-bind (old-name new-name #f))
         (at #f `(begin
                   (,right-defn (,(translate-field-getter-name type new-name) args)
                     ,(translate-static-field-getter tenv arg-type old-name))
                   (,right-defn (,(translate-field-setter-name type new-name) arg)
                     ,(translate-static-field-setter tenv arg-type old-name 'arg))))])))

  (provide translate-super-new translate-inits translate-member)
  (define (translate-super-new tenv arg-type super-new)
    (at (honu:ast-stx super-new)
        (cons 'super-new (map (lambda (a)
                                (list (at-ctxt (honu:name-arg-name a))
                                      (translate-expression tenv arg-type (honu:name-arg-value a))))
                              (honu:super-new-args super-new)))))
  
  (define (translate-inits inits)
    (cons 'init (map (lambda (i)
                       (at-ctxt (honu:formal-name i)))
                     inits)))
  
  (define (mangle-init-name name)
    (at name (string->symbol (string-append "init-" (symbol->string (syntax-e name))))))
  
  (define (translate-member tenv arg-type member)
    (match member
      [(struct honu:init-field (stx name _ value))
       (if value
           `(begin (init ([,(mangle-init-name name) ,(at-ctxt name)]
                          ,(translate-expression tenv arg-type value)))
                   (define ,(at-ctxt name) ,(mangle-init-name)))
           `(begin (init ([,(mangle-init-name name) ,(at-ctxt name)]))
                   (define ,(at-ctxt name) ,(mangle-init-name name))))]
      [(struct honu:field (stx name _ value))
       `(define ,(at-ctxt name) ,(translate-expression tenv arg-type value))]
      [(struct honu:method (stx name _ formals body))
       (translate-function stx name formals
                           (translate-expression tenv arg-type body))]))
  
  
  )