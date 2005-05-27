(module structure mzscheme
  (require (lib "etc.ss"))
  (require-for-syntax "private/structure-helper.ss"
                      (lib "kerncase.ss" "syntax")
                      (lib "stx.ss" "syntax")
                      (lib "list.ss"))
  
  (provide structure dot open open-in-context open-as)

  ;; Dangerous, but seems to work.
  (define-syntax define-syntaxes-ml
    (syntax-rules  ()
      ((_ . x) (define-syntaxes . x))))
  
  (define-syntax-set (structure)

    (define kernel-form-identifier-list/no-begin
      (append (map (lambda (x) (datum->syntax-object #'here x))
                   `(define-values-ml define-syntaxes-ml))
              (filter (lambda (id) (not (eq? 'begin (syntax-e id))))
                      (kernel-form-identifier-list #'here))))

    (define (stx-assoc id renames)
      (cond
        ((null? renames) #f)
        ((bound-identifier=? id (caar renames)) (car renames))
        (else (stx-assoc id (cdr renames)))))

    (define (remove-begins def)
      (kernel-syntax-case def #f
        ((begin defs ...)
         (apply append (map remove-begins (syntax->list #'(defs ...)))))
        (_ (list def))))
    
    (define (fix-expr e)
      (kernel-syntax-case e #f
        ((define-values x y) e)
        ((define-syntaxes x y) e)
        ((d x y) (or (module-identifier=? (quote-syntax define-values-ml) #'d)
                     (module-identifier=? (quote-syntax define-syntaxes-ml) #'d))
         e)
        (x #`(define-values () (begin x (values))))))
    
    (define (get-defs defs)
      (map fix-expr
           (apply append
                  (map (lambda (d)
                         (remove-begins
                          (local-expand d
                                        (syntax-local-context)
                                        kernel-form-identifier-list/no-begin)))
                       defs))))
    
    (define (get-ids def)
      (kernel-syntax-case def #f
        ((define-syntaxes vars body) (syntax->list #'vars))
        ((define-values vars body) (syntax->list #'vars))
        ((d vars body) (or (module-identifier=? (quote-syntax define-values-ml) #'d)
                           (module-identifier=? (quote-syntax define-syntaxes-ml) #'d))
         (syntax->list #'vars))
        (_ (raise-syntax-error 'structure "Internal error" def))))
    
    (define (rebuild ctxt val)
      (if (syntax? ctxt)
          (datum->syntax-object ctxt val ctxt ctxt)
          val))
    
    (define (rebuild-cons car cdr stx)
      (rebuild stx (cons car cdr)))
    
    (define (mark-ids def introducers)
      (let ((new-ids (map (lambda (id) (cons id (make-syntax-introducer)))
                          (get-ids def))))
        (values
         (syntax-case def ()
           ((ds . x) (module-identifier=? (quote-syntax define-syntaxes) #'ds)
            (rebuild-cons #'ds (mark-ids-helper #'x (append new-ids introducers)) def))
           ((dv . x) (module-identifier=? (quote-syntax define-values) #'dv)
            (rebuild-cons #'dv (mark-ids-helper #'x (append new-ids introducers)) def))
           ((d vars body) (module-identifier=? (quote-syntax define-values-ml) #'d)
            (rebuild def `(,(datum->syntax-object #'here 'define-values #'d #'d)
                           ,(mark-ids-helper #'vars (append new-ids introducers))
                           ,(mark-ids-helper #'body introducers))))
           ((d vars body) (module-identifier=? (quote-syntax define-syntaxes-ml) #'d)
            (rebuild def `(,(datum->syntax-object #'here 'define-syntaxes #'d #'d)
                           ,(mark-ids-helper #'vars (append new-ids introducers))
                           ,(mark-ids-helper #'body introducers)))))
        new-ids)))
        
    (define (mark-ids-helper def introducers)
      (let ((contents 
             (if (syntax? def)
                 (syntax-e def)
                 def)))
        (cond
          ((symbol? contents)
           (let ((introducer (stx-assoc def introducers)))
             (if introducer ((cdr introducer) def) def)))
          ((pair? contents)
           (rebuild-cons (mark-ids-helper (car contents) introducers)
                         (mark-ids-helper (cdr contents) introducers)
                         def))
          ((vector? contents)
           (rebuild def (list->vector
                         (map (lambda (x) (mark-ids-helper x introducers))
                              (vector->list contents)))))
          (else def))))
    
    (define (structure/proc stx)
      (syntax-case stx ()
        ((_ name provides body ...)
         (let ((defs (get-defs (syntax->list #'(body ...)))))
           (unless (identifier? #'name)
             (raise-syntax-error 'structure "Structure name must be an identifier" #'name))
           #`(begin
               #,@(let loop ((defined-ids null)
                             (defs defs))
                    (cond
                      ((null? defs)
                       (list
                        #`(define-syntaxes-ml (name)
                            (make-str
                             (remove-dups
                              (list
                               #,@(syntax-case #'provides () 
                                    (all (and (identifier? #'all)
                                              (module-identifier=? (quote-syntax provide-all)
                                                                   #'all))
                                     (map (lambda (id)
                                            `(cons 
                                              (quote ,(car id))
                                              (quote-syntax ,((cdr id)
                                                              (car id)))))
                                          (filter (lambda (id)
                                                    (bound-identifier=? 
                                                     (car id)
                                                     (datum->syntax-object 
                                                      #'provides
                                                      (syntax-object->datum (car id)))))
                                                  defined-ids)))
                                    ((provides ...)
                                     (map (lambda (provide)
                                            (let ((introducer (stx-assoc provide defined-ids)))
                                              (unless introducer
                                                (raise-syntax-error
                                                 'structure 
                                                 "Attempt to export undefined identifier"
                                                 provide))
                                              `(cons 
                                                (quote ,provide)
                                                (quote-syntax ,((cdr introducer) provide)))))
                                          (syntax->list #'(provides ...))))
                                    (p 
                                     (cond
                                       ((eq? 'provide-all (syntax-e #'p))
                                        (raise-syntax-error
                                         'structure
                                         "provide-all has been rebound"
                                         #'provides))
                                       (else
                                        (raise-syntax-error
                                         'structure
                                         "Export must have the form \"provide-all\" or \"(identifier ...)\""
                                         #'provides)))))))))))
                      (else
                       (let-values (((marked-def new-defined-ids)
                                     (mark-ids (car defs) defined-ids)))
                         (cons marked-def
                               (loop (append new-defined-ids defined-ids)
                                     (cdr defs))))))))))))
    )
  
  (define-syntax (open stx)
    (syntax-case stx ()
      ((_ top-name path ...)
       (datum->syntax-object #'here
         `(open-in-context ,#'top-name ,#'top-name ,@(syntax->list #'(path ...)))
         stx))))
  
  (define-syntax (open-in-context stx)
    (syntax-case stx ()
      ((_ context top-name path ...)
       (let ((env (open (cons #'top-name (syntax->list #'(path ...))) 'open)))
         (with-syntax ((((pub . hid) ...)
                        (map (lambda (x)
                               (cons (datum->syntax-object #'context (car x) stx)
                                     (cdr x)))
                             env)))
           #`(define-syntaxes-ml (pub ...)
               (values (make-rename-transformer (quote-syntax hid)) ...)))))))
       
  
  (define-syntax (dot-helper stx)
    (syntax-case stx ()
      ((_ path field)
       (begin
         (unless (identifier? #'field)
           (raise-syntax-error 'dot "Field to open must be an identifier" #'field))
         (cond
           ((stx-null? #'path) #'field)
           (else
            (let ((hid (assq (syntax-object->datum #'field)
                             (open (stx->list #'path) 'dot))))
              (unless hid
                (raise-syntax-error 'dot "Unknown field" #'field))
              (cdr hid))))))))

  (define-syntax (dot stx)
    (syntax-case stx ()
      ((_ path1 path-rest ...)
       (let*-values (((path field)
                      (split (cons #'path1 (syntax->list #'(path-rest ...))))))
         #`(begin0 (dot-helper #,path #,field))))))
                     
  (define-syntax (open-as stx)
    (syntax-case stx ()
      ((_ rename top-name path1 path-rest ...)
       (let-values (((path field)
                     (split (cons #'top-name (cons #'path1 (syntax->list #'(path-rest ...)))))))
         (unless (identifier? #'rename)
           (raise-syntax-error 'open-as "First position must be an identifier" #'rename))
         #`(define-syntaxes-ml (rename)
             (open-as-helper #'#,path #'#,field))))))
  )
