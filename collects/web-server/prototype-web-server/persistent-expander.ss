 (module persistent-expander mzscheme
   (require "abort-resume.ss")
   (require-for-syntax (lib "kerncase.ss" "syntax")
                       (lib "list.ss")
                       "labels.ss"
                       "elim-letrec.ss"
                       "normalizer.ss"
                       "elim-call-cc.ss"
                       "defunctionalize.ss")
   (provide lang-module-begin)
   (provide (all-from "abort-resume.ss"))
   
   ;; lang-module-begin
   ;; Does the normal module-begin stuff, except it hands off all
   ;; module forms to a collect macro.
   (define-syntax (lang-module-begin stx)
     (syntax-case stx ()
       [(_ form ...)
        #`(#%plain-module-begin
           ;#,(datum->syntax-object stx '(require-for-syntax mzscheme))
           (collect () (form ...)))]))
   
   ;; collect
   ;; (collect (def/expr ...) (module-form ...))
   ;; collect expands each module-form until it can recognize what kind of form it is.
   ;; If it's a define-syntaxes, require, require-for-syntax, or provide form,
   ;; it lets it "pass through" the collect macro to be executed by the primitive module
   ;; expander. If it's a define-values form, it expands the body and then reconstructs a
   ;; define-values form to put in the def/exprs list. If it's any other kind of form, it
   ;; fully expands the form and puts it on the def/exprs list.
   ;;
   ;; The fully expanded definitions and expressions are then passed (in the original
   ;; order) to the transform macro.
   (define-syntax (collect stx)
     (define (module-identifier-member id ids)
       (cond [(null? ids) #f]
             [(module-identifier=? id (car ids)) ids]
             [else (module-identifier-member id (cdr ids))]))
     (syntax-case stx ()
       [(_ rev-def/exprs (form0 . forms))
        (let ([expand-context (syntax-local-context)]
              [stop-list (list*
                          #'require
                          #'require-for-syntax
                          #'provide
                          (kernel-form-identifier-list #'here))])
          (let ([e-form0 (local-expand #'form0 expand-context stop-list)])
            (syntax-case e-form0 (begin define-values)
              [(keyword . _)
               (and (identifier? #'keyword)
                    (module-identifier-member #'keyword
                                              (list #'require #'require-for-syntax
                                                    #'provide #'define-syntaxes)))
               #`(begin #,e-form0 (collect rev-def/exprs forms))]
              [(begin e-form ...)
               #`(collect rev-def/exprs (e-form ... . forms))]
              [(define-values (id ...) expr)
               (andmap identifier? (syntax->list #'(id ...)))
               (let ([e-expr (local-expand #'expr 'expression (list #'#%top))])
                 #`(begin
                     (collect [(define-values (id ...) #,e-expr) . rev-def/exprs]
                              forms)))]
              [expr
               (let ([e-expr (local-expand #'expr 'expression (list #'#%top))])
                 #`(collect [#,e-expr . rev-def/exprs] forms))])))]
       [(_ rev-def/exprs ())
        (with-syntax ([(def/expr ...) (reverse (syntax->list #'rev-def/exprs))])
          #'(transform () (def/expr ...)))]))
   
   ;; **********************************************************************
   ;; **********************************************************************
   
   ;   (define-for-syntax myprint printf)
   
   ;; transform
   ;; This macro is where you put your transformations. Each def/expr is core mzscheme.
   ;; Furthermore, no def/expr is a define-syntaxes, require etc form.
   (define-syntax (transform stx)
     (syntax-case stx (define-values lambda)
       [(_ rev-defs [(define-values (var) (lambda (formals ...) proc-body)) . rest])
        #'(transform [(define-values (var) (lambda (formals ...) proc-body)) . rev-defs]
                     rest)]
       [(_ rev-defs [body-expr])
        (let* ([base-labeling (make-labeling (string->bytes/utf-8 (format "~a" (syntax-object->datum stx))))]
               [make-labeler (lambda (tag)
                               (lambda ()
                                 (datum->syntax-object tag (base-labeling))))])
          (let ([new-defs (foldl
                           (lambda (first rest)
                             (append 
                              (defunctionalize-definition
                                (elim-call/cc-from-definition
                                 (normalize-definition
                                  (elim-letrec-from-definition first)))
                                (make-labeler first))
                              rest))
                           '()
                           (syntax->list #'rev-defs))])
            (let-values ([(new-body-expr body-defs)
                          (defunctionalize
                            (elim-call/cc
                             (normalize-term
                              (elim-letrec #'body-expr)))
                            (make-labeler #'body-expr))])
              #`(begin
                  #,@new-defs
                  #,@body-defs
                  (abort/cc #,new-body-expr)))))]
       [(_ rev-defs [])
        (raise-syntax-error #f "module has no body expression" stx)]
       [_else
        (raise-syntax-error #f "extra body expression, or expression out of order" stx)])))