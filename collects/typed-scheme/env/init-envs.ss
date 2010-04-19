#lang scheme/base
(provide (all-defined-out))
(require "../utils/utils.ss"
         "type-env.ss" 
	 "type-name-env.ss"
	 "type-alias-env.ss"
         unstable/struct
         (rep type-rep object-rep filter-rep rep-utils)
	 (for-template (rep type-rep object-rep filter-rep) 
		       (types union)
		       mzlib/pconvert mzlib/shared scheme/base)
	 (types union convenience)         
	 mzlib/pconvert scheme/match mzlib/shared)

(define (initialize-type-name-env initial-type-names)
  (for-each (lambda (nm/ty) (register-resolved-type-alias (car nm/ty) (cadr nm/ty))) initial-type-names))

(define (initialize-type-env initial-env)
  (for-each (lambda (nm/ty) (register-type (car nm/ty) (cadr nm/ty))) initial-env))

(define (converter v basic sub)
  (define (gen-constructor sym)
    (string->symbol (string-append "make-" (substring (symbol->string sym) 7))))
  (match v
    [(Union: elems) `(make-Union (sort (list ,@(map sub elems)) < #:key Type-seq))]
    [(Base: n cnt) `(make-Base ',n (quote-syntax ,cnt))]
    [(Name: stx) `(make-Name (quote-syntax ,stx))]
    [(Struct: name parent flds proc poly? pred-id cert acc-ids maker-id)
     `(make-Struct ,(sub name) ,(sub parent) 
                   ,(sub flds) ,(sub proc) ,(sub poly?)
                   (quote-syntax ,pred-id) (syntax-local-certifier)
                   (list ,@(for/list ([a acc-ids]) `(quote-syntax ,a)))
                   (quote-syntax ,maker-id))]
    [(App: rator rands stx) `(make-App ,(sub rator) ,(sub rands) (quote-syntax ,stx))]
    [(Opaque: pred cert) `(make-Opaque (quote-syntax ,pred) (syntax-local-certifier))]
    [(Refinement: parent pred cert) `(make-Refinement ,(sub parent)
                                                      (quote-syntax ,pred)
                                                      (syntax-local-certifier))]
    [(Mu-name: n b) `(make-Mu ,(sub n) ,(sub b))]
    [(Poly-names: ns b) `(make-Poly (list ,@(map sub ns)) ,(sub b))]
    [(PolyDots-names: ns b) `(make-PolyDots (list ,@(map sub ns)) ,(sub b))]
    [(arr: dom rng rest drest kws names)
     `(make-arr ,(sub dom) ,(sub rng) ,(sub rest) ,(sub drest) ,(sub kws) 
                (list ,@(for/list ([i names]) `(quote-syntax ,i))))]
    [(TypeFilter: t p i)
     `(make-TypeFilter ,(sub t) ,(sub p) (quote-syntax ,i))]
    [(NotTypeFilter: t p i)
     `(make-NotTypeFilter ,(sub t) ,(sub p) (quote-syntax ,i))]
    [(Path: p i)
     `(make-Path ,(sub p) (quote-syntax ,i))]
    [(? (lambda (e) (or (Filter? e)
                        (Object? e)
                        (PathElem? e)))
        (app (lambda (v) (vector->list (struct->vector v))) (list-rest tag seq fv fi stx vals)))
     `(,(gen-constructor tag) ,@(map sub vals))]
    [(? Type?
        (app (lambda (v) (vector->list (struct->vector v))) (list-rest tag seq fv fi stx key vals)))
     `(,(gen-constructor tag) ,@(map sub vals))]
    [_ (basic v)]))

(define (bound-in-this-module id) 
  (let ([binding (identifier-binding id)])
    (if (and (list? binding) (module-path-index? (car binding)))
        (let-values ([(mp base) (module-path-index-split (car binding))])
          (not mp))
        #f)))

(define (tname-env-init-code)    
  (define (f id ty)
    (if (bound-in-this-module id)
        #`(register-type-name #'#,id #,(datum->syntax #'here (print-convert ty)))
        #f))
  (parameterize ((current-print-convert-hook converter)
                 (show-sharing #f)
                 (booleans-as-true/false #f))                   
    (with-syntax ([registers (filter (lambda (x) x) (type-name-env-map f))])
      #'(begin (begin-for-syntax  . registers)))))

(define (talias-env-init-code)    
  (define (f id ty)
    (if (bound-in-this-module id)
        #`(register-resolved-type-alias #'#,id #,(datum->syntax #'here (print-convert ty)))
        #f))
  (parameterize ((current-print-convert-hook converter)
                 (show-sharing #f)
                 (booleans-as-true/false #f))                   
    (with-syntax ([registers (filter (lambda (x) x) (type-alias-env-map f))])
      #'(begin (begin-for-syntax  . registers)))))

(define (env-init-code)    
  (define (f id ty)
    (if (bound-in-this-module id)
        #`(register-type #'#,id #,(datum->syntax #'here (print-convert ty)))
        #f))
  (parameterize ((current-print-convert-hook converter)
                 (show-sharing #f)
                 (booleans-as-true/false #f))                   
    (with-syntax ([registers (filter (lambda (x) x) (type-env-map f))])
      #'(begin (begin-for-syntax  . registers)))))



