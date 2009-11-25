#lang scheme/base
(provide (all-defined-out))
(require "../utils/utils.ss")

(require "type-env.ss" 
	 "type-name-env.ss"
	 "type-alias-env.ss"
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
    [(Union: elems) `(make-Union (sort (list ,@(map sub elems)) < #:key Type-seq) ',(Type-name v))]
    [(Base: n cnt) `(make-Base ',n (quote-syntax ,cnt) ',(Type-name v))]
    [(Name: stx) `(make-Name (quote-syntax ,stx) ',(Type-name v))]
    [(Struct: name parent flds proc poly? pred-id cert)
     `(make-Struct ,(sub name) ,(sub parent) ,(sub flds) ,(sub proc) ,(sub poly?) (quote-syntax ,pred-id) (syntax-local-certifier) ',(Type-name v))]
    [(App: rator rands stx) `(make-App ,(sub rator) ,(sub rands) (quote-syntax ,stx) ',(Type-name v))]
    [(Opaque: pred cert) `(make-Opaque (quote-syntax ,pred) (syntax-local-certifier) ',(Type-name v))]
    [(Refinement: parent pred cert) `(make-Refinement ,(sub parent)
                                                      (quote-syntax ,pred)
                                                      (syntax-local-certifier)
                                                      ',(Type-name v))]
    [(Mu-name: n b) `(make-Mu ,(sub n) ,(sub b) ',(Type-name v))]
    [(Poly-names: ns b) `(make-Poly (list ,@(map sub ns)) ,(sub b) ',(Type-name v))]
    [(PolyDots-names: ns b) `(make-PolyDots (list ,@(map sub ns)) ,(sub b) ',(Type-name v))]
    [(? (lambda (e) (or (LatentFilter? e)
                        (LatentObject? e)
                        (PathElem? e)))
        (app (lambda (v) (vector->list (struct->vector v))) (list-rest tag seq vals))) 
     `(,(gen-constructor tag) ,@(map sub vals))]
    [(? (lambda (e) (or (Type? e)))
        (app (lambda (v) (vector->list (struct->vector v))) (list-rest tag key seq name vals))) 
     `(,(gen-constructor tag) ,@(map sub vals) ',name)]
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



