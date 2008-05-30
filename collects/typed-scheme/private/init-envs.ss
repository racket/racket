#lang scheme/base
(provide (all-defined-out))

(require "type-env.ss" "type-rep.ss" "type-name-env.ss" "union.ss" "effect-rep.ss"
         "type-effect-convenience.ss" "type-alias-env.ss"
         "type-alias-env.ss")
(require mzlib/pconvert scheme/match mzlib/shared
         (for-template mzlib/pconvert mzlib/shared scheme/base "type-rep.ss" "union.ss" "effect-rep.ss"))    

(define (initialize-type-name-env initial-type-names)
  (for-each (lambda (nm/ty) (register-resolved-type-alias (car nm/ty) (cadr nm/ty))) initial-type-names))

(define (initialize-type-env initial-env)
  (for-each (lambda (nm/ty) (register-type (car nm/ty) (cadr nm/ty))) initial-env))

(define (converter v basic sub)
  (define (gen-constructor sym)
    (string->symbol (string-append "make-" (substring (symbol->string sym) 7))))
  (match v
    [(Union: elems) `(make-Union (list ,@(map sub elems)))]
    [(Name: stx) `(make-Name (quote-syntax ,stx))]
    [(Struct: name parent flds proc poly? pred-id cert)
     `(make-Struct ,(sub name) ,(sub parent) ,(sub flds) ,(sub proc) ,(sub poly?) (quote-syntax ,pred-id) (syntax-local-certifier))]
    [(App: rator rands stx) `(make-App ,(sub rator) ,(sub rands) (quote-syntax ,stx))]
    [(Opaque: pred cert) `(make-Opaque (quote-syntax ,pred) (syntax-local-certifier))]
    [(Mu-name: n b) `(make-Mu ,(sub n) ,(sub b))]
    [(Poly-names: ns b) `(make-Poly (list ,@(map sub ns)) ,(sub b))]
    [(? Type? (app (lambda (v) (vector->list (struct->vector v))) (list-rest tag seq vals))) 
     `(,(gen-constructor tag) ,@(map sub vals))]
    [(? Effect? (app (lambda (v) (vector->list (struct->vector v))) (list-rest tag seq vals))) 
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



