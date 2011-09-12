#lang racket/base
(provide (all-defined-out))
(require "../utils/utils.rkt"
         "global-env.rkt"
	 "type-name-env.rkt"
	 "type-alias-env.rkt"
         (rep type-rep object-rep filter-rep rep-utils)
	 (for-template (rep type-rep object-rep filter-rep)
		       (types union)
		       racket/shared racket/base)
	 (types union convenience)
	 mzlib/pconvert racket/match)

(define (initialize-type-name-env initial-type-names)
  (for-each (lambda (nm/ty) (register-resolved-type-alias (car nm/ty) (cadr nm/ty))) initial-type-names))

(define (initialize-type-env initial-env)
  (for-each (lambda (nm/ty) (register-type-if-undefined (car nm/ty) (cadr nm/ty))) initial-env))

(define (converter v basic sub)
  (define (gen-constructor sym)
    (string->symbol (string-append "make-" (substring (symbol->string sym) 7))))
  (match v
    [(Union: elems) `(make-Union (sort (list ,@(map sub elems)) < #:key Type-seq))]
    [(Base: n cnt pred marshaled _) marshaled]
    [(Name: stx) `(make-Name (quote-syntax ,stx))]
    [(fld: t acc mut) `(make-fld ,(sub t) (quote-syntax ,acc) ,mut)]
    [(Struct: name parent flds proc poly? pred-id cert maker-id)
     `(make-Struct (quote-syntax ,name) ,(sub parent)
                   ,(sub flds) ,(sub proc) ,(sub poly?)
                   (quote-syntax ,pred-id) (syntax-local-certifier)
                   (quote-syntax ,maker-id))]
    [(App: rator rands stx) `(make-App ,(sub rator) ,(sub rands) (quote-syntax ,stx))]
    [(Opaque: pred cert) `(make-Opaque (quote-syntax ,pred) (syntax-local-certifier))]
    [(Refinement: parent pred cert) `(make-Refinement ,(sub parent)
                                                      (quote-syntax ,pred)
                                                      (syntax-local-certifier))]
    [(Mu-name: n b) `(make-Mu ,(sub n) ,(sub b))]
    [(Poly-names: ns b) `(make-Poly (list ,@(map sub ns)) ,(sub b))]
    [(PolyDots-names: ns b) `(make-PolyDots (list ,@(map sub ns)) ,(sub b))]
    [(arr: dom rng rest drest kws)
     `(make-arr ,(sub dom) ,(sub rng) ,(sub rest) ,(sub drest) ,(sub kws))]
    [(TypeFilter: t p i)
     `(make-TypeFilter ,(sub t) ,(sub p) ,(if (identifier? i)
                                              `(quote-syntax ,i)
                                              i))]
    [(NotTypeFilter: t p i)
     `(make-NotTypeFilter ,(sub t) ,(sub p)
                          ,(if (identifier? i)
                               `(quote-syntax ,i)
                               i))]
    [(Path: p i)
     `(make-Path ,(sub p) ,(if (identifier? i)
                               `(quote-syntax ,i)
                               i))]
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
      #'(begin-for-syntax  . registers))))

(define (talias-env-init-code)
  (define (f id ty)
    (if (bound-in-this-module id)
        #`(register-resolved-type-alias #'#,id #,(datum->syntax #'here (print-convert ty)))
        #f))
  (parameterize ((current-print-convert-hook converter)
                 (show-sharing #f)
                 (booleans-as-true/false #f))
    (with-syntax ([registers (filter (lambda (x) x) (type-alias-env-map f))])
      #'(begin-for-syntax  . registers))))

(define (env-init-code syntax-provide? provide-tbl def-tbl)
  (define (f id ty)
    (if (and (bound-in-this-module id)
             ;; if there are no syntax provides, then we only need this identifier if it's provided
             #;(or syntax-provide? (dict-ref provide-tbl id #f)))
        #`(register-type #'#,id #,(datum->syntax #'here (print-convert ty)))
        #f))
  (parameterize ((current-print-convert-hook converter)
                 (show-sharing #f)
                 (booleans-as-true/false #f))
    (with-syntax ([registers (filter values (type-env-map f))])
      #'(begin-for-syntax . registers))))



