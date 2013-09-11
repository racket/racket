#lang racket/base

;; Support for defining the initial TR environment

(require "../utils/utils.rkt"
         "../utils/tc-utils.rkt"
         "global-env.rkt"
         "type-name-env.rkt"
         "type-alias-env.rkt"
         "mvar-env.rkt"
	 (rename-in racket/private/sort [sort raw-sort])
         (rep type-rep object-rep filter-rep rep-utils free-variance)
         (for-template (rep type-rep object-rep filter-rep)
                       (types union abbrev)
                       racket/shared (except-in racket/base sort)
		       (rename-in racket/private/sort [sort raw-sort]))
         (for-syntax syntax/parse racket/base)
         (types abbrev union)
         racket/syntax racket/dict racket/list
         mzlib/pconvert racket/match)

(provide ;; convenience form for defining an initial environment
         ;; used by "base-special-env.rkt" and "base-contracted.rkt"
         define-initial-env
         initialize-type-name-env
         initialize-type-env
         converter
         bound-in-this-module
         tname-env-init-code
         tvariance-env-init-code
         talias-env-init-code
         env-init-code
         mvar-env-init-code)

(define-syntax (define-initial-env stx)
  (syntax-parse stx
    [(_ initialize-env [id-expr ty] ...)
     #`(begin
         (define initial-env (make-env [id-expr (λ () ty)] ... ))
         (define (initialize-env) (initialize-type-env initial-env))
         (provide initialize-env))]))

(define (initialize-type-name-env initial-type-names)
  (for-each (lambda (nm/ty) (register-resolved-type-alias (car nm/ty) (cadr nm/ty))) initial-type-names))

(define (initialize-type-env initial-env)
  (for-each (lambda (nm/ty) (register-type-if-undefined (car nm/ty) (cadr nm/ty))) initial-env))

(define (converter v basic sub)
  (define (numeric? t) (match t [(Base: _ _ _ b) b] [(Value: (? number?)) #t] [_ #f]))
  (define (split-union ts)
    (define-values (nums others) (partition numeric? ts))
    (cond [(or (null? nums) (null? others))
	   ;; nothing interesting to do in this case
	   `(make-Union (,#'raw-sort (list ,@(map sub ts)) < Type-seq #f))]
	  [else
	   ;; we do a little more work to hopefully save a bunch in serialization space
	   ;; if we get a hit in the predefined-type-table
	   `(simple-Un ,(sub (apply Un nums)) ,(sub (apply Un others)))]))

  (define (gen-constructor sym)
    (string->symbol (string-append "make-" (substring (symbol->string sym) 7))))
  (match v
    [(? Rep? (app (lambda (v) (hash-ref predefined-type-table (Rep-seq v) #f)) (? values id))) id]
    [(Mu: var (Union: (list (Value: '()) (Pair: elem-ty (F: var)))))
     `(-lst ,(sub elem-ty))]
    [(Mu: var (Union: (list (Pair: elem-ty (F: var)) (Value: '()))))
     `(-lst ,(sub elem-ty))]
    [(Function: (list (arr: dom (Values: (list (Result: t (FilterSet: (Top:) (Top:)) (Empty:)))) #f #f '())))
     `(simple-> (list ,@(map sub dom)) ,(sub t))]
    [(Function: (list (arr: dom (Values: (list (Result: t (FilterSet: (TypeFilter: ft pth n)
								      (NotTypeFilter: ft pth n))
							(Empty:))))
			    #f #f '())))
     `(make-pred-ty (list ,@(map sub dom)) ,(sub t) ,(sub ft) ,(sub n) ,(sub pth))]
    [(Function: (list (arr: dom (Values: (list (Result: t (FilterSet: (NotTypeFilter: (== -False) pth 0)
								      (TypeFilter: (== -False) pth 0))
							(Path: pth 0))))
			    #f #f '())))
     `(->acc (list ,@(map sub dom)) ,(sub t) ,(sub pth))]
    [(Union: elems) (split-union elems)]
    [(Base: n cnt pred _) (int-err "Base type not in predefined-type-table" n)]
    [(Name: stx) `(make-Name (quote-syntax ,stx))]
    [(fld: t acc mut) `(make-fld ,(sub t) (quote-syntax ,acc) ,mut)]
    [(Struct: name parent flds proc poly? pred-id)
     `(make-Struct (quote-syntax ,name) ,(sub parent)
                   ,(sub flds) ,(sub proc) ,(sub poly?)
                   (quote-syntax ,pred-id))]
    [(App: rator rands stx) `(make-App ,(sub rator) ,(sub rands) (quote-syntax ,stx))]
    [(Opaque: pred) `(make-Opaque (quote-syntax ,pred))]
    [(Refinement: parent pred) `(make-Refinement ,(sub parent)
                                                 (quote-syntax ,pred))]
    [(Mu-name: n b) `(make-Mu ,(sub n) ,(sub b))]
    [(Poly-names: ns b) `(make-Poly (list ,@(map sub ns)) ,(sub b))]
    [(PolyDots-names: ns b) `(make-PolyDots (list ,@(map sub ns)) ,(sub b))]
    [(arr: dom rng rest drest kws)
     `(make-arr ,(sub dom) ,(sub rng) ,(sub rest) ,(sub drest) ,(sub kws))]
    [(TypeFilter: t p i)
     `(make-TypeFilter ,(sub t) ,(sub p) ,(if (identifier? i)
                                              `(quote-syntax ,i)
                                              `(list ,(car i) ,(cadr i))))]
    [(NotTypeFilter: t p i)
     `(make-NotTypeFilter ,(sub t) ,(sub p)
                          ,(if (identifier? i)
                               `(quote-syntax ,i)
                               `(list ,(car i) ,(cadr i))))]
    [(Path: p i)
     `(make-Path ,(sub p) ,(if (identifier? i)
                               `(quote-syntax ,i)
                               `(list ,(car i) ,(cadr i))))]
    [(? Rep? rep)
     `(,(gen-constructor (car (vector->list (struct->vector rep))))
       ,@(map sub (Rep-values rep)))]
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
    #`(begin #,@(filter values (type-name-env-map f)))))

(define (tvariance-env-init-code)
  (define (f id var)
    (if (bound-in-this-module id)
        #`(register-type-variance! #'#,id (list #,@(map variance->binding var)))
        #f))
  (parameterize ((current-print-convert-hook converter)
                 (show-sharing #f)
                 (booleans-as-true/false #f))
    #`(begin #,@(filter values (type-variance-env-map f)))))


(define (talias-env-init-code)
  (define (f id ty)
    (if (bound-in-this-module id)
        #`(register-resolved-type-alias #'#,id #,(datum->syntax #'here (print-convert ty)))
        #f))
  (parameterize ((current-print-convert-hook converter)
                 (show-sharing #f)
                 (booleans-as-true/false #f))
    #`(begin #,@(filter values (type-alias-env-map f)))))

(define (env-init-code syntax-provide? provide-tbl def-tbl)
  (define (f id ty)
    (if (bound-in-this-module id)
        #`(register-type #'#,id #,(datum->syntax #'here (print-convert ty)))
        #f))
  (parameterize ((current-print-convert-hook converter)
                 (show-sharing #f)
                 (booleans-as-true/false #f))
    #`(begin #,@(filter values (type-env-map f)))))

(define (mvar-env-init-code mvar-env)
  (define (f id v)
    (and v (bound-in-this-module id)
         #`(register-mutated-var #'#,id)))
  #`(begin #,@(filter values (dict-map mvar-env f))))



