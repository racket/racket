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

(define current-class-cache (make-parameter #f))

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
    [(Listof: elem-ty)
     `(-lst ,(sub elem-ty))]
    [(Function: (list (arr: dom (Values: (list (Result: t (FilterSet: (Top:) (Top:)) (Empty:)))) #f #f '())))
     `(simple-> (list ,@(map sub dom)) ,(sub t))]
    [(Function: (list (arr: dom (Values: (list (Result: t (FilterSet: (TypeFilter: ft pth)
                                                                      (NotTypeFilter: ft pth))
                                                        (Empty:))))
                            #f #f '())))
     `(make-pred-ty (list ,@(map sub dom)) ,(sub t) ,(sub ft) ,(sub pth))]
    [(Function: (list (arr: dom (Values: (list (Result: t (FilterSet: (NotTypeFilter: (== -False)
                                                                                      (Path: pth (list 0 0)))
                                                                      (TypeFilter: (== -False)
                                                                                   (Path: pth (list 0 0))))
                                                        (Path: pth (list 0 0)))))
                            #f #f '())))
     `(->acc (list ,@(map sub dom)) ,(sub t) ,(sub pth))]
    [(Result: t (FilterSet: (Top:) (Top:)) (Empty:)) `(-result ,(sub t))]
    [(Union: elems) (split-union elems)]
    [(Base: n cnt pred _) (int-err "Base type not in predefined-type-table" n)]
    [(Name: stx deps args struct?)
     `(make-Name (quote-syntax ,stx)
                 (list ,@(map (λ (x) `(quote-syntax ,x)) deps))
                 ,(and args
                       `(list ,@(map (λ (x) `(quote-syntax ,x)) args)))
                 ,struct?)]
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
    [(PolyRow-names: ns c b) `(make-PolyRow (list ,@(map sub ns))
                                            (quote ,c) ,(sub b))]
    [(Class: row inits fields methods augments init-rest)
     (cond [(and (current-class-cache)
                 (dict-ref (unbox (current-class-cache)) v #f)) => car]
           [else
            ;; FIXME: there's probably a better way to do this
            (define (convert members [inits? #f])
              (for/list ([m members])
                `(list (quote ,(car m))
                       ,(sub (cadr m))
                       ,@(if inits? (cddr m) '()))))
            (define class-type
              `(make-Class ,(sub row)
                           (list ,@(convert inits #t))
                           (list ,@(convert fields))
                           (list ,@(convert methods))
                           (list ,@(convert augments))
                           ,(sub init-rest)))
            (define name (gensym))
            (define cache-box (current-class-cache))
            (when cache-box
              (set-box! cache-box
                        (dict-set (unbox cache-box) v (list name class-type))))
            (if cache-box name class-type)])]
    [(arr: dom rng rest drest kws)
     `(make-arr ,(sub dom) ,(sub rng) ,(sub rest) ,(sub drest) ,(sub kws))]
    [(TypeFilter: t p)
     `(make-TypeFilter ,(sub t) ,(sub p))]
    [(NotTypeFilter: t p)
     `(make-NotTypeFilter ,(sub t) ,(sub p))]
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

(define (make-init-code map f)
  (define class-type-cache (box '()))
  (define (bound-f id v)
    (and (bound-in-this-module id) (f id v)))
  (parameterize ((current-class-cache class-type-cache)
                 (current-print-convert-hook converter)
                 ;; ignore sharing in all cases
                 (current-build-share-hook (λ (v basic sub) 'atomic))
                 (show-sharing #f)
                 (booleans-as-true/false #f))
    (define aliases (filter values (map bound-f)))
    #`(begin
        #,@(for/list ([name+type (dict-values (unbox class-type-cache))])
             (match-define (list name type) name+type)
             (datum->syntax #'here `(define ,name ,type)))
        #,@aliases)))

(define (quote-type ty)
  (datum->syntax #'here (print-convert ty)))

(define (tname-env-init-code)
  (make-init-code
    type-name-env-map
    (λ (id ty) #`(register-type-name #'#,id #,(quote-type ty)))))

(define (tvariance-env-init-code)
  (make-init-code
    type-variance-env-map
    (λ (id var) #`(register-type-variance! #'#,id (list #,@(map variance->binding var))))))

(define (talias-env-init-code)
  (make-init-code
    type-alias-env-map
    (λ (id ty) #`(register-resolved-type-alias #'#,id #,(quote-type ty)))))

(define (env-init-code)
  (make-init-code
    type-env-map
    (λ (id ty) #`(register-type #'#,id #,(quote-type ty)))))

(define (mvar-env-init-code mvar-env)
  (make-init-code
    (λ (f) (dict-map mvar-env f))
    (lambda (id v) (and v #`(register-mutated-var #'#,id)))))
