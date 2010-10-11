#lang racket/base

(require "../utils/utils.rkt")

(require (rep type-rep object-rep filter-rep rep-utils)
         "resolve.rkt"
         (utils tc-utils)
         racket/list
         racket/match         
         (except-in racket/contract ->* ->)
         (prefix-in c: racket/contract)
         (for-syntax racket/base syntax/parse)
	 (for-template racket/base racket/contract racket/promise racket/tcp racket/flonum))

(provide (all-defined-out)
         (rename-out [make-Listof -lst]
                     [make-MListof -mlst]))

;; convenient constructors


(define -App make-App)
(define -pair make-Pair)
(define -mpair make-MPair)
(define -val make-Value)
(define -Param make-Param)
(define -box make-Box)
(define -channel make-Channel)
(define -vec make-Vector)
(define -future make-Future)
(define (-seq . args) (make-Sequence args))

(define-syntax *Un
  (syntax-rules ()
    [(_ . args) (make-Union (list . args))]))


(define (make-Listof elem) (-mu list-rec (*Un (-val null) (-pair elem list-rec))))
(define (make-MListof elem) (-mu mlist-rec (*Un (-val null) (-mpair elem mlist-rec))))

(define (-lst* #:tail [tail (-val null)] . args)
  (for/fold ([tl tail]) ([a (reverse args)]) (-pair a tl)))

(define (-Tuple l)
  (foldr -pair (-val '()) l))

(define (-Tuple* l b)
  (foldr -pair b l))

(define (untuple t)
  (match (resolve t)
    [(Value: '()) null]
    [(Pair: a b) (cond [(untuple b) => (lambda (l) (cons a l))]
                       [else #f])]
    [_ #f]))

(define-match-expander Listof:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pat (~optional var-pat #:defaults ([var-pat #'var])))
       (syntax/loc stx (Mu: var-pat (Union: (list (Value: '()) (Pair: elem-pat (F: var-pat))))))])))

(define-match-expander List:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pats)
       #'(app untuple (? values elem-pats))])))

(define-match-expander MListof:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pat)
       #'(Mu: var (Union: (list (Value: '()) (MPair: elem-pat (F: var)))))])))


(d/c (-result t [f -no-filter] [o -no-obj])
  (c:->* (Type/c) (FilterSet? Object?) Result?)
  (make-Result t f o))

(d/c (-values args)
     (c:-> (listof Type/c) (or/c Type/c Values?))
     (match args
       ;[(list t) t]
       [_ (make-Values (for/list ([i args]) (-result i)))]))

;; basic types

(define promise-sym (string->uninterned-symbol "Promise"))

(define make-promise-ty
  (let ([s promise-sym])
    (lambda (t)
      (make-Struct s #f (list (make-fld t #'values #f)) #f #f #'promise? values #'values))))

(define -Listof (-poly (list-elem) (make-Listof list-elem)))

(define -Boolean (make-Base 'Boolean #'boolean?))
(define -Symbol (make-Base 'Symbol #'symbol?))
(define -Void (make-Base 'Void #'void?))
(define -Bytes (make-Base 'Bytes #'bytes?))
(define -Regexp (make-Base 'Regexp #'(and/c regexp? (not/c pregexp?) (not/c byte-regexp?))))
(define -PRegexp (make-Base 'PRegexp #'(and/c pregexp? (not/c byte-pregexp?))))
(define -Byte-Regexp (make-Base 'Byte-Regexp #'(and/c byte-regexp? (not/c byte-pregexp?))))
(define -Byte-PRegexp (make-Base 'Byte-PRegexp #'byte-pregexp?))
(define -String (make-Base 'String #'string?))
(define -Keyword (make-Base 'Keyword #'keyword?))
(define -Char (make-Base 'Char #'char?))
(define -Thread (make-Base 'Thread #'thread?))
(define -Resolved-Module-Path (make-Base 'Resolved-Module-Path #'resolved-module-path?))
(define -Module-Path (make-Base 'Module-Path #'module-path?))
(define -Module-Path-Index (make-Base 'Module-Path-Index #'module-path-index?))
(define -Compiled-Module-Expression (make-Base 'Compiled-Module-Expression #'compiled-module-expression?))
(define -Prompt-Tag (make-Base 'Prompt-Tag #'continuation-prompt-tag?))
(define -Cont-Mark-Set (make-Base 'Continuation-Mark-Set #'continuation-mark-set?))
(define -Path (make-Base 'Path #'path?))
(define -Namespace (make-Base 'Namespace #'namespace?))
(define -Output-Port (make-Base 'Output-Port #'output-port?))
(define -Input-Port (make-Base 'Input-Port #'input-port?))
(define -TCP-Listener (make-Base 'TCP-Listener #'tcp-listener?))

(define -FlVector (make-Base 'FlVector #'flvector?))

(define -Syntax make-Syntax)
(define -HT make-Hashtable)
(define -Promise make-promise-ty)

(define Univ (make-Univ))
(define Err (make-Error))

(define -Port (*Un -Output-Port -Input-Port))

(define -Pathlike (*Un -String -Path))
(define -Pathlike* (*Un -String -Path (-val 'up) (-val 'same)))
(define -Pattern (*Un -Bytes -Regexp -PRegexp -Byte-Regexp -Byte-PRegexp -String))

(define -top (make-Top))
(define -bot (make-Bot))
(define -no-filter (make-FilterSet -top -top))
(define -no-obj (make-Empty))


(d/c (-FS + -)
      (c:-> Filter/c Filter/c FilterSet?)
      (match* (+ -)
             [((Bot:) _) (make-FilterSet -bot -top)]
             [(_ (Bot:)) (make-FilterSet -top -bot)]
             [(+ -) (make-FilterSet + -)]))

(define -car (make-CarPE))
(define -cdr (make-CdrPE))
(define -syntax-e (make-SyntaxPE))

;; Numeric hierarchy
(define -Number (make-Base 'Number #'number?))

(define -InexactComplex (make-Base 'Inexact-Complex
                                   #'(and/c number?
                                            (lambda (x)
                                              (and (flonum? (imag-part x))
                                                   (flonum? (real-part x)))))))

;; default 64-bit floats
(define -Flonum (make-Base 'Flonum #'flonum?))
(define -NonnegativeFlonum (make-Base 'Nonnegative-Flonum
                                      #'(and/c flonum?
                                               (or/c positive? zero?)
                                               (lambda (x) (not (eq? x -0.0))))))
;; could be 32- or 64-bit floats
(define -InexactReal (make-Base 'Inexact-Real #'inexact-real?))

(define -ExactRational 
  (make-Base 'Exact-Rational #'(and/c number? rational? exact?)))
(define -Integer (make-Base 'Integer #'exact-integer?))
(define -ExactPositiveInteger
  (make-Base 'Exact-Positive-Integer #'exact-positive-integer?))

(define -PositiveFixnum
  (make-Base 'Positive-Fixnum #'(and/c number? fixnum? positive?)))
(define -NegativeFixnum
  (make-Base 'Negative-Fixnum #'(and/c number? fixnum? negative?)))

(define -Zero (-val 0))
(define -Real (*Un -InexactReal -ExactRational))
(define -Fixnum (*Un -PositiveFixnum -NegativeFixnum -Zero))
(define -NonnegativeFixnum (*Un -PositiveFixnum -Zero))
(define -ExactNonnegativeInteger (*Un -ExactPositiveInteger -Zero))
(define -Nat -ExactNonnegativeInteger)

(define -Byte -NonnegativeFixnum)



;; convenient syntax

(define-syntax -v 
  (syntax-rules ()
    [(_ x) (make-F 'x)]))

(define-syntax -poly
  (syntax-rules ()
    [(_ (vars ...) ty)
     (let ([vars (-v vars)] ...)
       (make-Poly (list 'vars ...) ty))]))

(define-syntax -polydots
  (syntax-rules ()
    [(_ (vars ... dotted) ty)
     (let ([dotted (-v dotted)]
           [vars (-v vars)] ...)
       (make-PolyDots (list 'vars ... 'dotted) ty))]))

(define-syntax -mu
  (syntax-rules ()
    [(_ var ty)
     (let ([var (-v var)])
       (make-Mu 'var ty))]))

;; function type constructors

(define top-func (make-Function (list (make-top-arr))))

(d/c (make-arr* dom rng 
                #:rest [rest #f] #:drest [drest #f] #:kws [kws null]
                #:filters [filters -no-filter] #:object [obj -no-obj])
  (c:->* ((listof Type/c) (or/c Values? ValuesDots? Type/c))
         (#:rest (or/c #f Type/c) 
          #:drest (or/c #f (cons/c Type/c symbol?))
          #:kws (listof Keyword?)
          #:filters FilterSet?
          #:object Object?)
         arr?)
  (make-arr dom (if (or (Values? rng) (ValuesDots? rng))
                    rng
                    (make-Values (list (-result rng filters obj))))
            rest drest (sort #:key Keyword-kw kws keyword<?)))

(define-syntax (->* stx)
  (define-syntax-class c
    (pattern x:id #:fail-unless (eq? ': (syntax-e #'x)) #f))
  (syntax-parse stx
    [(_ dom rng)       
     #'(make-Function (list (make-arr* dom rng)))]
    [(_ dom rst rng)
     #'(make-Function (list (make-arr* dom rng #:rest rst)))]
    [(_ dom rng :c filters)
     #'(make-Function (list (make-arr* dom rng #:filters filters)))]
    [(_ dom rng _:c filters _:c object)
     #'(make-Function (list (make-arr* dom rng #:filters filters #:object object)))]
    [(_ dom rst rng _:c filters)
     #'(make-Function (list (make-arr* dom rng #:rest rst #:filters filters)))]
    [(_ dom rst rng _:c filters : object)
     #'(make-Function (list (make-arr* dom rng #:rest rst #:filters filters #:object object)))]))

(define-syntax (-> stx)
  (define-syntax-class c
    (pattern x:id #:fail-unless (eq? ': (syntax-e #'x)) #f))
  (syntax-parse stx
    [(_ dom ... rng _:c filters _:c objects)
     #'(->* (list dom ...) rng : filters : objects)]
    [(_ dom ... rng :c filters)
     #'(->* (list dom ...) rng : filters)]
    [(_ dom ... rng)
     #'(->* (list dom ...) rng)]))

(define-syntax ->...
  (syntax-rules (:)
    [(_ dom rng)
     (->* dom rng)]
    [(_ dom (dty dbound) rng)
     (make-Function (list (make-arr* dom rng #:drest (cons dty 'dbound))))]
    [(_ dom rng : filters)
     (->* dom rng : filters)]
    [(_ dom (dty dbound) rng : filters)
     (make-Function (list (make-arr* dom rng #:drest (cons dty 'dbound) #:filters filters)))]))

(define (->acc dom rng path)
  (make-Function (list (make-arr* dom rng
                                  #:filters (-FS (-not-filter (-val #f) 0 path)
                                                 (-filter (-val #f) 0 path))
                                  #:object (make-Path path 0)))))

(define (cl->* . args)
  (define (funty-arities f)
    (match f
      [(Function: as) as]))
  (make-Function (apply append (map funty-arities args))))

(define-syntax cl->
  (syntax-parser
   [(_ [(dom ...) rng] ...)
    #'(cl->* (dom ... . -> . rng) ...)]))

(define-syntax (->key stx)  
  (syntax-parse stx
                [(_ ty:expr ... (~seq k:keyword kty:expr opt:boolean) ... rng)
                 #'(make-Function
                    (list
                     (make-arr* (list ty ...)
                                rng
                                #:kws (list (make-Keyword 'k kty opt) ...))))]))

(define (make-arr-dots dom rng dty dbound)
  (make-arr* dom rng #:drest (cons dty dbound)))

(define (-struct name parent flds constructor [proc #f] [poly #f] [pred #'dummy] [cert values])
  (make-Struct name parent flds proc poly pred cert constructor))

(d/c (-filter t i [p null])
     (c:->* (Type/c name-ref/c) ((listof PathElem?)) Filter/c)
     (if (or (type-equal? Univ t) (and (identifier? i) (is-var-mutated? i))) 
         -top
         (make-TypeFilter t p i)))

(d/c (-not-filter t i [p null])
     (c:->* (Type/c name-ref/c) ((listof PathElem?)) Filter/c)
     (if (or (type-equal? (make-Union null) t) (and (identifier? i) (is-var-mutated? i)))
         -top
         (make-NotTypeFilter t p i)))

(define (-filter-at t o)
  (match o
    [(Path: p i) (-filter t i p)]
    [_ -top]))
(define (-not-filter-at t o)
  (match o
    [(Path: p i) (-not-filter t i p)]
    [_ -top]))

(define (asym-pred dom rng filter)
  (make-Function (list (make-arr* (list dom) rng #:filters filter))))

(d/c make-pred-ty
  (case-> (c:-> Type/c Type/c)
          (c:-> (listof Type/c) Type/c Type/c Type/c)
          (c:-> (listof Type/c) Type/c Type/c integer? Type/c)
          (c:-> (listof Type/c) Type/c Type/c integer? (listof PathElem?) Type/c))
  (case-lambda 
    [(in out t n p)
     (define xs (for/list ([(_ i) (in-indexed (in-list in))]) i))
     (make-Function
      (list
       (make-arr* 
	in out 
	#:filters (-FS (-filter t (list-ref xs n) p) (-not-filter t (list-ref xs n) p)))))]
    [(in out t n)
     (make-pred-ty in out t n null)]
    [(in out t)
     (make-pred-ty in out t 0 null)]
    [(t) 
     (make-pred-ty (list Univ) -Boolean t 0 null)]))

(define true-filter (-FS -top -bot))
(define false-filter (-FS -bot -top))
(define true-lfilter (-FS -top -bot))
(define false-lfilter (-FS -bot -top))

(define (opt-fn args opt-args result)
  (apply cl->* (for/list ([i (in-range (add1 (length opt-args)))])                         
                 (make-Function (list (make-arr* (append args (take opt-args i)) result))))))

(define-syntax-rule (->opt args ... [opt ...] res)
  (opt-fn (list args ...) (list opt ...) res))
