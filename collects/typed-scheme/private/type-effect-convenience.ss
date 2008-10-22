#lang scheme/base  
(require "../utils/utils.ss")

(require (rep type-rep effect-rep)
         (utils tc-utils)
         scheme/match
         "type-comparison.ss"
         "type-effect-printer.ss"
         "union.ss"
         "subtype.ss"
         "type-utils.ss" 
         scheme/promise
         (for-syntax macro-debugger/stxclass/stxclass)
         (for-syntax scheme/base))

(provide (all-defined-out) 
         ;; these should all eventually go away
         make-Name make-ValuesDots make-Function make-top-arr make-Latent-Restrict-Effect make-Latent-Remove-Effect)

(define (one-of/c . args)
  (apply Un (map -val args)))

(define (-vet id) (make-Var-True-Effect id))
(define (-vef id) (make-Var-False-Effect id))

(define -rem make-Remove-Effect)
(define -rest make-Restrict-Effect)

(define (var->type-eff eff)
  (match eff
    [(Var-True-Effect: v) (make-Remove-Effect (make-Value #f) v)]
    [(Var-False-Effect: v) (make-Restrict-Effect (make-Value #f) v)]
    [_ eff]))

(define ((add-var v) eff)
  (match eff
    [(Latent-Var-True-Effect:) (-vet v)]
    [(Latent-Var-False-Effect:) (-vef v)]
    [(Latent-Restrict-Effect: t) (make-Restrict-Effect t v)]
    [(Latent-Remove-Effect: t) (make-Remove-Effect t v)]
    [(True-Effect:) eff]
    [(False-Effect:) eff]
    [_ (int-err "can't add var ~a to effect ~a" v eff)]))

(define-syntax (-> stx)
  (syntax-case* stx (:) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
    [(_ dom ... rng : eff1 eff2)
     #'(->* (list dom ...) rng : eff1 eff2)]
    [(_ dom ... rng : eff1 eff2)
     #'(->* (list dom ...) rng : eff1 eff2)]
    [(_ dom ... rng)
     #'(->* (list dom ...) rng)]))

(define-syntax ->*
  (syntax-rules (:)
    [(_ dom rng)       
     (make-Function (list (make-arr* dom rng)))]
    [(_ dom rst rng)
     (make-Function (list (make-arr* dom rng rst)))]
    [(_ dom rng : eff1 eff2)
     (make-Function (list (make-arr* dom rng #f eff1 eff2)))]
    [(_ dom rst rng : eff1 eff2)
     (make-Function (list (make-arr* dom rng rst eff1 eff2)))]))
(define-syntax ->...
  (syntax-rules (:)
    [(_ dom rng)
     (->* dom rng)]
    [(_ dom (dty dbound) rng)
     (make-Function (list (make-arr* dom rng #f (cons dty 'dbound) (list) (list))))]
    [(_ dom rng : eff1 eff2)
     (->* dom rng : eff1 eff2)]
    [(_ dom (dty dbound) rng : eff1 eff2)
     (make-Function (list (make-arr* dom rng #f (cons dty 'dbound) eff1 eff2)))]))
(define-syntax cl->
  (syntax-rules (:)
    [(_ [(dom ...) rng] ...)
     (make-Function (list (make-arr* (list dom ...) rng) ...))]
    [(_ [(dom ...) rng : eff1 eff2] ...)
     (make-Function (list (make-arr* (list dom ...) rng #f eff1 eff2) ...))]
    [(_ [(dom ...) rng rst : eff1 eff2] ...)
     (make-Function (list (make-arr* (list dom ...) rng rst eff1 eff2) ...))]))
(define (cl->* . args)
  (define (funty-arities f)
    (match f
      [(Function: as) as]))
  (make-Function (map car (map funty-arities args))))

(define-syntax (->key stx)
  (syntax-parse stx
                [(_ ty:expr ... ((k:keyword kty:expr opt:boolean)) ...* rng)
                 #'(make-Function
                    (list
                     (make-arr* (list ty ...)
                                rng
                                #f
                                #f
                                (list (make-Keyword 'k kty opt) ...)
                                null
                                null)))]))

(define make-arr*
  (case-lambda [(dom rng) (make-arr dom rng #f #f null (list) (list))]
               [(dom rng rest) (make-arr dom rng rest #f null (list) (list))]
               [(dom rng rest eff1 eff2) (make-arr dom rng rest #f null eff1 eff2)]
               [(dom rng rest drest eff1 eff2) (make-arr dom rng rest drest null eff1 eff2)]
               [(dom rng rest drest kws eff1 eff2)
                (make-arr dom rng rest drest (sort #:key Keyword-kw kws keyword<?) eff1 eff2)]))

(define (make-arr-dots dom rng dty dbound)
  (make-arr* dom rng #f (cons dty dbound) null null))

(define make-promise-ty
  (let ([s (string->uninterned-symbol "Promise")])
    (lambda (t)
      (make-Struct s #f (list t) #f #f #'promise? values))))

(define N (make-Base 'Number))
(define -Integer (make-Base 'Integer))
(define B (make-Base 'Boolean))
(define Sym (make-Base 'Symbol))
(define -Void (make-Base 'Void))
(define -Bytes (make-Base 'Bytes))
(define -Regexp (make-Base 'Regexp))
(define -PRegexp (make-Base 'PRegexp))
(define -Byte-Regexp (make-Base 'Byte-Regexp))
(define -Byte-PRegexp (make-Base 'Byte-PRegexp))
(define -String (make-Base 'String))
(define -Keyword (make-Base 'Keyword))
(define -Char (make-Base 'Char))
(define -Syntax make-Syntax)
(define -Prompt-Tag (make-Base 'Prompt-Tag))
(define -Cont-Mark-Set (make-Base 'Continuation-Mark-Set))
(define -Path (make-Base 'Path))
(define -Namespace (make-Base 'Namespace))
(define -Output-Port (make-Base 'Output-Port))
(define -Input-Port (make-Base 'Input-Port))  

(define -HT make-Hashtable)
(define -Promise make-promise-ty)

(define Univ (make-Univ))
(define Err (make-Error))

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


(define -values make-Values)

(define-syntax *Un
  (syntax-rules ()
    [(_ . args) (make-Union (list . args))]))


(define -pair make-Pair)
(define -base make-Base)

(define -struct make-Struct)
(define -val make-Value)

(define (make-Listof elem) (-mu list-rec (*Un (-val null) (-pair elem list-rec))))
(define -Listof (-poly (list-elem) (make-Listof list-elem)))

(define -lst make-Listof)
(define -Sexp (-mu x (*Un Sym N B -String (-val null) (-pair x x))))
(define -Port (*Un -Input-Port -Output-Port))

(define (-lst* #:tail [tail (-val null)] . args)
  (if (null? args)
      tail
      (-pair (car args) (apply -lst* #:tail tail (cdr args)))))


#;(define NE (-mu x (Un N (make-Listof x))))
(define -NE (-mu x (*Un N (-pair x (-pair Sym (-pair x (-val null)))))))

(define (Un/eff . args)
  (apply Un (map tc-result-t args)))

(define -Param make-Param)

(define make-pred-ty
  (case-lambda 
    [(in out t)
     (->* in out : (list (make-Latent-Restrict-Effect t)) (list (make-Latent-Remove-Effect t)))]
    [(t) (make-pred-ty (list Univ) B t)]))

(define -Pathlike (*Un -Path -String))
(define -Pathlike* (*Un (-val 'up) (-val 'same) -Path -String))
(define -Pattern (*Un -String -Bytes -Regexp -Byte-Regexp -PRegexp -Byte-PRegexp))
(define -Byte N)

(define (-Tuple l)
  (foldr -pair (-val '()) l))
(define -box make-Box)
(define -vec make-Vector)

(define Any-Syntax 
  (-mu x
       (-Syntax (*Un 
                 (-lst x)                       
                 (-mu y (*Un x (-pair x y)))
                 (make-Vector x)
                 (make-Box x)
                 N
                 B
                 -String
                 Sym))))

(define Ident (-Syntax Sym))

;; DO NOT USE if t contains #f
(define (-opt t) (*Un (-val #f) t))

(define-syntax (make-env stx)
  (syntax-case stx ()
    [(_ e ...)
     #`(list
        #,@(map (lambda (e)
                  (syntax-case e ()
                    [(nm ty)
                     (identifier? #'nm)
                     #`(list  #'nm ty)]
                    [(e ty extra-mods ...)
                     #'(let ([x (list (let ([new-ns
                                             (let* ([ns (make-empty-namespace)])
                                               (namespace-attach-module (current-namespace)
                                                                        'scheme/base
                                                                        ns)
                                               ns)])
                                        (parameterize ([current-namespace new-ns])
                                          (namespace-require 'scheme/base)
                                          (namespace-require 'extra-mods) ...
                                          e))
                                      ty)])
                         ;(display x) (newline)
                         x)]))
                (syntax->list #'(e ...))))]))

;; if t is of the form (Pair t* (Pair t* ... (Listof t*)))
;; return t*
;; otherwise, return t
;; generalize : Type -> Type
(define (generalize t)
  (let/ec exit
    (let loop ([t* t])
      (match t*
        [(Value: '()) (-lst Univ)]
        [(Mu: var (Union: (list (Value: '()) (Pair: _ (F: var))))) t*]
        [(Pair: t1 t2)
         (let ([t-new (loop t2)])
           (if (type-equal? (-lst t1) t-new)
               t-new
               (exit t)))]
        [_ (exit t)]))))



