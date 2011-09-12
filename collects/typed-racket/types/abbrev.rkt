#lang racket/base

(require "../utils/utils.rkt")

(require (rename-in (rep type-rep object-rep filter-rep rep-utils) [make-Base make-Base*])
         "resolve.rkt"
         (utils tc-utils)
         racket/list
         racket/match
         racket/function
         racket/pretty
         racket/place
         unstable/function
         racket/udp
         (except-in racket/contract/base ->* ->)
         (prefix-in c: racket/contract/base)
         (for-syntax racket/base syntax/parse)
	 (for-template racket/base racket/contract/base racket/promise racket/tcp racket/flonum)
         racket/pretty racket/udp racket/place
         ;; for base type predicates
         racket/promise racket/tcp racket/flonum)

(provide (except-out (all-defined-out) Promise make-Base)
         (rename-out [make-Listof -lst]
                     [make-MListof -mlst]))

;; all the types defined here are not numeric
(define (make-Base name contract predicate marshaled)
  (make-Base* name contract predicate marshaled #f))

;; convenient constructors


(define -App make-App)
(define -pair make-Pair)
(define -mpair make-MPair)
(define -val make-Value)
(define -Param make-Param)
(define -box make-Box)
(define -channel make-Channel)
(define -thread-cell make-ThreadCell)
(define -set make-Set)
(define -vec make-Vector)
(define -future make-Future)
(define (-seq . args) (make-Sequence args))


(define (flat t)
  (match t
    [(Union: es) es]
    [(Values: (list (Result: (Union: es) _ _))) es]
    [(Values: (list (Result: t _ _))) (list t)]
    [_ (list t)]))

;; Simple union constructor.
;; Flattens nested unions and sorts types, but does not check for
;; overlapping subtypes.
(define-syntax *Un
  (syntax-rules ()
    [(_ . args) (make-Union (remove-dups (sort (apply append (map flat (list . args))) type<?)))]))


(define (make-Listof elem) (-mu list-rec (*Un (-val null) (-pair elem list-rec))))
(define (make-MListof elem) (-mu mlist-rec (*Un (-val null) (-mpair elem mlist-rec))))

(define (-lst* #:tail [tail (-val null)] . args)
  (for/fold ([tl tail]) ([a (reverse args)]) (-pair a tl)))

(define (-Tuple l)
  (foldr -pair (-val '()) l))

(define (-Tuple* l b)
  (foldr -pair b l))

(define (untuple t)
  ;; FIXME - do we really need resolution here?
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


(define/cond-contract (-result t [f -no-filter] [o -no-obj])
  (c:->* (Type/c) (FilterSet? Object?) Result?)
  (make-Result t f o))

(define/cond-contract (-values args)
     (c:-> (listof Type/c) (or/c Type/c Values?))
     (match args
       ;[(list t) t]
       [_ (make-Values (for/list ([i args]) (-result i)))]))

;; basic types


(define Promise #f)
(define promise-id #'Promise)
(define make-promise-ty
 (lambda (t)
   (make-Struct promise-id #f (list (make-fld t #'values #f)) #f #f #'promise? values #'values)))

(define -Listof (-poly (list-elem) (make-Listof list-elem)))

(define -Boolean (make-Base 'Boolean #'boolean? boolean? #'-Boolean))
(define -Symbol (make-Base 'Symbol #'symbol? symbol? #'-Symbol))
(define -Void (make-Base 'Void #'void? void? #'-Void))
(define -Undefined
  (make-Base 'Undefined
             #'(lambda (x) (equal? (letrec ([y y]) y) x)) ; initial value of letrec bindings
             (lambda (x) (equal? (letrec ([y y]) y) x))
             #'-Undefined))
(define -Bytes (make-Base 'Bytes #'bytes? bytes? #'-Bytes))
(define -String (make-Base 'String #'string? string? #'-String))


(define -Base-Regexp (make-Base 'Base-Regexp
                           #'(and/c regexp? (not/c pregexp?))
                           (conjoin regexp? (negate pregexp?))
                           #'-Regexp))
(define -PRegexp (make-Base 'PRegexp
                            #'pregexp?
                            pregexp?
                            #'-PRegexp))
(define -Regexp (*Un -PRegexp -Base-Regexp))

(define -Byte-Base-Regexp (make-Base 'Byte-Regexp
                                #'(and/c byte-regexp? (not/c byte-pregexp?))
                                (conjoin byte-regexp? (negate byte-pregexp?))
                                #'-Byte-Regexp))
(define -Byte-PRegexp (make-Base 'Byte-PRegexp #'byte-pregexp? byte-pregexp? #'-Byte-PRegexp))
(define -Byte-Regexp (*Un -Byte-Base-Regexp -Byte-PRegexp))

(define -Pattern (*Un -Bytes -Regexp -Byte-Regexp -String))








(define -Keyword (make-Base 'Keyword #'keyword? keyword? #'-Keyword))
(define -Char (make-Base 'Char #'char? char? #'-Char))
(define -Thread (make-Base 'Thread #'thread? thread? #'-Thread))
(define -Resolved-Module-Path (make-Base 'Resolved-Module-Path #'resolved-module-path? resolved-module-path? #'-Resolved-Module-Path))
(define -Module-Path-Index (make-Base 'Module-Path-Index #'module-path-index? module-path-index? #'-Module-Path-Index))
(define -Compiled-Module-Expression (make-Base 'Compiled-Module-Expression #'compiled-module-expression? compiled-module-expression? #'-Compiled-Module-Expression))
(define -Compiled-Non-Module-Expression
  (make-Base 'Compiled-Non-Module-Expression
             #'(and/c    compiled-expression? (not/c  compiled-module-expression?))
               (conjoin  compiled-expression? (negate compiled-module-expression?))
             #'-Compiled-Non-Module-Expression))
(define -Compiled-Expression (*Un -Compiled-Module-Expression -Compiled-Non-Module-Expression))
(define -Prompt-Tag (make-Base 'Prompt-Tag #'continuation-prompt-tag? continuation-prompt-tag? #'-Prompt-Tag))
(define -Cont-Mark-Set (make-Base 'Continuation-Mark-Set #'continuation-mark-set? continuation-mark-set? #'-Cont-Mark-Set))
(define -Path (make-Base 'Path #'path? path? #'-Path))
(define -OtherSystemPath (make-Base 'OtherSystemPath
                           #'(and/c path-for-some-system? (not/c path?))
                             (conjoin path-for-some-system? (negate path?))
                             #'-OtherSystemPath))
(define -Namespace (make-Base 'Namespace #'namespace? namespace? #'-Namespace))
(define -Output-Port (make-Base 'Output-Port #'output-port? output-port? #'-Output-Port))
(define -Input-Port (make-Base 'Input-Port #'input-port? input-port? #'-Input-Port))
(define -TCP-Listener (make-Base 'TCP-Listener #'tcp-listener? tcp-listener? #'-TCP-Listener))
(define -UDP-Socket (make-Base 'UDP-Socket #'udp? udp? #'-UDP-Socket))

(define -FlVector (make-Base 'FlVector #'flvector? flvector? #'-FlVector))

(define -Syntax make-Syntax)
(define -HT make-Hashtable)
(define -Promise make-promise-ty)

(define -HashTop (make-HashtableTop))

(define Univ (make-Univ))
(define Err (make-Error))

;A Type that corresponds to the any contract for the
;return type of functions
;FIXME
;This is not correct as Univ is only a single value.
(define ManyUniv Univ)

(define -Port (*Un -Output-Port -Input-Port))

(define -SomeSystemPath (*Un -Path -OtherSystemPath))
(define -Pathlike (*Un -String -Path))
(define -SomeSystemPathlike (*Un -String -SomeSystemPath))
(define -Pathlike* (*Un -String -Path (-val 'up) (-val 'same)))
(define -SomeSystemPathlike* (*Un -String -SomeSystemPath(-val 'up) (-val 'same)))
(define -PathConventionType (*Un (-val 'unix) (-val 'windows)))



(define -Pretty-Print-Style-Table
  (make-Base 'Pretty-Print-Style-Table #'pretty-print-style-table? pretty-print-style-table? #'-Pretty-Print-Style-Table))


(define -Read-Table (make-Base 'Read-Table #'readtable? readtable? #'-Read-Table))

(define -Special-Comment
  (make-Base 'Special-Comment #'special-comment? special-comment? #'-Special-Comment))

(define -Custodian (make-Base 'Custodian #'custodian? custodian? #'Custodian))

(define -Parameterization (make-Base 'Parameterization #'parameterization? parameterization? #'-Parameterization))


(define -Inspector (make-Base 'Inspector #'inspector inspector? #'-Inspector))

(define -Namespace-Anchor (make-Base 'Namespace-Anchor #'namespace-anchor? namespace-anchor? #'-Namespace-Anchor))

(define -Variable-Reference (make-Base 'Variable-Reference #'variable-reference? variable-reference? #'-Variable-Reference))


(define -Internal-Definition-Context (make-Base 'Internal-Definition-Context
                                      #'internal-definition-context?
                                      internal-definition-context?
                                      #'-Internal-Definition-Context))

(define -Subprocess
  (make-Base 'Subprocess #'subprocess? subprocess? #'-Subprocess))
(define -Security-Guard
  (make-Base 'Security-Guard #'security-guard? security-guard? #'-Security-Guard))
(define -Thread-Group
  (make-Base 'Thread-Group #'thread-group? thread-group? #'-Thread-Group))
(define -Struct-Type-Property
  (make-Base 'Struct-Type-Property #'struct-type-property? struct-type-property? #'Struct-Type-Property))
(define -Impersonator-Property
  (make-Base 'Impersonator-Property #'impersonator-property? impersonator-property? #'-Impersonator-Property))




(define -Semaphore (make-Base 'Semaphore #'semaphore? semaphore? #'-Semaphore))
(define -Bytes-Converter (make-Base 'Bytes-Converter #'bytes-converter? bytes-converter? #'-Bytes-Converter))
(define -Pseudo-Random-Generator
  (make-Base 'Pseudo-Random-Generator #'pseudo-random-generator? pseudo-random-generator? #'-Pseudo-Random-Generator))


(define -Logger (make-Base 'Logger #'logger? logger? #'-Logger))
(define -Log-Receiver (make-Base 'LogReceiver #'log-receiver? log-receiver? #'-Log-Receiver))


(define -Place
  (make-Base 'Place #'place? place? #'-Place))
(define -Base-Place-Channel
  (make-Base 'Base-Place-Channel #'(and/c place-channel? (not/c place?))  (conjoin place-channel? (negate place?))  #'-Base-Place-Channel))

(define -Place-Channel (*Un -Place -Base-Place-Channel))

(define -Will-Executor
  (make-Base 'Will-Executor #'will-executor? will-executor? #'-Will-Executor))




(define -top (make-Top))
(define -bot (make-Bot))
(define -no-filter (make-FilterSet -top -top))
(define -no-obj (make-Empty))


(define/cond-contract (-FS + -)
      (c:-> Filter/c Filter/c FilterSet?)
      (match* (+ -)
             [((Bot:) _) (make-FilterSet -bot -top)]
             [(_ (Bot:)) (make-FilterSet -top -bot)]
             [(+ -) (make-FilterSet + -)]))

(define -car (make-CarPE))
(define -cdr (make-CdrPE))
(define -syntax-e (make-SyntaxPE))


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

(define/cond-contract (make-arr* dom rng
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

(define/cond-contract (-filter t i [p null])
     (c:->* (Type/c name-ref/c) ((listof PathElem?)) Filter/c)
     (if (or (type-equal? Univ t) (and (identifier? i) (is-var-mutated? i)))
         -top
         (make-TypeFilter t p i)))

(define/cond-contract (-not-filter t i [p null])
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

(define/cond-contract make-pred-ty
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
