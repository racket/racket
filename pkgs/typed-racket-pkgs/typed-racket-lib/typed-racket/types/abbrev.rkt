#lang racket/base

;; This module provides abbreviations that are used to construct types
;; and data that show up in types. These are intended for internal use
;; within Typed Racket implementation code.

(require "../utils/utils.rkt"
         racket/list
         racket/match
         racket/function
         racket/undefined
         unstable/function

         (prefix-in c: (contract-req))
         (rename-in (rep type-rep filter-rep object-rep rep-utils)
                    [make-Base make-Base*])
         (types union numeric-tower)
         ;; Using this form so all-from-out works
         "base-abbrev.rkt" "match-expanders.rkt"

         (for-syntax racket/base syntax/parse)

         ;; for base type contracts and predicates
         ;; use '#%place to avoid the other dependencies of `racket/place`
         (for-template
           racket/base
           racket/contract/base
           racket/undefined
           (only-in racket/pretty pretty-print-style-table?)
           (only-in racket/udp udp?)
           (only-in racket/tcp tcp-listener?)
           (only-in racket/flonum flvector?)
           (only-in racket/extflonum extflvector?)
           (only-in racket/fixnum fxvector?)
           (only-in '#%place place? place-channel?))
         (only-in racket/pretty pretty-print-style-table?)
         (only-in racket/udp udp?)
         (only-in racket/tcp tcp-listener?)
         (only-in racket/flonum flvector?)
         (only-in racket/extflonum extflvector?)
         (only-in racket/fixnum fxvector?)
         (only-in '#%place place? place-channel?))

(provide (except-out (all-defined-out) make-Base)
         (all-from-out "base-abbrev.rkt" "match-expanders.rkt"))

;; All the types defined here are not numeric
(define (make-Base name contract predicate)
  (make-Base* name contract predicate #f))

;; Convenient constructors
(define -App make-App)
(define -mpair make-MPair)
(define (-Param t1 [t2 t1]) (make-Param t1 t2))
(define -box make-Box)
(define -channel make-Channel)
(define -async-channel make-Async-Channel)
(define -thread-cell make-ThreadCell)
(define -Promise make-Promise)
(define -set make-Set)
(define -vec make-Vector)
(define -future make-Future)
(define -evt make-Evt)

(define (-seq . args) (make-Sequence args))

(define (one-of/c . args)
  (apply Un (map -val args)))

(define (-opt t) (Un (-val #f) t))

(define (-ne-lst t) (-pair t (-lst t)))

;; Convenient constructor for Values
;; (wraps arg types with Result)
(define/cond-contract (-values args)
  (c:-> (c:listof Type/c) (c:or/c Type/c Values?))
  (match args
    ;[(list t) t]
    [_ (make-Values (for/list ([i (in-list args)]) (-result i)))]))

;; Convenient constructor for ValuesDots
;; (wraps arg types with Result)
(define/cond-contract (-values-dots args dty dbound)
  (c:-> (c:listof Type/c) Type/c (c:or/c symbol? c:natural-number/c)
        ValuesDots?)
  (make-ValuesDots (for/list ([i (in-list args)]) (-result i))
                   dty dbound))

;; Basic types
(define -Listof (-poly (list-elem) (make-Listof list-elem)))
(define/decl -Boolean (Un -False -True))
(define/decl -Undefined
  (make-Base 'Undefined
             #'(lambda (x) (eq? x undefined))
             (lambda (x) (eq? x undefined))))
(define/decl -Bytes (make-Base 'Bytes #'bytes? bytes?))
(define/decl -Base-Regexp (make-Base 'Base-Regexp
                           #'(and/c regexp? (not/c pregexp?))
                           (conjoin regexp? (negate pregexp?))))
(define/decl -PRegexp (make-Base 'PRegexp
				 #'pregexp?
				 pregexp?))
(define/decl -Regexp (Un -PRegexp -Base-Regexp))
(define/decl -Byte-Base-Regexp
  (make-Base 'Byte-Base-Regexp
	     #'(and/c byte-regexp? (not/c byte-pregexp?))
	     (conjoin byte-regexp? (negate byte-pregexp?))))
(define/decl -Byte-PRegexp
  (make-Base 'Byte-PRegexp #'byte-pregexp? byte-pregexp?))
(define/decl -Byte-Regexp (Un -Byte-Base-Regexp -Byte-PRegexp))
(define/decl -Pattern (Un -Bytes -Regexp -Byte-Regexp -String))
(define/decl -Keyword (make-Base 'Keyword #'keyword? keyword?))
(define/decl -Thread (make-Base 'Thread #'thread? thread?))
(define/decl -Module-Path
  (Un -Symbol -String
      (-lst* (-val 'quote) -Symbol)
      (-lst* (-val 'lib) -String)
      (-lst* (-val 'file) -String)
      (-pair (-val 'planet)
	     (Un (-lst* -Symbol)
		 (-lst* -String)
		 (-lst* -String
			(-lst*
			 -String -String
			 #:tail (make-Listof
				 (Un -Nat
				     (-lst* (Un -Nat (one-of/c '= '+ '-))
					    -Nat)))))))))
(define/decl -Resolved-Module-Path (make-Base 'Resolved-Module-Path #'resolved-module-path? resolved-module-path?))
(define/decl -Module-Path-Index (make-Base 'Module-Path-Index #'module-path-index? module-path-index?))
(define/decl -Compiled-Module-Expression (make-Base 'Compiled-Module-Expression #'compiled-module-expression? compiled-module-expression?))
(define/decl -Compiled-Non-Module-Expression
  (make-Base 'Compiled-Non-Module-Expression
             #'(and/c    compiled-expression? (not/c  compiled-module-expression?))
	     (conjoin  compiled-expression? (negate compiled-module-expression?))))
(define/decl -Compiled-Expression (Un -Compiled-Module-Expression -Compiled-Non-Module-Expression))
(define/decl -Cont-Mark-Set (make-Base 'Continuation-Mark-Set #'continuation-mark-set? continuation-mark-set?))
(define/decl -Path (make-Base 'Path #'path? path?))
(define/decl -OtherSystemPath
  (make-Base 'OtherSystemPath
	     #'(and/c path-for-some-system? (not/c path?))
	     (conjoin path-for-some-system? (negate path?))))
(define/decl -Namespace (make-Base 'Namespace #'namespace? namespace?))
(define/decl -Output-Port (make-Base 'Output-Port #'output-port? output-port?))
(define/decl -Input-Port (make-Base 'Input-Port #'input-port? input-port?))
(define/decl -TCP-Listener (make-Base 'TCP-Listener #'tcp-listener? tcp-listener?))
(define/decl -UDP-Socket (make-Base 'UDP-Socket #'udp? udp?))
(define/decl -FlVector (make-Base 'FlVector #'flvector? flvector?))
(define/decl -ExtFlVector (make-Base 'ExtFlVector #'extflvector? extflvector?))
(define/decl -FxVector (make-Base 'FxVector #'fxvector? fxvector?))
(define -Syntax make-Syntax)
(define/decl In-Syntax
  (-mu e
       (Un -Null -Boolean -Symbol -String -Keyword -Char -Number
           (make-Vector (-Syntax e))
           (make-Box (-Syntax e))
           (make-Listof (-Syntax e))
           (-pair (-Syntax e) (-Syntax e)))))
(define/decl Any-Syntax (-Syntax In-Syntax))
(define (-Sexpof t)
  (-mu sexp
       (Un -Null
           -Number -Boolean -Symbol -String -Keyword -Char
           (-pair sexp sexp)
           (make-Vector sexp)
           (make-Box sexp)
           t)))
(define/decl -Flat
  (-mu flat
       (Un -Null -Number -Boolean -Symbol -String -Keyword -Char
           (-pair flat flat))))
(define/decl -Sexp (-Sexpof (Un)))
(define Syntax-Sexp (-Sexpof Any-Syntax))
(define Ident (-Syntax -Symbol))
(define -HT make-Hashtable)
(define/decl -BoxTop (make-BoxTop))
(define/decl -ChannelTop (make-ChannelTop))
(define/decl -Async-ChannelTop (make-Async-ChannelTop))
(define/decl -HashTop (make-HashtableTop))
(define/decl -VectorTop (make-VectorTop))
(define/decl -MPairTop (make-MPairTop))
(define/decl -Thread-CellTop (make-ThreadCellTop))
(define/decl -Prompt-TagTop (make-Prompt-TagTop))
(define/decl -Continuation-Mark-KeyTop (make-Continuation-Mark-KeyTop))
(define/decl -Port (Un -Output-Port -Input-Port))
(define/decl -SomeSystemPath (Un -Path -OtherSystemPath))
(define/decl -Pathlike (Un -String -Path))
(define/decl -SomeSystemPathlike (Un -String -SomeSystemPath))
(define/decl -Pathlike* (Un -String -Path (-val 'up) (-val 'same)))
(define/decl -SomeSystemPathlike*
  (Un -String -SomeSystemPath(-val 'up) (-val 'same)))
(define/decl -PathConventionType (Un (-val 'unix) (-val 'windows)))
(define/decl -Pretty-Print-Style-Table
  (make-Base 'Pretty-Print-Style-Table #'pretty-print-style-table? pretty-print-style-table?))
(define/decl -Read-Table
  (make-Base 'Read-Table #'readtable? readtable?))
(define/decl -Special-Comment
  (make-Base 'Special-Comment #'special-comment? special-comment?))
(define/decl -Custodian (make-Base 'Custodian #'custodian? custodian?))
(define/decl -Parameterization (make-Base 'Parameterization #'parameterization? parameterization?))
(define/decl -Inspector (make-Base 'Inspector #'inspector inspector?))
(define/decl -Namespace-Anchor (make-Base 'Namespace-Anchor #'namespace-anchor? namespace-anchor?))
(define/decl -Variable-Reference (make-Base 'Variable-Reference #'variable-reference? variable-reference?))
(define/decl -Internal-Definition-Context
  (make-Base 'Internal-Definition-Context
	     #'internal-definition-context?
	     internal-definition-context?))
(define/decl -Subprocess
  (make-Base 'Subprocess #'subprocess? subprocess?))
(define/decl -Security-Guard
  (make-Base 'Security-Guard #'security-guard? security-guard?))
(define/decl -Thread-Group
  (make-Base 'Thread-Group #'thread-group? thread-group?))
(define/decl -Struct-Type-Property
  (make-Base 'Struct-Type-Property #'struct-type-property? struct-type-property?))
(define/decl -Impersonator-Property
  (make-Base 'Impersonator-Property #'impersonator-property? impersonator-property?))
(define/decl -Semaphore (make-Base 'Semaphore #'semaphore? semaphore?))
(define/decl -Bytes-Converter (make-Base 'Bytes-Converter #'bytes-converter? bytes-converter?))
(define/decl -Pseudo-Random-Generator
  (make-Base 'Pseudo-Random-Generator #'pseudo-random-generator? pseudo-random-generator?))
(define/decl -Logger (make-Base 'Logger #'logger? logger?))
(define/decl -Log-Receiver (make-Base 'LogReceiver #'log-receiver? log-receiver?))
(define/decl -Log-Level (one-of/c 'fatal 'error 'warning 'info 'debug))
(define/decl -Place (make-Base 'Place #'place? place?))
(define/decl -Base-Place-Channel
  (make-Base 'Base-Place-Channel #'(and/c place-channel? (not/c place?))  (conjoin place-channel? (negate place?))))
(define/decl -Place-Channel (Un -Place -Base-Place-Channel))
(define/decl -Will-Executor
  (make-Base 'Will-Executor #'will-executor? will-executor?))

;; Paths
(define/decl -car (make-CarPE))
(define/decl -cdr (make-CdrPE))
(define/decl -syntax-e (make-SyntaxPE))
(define/decl -force (make-ForcePE))

;; Structs
(define (-struct name parent flds [proc #f] [poly #f] [pred #'dummy])
  (make-Struct name parent flds proc poly pred))

;; Function type constructors
(define/decl top-func (make-Function (list)))

(define (asym-pred dom rng filter)
  (make-Function (list (make-arr* (list dom) rng #:filters filter))))

(define/cond-contract make-pred-ty
  (c:case-> (c:-> Type/c Type/c)
            (c:-> (c:listof Type/c) Type/c Type/c Type/c)
            (c:-> (c:listof Type/c) Type/c Type/c Object? Type/c))
  (case-lambda
    [(in out t p)
     (->* in out : (-FS (-filter t p) (-not-filter t p)))]
    [(in out t)
     (make-pred-ty in out t (make-Path null (list 0 0)))]
    [(t)
     (make-pred-ty (list Univ) -Boolean t (make-Path null (list 0 0)))]))

(define/decl -true-filter (-FS -top -bot))
(define/decl -false-filter (-FS -bot -top))

(define (opt-fn args opt-args result #:rest [rest #f] #:kws [kws null])
  (apply cl->* (for/list ([i (in-range (add1 (length opt-args)))])
                 (make-Function (list (make-arr* (append args (take opt-args i)) result
                                                 #:rest rest #:kws kws))))))

(define-syntax-rule (->opt args ... [opt ...] res)
  (opt-fn (list args ...) (list opt ...) res))

;; class utilities

(begin-for-syntax
 (define-syntax-class names+types
   #:attributes (data)
   (pattern [(name:id type) ...]
            #:with data #'(list (list (quote name) type) ...)))

 (define-syntax-class names+types+opt
   #:attributes (data no-opts)
   (pattern [(name:id type opt?) ...]
            #:with data #'(list (list (quote name) type opt?) ...)
            #:with no-opts #'(list (list (quote name) type) ...)))

 (define-splicing-syntax-class -class-clause
   #:attributes (inits fields methods augments)
   (pattern (~seq #:init sub-clauses:names+types+opt)
            #:with inits #'sub-clauses.data
            #:with fields #'null
            #:with methods #'null
            #:with augments #'null)
   (pattern (~seq #:init-field sub-clauses:names+types+opt)
            #:with inits #'sub-clauses.data
            #:with fields #'sub-clauses.no-opts
            #:with methods #'null
            #:with augments #'null)
   (pattern (~seq #:method sub-clauses:names+types)
            #:with inits #'null
            #:with fields #'null
            #:with methods #'sub-clauses.data
            #:with augments #'null)
   (pattern (~seq #:field sub-clauses:names+types)
            #:with inits #'null
            #:with fields #'sub-clauses.data
            #:with methods #'null
            #:with augments #'null)
   (pattern (~seq #:augment sub-clauses:names+types)
            #:with inits #'null
            #:with fields #'null
            #:with methods #'null
            #:with augments #'sub-clauses.data)))

(define-syntax (-class stx)
  (syntax-parse stx
    [(_ (~or (~optional (~seq #:row var:expr)
                        #:defaults ([var #'#f]))
             ?clause:-class-clause) ...)
     #'(make-Class
        var
        (append ?clause.inits ...)
        (append ?clause.fields ...)
        (append ?clause.methods ...)
        (append ?clause.augments ...)
        #f)]))

(define-syntax-rule (-object . ?clauses)
  (make-Instance (-class . ?clauses)))

