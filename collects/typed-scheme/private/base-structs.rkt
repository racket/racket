#lang racket/base

(require 
 "../utils/utils.rkt"
 (utils tc-utils)
 (env init-envs)          
 (except-in (rep filter-rep object-rep type-rep) make-arr)
 (types convenience union)
 (only-in (types convenience) [make-arr* make-arr])          
 (typecheck tc-structs))

(require (for-template racket/base))

(provide initialize-structs)

(define-syntax define-hierarchy
  (syntax-rules (define-hierarchy)
    [(_ parent ([name : type] ...)
        (define-hierarchy child (spec ...) grand ...)
        ...)
     (begin
       (d-s parent ([name : type] ...))
       (define-sub-hierarchy [child parent] (type ...) (spec ...) grand ...)
       ...)]))

(define-syntax define-sub-hierarchy
  (syntax-rules (define-hierarchy)
    [(_ [child parent] (inheritance ...) ([name : type] ...)
        (define-hierarchy grandchild (spec ...) great ...)
        ...)
     (begin
       (d-s [child parent] ([name : type] ...) (inheritance ...))
       (define-sub-hierarchy [grandchild child]
         (inheritance ... type ...) (spec ...)
         great
         ...)
       ...)]))


(define (initialize-structs)
  

  (define-hierarchy srcloc
    ([source : Univ]
     [line : (*Un -Integer (-val #f))]
     [column : (*Un -Integer (-val #f))]
     [position : (*Un -Integer (-val #f))]
     [span : (*Un -Integer (-val #f))]))

  (define-hierarchy date 
    ([second : -Number]
     [minute : -Number]
     [hour : -Number]
     [day : -Number]
     [month : -Number] 
     [year : -Number]
     [weekday : -Number]
     [year-day : -Number]
     [dst? : -Boolean]
     [time-zone-offset : -Number]))
  
  (define-hierarchy arity-at-least
    ([value : -Nat]))

  (define-hierarchy exn
    ([message : -String] [continuation-marks : -Cont-Mark-Set])
    
    (define-hierarchy exn:break ([continuation : top-func]))

    (define-hierarchy exn:fail ()

      (define-hierarchy exn:fail:contract ()
        (define-hierarchy exn:fail:contract:arity ())
        (define-hierarchy exn:fail:contract:divide-by-zero ())
        (define-hierarchy exn:fail:contract:non-fixnum-result ())
        (define-hierarchy exn:fail:contract:continuation ())
        (define-hierarchy exn:fail:contract:variable ()))

      (define-hierarchy exn:fail:syntax ([exprs : (-lst (-Syntax Univ))]))

      (define-hierarchy exn:fail:read
        ([srclocs : (-lst Univ)]) ;; cce: Univ here should be srcloc
        (define-hierarchy exn:fail:read:eof ())
        (define-hierarchy exn:fail:read:non-char ()))

      (define-hierarchy exn:fail:filesystem ()
        (define-hierarchy exn:fail:filesystem:exists ())
        (define-hierarchy exn:fail:filesystem:version ()))

      (define-hierarchy exn:fail:network ())

      (define-hierarchy exn:fail:out-of-memory ())

      (define-hierarchy exn:fail:unsupported ())

      (define-hierarchy exn:fail:user ())))

  ;; cce: adding exn:break would require a generic type for continuations
  )