#lang racket/base

(require
 "../utils/utils.rkt"
 (rep type-rep)
 (types abbrev numeric-tower union)
 (typecheck tc-structs)
 ;;For tests
 (prefix-in k: '#%kernel))

(require (for-template racket/base (prefix-in k: '#%kernel)))

(provide initialize-structs -Date -Srcloc -Date -Arity-At-Least -Exn)

(define-syntax define-hierarchy
  (syntax-rules (define-hierarchy)
    [(_ parent (opts ...) ([name : type] ...)
        (define-hierarchy child (child-opts ...) (spec ...)  grand ...)
        ...)
     (begin
       (d-s parent ([name : type] ...) opts ...)
       (define-sub-hierarchy [child parent] (type ...) (child-opts ...) (spec ...)  grand ...)
       ...)]))

(define-syntax define-sub-hierarchy
  (syntax-rules (define-hierarchy)
    [(_ [child parent] (inheritance ...) (opts ...) ([name : type] ...)
        (define-hierarchy grandchild (grandchild-opts ...)(spec ...) great ...)
        ...)
     (begin
       (d-s [child parent] ([name : type] ...) (inheritance ...) opts ...)
       (define-sub-hierarchy [grandchild child]
         (inheritance ... type ...) (grandchild-opts ...) (spec ...)
         great
         ...)
       ...)]))

(define -Srcloc (make-Name #'srcloc null #f #t))
(define -Date (make-Name #'date null #f #t))
(define -Arity-At-Least
  (make-Name #'arity-at-least null #f #t))
(define -Exn (make-Name #'exn null #f #t))


(define (initialize-structs)


  (define-hierarchy srcloc (#:kernel-maker k:srcloc)
    ([source : Univ]
     [line : (Un -Integer (-val #f))]
     [column : (Un -Integer (-val #f))]
     [position : (Un -Integer (-val #f))]
     [span : (Un -Integer (-val #f))]))

  (define-hierarchy date (#:kernel-maker k:date)
    ([second : -Nat]
     [minute : -Nat]
     [hour : -Nat]
     [day : -Nat]
     [month : -Nat]
     [year : -Nat]
     [week-day : -Nat]
     [year-day : -Nat]
     [dst? : -Boolean]
     [time-zone-offset : -Integer]))

  (define-hierarchy arity-at-least (#:kernel-maker k:arity-at-least)
    ([value : -Nat]))

  (define-hierarchy exn (#:kernel-maker k:exn)
    ([message : -String] [continuation-marks : -Cont-Mark-Set])

    (define-hierarchy exn:break (#:kernel-maker k:exn:break)
     ([continuation : top-func]))

    (define-hierarchy exn:fail (#:kernel-maker k:exn:fail) ()

      (define-hierarchy exn:fail:contract (#:kernel-maker k:exn:fail:contract) ()
        (define-hierarchy exn:fail:contract:arity (#:kernel-maker k:exn:fail:contract:arity) ())
        (define-hierarchy exn:fail:contract:divide-by-zero (#:kernel-maker k:exn:fail:contract:divide-by-zero) ())
        (define-hierarchy exn:fail:contract:non-fixnum-result (#:kernel-maker k:exn:fail:contract:non-fixnum-result) ())
        (define-hierarchy exn:fail:contract:continuation (#:kernel-maker k:exn:fail:contract:continuation) ())
        (define-hierarchy exn:fail:contract:variable (#:kernel-maker k:exn:fail:contract:variable) ()))

      (define-hierarchy exn:fail:syntax (#:kernel-maker k:exn:fail:syntax) ([exprs : (-lst Any-Syntax)]))

      (define-hierarchy exn:fail:read (#:kernel-maker k:exn:fail:read)
        ([srclocs : (-lst Univ)]) ;; cce: Univ here should be srcloc
        (define-hierarchy exn:fail:read:eof (#:kernel-maker k:exn:fail:read:eof) ())
        (define-hierarchy exn:fail:read:non-char (#:kernel-maker k:exn:fail:read:non-char) ()))

      (define-hierarchy exn:fail:filesystem (#:kernel-maker k:exn:fail:filesystem) ()
        (define-hierarchy exn:fail:filesystem:exists (#:kernel-maker k:exn:fail:filesystem:exists) ())
        (define-hierarchy exn:fail:filesystem:version (#:kernel-maker k:exn:fail:filesystem:version) ()))

      (define-hierarchy exn:fail:network (#:kernel-maker k:exn:fail:network) ())

      (define-hierarchy exn:fail:out-of-memory (#:kernel-maker k:exn:fail:out-of-memory) ())

      (define-hierarchy exn:fail:unsupported (#:kernel-maker k:exn:fail:unsupported) ())

      (define-hierarchy exn:fail:user (#:kernel-maker k:exn:fail:user) ())))

  (void))
