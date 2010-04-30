#lang scheme/base

;; these are libraries providing functions we add types to that are not in scheme/base
(require
 "extra-procs.rkt"
 "../utils/utils.rkt"
 (only-in scheme/list cons? take drop add-between last filter-map)
 (only-in rnrs/lists-6 fold-left)
 '#%paramz
 (only-in racket/match/runtime match:error)
 scheme/promise
 string-constants/string-constant
 ;(prefix-in ce: test-engine/scheme-tests)
 (for-syntax
  scheme/base syntax/parse
  (only-in unstable/syntax syntax-local-eval)
  (utils tc-utils)
  (env init-envs)          
  (except-in (rep filter-rep object-rep type-rep) make-arr)
  (types convenience union)
  (only-in (types convenience) [make-arr* make-arr])          
  (typecheck tc-structs))
 (for-meta 2 scheme/base syntax/parse))


(define-for-syntax (initialize-others)

  (define-syntax define-hierarchy
    (syntax-rules (define-hierarchy)
      [(_ parent ([name : type] ...)
          (define-hierarchy child (spec ...) grand ...)
          ...)
       (begin
         (d-s parent ([name : type] ...) ())
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

  (define-hierarchy exn
    ([message : -String] [continuation-marks : -Cont-Mark-Set])

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

(provide (for-syntax initial-env/special-case initialize-others initialize-type-env)
         define-initial-env)

(define-syntax (define-initial-env stx)
    (syntax-case stx ()
      [(_ initial-env make-promise-ty language-ty qq-append-ty
          [id-expr ty] ...)
       (with-syntax ([(_ make-promise . _)
                      (local-expand #'(delay 3)
                                    'expression
                                    null)]
                     [language
                      (local-expand #'(this-language)
                                    'expression
                                    null)]
                     [(_ qq-append . _)
                      (local-expand #'`(,@'() 1)
                                    'expression
                                    null)]
                     [(id ...)
                      (for/list ([expr (syntax->list #'(id-expr ...))])
                        (syntax-local-eval expr))])
         #`(define-for-syntax initial-env
             (make-env
              [make-promise make-promise-ty]
              [language language-ty]
              [qq-append qq-append-ty]
              [id ty] ...)))]))




(define-initial-env initial-env/special-case
  ;; make-promise
  (-poly (a) (-> (-> a) (-Promise a)))
  ;; language
  -Symbol
  ;; qq-append
  (-poly (a b) 
         (cl->*
          (-> (-lst a) (-val '()) (-lst a))
          (-> (-lst a) (-lst b) (-lst (*Un a b)))))
  ;; make-sequence
  [(syntax-parse (local-expand #'(for ([x '()]) x) 'expression #f)
     #:context #'make-sequence
     #:literals (let-values quote)
     [(let-values ([_ (m-s '(_) '())]) . _)
      #'m-s])
   (-poly (a) 
          (let ([seq-vals 
                 (lambda ([a a])
                   (-values (list 
                             (-> Univ a)
                             (-> Univ Univ)
                             Univ
                             (-> Univ Univ)
                             (-> a Univ)
                             (-> Univ a Univ))))])
            (cl->* (-> Univ (-lst a) (seq-vals))
                   (-> Univ (-vec a) (seq-vals))
                   (-> Univ -String (seq-vals -Char))
                   (-> Univ -Bytes (seq-vals -Nat))
                   (-> Univ -Input-Port (seq-vals -Nat)))))])




(begin-for-syntax   
  (initialize-type-env initial-env/special-case)
  (initialize-others))



