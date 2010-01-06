#lang scheme/base

;; these are libraries providing functions we add types to that are not in scheme/base
(require
 "extra-procs.ss"
 "../utils/utils.ss"
 (only-in scheme/list cons? take drop add-between last filter-map)
 (only-in rnrs/lists-6 fold-left)
 '#%paramz
 (only-in scheme/match/runtime match:error)
 scheme/promise
 string-constants/string-constant
 (prefix-in ce: test-engine/scheme-tests)
 (for-syntax
  scheme/base syntax/parse
  (utils tc-utils)
  (env init-envs)          
  (except-in (rep filter-rep object-rep type-rep) make-arr)
  (types convenience union)
  (only-in (types convenience) [make-arr* make-arr])          
  (typecheck tc-structs)))


(define-for-syntax (initialize-others)
  (d-s srcloc
       ([source : Univ]
        [line : (*Un -Integer (-val #f))]
        [column : (*Un -Integer (-val #f))]
        [position : (*Un -Integer (-val #f))]
        [span : (*Un -Integer (-val #f))])
       ())
  (d-s date 
       ([second : -Number] [minute : -Number] [hour : -Number] [day : -Number] [month : -Number] 
        [year : -Number] [weekday : -Number] [year-day : -Number] [dst? : -Boolean] [time-zone-offset : -Number])
       ())
  (d-s exn ([message : -String] [continuation-marks : -Cont-Mark-Set]) ())
  (d-s (exn:fail exn) () (-String -Cont-Mark-Set))
  (d-s (exn:fail:read exn:fail) ([srclocs : (-lst Univ)]) (-String -Cont-Mark-Set))
  (d-s (exn:fail:read:eof exn:fail:read) () (-String -Cont-Mark-Set (-lst Univ)))
  )

(provide (for-syntax initial-env/special-case initialize-others initialize-type-env)
         define-initial-env)

(define-syntax (define-initial-env stx)
    (syntax-case stx ()
      [(_ initial-env make-promise-ty language-ty qq-append-ty cl ...)
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
                                    null)])
         #`(define-for-syntax initial-env
             (make-env
              [make-promise make-promise-ty]
              [language language-ty]
              [qq-append qq-append-ty]
              cl ...)))]))




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
  [(syntax-parse (local-expand #'(ce:test) 'expression null)
                 #:context #'ce:test
     [(_ ce-t:id) #'ce-t])
   (-> -Void)]
  
  [(syntax-parse (local-expand #'(ce:check-expect 1 1) 'module #f)
                 #:literals (let when define-values)
    [(define-values _ 
       (let ((_ _))
         (when _ 
           (insert-test _ (lambda () (check-values-expected _ _ _ _))))))
     #'insert-test])
   (Univ (-> Univ) . -> . -Void)]
  [(syntax-parse (local-expand #'(ce:check-expect 1 1) 'module #f)
                 #:literals (let when define-values)
                 ;#:literal-sets (kernel-literals)
    [(define-values _ 
       (let ((_ _))
         (when _ 
           (insert-test _ (lambda () (check-values-expected _ _ _ _))))))
     #'check-values-expected])
   ((-> Univ) Univ Univ Univ . -> . -Void)]
  [(syntax-parse (local-expand #'(ce:check-expect 1 1) 'module #f)
                 #:literals (let when define-values)
                 ;#:literal-sets (kernel-literals)
    [(define-values _ 
       (let ((_ (nvv _ _ builder _)))
         _))
     #'builder])
   (-> Univ)])
     
     
     

(begin-for-syntax   
  (initialize-type-env initial-env/special-case)
  (initialize-others))



