#lang scheme/base

(require typed-scheme/utils/utils
         (prefix-in ce: test-engine/scheme-tests)
         (for-syntax
          scheme/base syntax/parse
          (utils tc-utils)
          (env init-envs)          
          (except-in (rep filter-rep object-rep type-rep) make-arr)
          (types convenience union)
          (only-in (types convenience) [make-arr* make-arr])))

(define-for-syntax ce-env
  (make-env   
   ;; test*
   [(syntax-parse (local-expand #'(ce:test) 'expression null)
      #:context #'ce:test
      [(_ ce-t:id) #'ce-t])
    (-> -Void)]   
   ;; insert-test
   [(syntax-parse (local-expand #'(ce:check-expect 1 1) 'module #f)
      #:literals (let when define-values)
      [(define-values _ 
         (let ((_ _))
           (when _ 
             (insert-test _ (lambda () (check-values-expected _ _ _ _))))))
       #'insert-test])
    (Univ (-> Univ) . -> . -Void)]
   ;; check-values-expected
   [(syntax-parse (local-expand #'(ce:check-expect 1 1) 'module #f)
      #:literals (let when define-values)
      [(define-values _ 
         (let ((_ _))
           (when _ 
             (insert-test _ (lambda () (check-values-expected _ _ _ _))))))
       #'check-values-expected])
    ((-> Univ) Univ Univ Univ . -> . -Void)]
   ;; builder
   [(syntax-parse (local-expand #'(ce:check-expect 1 1) 'module #f)
      #:literals (let when define-values)
      [(define-values _ 
         (let ((_ (nvv _ _ builder _)))
           _))
       #'builder])
    (-> Univ)]))

(begin-for-syntax (initialize-type-env ce-env))