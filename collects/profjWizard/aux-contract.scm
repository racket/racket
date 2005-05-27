(module aux-contract mzscheme
  
  (require-for-syntax (file "aux-syntax.scm"))
  (require (lib "contract.ss"))
  
  (provide 
   define-as-contract ;; <definition>
   )
  
  ;; (define-as-contract string (id x ...) body ...)
  ;; introduces Id for export as a contract and 
  ;; is-id? for local testing as a function
  (define-syntax (define-as-contract stx)
    (syntax-case stx ()
      [(_ message (name . args) . body)
       (with-syntax ([is-name (prefix-id-suffix "is-" (syntax name) "?")]
                     [ct-name (cap-id (syntax name))])
         (syntax
          (begin 
            (define (is-name . args) . body)
            (define ct-name (flat-named-contract message is-name)))))]))
  
  )
