(module contracts mzscheme
  
  (provide beginner-contract intermediate-contract advanced-contract)
  
  (require "contracts-helpers.ss")

  (require-for-syntax "contract-transformers.ss")
  
  ;; contract function <contract> code)
  ;; <contract> = flat-contract || arrow-contract, in syntax understood by contract-transformers.scm
  ;; wraps the value code with a contract enforcer function for the given contract
  
  (define-syntaxes (beginner-contract intermediate-contract advanced-contract)
    (let ()
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; CONTRACT MACRO
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ; contract-template: (syntax -> syntax) -> (syntax -> syntax)
      ; returns a contract parser. uses the translator given as an argument
      (define contract-template
        (lambda (translator)
          (lambda (stx)
            (syntax-case stx (-> quote)
              ((_ 'name-to-bind 'func-to-wrap cnt)
               (with-syntax ([parsed-contract (translator (syntax cnt))])
			    (syntax 
			     (define-values (name-to-bind) 
			       ((contract-enforcer parsed-contract) func-to-wrap)))))
              [_ (raise-syntax-error 'contracts.ss "internal error.1")]))))
      
      
      (define beginner-contract/func (contract-template beginner-translate-contract))
      (define intermediate-contract/func (contract-template intermediate-translate-contract))
      (define advanced-contract/func (contract-template advanced-translate-contract))
      
      (values beginner-contract/func
              intermediate-contract/func
              advanced-contract/func))))
