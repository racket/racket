(module define-data mzscheme
  
  (require-for-syntax "advanced-contracts.ss" 
                      "hilighters.ss"
		      "contract-transformers.ss")
  (require "contracts-helpers.ss"
	   "hilighters.ss")
  
  (provide beginner-define-data
           intermediate-define-data 
           advanced-define-data
           
           beginner-dd-builder
           intermediate-dd-builder
           advanced-dd-builder)
  
  ;;;;;;;;;;;;;;;;;; define-data
 #|
 
execution is broken into two parts to support recursive definitions: first we bind all the names we need, and then define them.

*-define-data binds the name if the definition as syntax as a list that has three items: the name of the data definition, the 
symbol 'define-data (just to distinguish what it is), and a syntax object that represents the name of the contract structure
that enforces this definition (called <name>-contract). also expands into the definition of this contract-struct.

the second stage actually runs the transformations on the given contracts. since all other names from define-datas have been bound by now, 
it allows for recursion. the contract enforcer is defined in contracts-helpers.scm

|#

  (define-syntaxes (beginner-dd-builder intermediate-dd-builder advanced-dd-builder) 
    (let ()
      
      (define dd-builder-template
        (lambda (translator) 
          (lambda (stx)
           
            (syntax-case stx ()
              [(_ name src e1 e2 ...)
               
		 (with-syntax  ([(translated-cnts ...) (map translator (syntax-e (syntax (e1 e2 ...))))])
	     
                 (syntax
                    (letrec ([me (make-flat-contract
                                  
                                  ;enforcer
                                  (lambda (value) 
                                    (define-data-enforcer me value (list translated-cnts ...)))
                                  
                                  ;hilighter
                                  (lambda (path)
                                    (let ([hilighter (mk-define-data-hilighter 'name (map contract-hilighter (list translated-cnts ...)))])
                                      (hilighter path)))
                                  
                                  ;stx locations
                                  (syntax-source src)
                                  (syntax-line src)
                                  (syntax-column src)
                                  (syntax-position src)
                                  (syntax-span src)
                                  
                                  ;predicate
                                  (lambda (x)
                                    (ormap (lambda (c) ((flat-contract-predicate c) x)) (list translated-cnts ...))))])
                      me)))]))))
      
      
      (values (dd-builder-template beginner-translate-contract)
              (dd-builder-template intermediate-translate-contract)
              (dd-builder-template advanced-translate-contract))))
  
  
  (define-syntaxes (beginner-define-data intermediate-define-data advanced-define-data)
    (let ()
      
      (define define-data-template
        (lambda (builder)
          (lambda (stx)
            (syntax-case stx ()
              [(_ name cnt1 cnt2 ...)
               (with-syntax 
                   ([cnt-builder builder]
                    [contract-name 
                     (datum->syntax-object (syntax name) (string->symbol (string-append (symbol->string (syntax-object->datum (syntax name))) "-contract")))]
                    [ret stx])
                 (syntax 
                  (begin
                    (define-syntax name (list #'name 'define-data #'contract-name))
                    (define contract-name (cnt-builder name #'ret cnt1 cnt2 ...)))
                  ))]))))
      
      (values (define-data-template #'beginner-dd-builder)
              (define-data-template #'intermediate-dd-builder)
              (define-data-template #'advanced-dd-builder)))))
