(module boundmap mzscheme
  (require racket/contract/base
           "private/boundmap.rkt")
  
  (define-syntax provide/contract*
    (syntax-rules ()
      [(_ [(name0 name ...) contract])
       (begin (provide/contract [name0 contract])
              (provide/contract (rename name0 name contract)) ...)]
      [(_ [name contract])
       (provide/contract [name contract])]
      [(_ [name contract] ...)
       (begin (provide/contract* [name contract]) ...)]))

  (define-syntax (provide-mapping-code/contract stx)
    (syntax-case stx ()
      [(_ make-identifier-mapping
          identifier-mapping? identifier-mapping?/out
          identifier-mapping-get
          identifier-mapping-put!
          identifier-mapping-for-each
          identifier-mapping-map
          identifier=?)
       (syntax
	(provide/contract*
	 [make-identifier-mapping (-> identifier-mapping?)]
	 [identifier-mapping?/out (any/c . -> . boolean?)]
         [identifier-mapping-get (->* (identifier-mapping?
                                       identifier?)
                                      ((-> any))
                                      any)]
	 [identifier-mapping-put! (identifier-mapping?
				   identifier?
				   any/c
				   . -> .
				   void?)]
	 [identifier-mapping-for-each (identifier-mapping?
				       (identifier? any/c . -> . any)
				       . -> .
				       void?)]
	 [identifier-mapping-map (identifier-mapping?
				  (identifier? any/c . -> . any)
				  . -> .
				  (listof any/c))]))]))

  (provide-mapping-code/contract
   make-bound-identifier-mapping
   bound-identifier-mapping? bound-identifier-mapping?
   bound-identifier-mapping-get
   bound-identifier-mapping-put!
   bound-identifier-mapping-for-each
   bound-identifier-mapping-map
   bound-identifier=?)
  
  (provide-mapping-code/contract
   [make-module-identifier-mapping make-free-identifier-mapping]
   module-identifier-mapping?
   [module-identifier-mapping? free-identifier-mapping?]
   [module-identifier-mapping-get free-identifier-mapping-get]
   [module-identifier-mapping-put! free-identifier-mapping-put!]
   [module-identifier-mapping-for-each free-identifier-mapping-for-each]
   [module-identifier-mapping-map free-identifier-mapping-map]
   module-identifier=?))
