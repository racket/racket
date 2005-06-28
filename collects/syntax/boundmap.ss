
(module boundmap mzscheme
  (require (lib "contract.ss")
	   "private/boundmap.ss")
  
  (define-syntax (provide-mapping-code/contract stx)
    (syntax-case stx ()
      [(_ make-identifier-mapping
          identifier-mapping?
          identifier-mapping-get
          identifier-mapping-put!
          identifier-mapping-for-each
          identifier-mapping-map
          identifier=?)
       (and (identifier? (syntax identifier-mapping))
            (identifier? (syntax identifier-mapping-get))
            (identifier? (syntax identifier-mapping-put!))
            (identifier? (syntax identifier-mapping-for-each))
            (identifier? (syntax identifier-mapping-map)))
       (syntax
	(provide/contract
	 [make-identifier-mapping (-> identifier-mapping?)]
	 [identifier-mapping? (any/c . -> . boolean?)]
	 [identifier-mapping-get (opt->*
				  (identifier-mapping?
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
   bound-identifier-mapping?
   bound-identifier-mapping-get
   bound-identifier-mapping-put!
   bound-identifier-mapping-for-each
   bound-identifier-mapping-map
   bound-identifier=?)
  
  (provide-mapping-code/contract
   make-module-identifier-mapping
   module-identifier-mapping?
   module-identifier-mapping-get
   module-identifier-mapping-put!
   module-identifier-mapping-for-each
   module-identifier-mapping-map
   module-identifier=?))
