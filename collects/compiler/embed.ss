
(module embed mzscheme
  (require (lib "unit.ss")
           (lib "contract.ss"))
  
  (require "sig.ss")
  
  (require "embed-unit.ss"
	   "embed-sig.ss")
  
  (define-values/invoke-unit/infer compiler:embed@)
  
  (provide/contract [make-embedding-executable
                     (opt-> (path-string?
                             any/c
                             any/c
                             (listof (list/c (or/c boolean? symbol?) any/c))
                             (listof path-string?)
                             any/c
                             (listof string?))
                            ((listof (cons/c symbol? any/c))
                             any/c
                             symbol?)
                            void?)])
  (provide write-module-bundle
	   create-embedding-executable
           embedding-executable-is-directory?
           embedding-executable-put-file-extension+style+filters
	   embedding-executable-add-suffix))

