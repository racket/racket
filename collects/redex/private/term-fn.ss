(module term-fn mzscheme
  (provide make-term-fn
           term-fn?
           term-fn-get-id)
  
  (define-values (struct-type make-term-fn term-fn? term-fn-get term-fn-set!) 
    (make-struct-type 'term-fn #f 1 0))
  (define term-fn-get-id (make-struct-field-accessor term-fn-get 0)))
