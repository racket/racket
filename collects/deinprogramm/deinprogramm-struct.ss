#lang scheme/base
  (provide prop:deinprogramm-struct
	   deinprogramm-struct?)
  
  (define-values (prop:deinprogramm-struct deinprogramm-struct? deinprogramm-struct-ref)
    (make-struct-type-property 'deinprogramm-struct))
