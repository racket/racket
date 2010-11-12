
(module utils mzscheme
  (require mred
	   mzlib/class)

  (provide define-accessor
	   define/provide-struct)

  (define-syntax define-accessor
    (syntax-rules ()
      [(_ margin get-margin)
       (define-syntax margin
	 (syntax-id-rules ()
	   [(margin arg) ((get-margin) arg...)]
	   [margin (get-margin)]))]))
  

  (define-syntax define/provide-struct
    (syntax-rules ()
      [(_ id flds)
       (begin
	 (define-struct id flds)
	 (provide (struct id flds)))])))



