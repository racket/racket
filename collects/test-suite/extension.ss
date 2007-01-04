
(module extension mzscheme
  (provide add-test-suite-extension
	   test-suite-extensions)

  (define (add-test-suite-extension button icon callback)
    (test-suite-extensions (append
			    (test-suite-extensions)
			    (list (list button icon callback)))))

  (define test-suite-extensions (make-parameter null)))


     