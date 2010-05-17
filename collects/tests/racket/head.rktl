
(load-relative "loadtest.rktl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; net/head tests
;;

(require net/head)

(for-each 
 (lambda (addr)
   (test '("o.gu@racket-lang.com") extract-addresses (car addr) 'address)
   (test '("o.gu@racket-lang.com" "o.gu@racket-lang.com") extract-addresses 
	 (format "~a, ~a" (car addr) (car addr))
	 'address)
   (test (list (cdr addr)) extract-addresses (car addr) 'name)
   (for-each 
    (lambda (addr2)
      (let ([two (format " ~a, \n\t~a" (car addr) (car addr2))])
	(test '("o.gu@racket-lang.com" "s.gu@racket-lang.org") extract-addresses two 'address)
	(test (list (cdr addr) (cdr addr2)) extract-addresses two 'name)))
    '(("s.gu@racket-lang.org" . "s.gu@racket-lang.org")
      ("<s.gu@racket-lang.org>" . "s.gu@racket-lang.org")
      ("s.gu@racket-lang.org (Gu, Sophia)" . "Gu, Sophia")
      ("s.gu@racket-lang.org (Sophia Gu)" . "Sophia Gu")
      ("s.gu@racket-lang.org (Sophia \"Sophie\" Gu)" . "Sophia \"Sophie\" Gu")
      ("Sophia Gu <s.gu@racket-lang.org>" . "Sophia Gu")
      ("\"Gu, Sophia\" <s.gu@racket-lang.org>" . "\"Gu, Sophia\"")
      ("\"Gu, Sophia (Sophie)\" <s.gu@racket-lang.org>" . "\"Gu, Sophia (Sophie)\""))))
 '(("o.gu@racket-lang.com" . "o.gu@racket-lang.com")
   ("<o.gu@racket-lang.com>" . "o.gu@racket-lang.com")
   ("o.gu@racket-lang.com (Gu, Oliver)" . "Gu, Oliver")
   ("o.gu@racket-lang.com (Oliver Gu)" . "Oliver Gu")
   ("o.gu@racket-lang.com (Oliver \"Ollie\" Gu)" . "Oliver \"Ollie\" Gu")
   ("o.gu@racket-lang.com (Oliver \"Ollie Gu)" . "Oliver \"Ollie Gu")
   ("Oliver Gu <o.gu@racket-lang.com>" . "Oliver Gu")
   ("\"Gu, Oliver\" <o.gu@racket-lang.com>" . "\"Gu, Oliver\"")
   ("\"Gu, Oliver (Ollie)\" <o.gu@racket-lang.com>" . "\"Gu, Oliver (Ollie)\"")
   ("\"Gu, Oliver (Ollie\" <o.gu@racket-lang.com>" . "\"Gu, Oliver (Ollie\"")
   ("\"Gu, Oliver (Ollie, himself)\" <o.gu@racket-lang.com>" . "\"Gu, Oliver (Ollie, himself)\"")))


(report-errs)
