
(load-relative "loadtest.rktl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; url.ss tests
;;

(require net/head)

(for-each 
 (lambda (addr)
   (test '("o.gu@plt-scheme.com") extract-addresses (car addr) 'address)
   (test '("o.gu@plt-scheme.com" "o.gu@plt-scheme.com") extract-addresses 
	 (format "~a, ~a" (car addr) (car addr))
	 'address)
   (test (list (cdr addr)) extract-addresses (car addr) 'name)
   (for-each 
    (lambda (addr2)
      (let ([two (format " ~a, \n\t~a" (car addr) (car addr2))])
	(test '("o.gu@plt-scheme.com" "s.gu@plt-scheme.org") extract-addresses two 'address)
	(test (list (cdr addr) (cdr addr2)) extract-addresses two 'name)))
    '(("s.gu@plt-scheme.org" . "s.gu@plt-scheme.org")
      ("<s.gu@plt-scheme.org>" . "s.gu@plt-scheme.org")
      ("s.gu@plt-scheme.org (Gu, Sophia)" . "Gu, Sophia")
      ("s.gu@plt-scheme.org (Sophia Gu)" . "Sophia Gu")
      ("s.gu@plt-scheme.org (Sophia \"Sophie\" Gu)" . "Sophia \"Sophie\" Gu")
      ("Sophia Gu <s.gu@plt-scheme.org>" . "Sophia Gu")
      ("\"Gu, Sophia\" <s.gu@plt-scheme.org>" . "\"Gu, Sophia\"")
      ("\"Gu, Sophia (Sophie)\" <s.gu@plt-scheme.org>" . "\"Gu, Sophia (Sophie)\""))))
 '(("o.gu@plt-scheme.com" . "o.gu@plt-scheme.com")
   ("<o.gu@plt-scheme.com>" . "o.gu@plt-scheme.com")
   ("o.gu@plt-scheme.com (Gu, Oliver)" . "Gu, Oliver")
   ("o.gu@plt-scheme.com (Oliver Gu)" . "Oliver Gu")
   ("o.gu@plt-scheme.com (Oliver \"Ollie\" Gu)" . "Oliver \"Ollie\" Gu")
   ("o.gu@plt-scheme.com (Oliver \"Ollie Gu)" . "Oliver \"Ollie Gu")
   ("Oliver Gu <o.gu@plt-scheme.com>" . "Oliver Gu")
   ("\"Gu, Oliver\" <o.gu@plt-scheme.com>" . "\"Gu, Oliver\"")
   ("\"Gu, Oliver (Ollie)\" <o.gu@plt-scheme.com>" . "\"Gu, Oliver (Ollie)\"")
   ("\"Gu, Oliver (Ollie\" <o.gu@plt-scheme.com>" . "\"Gu, Oliver (Ollie\"")
   ("\"Gu, Oliver (Ollie, himself)\" <o.gu@plt-scheme.com>" . "\"Gu, Oliver (Ollie, himself)\"")))


(report-errs)
