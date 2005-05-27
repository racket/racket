(module turtles mzscheme
  (require (lib "unitsig.ss")
	   (lib "mred-sig.ss" "mred")
	   (lib "mred.ss" "mred")
	   "turtle-sig.ss"
	   "turtle-unit.ss")

  (provide-signature-elements turtle^)
  (provide split split* tprompt)

  (define-values/invoke-unit/sig turtle^ turtle@ #f mred^)
 
  (define-syntax split
    (lambda (x)
      (syntax-case x ()
        ((_ args ...)
	 (syntax (splitfn (lambda () args ...)))))))

 (define-syntax split*
   (syntax-rules ()
     [(_ e0 e ...)
      (split*fn (list (lambda () e0) (lambda () e) ...))]))

  (define-syntax tprompt
    (lambda (x)
      (syntax-case x ()
        ((_ e1 ...)
	 (syntax (tpromptfn (lambda () e1 ...))))))))