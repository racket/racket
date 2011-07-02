;;;
;;; <program.rkt> ---- SRFI-7 program
;;; based on MJ Ray's code for SRFI 0

(module program mzscheme
  (require-for-syntax syntax/stx
		      srfi/features)
  (require mzlib/include)
  (provide program)
  
  (define-syntax require-feature
    (lambda (x)
      (syntax-case x ()
	((_ id)
	 (with-syntax ((require-spec
			(datum->syntax-object
			 (syntax id)
			 (feature->require-clause
			  (syntax-object->datum (syntax id))))))
	   (syntax
	    (require require-spec)))))))
  
  (define-syntax program
    (lambda (x)
      (syntax-case* x (requires files code feature-cond and or not else)
		    module-or-top-identifier=?
	((_)
	 (syntax (begin (void))))
	((_
	  (requires feature-id ...)
	  more ...)
	 (syntax
	  (begin 
	    (require-feature feature-id) ...
	    (program more ...))))
	((_
	  (files filename ...)
	  more ...)
	 (with-syntax ((_x x))
	   (syntax
	    (begin
	      (include-at/relative-to _x filename filename) ...
	      (program
	       more ...)))))
	((_
	  (code stuff ...)
	  more ...)
	 (syntax
	  (begin
	    stuff ...
	    (program more ...))))
	((_
	  (feature-cond)
	  more ...)
	 (syntax
	  (program
	   more ...)))
	((_
	  (feature-cond (else stuff ...))
	  more ...)
	 (syntax
	  (program
	   stuff ...
	   more ...)))

	((_
	  (feature-cond ((and) stuff ...)
			rest ...)
	  more ...)
	 (syntax
	  (program
	   stuff ...
	   more ...)))

	((_
	  (feature-cond ((and requirement1 requirement ...) stuff ...)
			rest ...)
	  more ...)
	 (syntax
	  (program
	   (feature-cond (requirement1
			  (feature-cond ((and requirement ...) stuff ...)))
			 rest ...)
	   more ...)))
      
	((_
	  (feature-cond ((or) stuff ...)
			rest ...)
	  more ...)
	 (syntax
	  (program
	   (feature-cond rest ...)
	   more ...)))
	((_
	  (feature-cond ((or requirement1 requirement ...) stuff ...)
			rest ...)
	  more ...)
	 (syntax
	  (program
	   (feature-cond (requirement1 stuff ...)
			 ((or requirement ...) stuff ...)
			 rest ...)
	   more ...)))

	((_
	  (feature-cond ((not requirement) stuff ...)
			rest ...)
	  more ...)
	 (syntax
	  (program
	   (feature-cond (requirement
			  (feature-cond rest ...))
			 (else
			  stuff ...)))))
	((_
	  (feature-cond (requirement stuff ...)
			rest ...)
	  more ...)
	 (if (feature-present? (syntax-object->datum (syntax requirement)))
	     (syntax
	      (program
	       stuff ...
	       more ...))
	     (syntax
	      (program
	       (feature-cond rest ...)
	       more ...))))))))

;;; program.rkt ends here
