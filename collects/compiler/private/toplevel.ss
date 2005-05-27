;; routines for top-level entities
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-2001 PLT

(module toplevel mzscheme
  (require (lib "unitsig.ss"))

  (require "sig.ss")

  (provide toplevel@)
  (define toplevel@
    (unit/sig
	compiler:top-level^
      (import compiler:library^
	      compiler:cstructs^)
      
      ;;-------------------------------------------------------------
      ;; This contains information about a top-level block, either at
      ;; file level, or within a unit; typically a sequence of defines
      ;; but could be anything
      ;;
      (define-struct block (source ; list of top-level ASTs
			    codes  ; list of `code' structures (in parallel with source)
			    max-arity))
      (define make-empty-block (lambda () (make-block null null 0)))

      (define block:register-max-arity!
	(lambda (b n)
	  (set-block-max-arity! b (max n (block-max-arity b)))))

      ;; Add a local variable to a code record.
      ;; If the local variable is in a case-code, add it from
      ;;  the case-code and it will be automatically added
      ;;  to the case-code's parent procedure-code.
      (define (add-code-local+used-vars! code vars)
	(set-code-local-vars! code (set-union vars (code-local-vars code)))
	(set-code-used-vars! code (set-union vars (code-used-vars code)))
	(when (case-code? code)
	  ;; If this is just a case, also add it to the parent,
	  ;; which is the real closure
	  (add-code-local+used-vars! (code-parent code) vars)))
      
      ;; Remove a free variable from a code record.
      ;; If the free variable is used in a case-code, remove it from
      ;;  the case-code and it will be automatically removed
      ;;  from the case-code's parent procedure-code (if appropriate).
      (define (remove-code-free-vars! code vars)
	(set-code-free-vars! code (set-minus (code-free-vars code) vars))
	(set-code-captured-vars! code (set-minus (code-captured-vars code) vars))
	(let ([code (if (case-code? code)
			;; If this is just a case, recalculate the parent's free,
			;;  which is the free set for the real closure
			(let ([code (code-parent code)])
			  (let loop ([fv empty-set]
				     [cv empty-set]
				     [cases (procedure-code-case-codes code)])
			    (if (null? cases)
				(begin
				  (set-code-free-vars! code fv)
				  (set-code-captured-vars! code cv))
				(loop (set-union (code-free-vars (car cases)) fv)
				      (set-union (code-captured-vars (car cases)) cv)
				      (cdr cases))))
			  code)
			code)])
	  ;; At this point, we go the code's parent and
	  ;;   adjust the free/captured variable information.
	  (let ([pcode (or (code-case-parent code)
			   (code-parent code))])
	    (when pcode
	      (let ([children (code-children pcode)])
		(unless (ormap (lambda (child)
				 (not (set-empty? (set-intersect vars (code-free-vars code)))))
			       children)
		  ;; No other child uses the variable
		  (remove-code-free-vars! pcode vars)))))))

      ;; Notes on some other possible functions:
      ;;   add-code-global-vars - add to all [case-]ancestors
      ;;   remove-code-captured-vars - parent handling is the same
      ;;                               as remove-code-free-vars

      )))

