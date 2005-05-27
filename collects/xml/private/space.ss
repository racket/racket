
(module space mzscheme
  (require (lib "unitsig.ss")
	  (lib "list.ss"))

  (require "sig.ss")

  (provide space@)

  (define space@
    (unit/sig space^
      (import xml-structs^)
      
      ;; eliminate-whitespace : (listof Symbol) (Bool -> Bool) -> Element -> Element
      (define (eliminate-whitespace special eliminate-special?)
	(letrec ([blank-it
		  (lambda (el)
		    (let ([name (element-name el)]
			  [content (map (lambda (x)
					  (if (element? x) (blank-it x) x))
					(element-content el))])
		      (make-element
		       (source-start el)
		       (source-stop el)
		       name
		       (element-attributes el)
		       (cond
			[(eliminate-special? (memq (element-name el) special))
			 (filter (lambda (s)
				   (not (and (pcdata? s)
					     (or (all-blank (pcdata-string s))
						 (error 'eliminate-blanks "Element <~a> is not allowed to contain text ~s" name (pcdata-string s))))))
				 content)]
			[else content]))))])
	  blank-it))
      
      ;; all-blank : String -> Bool
      (define (all-blank s) (andmap char-whitespace? (string->list s))))))

