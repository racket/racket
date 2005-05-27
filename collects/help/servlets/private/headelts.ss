; elements to go in HEAD part of HTML document

(module headelts mzscheme
  (require (lib "list.ss"))

  (provide hd-css
           hd-links)

  ; cascading style sheet rules for Help Desk
 
  ; (listof (tag attrib+))
  ;   where attrib is a property name, value pair
  ;         where a value is a symbol or (listof symbol)
  (define css-rules
    '((BODY (background-color white)
	    (font-family (Helvetica sans-serif)))))

  (define nl (string #\newline))

  (define (css-rules->style)
    (apply string-append
     (map
      (lambda (s)
	(string-append s nl))
      (map 
       (lambda (rule)
	 (let ([tag (car rule)]
	       [attribs (cdr rule)])
	   (string-append 
	    (symbol->string tag)
	    " {"
	    (foldr
	     (lambda (s a)
	       (if a
		   (string-append s "; " a)
		   s))
	     #f
	     (map 
		(lambda (attrib)
		  (let ([property (car attrib)]
			[vals (cadr attrib)])
		    (string-append (symbol->string property) ":"
				   (if (pair? vals)
				       (foldr
					(lambda (s a)
					  (if a
					      (string-append s "," a)
					      s))
					#f
					(map symbol->string vals))
				       (symbol->string vals)))))
		attribs))
	    "}")))
       css-rules))))

  (define hd-css
    `(STYLE ((TYPE "text/css"))
	    ,(css-rules->style)))

  ; LINKs for showing PLT icon

  (define hd-links
    `((LINK ((REL "icon") (HREF "/help/servlets/plticon.ico") 
	     (TYPE "image/ico")))
      (LINK ((REL "SHORTCUT ICON") (HREF "/help/servlets/plticon.ico"))))))


