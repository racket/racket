;; copyright by Paul Graunke June 2000 AD

(module html-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "file.ss")
	   (lib "list.ss")
	   (lib "etc.ss")
	   (lib "include.ss")
	   "html-sig.ss"
	   "sgml-reader-sig.ss"
	   (lib "xml-sig.ss" "xml"))

  (provide html@)

  (define html@
    (unit/sig html^
      (import xml^ (sgml : sgml-reader^))
      
      ;; Html-content = Html-element | Pc-data | Entity   
      
      (include "html-structs.ss")
      (include "case.ss")
      
      ;; xml->html : Document -> Html
      (define (xml->html doc)
	(let ([root (document-element doc)])
	  (unless (eq? 'html (element-name root))
	    (error 'xml->html "This is not an html document.  Expected 'html, given ~a" (element-name root)))
	  (make-html (element-attributes root) (xml-contents->html (element-content root)))))
      
      
      ;; xml-content->html : (listof Content) -> (listof Html-element)
      (define (xml-contents->html contents)
	(foldr xml-single-content->html
	       null
	       contents))
      
      ;; read-xhtml : [Input-port] -> Html
      (define read-xhtml (compose xml->html read-xml))
      
      ;; peel-f : (Html-content -> Bool) (listof Html-content) (listof Html-content) -> (listof Html-content)
      (define (peel-f toss? to-toss acc0)
	(foldr (lambda (x acc)
		 (if (toss? x)
		     (append (html-full-content x) acc)
		     (cons x acc)))
	       acc0
	       to-toss))
      
      ;; repackage-html : (listof Html-content) -> Html
      (define (repackage-html contents)
	(let* ([html (memf html? contents)]
	       [peeled (peel-f html? contents null)]
	       [body (memf body? peeled)])
	  (make-html (if html
			 (html-element-attributes (car html))
			 null)
		     (append (filter head? peeled)
			     (list (make-body (if body
						  (html-element-attributes (car body))
						  null)
					      (filter (compose not head?) (peel-f body? peeled null))))))))
      
      ;; clean-up-pcdata : (listof Content) -> (listof Content)
      ;; Each pcdata inside a tag that isn't supposed to contain pcdata is either
      ;; a) appended to the end of the previous subelement, if that subelement may contain pcdata
      ;; b) prepended to the front of the next subelement, if that subelement may contain pcdata
      ;; c) discarded
      ;; unknown tags may contain pcdata
      ;; the top level may contain pcdata
      (define clean-up-pcdata
	;; clean-up-pcdata : (listof Content) -> (listof Content)
	(letrec ([clean-up-pcdata
		  (lambda (content)
		    (map (lambda (to-fix)
			   (cond
			    [(element? to-fix)
			     (recontent-xml to-fix
					    (let ([possible (may-contain (element-name to-fix))]
						  [content (element-content to-fix)])
					      (if (or (not possible) (memq 'pcdata possible))
						  (clean-up-pcdata content)
						  (eliminate-pcdata content))))]
			    [else to-fix]))
			 content))]
		 [eliminate-pcdata
					;: (listof Content) -> (listof Content)
		  (lambda (content)
		    (let ([non-elements (first-non-elements content)]
			  [more (memf element? content)])
		      (if more
			  (let* ([el (car more)]
				 [possible (may-contain (element-name el))])
			    (if (or (not possible) (memq 'pcdata possible))
				(cons (recontent-xml el (append non-elements (clean-up-pcdata (element-content el)) (eliminate-pcdata (first-non-elements (cdr more)))))
				      (or (memf element? (cdr more)) null))
				(cons (recontent-xml el (eliminate-pcdata (element-content el)))
				      (eliminate-pcdata (cdr more)))))
			  null)))])
	  clean-up-pcdata))
      
      ;; first-non-elements : (listof Content) -> (listof Content)
      (define (first-non-elements content)
	(cond
	 [(null? content) null]
	 [else (if (element? (car content))
		   null
		   (cons (car content) (first-non-elements (cdr content))))]))
      
      ;; recontent-xml : Element (listof Content) -> Element
      (define (recontent-xml e c)
	(make-element (source-start e) (source-stop e) (element-name e) (element-attributes e) c))
      
      ;; implicit-starts : Symbol Symbol -> (U #f Symbol)
      (define (implicit-starts parent child)
	(or (and (eq? child 'tr) (eq? parent 'table) 'tbody)
	    (and (eq? child 'td) (memq parent '(table tbody tfoot thead)) 'tr)))
      
      ;; may-contain : Kid-lister
      (define may-contain
	(sgml:gen-may-contain (call-with-input-file (find-library "html-spec" "html") read)))

      (define may-contain-anything
	(sgml:gen-may-contain null))

      (define use-html-spec (make-parameter #t))
      
      ;; read-html-as-xml : [Input-port] -> (listof Content)
      (define read-html-as-xml
	(case-lambda 
	 [(port)
	  ((if (use-html-spec) clean-up-pcdata values)
	   ((sgml:gen-read-sgml (if (use-html-spec)
				    may-contain 
				    may-contain-anything)
				implicit-starts) port))]
	 [() (read-html-as-xml (current-input-port))]))
      
      ;; read-html : [Input-port] -> Html
      (define read-html
	(compose repackage-html xml-contents->html read-html-as-xml)))))
