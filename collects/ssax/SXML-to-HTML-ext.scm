;	   HTML Authoring in SXML for my personal Web pages
;
; The present file defines several functions and higher-order
; SXML "tags" that are used to compose HTML pages on my web site.
; In LaTeX terms, this file is similar to article.cls.
;
; See http://pobox.com/~oleg/ftp/Scheme/xml.html#XML-authoring
; for more examples and explanation.
;
; IMPORT
; Approporiate Prelude: myenv.scm or myenv-bigloo.scm
; srfi-13-local.scm or the appropriate native implementation of SRFI-13
; util.scm
; SXML-tree-trans.scm
; SXML-to-HTML.scm
; OS:file-length, unless it is included into the core system
;   (see myenv-bigloo.scm for example)
;
; $Id: SXML-to-HTML-ext.scm,v 1.2 2004/11/09 14:11:39 sperber Exp $

; skip the lst trough the first significant element
; return the tail of lst such that (car result) is significant
; Insignificant elems are '(), #f, and lists made of them
; If all of the list is insignificant, return #f
(define (signif-tail lst)
  (define (signif? obj)
    (and (not (null? obj)) obj
	 (if (pair? obj)
	     (or (signif? (car obj))
		 (signif? (cdr obj)))
	     obj)))
  (and (signif? lst)
       (assert (pair? lst))
       (if (signif? (car lst)) lst
	   (signif-tail (cdr lst)))))

; Procedure make-header HEAD-PARMS
; Create the 'head' SXML/HTML tag. HEAD-PARMS is an assoc list of
; (h-key h-value), where h-value is a typically string;
; h-key is a symbol:
; title, description, AuthorAddress, keywords,
; Date-Revision-yyyymmdd, Date-Creation-yyyymmdd,
; long-title
; One of the h-key can be Links.
; In that case, h-value is a list of
;	(l-key l-href (attr value) ...)
; where l-key is one of the following:
;	start, contents, prev, next, top, home

(define (make-header head-parms)
  `(head
    (title ,(lookup-def 'title head-parms))
    ,(map
      (lambda (key)
	(let ((val (lookup-def key head-parms warn: #f)))
	  (and val
	       `(meta (@ (name ,(symbol->string key)) (content ,val))))))
      '(description AuthorAddress keywords
	Date-Revision-yyyymmdd Date-Creation-yyyymmdd))
    ,(let ((links (lookup-def 'Links head-parms '())))
      (and (pair? links)
	   (map
	    (lambda (link-key)
	      (let ((val (lookup-def link-key links #f)))
		(and val
		  (let ((val (if (not (pair? val)) (list val) val)))
		     `(link (@ (rel ,(symbol->string link-key))
			       (href ,(car val))
			       ,@(cdr val)))))))
	    '(start contents prev next)))))
)

; Create a navigational bar. The argument head-parms is the same
; as the one passed to make-header. We're only concerned with the
; h-value Links
(define (make-navbar head-parms)
  (let ((links (lookup-def 'Links head-parms '()))
	(nav-labels '((prev . "previous")
		      (next . "next")
		      (contents . "contents")
		      (top . "top"))))
    (and (pair? links)
      `(div (@ (align "center") (class "navbar"))
	 ,(let loop ((nav-labels nav-labels) (first? #t))
	    (if (null? nav-labels) '()
		(let ((val (lookup-def (caar nav-labels) links warn: #f)))
		  (if (not val)
		      (loop (cdr nav-labels) first?)
		      (cons
		       (list " " (if first? #f '(n_)) " "
			     `(a (@ (href ,val)) ,(cdar nav-labels)))
		       (loop (cdr nav-labels) #f))))))
	 (hr)))
))
			      

; Create a footer. The argument head-parms is the same
; as passed to make-header.
(define (make-footer head-parms)
  `((br)
    (div (hr))
    (h3 "Last updated "
	,(let* ((date-revised
		  (lookup-def 'Date-Revision-yyyymmdd head-parms))
		(year (string->integer date-revised 0 4))
		(month (string->integer date-revised 4 6))
		(day   (string->integer date-revised 6 8))
		(month-name
		 (vector-ref
		  '#("January" "February" "March" "April" "May" "June"
		    "July"   "August" "September" "October" "November"
		    "December")
		  (dec month))))
	   (list month-name " " day ", " year)))
    ,(let ((links (lookup-def 'Links head-parms '())))
       (and (pair? links)
	    (let ((home (lookup-def 'home links warn: #f)))
	      (and home
		   `(p "This site's top page is "
		       (a (@ (href ,home)) (strong ,home)))))))
    (div 
      (address "oleg-at-pobox.com or oleg-at-acm.org or oleg-at-computer.org"
       (br)
       "Your comments, problem reports, questions are very welcome!"))
    (p (font (@ (size "-2")) "Converted from SXML by SXML->HTML"))
    ,(let ((rcs-id (lookup-def 'rcs-id head-parms #f)))
       (and rcs-id `(h4 ,rcs-id)))
    ))

; Bindings for the post-order function, which traverses the SXML tree
; and converts it to a tree of fragments

; The universal transformation from SXML to HTML. The following rules
; work for every HTML, present and future
(define universal-conversion-rules
  `((@
      ((*default*       ; local override for attributes
        . ,(lambda (attr-key . value) (enattr attr-key value))))
      . ,(lambda (trigger . value) (cons '@ value)))
    (*default* . ,(lambda (tag . elems) (entag tag elems)))
    (*text* . ,(lambda (trigger str) 
		 (if (string? str) (string->goodHTML str) str)))
    (n_		; a non-breaking space
     . ,(lambda (tag . elems)
	  (cons "&nbsp;" elems)))))

; A variation of universal-conversion-rules which keeps '<', '>', '&'
; and similar characters intact. The universal-protected-rules are
; useful when the tree of fragments has to be traversed one more time.
(define universal-protected-rules
  `((@
      ((*default*       ; local override for attributes
        . ,(lambda (attr-key . value) (enattr attr-key value))))
      . ,(lambda (trigger . value) (cons '@ value)))
    (*default* . ,(lambda (tag . elems) (entag tag elems)))
    (*text* . ,(lambda (trigger str) 
		 str))
    (n_		; a non-breaking space
     . ,(lambda (tag . elems)
	  (cons "&nbsp;" elems)))))

; The following rules define the identity transformation
(define alist-conv-rules
  `((*default* . ,(lambda (tag . elems) (cons tag elems)))
    (*text* . ,(lambda (trigger str) str))))


; Find the 'Header' node within the 'Content' SXML expression.
; Currently this query is executed via a transformation, with
; rules that drop out everything but the 'Header' node.
; We use the _breadth-first_ traversal of the Content tree.
(define (find-Header Content)
  (letrec 
    ((search-rules
	 `((*default*
	    *preorder*
	    . ,(lambda (tag . elems)
		 (let loop ((elems elems) (worklist '()))
		   (cond
		    ((null? elems) 
		     (if (null? worklist) '()
			 (pre-post-order worklist search-rules)))
		    ((not (pair? (car elems))) (loop (cdr elems) worklist))
		    ((eq? 'Header (caar elems)) (car elems)) ; found
		    (else (loop (cdr elems) (cons (car elems) worklist)))))))
	   )))
    (lookup-def 'Header
		(list (pre-post-order Content search-rules))
		)))


; Transformation rules that define a number of higher-order tags,
; which give "style" to all my pages.
; Some of these rules require a pre-post-order iterator
; See xml.scm or any other of my web page master files for an example
; of using these stylesheet rules

(define (generic-web-rules Content additional-rules)
  (append
   additional-rules
   universal-conversion-rules
   `((html:begin 
      . ,(lambda (tag . elems)
	   (list
	     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""
	     nl 
	     "\"http://www.w3.org/TR/html4/loose.dtd\">" nl
	     "<html>" nl
	     elems
	     "</html>" nl)))

     (Header
      *preorder*
      . ,(lambda (tag . headers)
	   (post-order (make-header headers) universal-conversion-rules)
	   ))

     (body
      . ,(lambda (tag . elems)
	   (list "<body bgcolor=\"#FFFFFF\">" nl elems "</body>")))

     (navbar			; Find the Header in the Content
      . ,(lambda (tag)		; and create the navigation bar
	   (let ((header-parms (find-Header Content)))
	     (post-order (make-navbar header-parms)
			 universal-conversion-rules))))
     
     (footer			; Find the Header in the Content
      . ,(lambda (tag)		; and create the footer of the page
	   (let ((header-parms (find-Header Content)))
	     (post-order (make-footer header-parms)
			 universal-conversion-rules))))
     
     (page-title		; Find the Header in the Content
      . ,(lambda (tag)		; and create the page title rule
	   (let ((header-parms (find-Header Content)))
	     (list "<h1 align=center>" 
		   (lookup-def 'long-title header-parms) "</h1>" nl))))


     (Section	; (Section level "content ...")
      . ,(lambda (tag level head-word . elems)
	   (list "<br>&nbsp;<a name=\"" head-word "\">&nbsp;</a>" nl
		 "<h" level ">"  head-word elems "</h" level ">" nl)))

     (TOC	; Re-scan the Content for "Section" tags and generate
      . ,(lambda (tag)	; the Hierarchical Table of contents
	   (let ((sections
		  (post-order Content
		    `((Section	; (Section level "content ...")
		       ((*text* . ,(lambda (tag str) str)))
		       . ,(lambda (tag level head-word . elems)
			    (vector level
				    (list "<li><a href=\"#" head-word
					  "\">" head-word elems "</a>" nl))))
		      (*default*
		       . ,(lambda (attr-key . elems) elems))
		      (*text* . ,(lambda (trigger str) '()))))))
	     ;(cerr sections)
	     (list "<div>"
	      (let loop ((curr-level 1) (sections sections))
	       (cond
		((null? sections)
		 (let fill ((curr-level curr-level))
		   (if (> curr-level 1)
		       (cons "</ol>" (fill (dec curr-level)))
		       '())))
		((null? (car sections)) (loop curr-level (cdr sections)))
		((pair? (car sections)) (loop curr-level
					      (append (car sections)
						      (cdr sections))))
		((vector? (car sections))
		 (let ((new-level (vector-ref (car sections) 0)))
		   (cond
		    ((= new-level curr-level)
		     (cons (vector-ref (car sections) 1)
			   (loop curr-level (cdr sections))))
		    ((= (inc new-level) curr-level)
		     (cons "</ol>"
			   (cons (vector-ref (car sections) 1)
				 (loop new-level (cdr sections)))))
		    ((= new-level (inc curr-level))
		     (cons nl (cons "<ol>"
			   (cons (vector-ref (car sections) 1)
				 (loop new-level (cdr sections))))))
		    (else 
		     (error "inconsistent levels: " curr-level new-level)))))
		(else "wrong item: " sections)))
	      nl "</div>" nl))))

     (bibitem *macro*
       . ,(lambda (tag label key . text)
	   `(p (a (@ (name ,key)) "[" ,label "]") " " ,text)))

     (cite		; ought to locate the label and use the label!
      . ,(lambda (tag key)
	   (list "[<a href=\"#" key "\">" key "</a>]")))


     (trace		; A debugging aid
      . ,(lambda (tag . content)
	   (cerr tag content nl)
	   '()))

     (URL  *macro*
      . ,(lambda (tag url)
	   `((br) "<" (a (@ (href ,url)) ,url) ">")))


     (verbatim	; set off pieces of code: one or several lines
      . ,(lambda (tag . lines)
	   (list "<pre>"
		 (map (lambda (line) (list "     " line nl))
		      lines)
		 "</pre>")))
		; (note . text-strings)
		; A note (remark), similar to a footnote
     (note
      . ,(lambda (tag . text-strings)
	   (list " <font size=\"-1\">[" text-strings "]</font>" nl)))

		; A reference to a file
     (fileref
      . ,(lambda (tag pathname . descr-text)
	   (list "<a href=\"" pathname "\">"
		 (car (reverse (string-split pathname '(#\/))))
		 "</a> [" 
		 (let ((file-size (OS:file-length pathname)))
		   (if (not (positive? file-size))
		       (error "File not found: " pathname))
		   (cond
		    ((< file-size 1024) "&lt;1K")
		    (else (list (quotient (+ file-size 1023) 1024) "K"))))
		 "]<br>"
		 descr-text)))

		; A reference to a plain-text file (article)
     (textref
       . ,(lambda (tag pathname title . descr)
	    (let ((file-size (OS:file-length pathname)))
	      (if (not (positive? file-size))
		  (error "File not found: " pathname))
	      (list "<a href=\"" pathname "\">" title
		    "</a> <font size=\"-1\">[plain text file]</font><br>" nl
		    descr))))

		; A reference to an anchor in the present file
		; (local-ref target . title)
		; If title is given, generate a regular
		;	<a href="#target">title</a>
		; Otherwise, transform the content so that a
		; construct that may generate an anchor 'target' (such
		; as Section or Description-unit) is re-written to the
		; title SXML. All other constructs re-write to
		; nothing.
     (local-ref
      *macro*
      . ,(lambda (tag target . title)
	   (let
	       ((title
		 (if (pair? title) title	; it is given explicitly
		     (pre-post-order Content
		       `((*text* . ,(lambda (trigger str) '()))
			 (*default*
			  . ,(lambda (tag . elems)
			       (let ((first-sign (signif-tail elems)))
				 (if first-sign
				     (let ((second-sign
					    (signif-tail (cdr first-sign))))
				       (assert (not second-sign))
				       (car first-sign))
				     '()))))
			 (Description-unit
			  *preorder*
			  . ,(lambda (tag key title . elems)
			       (if (equal? key target)
				   (list title)
				   '()))))))))
	     (assert (pair? title))
	     (cerr "title: " title nl)
	     `(a (@ (href #\# ,target)) ,title))))

		; Unit of a description for a piece of code
		; (Description-unit key title . elems)
		; where elems is one of the following:
		; headline, body, platforms, version
     (Description-unit
      ((headline
	. ,(lambda (tag . elems)
	     (list "<dt>" elems "</dt>" nl)))
       (body
	. ,(lambda (tag . elems)
	     (list "<dd>" elems "</dd>" nl)))
       (platforms
	. ,(lambda (tag . elems)
	     (list "<dt><strong>Platforms</strong><dt><dd>"
		   elems "</dd>" nl)))
       (version
	. ,(lambda (tag . elems)
	     (list "<dt><strong>Version</strong><dt><dd>"
		   "The current version is " elems ".</dd>" nl)))
       (references
	. ,(lambda (tag . elems)
	     (list "<dt><strong>References</strong><dt><dd>"
		   elems "</dd>" nl)))
       (requires
	. ,(lambda (tag . elems)
	     (list "<dt><strong>Requires</strong><dt><dd>"
		   elems "</dd>" nl)))
       )
      . ,(lambda (tag key title . elems)
	   (post-order
	    `((a (@ (name ,key)) (n_))
	      (h2 ,title)
	      (dl (insert-elems))
	      )
	    `(,@universal-protected-rules
	      (insert-elems
	       . ,(lambda (tag) elems))))))
)))
