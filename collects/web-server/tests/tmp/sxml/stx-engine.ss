; Module header is generated automatically
#cs(module stx-engine mzscheme
(require (lib "defmacro.ss"))
(require (rename (lib "pretty.ss") pp pretty-print))
(require (lib "string.ss" "srfi/13"))
(require "sxml-tools.ss")
(require "sxpathlib.ss")
(require "sxpath-ext.ss")
(require "txpath.ss")
(require "sxpath.ss")
(require "libmisc.ss")
(require (lib "ssax.ss" "web-server/tests/tmp/ssax"))

;; $Id: stx-engine.scm,v 1.9403 2002/12/25 19:33:48 kl Exp kl $

; DL: if you are not using "access-remote.scm", uncomment the following line
;(define open-input-resource open-input-file)

;=============================================================================
; Auxilliary 

(define stx:version 
  (string-append " $Revision: 1.9403 $" nl " $Date: 2002/12/25 19:33:48 $"))

(define (stx:error . messages)
  (cerr nl "STX: ")
  (apply cerr messages)
  (cerr nl)
  (exit -1))

; Reads content of a given SXML element 'obj' using Scheme reader.
; The content has to be a list of strings (first of them will be read).
; If the content is empty, "" is returned.
(define (stx:read-content obj objname)
  (let ((ct (sxml:content obj)))
    (cond 
      ((null? ct) "")
      ((string? (car ct))
       (with-exception-handler         
        (lambda(mes)
          (apply stx:error 
                 `("Error " ,nl ,mes ,nl "reading " ,objname " code:" ,nl
                   ,(car ct) ,nl "from element" ,nl
                   ,@(sxml:clean-feed (sxml:sxml->xml obj)) ,nl))
          (exit))
        (lambda()
          (call-with-input-string (car ct) read))))
      (else 
	    (stx:error "Invalid " objname " element:" nl obj)))
      ))


(define (stx:clean-feed . fragments)
  (reverse
  (let loop ((fragments fragments) (result '()))
    (cond
      ((null? fragments) result)
      ((not (car fragments)) (loop (cdr fragments) result))
      ((null? (car fragments)) (loop (cdr fragments) result))
      (else
        (loop (cdr fragments) 
	      (cons (car fragments) result)))))))

; DL: Borrowed from the older version of SXML Tools
; Filter the 'fragments'
; The fragments are a list of strings, characters,
; numbers, thunks, #f -- and other fragments.
; The function traverses the tree depth-first, and returns a list
; of strings, characters and executed thunks, and ignores #f and '().
;
; If all the meaningful fragments are strings, then
;  (apply string-append ... )
; to a result of this function will return its string-value 
;
; It may be considered as a variant of Oleg Kiselyov's SRV:send-reply:
; While SRV:send-reply displays fragments, this function returns the list 
; of meaningful fragments and filter out the garbage.
(define (sxml:clean-feed . fragments)
  (reverse
  (let loop ((fragments fragments) (result '()))
    (cond
      ((null? fragments) result)
      ((not (car fragments)) (loop (cdr fragments) result))
      ((null? (car fragments)) (loop (cdr fragments) result))
      ((pair? (car fragments))
        (loop (cdr fragments) 
	      (loop (car fragments) result)))
 ;      ((procedure? (car fragments))
 ;         (loop (cdr fragments) 
 ;              (cons ((car fragments))
 ;	       result)))
      (else
        (loop (cdr fragments) 
	      (cons (car fragments) result)))))))

;-----------------------------------------------------------------------------
; This functions will be probably moved to sxml-tools

; Transforms top-level *NAMESPACES* in SXML document 
; parsed using SSAX 4.9 to aux-list representation compatible to
; SXML-spec. ver. 2.5
(define (sxml:refactor-ns tree)
  (if (and (pair? tree) (pair? (cdr tree)) (pair? (cadr tree))
	   (eq? '*NAMESPACES* (caadr tree)))
    `(,(car tree) (@@ (*NAMESPACES* ,@(cdadr tree))) ,@(cddr tree))
    tree))
 
; Reads XML document as SXML tree. NS prefixes declared in XML document
; are used as namespace-id's.
(define (sxml:xml->sxml-autoprefix name)
  (sxml:refactor-ns ; workaround for SSAX 4.9
    (let ((ns-list (sxml:extract-prefix-assigs name)))  
    (ssax:xml->sxml
      (open-input-resource name)
      ns-list))))

; Extracts a value of attribute with given name from attr-list
;(define (sxml:attr-from-list attr-list name)
;	    (cond 
;	      ((assq name attr-list) 
;	       => cadr)
;	      (else #f)))

; Reads a root element of given XML file and returns a list
; of NS-prefix/URIs declared as a list of pairs.
(define (sxml:extract-prefix-assigs file)
  (call-with-input-file
      file
    (lambda (p)
      (ssax:skip-S p)
      (let loop ((lst (ssax:read-markup-token p)))
        (case (car lst)
          ((PI)  ; Processing instruction           
           (ssax:skip-pi p)  ; ignore until the end           
           (ssax:skip-S p)
           (loop (ssax:read-markup-token p)))
          ((START)
           (filter-and-map
            (lambda(x)
              (and (pair? (car x)) (eq? 'xmlns (caar x))))
            (lambda(x)
              (cons (cdar x) (cdr x)))
            (ssax:read-attributes p '())))
          (else
           (display "Unknown token type: ")
           (display (car lst))
           (exit)))))))


;=============================================================================
; Tree transformation

; stx:apply-templates:: <tree> x <templates> x <root> x <environment> -> <new-tree>
; where
; <templates> ::= <default-template> <text-template> <template>*
; <default-template> ::= (*default* . <handler>)
; <text-template> ::= (*text* . <handler>)
; <template>  ::= (<matcher> <handler>) | ( XMLname <handler>)
; <root>     ::= <document-root>
; <environment> ::= <lambda-tuple>
; <matcher>  ::= <node> <root> -> <nodeset>
; <handler> :: <node> <templates> <root> <environment> -> <new-node>
;
; The stx:apply-templates function visits top-level nodes of a given tree and 
; process them in accordance with a list of templates given. 
; If a node is a textual one then it is processed usind 'text-template',
; which has to be second element in given list of templates. 
; If a node is a pair then stx:apply-templates looks up a corresponding template
; among  given <templates> using stx:find-template function. 
; If failed, stx:apply-templates tries to locate a *default* template, 
; which has to be first element in given list of templates. It's an
; error if this latter attempt fails as well.  
; Having found a template, its handler is applied to the current node. 
; The result of the handler application, which should
; also be a <tree>, replaces the current node in output tree.
;
; This function is slightly similar to Oleg Kiselyov's "pre-post-order" function
; with *preorder* bindings. 
(define (stx:apply-templates tree templates root environment)
  (cond
    ((nodeset? tree)
     (map (lambda (a-tree) 
	    (stx:apply-templates a-tree templates root environment)) 
	  tree))
    ((pair? tree) 
     (cond
       ((tee-4 "Template: " 
	(stx:find-template tree 
		      (cddr templates) ; *default* and *text* skipped
		      root)) 
	=> (lambda (template) 
	     ((cadr template) tree templates root environment)))
       (else 
	 (if (eq? '*default* (caar templates))
	   ((cadar templates) tree templates root environment)
	   (stx:error "stx:apply-templates: There is no template in: " templates
		      ; nl "for: " tree
		      )) 
	 )))
    ((string? tree) ; *text*
	 (if (eq? '*text* (caadr templates))
	   ((cadadr templates) tree)
	   (stx:error "stx:apply-templates: There is no *text* templates for: " 
		      templates))) 
    (else (stx:error "Unexpected type of node: " tree))))

;  stx:find-template: <node> x <templates> x <root> -> <template>
;  This function returns first template in <templates> whouse <matcher>
;  matches given <node>
;  <matcher> matches node if:
;    - if it is a symbol and its the same as the name of the node matched
;    - if it is a procedure (sxpath/txpath generated one) then it is 
;     applyed (with respect to given <root>) sequentially to the matched node 
;     and its parents until the matched node is a member of a resulting nodeset 
;     or root node is reached. In the first case the node matches successfuly, 
;     in the second case it does not. 
(define (stx:find-template node templates root)
  (let ((pattern-matches? 
	  (lambda (node pattern-test) 
	    (let rpt ((context-node node))
	      (cond 
		((null? context-node) #f)
		((memq node (pattern-test context-node
                                          `((*root* ,root))))
		 #t)
		(else ; try PARENT
		  (rpt ((sxml:node-parent root) context-node))))))))  
    (let rpt ((bnd templates)) 
      (cond ((null? bnd) #f)
	    ((and (symbol? (caar bnd)) (eq? (caar bnd) (car node)))
	     (car bnd))
	    ((and (procedure? (caar bnd)) ; redundant?
		  (pattern-matches? node (caar bnd)))
	     (car bnd))
	    (else (rpt (cdr bnd)))))))

; Returns SXML tree for a given link.
; A link is a lambda-tuple of its attributes.
(define (stx:load-sst link)
 ((cond 
    ((equal? (link 'type) "stx")
       (lambda(x)
	 (call-with-input-file x read)))
    ((equal? (link 'type) "sxml")
     (stx:make-stx-stylesheet
       (lambda(x)
	 (call-with-input-file x read))))
    ; default is xml
    (else 
       (lambda(x)
         (stx:make-stx-stylesheet
          (sxml:xml->sxml-autoprefix x)))))
   (link 'href)))

; Transform top-level objects of stx:stylesheet 
; to a list whose car is corresponding template (#f no templates generated) 
; and whose cadr is corresponding environment binding (#f if no bindings),
(define (stx:stx->tmpl+env stx-objects)
 (let rpt ((objs stx-objects)
	   (templts '())
	   (envrt '()))
  (cond
    ((null? objs) (list (reverse templts)
			envrt))
    ; templates
    ((eq? (caar objs) 'stx:template)
     (let* ((obj (car objs))
	(handler (caddr obj)))
       (rpt 
	 (cdr objs)
	 (cond 
	   ((sxml:attr obj 'match) 
	    => (lambda (x) 
		 (cons 
		   (list `(sxpath ,x) handler)
		   ;(list `(sxp:xpath+root ,x) handler)
		   templts)))
	   ((sxml:attr obj 'match-lambda)
	    => (lambda (x) 
		 (cons (list x handler) templts)))
	   (else 
	     (verb-2 nl "NO match for: " (cadr obj))
	     templts))
	 (cond 
	   ((sxml:attr obj 'name)
	    => (lambda (x) 
		 (cons  
		   (list (string->symbol x) handler) envrt)))
	   (else 
	     (verb-2 nl "NO name for: " (cadr obj) 
		     "==" (sxml:attr obj 'name))
	     envrt)))))
    ((eq? (caar objs) 'stx:variable)
      (let* ((obj (car objs))
	     (name (sxml:attr obj 'name)) 
	     (code  (caddr obj)))              ; (sxml:content obj)
	(rpt 
	(cdr objs)
	templts
	(cons (list (string->symbol name) code) envrt))))
    (else
      (verb-2 nl "Unrecognized object: " (caar objs) nl)
      (rpt (cdr objs) templts envrt)))))

;
(define (stx:write-ss t+e fname)
  (let* ((of
          ; DL: replacing this non-R5RS call with the following piece of code
          ;(open-output-file fname 'replace)
          (begin 
            (when (file-exists? fname) (delete-file fname))
            (open-output-file fname)))
	 (wrt (lambda x 
		(for-each (lambda(y) (display y of)) x))))
    (wrt    "#cs(module transform mzscheme" nl 
            "(require" nl
            "(rename (lib \"list.ss\") sort mergesort)" nl
            "(lib \"stx-engine.ss\" \"sxml\")" nl
            "(lib \"util.ss\" \"ssax\")" nl 
            "(lib \"txpath.ss\" \"sxml\")" nl
            "(lib \"sxpath-ext.ss\" \"sxml\")" nl 
            "(lib \"sxml-tools.ss\" \"sxml\")" nl
            "(lib \"sxpathlib.ss\" \"sxml\")" nl
            "(lib \"libmisc.ss\" \"sxml\")" nl
            "(lib \"myenv.ss\" \"ssax\")" nl
            "(lib \"common.ss\" \"sxml\"))" nl
	    "(provide stylesheet)" nl)
    
    (wrt nl "(define stylesheet (list " nl "(list ; templates:")
  (for-each
    (lambda(x)
      (wrt nl "(list ") 
      (pp (car x) of)
      (wrt "") 
      (pp (cadr x) of)
      (wrt ")" nl))
    (car t+e))
    (wrt ") ; end templates" nl nl "( list ; environment:")
  (for-each
    (lambda(x)
      (wrt nl "(list '" (car x) nl) 
      (pp (cadr x) of)
      (wrt ") ; end of `" (car x) "'" nl))
    (cadr t+e))
    (wrt ")  ; end environment" nl)
    (wrt ")) ; end stylesheet" nl)
    (wrt ")" nl) ; end module 
  ))

; transformate given SXML document <doc> using stylesheet <sst> in SXML
; format
(define (stx:transform-dynamic doc sst-sxml)
  (stx:transform
    doc
    (stx:eval-transformer
      (stx:translate sst-sxml))))

; transformate given SXML document <doc> loading prepared stylesheet from
; a file <sst-file> 
; DL: commented out because of DYNAMIC-REQUIRE
;(define (stx:transform-static doc sst-file) 
;  (stx:transform
;    doc 
;    (dynamic-require sst-file 'stylesheet)))

; writes to a file <sst-file> prepared stylesheet given in SXML format <sst>
(define (stx:write-transformer sst file)
     (stx:write-ss (stx:translate sst) file))

; evalutes components  of given (in Scheme) STX stylesheet and returns a "prepared"
; stylesheet where all the necessary S-expressions are evaluated 
(define (stx:eval-transformer sst-scm)
  (list 
	(map 
	  (lambda(x)
	    (list (eval (car x))
		  (eval (cadr x))))
	  (car sst-scm))
	(map 
	  (lambda(x)
	    (list (car x)
		  (eval (cadr x))))
	  (cadr sst-scm))))
    

; transforms given SXML document <doc> using prepared (compiled or eval'uated)
; stylesheet <sst-lambda>
(define (stx:transform doc sst-lambda)
  (let ((string-out sxml:sxml->html))
   (stx:apply-templates doc
      ; bindings
      (tee-3
	"Templates: "
       (append
	 `((*default* 
	       ,(lambda (node bindings root environment)
		   (stx:apply-templates (sxml:content node) 
				      bindings 
				      root environment)))
	   (*text* 
	      ,string-out))
	  (car sst-lambda)))
      ;root 
      doc
      ; environment
      (apply 
	lambda-tuple
	(tee-3 
	  "Environment: "
	  (append 
	    `((stx:version ,stx:version))
	  (cadr sst-lambda) 
	    ))))))
  
; translate given STX stylesheet <sst> from SXML to Scheme 
(define (stx:translate sst)
  (let* 
    (
 ;     (output-attr
 ;       ; lambda tuple of 'xsl:output' attributes
 ;       (apply lambda-tuple
 ;	      (cond
 ;		(((if-car-sxpath '(xsl:stylesheet xsl:output @)) sst)
 ;		 =>  cdr)
 ;		(else '((method "html"))))))
 ;     ((string-out)
 ;      (case (string->symbol (output-attr 'method))
 ;	((text) self)
 ;	((xml)  sxml:string->xml)
 ;	(else   sxml:sxml->html)))
     (stx-sst
       (tee-2
	 "STX stylesheets: "
	 (append sst
		 (apply append
			(map
			  (lambda(x)
			    (tee-2 
			      "IMPORTED: " 
			      ; just stx:template and stx:variable elements
			      ; are used from imported _STX_ stylesheets
			      ((sxpath '((*or* stx:template stx:variable))) 
			       (stx:load-sst (apply lambda-tuple 
						    (sxml:attr-list x))))))
			  ((sxpath '((*or* xsl:import stx:import))) sst))))))
     (templates+env
       (tee-2
	 "templates+env"
	 (stx:stx->tmpl+env
	  ((sxpath '(*)) stx-sst))))
     )
   templates+env))

; Generates an stx:stylesheet from a stylesheet represented as <stx-tree>
; in SXML format
(define (stx:make-stx-stylesheet stx-tree)
  (let* 
    ((output-attr
       ; lambda tuple of 'xsl:output' attributes
       (apply lambda-tuple
	      (cond
		(((if-car-sxpath '(xsl:stylesheet xsl:output @)) stx-tree)
		 =>  cdr)
		(else '((method "html"))))))
     (string-out
      (case (string->symbol (output-attr 'method))
	((text) 'self)
	((xml)  'sxml:string->xml)
	(else   'sxml:sxml->html)))
     (custom-prefixes 
       (map string->symbol
	    ((sxpath '(xsl:stylesheet stx:import @ prefix *text*))
	     stx-tree))))
     (cons 'stx:stylesheet
   (map
      (lambda(x)
	(cond 
	  ; xsl:template
	  ((eq? (car x) 'xsl:template)
	   (stx:xsl->stx x output-attr string-out custom-prefixes
			 stx-tree))
	  ; stx:template
	  ((eq? (car x) 'stx:template)
	   (stx:scm->stx x))
	  ; stx:variable
	  ((eq? (car x) 'stx:variable)
	   (stx:stx-var->stx x))
	  ; xsl:variable
	  ((eq? (car x) 'xsl:variable)
	   (stx:xsl-var->stx x))
	  (else x)))
      ((sxpath `(xsl:stylesheet *)) stx-tree)))
    ))

;-----------------------------------------------------------------------------
; Transformers for XSL stylesheet elements

; Transforms element stx:template (extracted from XSL stylesheet and
; represented in SXML format) to stx:template
(define (stx:scm->stx tmplt)
  `(stx:template (@ ,@(sxml:attr-list tmplt))
	(lambda (current-node stx:templates current-root $)
		 ,(stx:read-content tmplt "<stx:template@match>"))))

; Transforms STX element stx:var to stx:variable
(define (stx:stx-var->stx var)
  `(stx:variable (@ ,@(sxml:attr-list var))
		 ,(stx:read-content var "<stx:variable")))

; Transforms XSL element xsl:var to stx:variable
(define (stx:xsl-var->stx var)
  `(stx:variable (@ ,@(sxml:attr-list var))
		 ',(sxml:content var)))

(define (stx:attr->html attr)
	 (if (equal? "" (cadr attr))
              `(list " " ,(sxml:ncname attr))
              `(list " " ,(sxml:ncname attr) "='" ,(cadr attr) "'")))

; transforms an xsl:template to stx:template
(define (stx:xsl->stx tmplt output-attr doc-string-out 
		       custom-prefixes c-root)
  (let* 
    ; list of template's attributes
    ((attr-list (sxml:attr-list tmplt))
     (sst-method (cond 
		   ((sxml:attr-from-list attr-list 'stx:method)
		    => string->symbol)
	           ; xml is default
		   (else 'xml)))
     ; output string for _template_  (not document!) conversion
     (sst-string-out
       (case sst-method
	 ((text) self)
	 ((html)  sxml:string->html)
	 (else   sxml:sxml->xml))))

  `(stx:template (@ ,@attr-list)
	(lambda (current-node stx:templates current-root $)
	  ,(cons 'list
	  ;(stx:clean-feed
	  (stx:apply-templates 
	    (sxml:content tmplt)
	    `((*default* 
		 ,(lambda (tt-node bindings root environment)
		     (if (cond ((sxml:name->ns-id (car tt-node)) 
				=> (lambda(x) 
				     (member (string->symbol x)
					     custom-prefixes)))
			       (else #f))	 
		       ; user-defined tag 
		       `(stx:call-function ,(sxml:ncname tt-node) 
					   "Custom Tag"
					   ,tt-node
					   $)
		       ; XHTML tag
		       (let ((nm (sxml:ncname tt-node))
			     (content (sxml:content tt-node)))
			 (if (null? content)
			   `(list "<" ,nm ,@(map stx:attr->html 
					   (sxml:attr-list tt-node)) "/>")
			   `(list "<" ,nm ,@(map stx:attr->html 
					   (sxml:attr-list tt-node)) ">"
			     ,@(stx:apply-templates content 
						    bindings root environment)
			     "</" ,nm ">" ))))
		     ))
	      (*text*  ; text string in template 
		 ,sst-string-out)
	      (xsl:apply-templates ; t-node is <xsl:apply-templates/>
		 ,(lambda (t-node bindings root environment) 
		     `(stx:apply-templates
			,(cond 
			   ((sxml:attr t-node 'select)
			    => (lambda (lp)
				 `((sxpath ,lp) current-node current-root)
				 ;`((sxp:xpath+root ,lp) current-node current-root)
				 ))
			   (else '(sxml:content current-node)))
			stx:templates current-root $)))
              (xsl:if
                ,(lambda (t-node bindings root environment)
			       ``(stx:apply-templates
                                 ,(sxml:content ,t-node)
                                 bindings
                                 root
                                 environment)
                             ))
	      (xsl:call-template ; t-node is <xsl:call-template/>
		 ,(lambda (t-node bindings root environment) 
		     `(stx:call-function ,(sxml:attr t-node 'name)
					"Named Template"
					,t-node
					$)))
	      (xsl:value-of  ; t-node is <xsl:value-of/>
		 ,(lambda (t-node bindings root environment) 
		     `(,(if (equal? "yes" 
				      (sxml:attr t-node 'disable-output-escaping))
			   'self
			   doc-string-out)
			(sxml:string 
			   ((sxpath ,(sxml:attr t-node 'select)) 
			   ;((sxp:xpath ,(sxml:attr t-node 'select)) 
			    current-node))))) 
	      ; <xsl:copy-of/>
	      (xsl:copy-of  
		 ,(lambda (t-node bindings root environment) 
		     `((sxpath ,(sxml:attr t-node 'select)) current-node))) 
		     ;`((sxp:xpath ,(sxml:attr t-node 'select)) current-node))) 
	      (stx:eval      ; t-node is <stx:eval/>
		 ,(lambda (t-node bindings root environment)  
		     (let ((content
			     (stx:read-content t-node "<stx:eval>")))
		     `(call-with-err-handler
		       (lambda()
			 (eval ,content))
		       (lambda(mes)
			 (apply stx:error `("Error " ,nl ,mes ,nl 
					    "while evaluating code:" ,nl 
					    ,,content 
					    ,nl "from element" ,nl 
					    ,@(sxml:clean-feed 
						(sxml:sxml->xml
						  ',t-node)))))))))
	      ) c-root 
	    (lambda-tuple) ; compile-time environment
	    )))))) 

; A helper for stx:xsl->stx 
(define-macro stx:call-function 
  (lambda (name type tpl-node $-env)
  `(let ((fn (,$-env 
	      (string->symbol ,name))))
    (if 
      (eq? fn '*LT-NOT-FOUND*)
      (apply stx:error 
	     (append 
	      (list "Undefined " ,type  " with name " ,name " is called by:" nl)
	       (sxml:clean-feed (sxml:sxml->xml ',tpl-node))
	       (list nl "Valid names: ") (map car (,$-env))
				))
      (call-with-err-handler
	(lambda()
	  (sxml:clean-feed
 	      (fn current-node stx:templates current-root (,$-env '*LT-ADD* 
						      `(stx:param ,',tpl-node)))))
	(lambda (mes)
	  (apply stx:error 
		 (list ,type " evaluation ERROR" 
		   nl mes nl "for:" nl
		   (sxml:clean-feed 
		     (sxml:sxml->xml ',tpl-node))))))))))

;==============================================================================
; Wrappers

;(define stx:eval eval)
;(define txpath sxp:xpath+root)
    
; NOTE: namespace for vars and templates is shared!

(provide (all-defined)))
