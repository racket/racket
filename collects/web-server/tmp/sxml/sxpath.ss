; Module header is generated automatically
#cs(module sxpath mzscheme
(require (lib "string.ss" "srfi/13"))
(require (lib "ssax.ss" "web-server/tmp/ssax"))
(require "sxml-tools.ss")
(require "sxpathlib.ss")
(require "sxpath-ext.ss")
(require "txpath.ss")
(require "xpath-parser.ss")

;; $Id: sxpath.scm,v 1.5 2005/09/07 09:27:34 lizorkin Exp $
;; Highghest level SXPath 
;; Refactored from sxml-tools.scm and sxpathlib.scm

;==============================================================================
; Abbreviated SXPath

; Evaluate an abbreviated SXPath
;	sxpath:: AbbrPath -> Converter, or
;	sxpath:: AbbrPath -> Node|Nodeset -> Nodeset
; AbbrPath is a list. It is translated to the full SXPath according
; to the following rewriting rules
; (sxpath '()) -> (node-join)
; (sxpath '(path-component ...)) ->
;		(node-join (sxpath1 path-component) (sxpath '(...)))
; (sxpath1 '//) -> (sxml:descendant-or-self sxml:node?)
; (sxpath1 '(equal? x)) -> (select-kids (node-equal? x))
; (sxpath1 '(eq? x))    -> (select-kids (node-eq? x))
; (sxpath1 '(*or* ...))  -> (select-kids (ntype-names??
;                                          (cdr '(*or* ...))))
; (sxpath1 '(*not* ...)) -> (select-kids (sxml:complement 
;                                         (ntype-names??
;                                          (cdr '(*not* ...)))))
; (sxpath1 '(ns-id:* x)) -> (select-kids 
;                                      (ntype-namespace-id?? x))
; (sxpath1 ?symbol)     -> (select-kids (ntype?? ?symbol))
; (sxpath1 ?string)     -> (txpath ?string)
; (sxpath1 procedure)   -> procedure
; (sxpath1 '(?symbol ...)) -> (sxpath1 '((?symbol) ...))
; (sxpath1 '(path reducer ...)) ->
;		(node-reduce (sxpath path) (sxpathr reducer) ...)
; (sxpathr number)      -> (node-pos number)
; (sxpathr path-filter) -> (filter (sxpath path-filter))
(define (sxpath path . ns-binding)
  (let ((ns-binding (if (null? ns-binding) ns-binding (car ns-binding))))
  (let loop ((converters '())
             (root-vars '())  ; a list of booleans, one per location step:
	                      ;  #t - location step function is binary
                              ;  #f - location step function is unary
             (path (if (string? path) (list path) path)))
    (cond
      ((null? path)  ; parsing is finished
       (lambda (node . var-binding)
         (let ((var-binding
                (if (null? var-binding) var-binding (car var-binding))))
           (let rpt ((nodeset (as-nodeset node))
                     (conv (reverse converters))
                     (r-v (reverse root-vars)))
             (if
              (null? conv)  ; the path is over
              nodeset
              (rpt
               (if (car r-v)  ; the current converter consumes 2 arguments
                   ((car conv) nodeset var-binding)
                   ((car conv) nodeset))
               (cdr conv)
               (cdr r-v)))))))
      ; *or* handler
      ((and (pair? (car path)) 
            (not (null? (car path)))
            (eq? '*or* (caar path)))
       (loop (cons (select-kids (ntype-names?? (cdar path))) converters)
             (cons #f root-vars)
             (cdr path)))
      ; *not* handler 
      ((and (pair? (car path)) 
            (not (null? (car path)))
            (eq? '*not* (caar path)))
       (loop (cons
              (select-kids (sxml:complement (ntype-names?? (cdar path))))
              converters)
             (cons #f root-vars)
             (cdr path)))
      ((procedure? (car path))
       (loop (cons (car path) converters)
             (cons #t root-vars)
             (cdr path)))
      ((eq? '// (car path))
       (if (or (null? (cdr path))
               (not (symbol? (cadr path)))
               (eq? (cadr path) '@))
           (loop (cons (sxml:descendant-or-self sxml:node?)
                       converters)
                 (cons #f root-vars)
                 (cdr path))
           (loop (cons (sxml:descendant (ntype?? (cadr path)))
                       converters)
                 (cons #f root-vars)
                 (cddr path))))
      ((symbol? (car path))
       (loop (cons (select-kids (ntype?? (car path))) converters)
             (cons #f root-vars)
             (cdr path)))
      ((string? (car path))
       (and-let*
        ((f (sxml:xpath-expr (car path) ns-binding)))  ; DL: was: txpath
        (loop (cons f converters)
              (cons #t root-vars)
              (cdr path))))
      ((and (pair? (car path)) (eq? 'equal? (caar path)))
       (loop (cons (select-kids (apply node-equal? (cdar path))) converters)
             (cons #f root-vars)
             (cdr path)))
      ; ns-id:* handler 
      ((and (pair? (car path)) (eq? 'ns-id:* (caar path)))
       (loop
        (cons (select-kids (ntype-namespace-id?? (cadar path))) converters)
        (cons #f root-vars)
        (cdr path)))
      ((and (pair? (car path)) (eq? 'eq? (caar path)))
       (loop (cons (select-kids (apply node-eq? (cdar path))) converters)
             (cons #f root-vars)
             (cdr path)))      
      ((pair? (car path))
       (and-let*
        ((select
          (if
           (symbol? (caar path))
           (lambda (node . var-binding)
             ((select-kids (ntype?? (caar path))) node))
           (sxpath (caar path) ns-binding))))
        (let reducer ((reducing-path (cdar path))
                      (filters '()))
          (cond
            ((null? reducing-path)
             (loop
              (cons
               (lambda (node var-binding)
                 (map-union
                  (lambda (node)
                    (let label ((nodeset (select node var-binding))
                                (fs (reverse filters)))
                      (if
                       (null? fs)
                       nodeset
                       (label
                        ((car fs) nodeset var-binding)
                        (cdr fs)))))
                  (if (nodeset? node) node (list node))))
               converters)
              (cons #t root-vars)
              (cdr path)))
            ((number? (car reducing-path))
             (reducer
              (cdr reducing-path)
              (cons
               (lambda (node var-binding)
                 ((node-pos (car reducing-path)) node))
               filters)))
            (else
             (and-let*
              ((func (sxpath (car reducing-path) ns-binding)))
              (reducer
               (cdr reducing-path)
               (cons
                (lambda (node var-binding)
                  ((sxml:filter
                    (lambda (n) (func n var-binding)))
                    node))
                filters))))))))
      (else
       (cerr "Invalid path step: " (car path))
       #f)))))


;==============================================================================
; Wrappers 

; sxpath always returns a list, which is #t in Scheme 
; if-sxpath returns #f instead of empty list
(define (if-sxpath path)
  (lambda (obj)
   (let ((x ((sxpath path) obj)))
     (if (null? x) #f x))))

; Returns first node found, if any.
; Otherwise returns #f.
(define (if-car-sxpath path)
  (lambda (obj)
   (let ((x ((sxpath path) obj)))
     (if (null? x) #f (car x)))))

; Returns first node found, if any.
; Otherwise returns empty list.
(define (car-sxpath path)
  (lambda (obj)
   (let ((x ((sxpath path) obj)))
     (if (null? x) '() (car x)))))

;==============================================================================
; lookup by a value of ID type attribute
; See also sxml:lookup in sxml-tools

; Built an index as a list of (ID_value . element) pairs for given
; node. lpaths are location paths for attributes of type ID.
(define (sxml:id-alist node . lpaths)
  (apply
    append
    (map 
      (lambda(lp)
	(let ((lpr (reverse lp)))
	  (map 
	    (lambda (nd)
	      (cons (sxml:attr nd (car lpr))
		    nd))
	    ; Selects elements with ID attributes
	    ;  using (lpath ,(node-self (sxpath '(@ attrname))))
	    ((sxpath (reverse (cons 
			  (lambda(n r+v)
			   ((node-self (sxpath `(@ ,(car lpr)))) n))
				(cddr lpr)))) node))   
	  ))
      lpaths)))


(provide (all-defined)))
