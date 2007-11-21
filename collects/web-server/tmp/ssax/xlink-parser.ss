; Module header is generated automatically
#cs(module xlink-parser mzscheme
(require "common.ss")
(require "myenv.ss")
(require "util.ss")
(require (lib "string.ss" "srfi/13"))
(require "access-remote.ss")
(require "sxpathlib.ss")

;; Parser for XML documents that contain XLink elements
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin
;
; Returns an SXML presentation for a document plus additional information
; extracted from XLink markup (described below)
;
; 'SSAX:XML->SXML+xlink' function is the core of the programme. This funcion
; is a modified Oleg Kiselyov's 'SSAX:XML->SXML' function.
; 'SSAX:XML->SXML+xlink' has a complicated seed which consists of ten elements:
;  xlink:seed = (list  mode  sxlink-arcs  sxpointer  stack
;                      locators+resources  arcs  declared-labels)
; 
; 1. mode = 'general, 'extended or 'none. They have the following meaning:
;  - 'general - there are no XLink elements among current element's ancestors.
; So, 'extended' or 'simple' elements are expected (others don't have any XLink
; semantical meaning)
;  - 'extended - for elements that are direct children of an extended link
; element, i.e. 'locator', 'resource' or 'arc'
;  - 'none - no XLink elements are expected niether in the current element
; nor in any of its descendants
;
; 2. sxlink-arcs - contains information extracted from XLink elements. 
;  sxlink-arcs = (list  sxlink-arc
;                          sxlink-arc
;                          ...)
;  sxlink-arc - as defined in the SXLink Specification
;
; 3. Reverse S-expression representation for XPointer ChildSeq for a currently
; processed element
;  sxpointer ::= (listof number)
; For example, '(5 4 1) corresponds to "/1/4/5"  
;
; 4. stack - a list of stack-elements. This list has the following semantics:
; - new stack-element is added when the beginning of each element is processed
; - the stack-element is consumed at the finish-element (of the same element)
;  stack = (list  stack-element
;                 stack-element
;                 ...)
;  stack-element = (list  position  xlink-values)
;  position - a position within a file
;  xlink-values = (list  type  href  role  arcrole  show  actuate  label  from  to)
; where, for example, 'type' is the value of xlink:type attribute or #f if 
; there is no such attribute
;
; The other parameters of the seed are presented when an extended link is
; processed
;
; 5. locators+resources - locator and resource elements defined within an
; extended link. They are temporarily stored in this parameter. This info
; is converted into an 'sxlink-arcs' parameter when the end-tag for an
; extended link element is encountered
;  locators+resources = (list  locator-or-resource
;                              locator-or-resource
;                              ...)
;  locator-or-resource = (list  uri  fragment  role  label
;                               position  element)
;  label - a string representing the value of xlink:label attribute, or #f if
; this attribute was omitted 
;
; 6. arcs - information about arce defined within an extended link. This info
; is converted into an 'sxlink-arcs' parameter when the end-tag for an
; extended link element is encountered
;  arcs = (list  arc-info
;                arc-info
;                ...)
;  arc-info = (list  arcrole  show  actuate  from  to
;                    position  element)
;  from - a string representing the value of xlink:from attribute, or #f if
; this attribute was omitted 
;  to - the same for an xlink:to attribute
;
; 7. declared-labels - labels declared within an extended link. This parameter
; is used for constraint checking
;  declared-labels = (list  label  label ...)
;  label - a string

; Some global constants
(define xlink:namespace-uri 'http://www.w3.org/1999/xlink)
(define xlink:linkbase-uri "http://www.w3.org/1999/xlink/properties/linkbase")

;=========================================================================
; A 'seed' datatype
;  xlink:seed = (list  mode  sxlink-arcs  sxpointer  stack
;                      locators+resources  arcs  declared-labels)
; The last three parameters are optional. See a head comment for details

;------------------------------------------------
; Two constructors for a seed
; They are introducted in order to control (possible) future modifications of
; a 'seed' list

; This function constructs a seed consisting only of six compulsory elements
(define (xlink:make-small-seed mode sxlink-arcs sxpointer stack)
  (list mode sxlink-arcs sxpointer stack))

; The similar function which makes a full-length seed
(define (xlink:make-full-seed mode sxlink-arcs sxpointer stack
                              locators+resources arcs declared-labels)
  (list mode sxlink-arcs sxpointer stack
        locators+resources arcs declared-labels))

;------------------------------------------------
; Accessor functions

(define (xlink:seed-mode seed)
  (car seed))
(define (xlink:seed-sxlink-arcs seed)
  (cadr seed))
(define (xlink:seed-sxpointer seed)
  (list-ref seed 2))
(define (xlink:seed-stack seed)
  (list-ref seed 3))

; We assume that the seed has the full length for the latter four functions
(define (xlink:seed-locators+resources seed)
  (list-ref seed 4))
(define (xlink:seed-arcs seed)
  (list-ref seed 5))
(define (xlink:seed-declared-labels seed)
  (list-ref seed 6))


;=========================================================================
; Here basic functions for special datatypes are collected

;------------------------------------------------
; 2. 'sxlink-arcs' datatype

; Adds the arc defined by the XLink simple link to 'sxlink-arcs'
(define (xlink:add-simple
         xlink-values element position sxpointer sxlink-arcs)
  (let ((href (xlink:values-href xlink-values))
        (role (xlink:values-role xlink-values))
        (arcrole (xlink:values-arcrole xlink-values))
        (title (xlink:values-title xlink-values))
        (show (xlink:values-show xlink-values))
        (actuate (xlink:values-actuate xlink-values)))
    (if
     (not href)   ; the link is untraversable
     sxlink-arcs  ; no arc added
     (call-with-values
      (lambda ()
        (let ((lst (string-split href (list #\#) 2)))
          (cond
            ((= (length lst) 1)  ; no XPointer fragment identifier
             (values (car lst) #f))
            ((= (string-length (car lst)) 0)  ; addresses the same document
             (values #f (cadr lst)))
            (else
             (values (car lst) (cadr lst))))))
      (lambda (uri-ending fragment)
        (cons
         `(,(if (equal? arcrole xlink:linkbase-uri)
                'linkbase 'simple)
           (from
            (uri)  ; goes from this document
            (nodes ,element)
            (xpointer ,(xlink:sxpointer->childseq sxpointer)))
           (to
            (uri ,@(if uri-ending (list uri-ending) '()))
            ,@(if fragment `((xpointer ,fragment)) '())
            ,@(if role `((role ,role)) '())
            ,@(if title `((title ,title)) '()))
           ,@(if arcrole `((arcrole ,arcrole)) '())
           ,@(if show `((show ,show)) '())
           ,@(if actuate `((actuate ,actuate)) '())
           (declaration
            (uri)  ; in this document
            (nodes ,element)
            (xpointer ,(xlink:sxpointer->childseq sxpointer))
            (position ,position)))
         sxlink-arcs))))))

; This function appends information to 'sxlink-arcs' according to
; 'locators+resources' and 'arcs' parameters.
; The function is called at the end-tag of an extended link element.
(define (xlink:add-extended
         locators+resources arcs sxlink-arcs declaration)
  (let (; like map, but applies the function to each pair of the arguments
        (map-join
         (lambda (func arg-lst1 arg-lst2)
           (let ((arg-lst1 (reverse arg-lst1)))
             (let iterate-second ((lst2 (reverse arg-lst2))
                                  (res '()))
               (if
                (null? lst2)  ; everyone processed
                res
                (let iterate-first ((lst1 arg-lst1)
                                    (res res))
                  (if
                   (null? lst1)  ; the iteration loop finished
                   (iterate-second (cdr lst2) res)
                   (iterate-first
                    (cdr lst1)
                    (cons (func (car lst1) (car lst2)) res)))))))))
        ; a stub for determining whether a locator-or-resouces is a local
        ; or remote one
        (resource?
         (lambda (locator-or-resource)
           ; Resource iff info contains subelement 'nodes
           (assq 'nodes (xlink:resource-data locator-or-resource)))))    
    (let loop ((arcs arcs)
               (sxlink-arcs sxlink-arcs))
      (if
       (null? arcs)  ; all arcs processed
       sxlink-arcs
       (loop
        (cdr arcs)
        (let ((arc-info (car arcs)))
          (append
           (map-join
            (lambda (starting ending)
              `(,(cond  ; determining arc name
                   ((xlink:arc-info-linkbase arc-info)                    
                    'linkbase)
                   ((and (resource? starting)
                         (not (resource? ending)))
                    'outbound)
                   ((and (not (resource? starting))
                         (resource? ending))
                    'inbound)
                   ((and (resource? starting) (resource? ending))
                    'local-to-local)
                   (else
                    'third-party))
                (from ,@(xlink:resource-data starting))
                (to ,@(xlink:resource-data ending))
                ,@(xlink:arc-info-data arc-info)
                ,declaration))
            (let ((from (xlink:arc-info-from arc-info)))
              (if
               (not from)  ; arc outgoes from every resource
               locators+resources
               (filter
                (lambda (locator-or-resource)
                  (equal? from
                          (xlink:resource-label locator-or-resource)))
                locators+resources)))
            (let ((to (xlink:arc-info-to arc-info)))
              (if
               (not to)  ; arc comes to every resource
               locators+resources
               (filter
                (lambda (locator-or-resource)
                  (equal? to
                          (xlink:resource-label locator-or-resource)))
                locators+resources))))
           sxlink-arcs)))))))

;------------------------------------------------
; 3. 'sxpointer' datatype
; Reverse S-expression representation for XPointer ChildSeq for a currently
; processed element
;  sxpointer ::= (listof number)
; For example, '(5 4 1) corresponds to "/1/4/5"  

(define (xlink:sxpointer->childseq sxpointer)
  (apply
   string-append
   (map
    (lambda (num) (string-append "/" (number->string num)))
    (reverse sxpointer))))

; Forms sxpointer for the following sibling element of the current element
(define (xlink:sxpointer4sibling sxpointer)
  (cons (+ 1 (car sxpointer)) (cdr sxpointer)))

;------------------------------------------------
; 5. 'locators+resources' datatype
; locators+resources - locator and resource elements defined within an
; extended link. They are temporarily stored in this parameter. This info
; is converted into an 'sxlink-arcs' parameter when the end-tag for an
; extended link element is encountered
;  locators+resources = (list  locator-or-resource
;                              locator-or-resource
;                              ...)
;  locator-or-resource = (list  label  resource-data)
;  resource-data - whatever required to describe the resource in terms of
; the SXLink Specification

; Constructor
(define (xlink:make-locator-or-resource label resource-info)
  (list label resource-info))

; Accessors
; NOTE: We don't apply teta-reduction for the sake of easier bug detection
(define (xlink:resource-label locator-or-resource)
  (car locator-or-resource))
(define (xlink:resource-data locator-or-resource)
  (cadr locator-or-resource))

; If the following XLink constraint is fulfilled, adds information about the
; XLink locator element to 'locators+resources'. Otherwise, displays an error
; message and doesn't add anything.
;  Constraint: Attributes on Locator Element
;  The locator-type element must have the locator attribute (see 5.4 Locator
;  Attribute (href)). The locator attribute (href) must have a value supplied.
(define (xlink:add-locator xlink-values position element locators+resources)
  (let ((href (xlink:values-href xlink-values))
        (role (xlink:values-role xlink-values))
        (title (xlink:values-title xlink-values))
        (label (xlink:values-label xlink-values)))
    (cond
      ((not href)
       (xlink:parser-error
        position "locator element doesn't have an xlink:href attribute")
       locators+resources)
      (else
       (let ((lst (string-split href (list #\#) 2)))
         (call-with-values
          (lambda ()
            (cond
              ((= (length lst) 1) (values (car lst) #f))
              ((= (string-length (car lst)) 0) (values #f (cadr lst)))
              (else (values (car lst) (cadr lst)))))
          (lambda (uri fragment)
            (cons
             (xlink:make-locator-or-resource
              label
              `((uri ,@(if uri (list uri) '()))
                ,@(if fragment `((xpointer ,fragment)) '())
                ,@(if role `((role ,role)) '())
                ,@(if title `((title ,title)) '())))
             locators+resources))))))))

; Adds information concerning XLink resource element to 'locators+resources'
(define (xlink:add-resource xlink-values element sxpointer locators+resources)
  (let ((role (xlink:values-role xlink-values))
        (label (xlink:values-label xlink-values))
        (title (xlink:values-title xlink-values)))
    (cons
     (xlink:make-locator-or-resource
      label
      `((uri)
        (nodes ,element)
        (xpointer ,(xlink:sxpointer->childseq sxpointer))
        ,@(if role `((role ,role)) '())
        ,@(if title `((title ,title)) '())))
     locators+resources)))

;------------------------------------------------
; 6. 'arcs' datatype
; arcs - information about arce defined within an extended link. This info
; is converted into an 'sxlink-arcs' parameter when the end-tag for an
; extended link element is encountered
;  arcs = (list  arc-info
;                arc-info
;                ...)
;  arc-info = (list  from  to  linkbase  position  data)
;  linkbase - a boolean: whether a linkbase arc
;  arc-data - whatever required to describe the arc in terms of the SXLink
; Specification

; Constructor
(define (xlink:make-arc-info from to linkbase position data)
  (list from to linkbase position data))

; Accessors
; NOTE: We don't apply teta-reduction for the sake of easier bug detection
(define (xlink:arc-info-from arc-info)
  (car arc-info))
(define (xlink:arc-info-to arc-info)
  (cadr arc-info))
(define (xlink:arc-info-linkbase arc-info)
  (list-ref arc-info 2))
(define (xlink:arc-info-position arc-info)
  (list-ref arc-info 3))
(define (xlink:arc-info-data arc-info)
  (list-ref arc-info 4))

; Adds arc information to 'arcs' datatype. A side effect - checks the following
; XLink constraint:
;  Constraint: No Arc Duplication
;  Each arc-type element must have a pair of from and to xlink-values that does
;  not  repeat the from and to xlink-values (respectively) for any other
;  arc-type element in the same extended link; that is, each pair in a link
;  must be unique.
(define (xlink:add-arc xlink-values position element arcs)
  (let ((arcrole (xlink:values-arcrole xlink-values))
        (title (xlink:values-title xlink-values))
        (show (xlink:values-show xlink-values))
        (actuate (xlink:values-actuate xlink-values))
        (from (xlink:values-from xlink-values))
        (to (xlink:values-to xlink-values)))
    (let loop ((as arcs))
      (if
       (null? as)
       (cons
        (xlink:make-arc-info
         from to
         (equal? arcrole xlink:linkbase-uri)
         position
         `(,@(if arcrole `((arcrole ,arcrole)) '())
           ,@(if title `((title ,title)) '())
           ,@(if show `((show ,show)) '())
           ,@(if actuate `((actuate ,actuate)) '())))
        arcs)
       (let ((from2 (xlink:arc-info-from (car as)))
             (to2 (xlink:arc-info-to (car as))))
         (when
             (and (or (not from) (not from2) (equal? from from2))
                  (or (not to) (not to2) (equal? to to2)))
           (xlink:parser-error position "duplicate arcs - xlink:from"
                        (if from (string-append "=" from) " - omitted")
                        ", xlink:to"
                        (if to (string-append "=" to) " - omitted")))
         (loop (cdr as)))))))

; XLink specification, 5.1.3:
;  If no arc-type elements are provided in an extended link, then by extension
;  the missing from and to xlink-values are interpreted as standing for all the
;  labels in that link.
; Inserts such a default arc if 'arcs' are empty
(define (xlink:add-default-arc element arcs)
  (if (null? arcs)
      (list (xlink:make-arc-info
             #f #f #f
             0  ; position is dummy here, since it will never be used
             '()  ; none of the attributes arcrole, title, show, actuate
             ))
      arcs))

;------------------------------------------------
; 7. 'declared-labels' datatype
; declared-labels - labels declared within an extended link. This parameter
; is used for constraint checking
;  declared-labels = (list  label  label ...)
;  label - a string

; If an xlink:label attribute is presented in 'xlink-values', it's value is added
; to 'declared-labels'. Otherwise, 'declared-labels' remain unchainged
(define (xlink:add-declared-label xlink-values declared-labels)
  (let((label (xlink:values-label xlink-values)))
    (if(not label)
       declared-labels
       (cons label declared-labels))))

; The function checks the following XLink constraint
;  Constraint: label, from, and to xlink-values
;  The value of a label, from, or to attribute must be an NCName. If a value 
;  is supplied for a from or to attribute, it must correspond to the same value
;  for some label attribute on a locator- or resource-type element that appears
;  as a direct child inside the same extended-type element as does the arc-type 
;  element. 
; Error message is displayed if some label was undeclared.
; The function always returns #t.
; It is called at the end-tag of an extended link element
(define (xlink:all-labels-declared arcs declared-labels)
  (let loop ((arcs arcs))
    (if
     (null? arcs)
     #t
     (let((arc-info (car arcs)))
       (let((from (xlink:arc-info-from arc-info))
            (to (xlink:arc-info-to arc-info))
            (position (xlink:arc-info-position arc-info)))       
         (when (and from (not (member from declared-labels)))
           (xlink:parser-error position "label not defined - xlink:from=" from))
         (when (and to (not (member to declared-labels)))
           (xlink:parser-error position "label not defined - xlink:to=" to))
         (loop (cdr arcs)))))))


;=========================================================================
; Some simple functions working with attributes
;  xlink-values = (list
;                   type href role arcrole title show actuate label from to)

;------------------------------------------------
; Trivial constructor and accessor functions
; These functions are used as a level of abstraction

; Constructs a datatype (just a list in a current implementation) which
; contains xlink-values of all xlink-related attributes. For example, 'type'
; is the value of xlink:type attribute or #f if there is no such attribute.
; This datatype will be called 'xlink-values' in the latter text
(define (xlink:construct-xlink-values
         type href role arcrole title show actuate label from to)
  (list type href role arcrole title show actuate label from to))

; Accessors
; NOTE: We don't apply teta-reduction for the sake of easier bug detection
(define (xlink:values-type xlink-values)
  (car xlink-values))
(define (xlink:values-href xlink-values)
  (cadr xlink-values))
(define (xlink:values-role xlink-values)
  (list-ref xlink-values 2))
(define (xlink:values-arcrole xlink-values)
  (list-ref xlink-values 3))
(define (xlink:values-title xlink-values)
  (list-ref xlink-values 4))
(define (xlink:values-show xlink-values)
  (list-ref xlink-values 5))
(define (xlink:values-actuate xlink-values)
  (list-ref xlink-values 6))
(define (xlink:values-label xlink-values)
  (list-ref xlink-values 7))
(define (xlink:values-from xlink-values)
  (list-ref xlink-values 8))
(define (xlink:values-to xlink-values)
  (list-ref xlink-values 9))

;------------------------------------------------
; Functions which read attributes

; The function is given a list called 'attributes' (in SSAX parser). This list
; has the form
;  attributes = (list  attribute  
;                      attribute
;                      ...)
;  attribute = (cons  (cons  namespace-prefix  attribute-name)
;                     attribute-value )
;           or (cons  attribute-name  attribute-value )
;  namespaces - defined in "ssax.scm"
; reads XLink attributes' values and returns a 'xlink-values' datatype
; (the result of 'xlink:construct-xlink-values' function)
(define (xlink:read-attributes attributes namespaces)
  (let loop ((attributes attributes)
             (type #f) (href #f) (role #f) (arcrole #f) (title #f) (show #f)
             (actuate #f) (label #f) (from #f) (to #f))
    (if(null? attributes)  ; the attribute list is over
       (xlink:construct-xlink-values
        type href role arcrole title show actuate label from to)
       (let ((attribute (car attributes)))                  
         (if
          (not (pair? (car attribute)))  ; attribute doesn't have namespace
          (loop (cdr attributes) 
                type href role arcrole title show actuate label from to)
          (let ((namespace-prefix (caar attribute))
                (attribute-name (cdar attribute))
                (attribute-value (cdr attribute)))
            (let ((namespace-uri
                   (let rpt ((ns namespaces))
                     (cond
                       ((null? ns) namespace-prefix)
                       ((equal? (cadar ns) namespace-prefix) (cddar ns))
                       (else (rpt (cdr ns)))))))
              (if
               (not (equal? namespace-uri xlink:namespace-uri))
               (loop (cdr attributes)
                     type href role arcrole title show actuate label from to)
               (case attribute-name
                 ((type) (loop (cdr attributes) attribute-value href role 
                               arcrole title show actuate label from to))
                 ((href) (loop (cdr attributes) type attribute-value role
                               arcrole title show actuate label from to))
                 ((role) (loop (cdr attributes) type href attribute-value 
                               arcrole title show actuate label from to))
                 ((arcrole)
                  (loop (cdr attributes) type href role attribute-value title
                        show actuate label from to))
                 ((title) (loop (cdr attributes) type href role arcrole
                                attribute-value show actuate label from to))
                 ((show) (loop (cdr attributes) type href role arcrole title
                               attribute-value actuate label from to))
                 ((actuate) (loop (cdr attributes) type href role arcrole
                                  title show attribute-value label from to))
                 ((label) (loop (cdr attributes) type href role arcrole title
                                show actuate attribute-value from to))
                 ((from) (loop (cdr attributes) type href role arcrole title
                               show actuate label attribute-value to))
                 ((to) (loop (cdr attributes) type href role arcrole title
                             show actuate label from attribute-value))
                 (else (loop (cdr attributes) type href role arcrole title
                             show actuate label from to)))))))))))

; Reads SXML element's attributes
;  element - an SXML node representing an element
;  ns-prefixes = (list  (list  prefix  namespace-uri)
;                       (list  prefix  namespace-uri)
;                       ...)
;  prefix - a symbol
;  namespace-uri - a string
; An 'xlink-values' datatype is returned
(define (xlink:read-SXML-attributes element ns-prefixes)
  (let ((attr-node ((select-kids (ntype?? '@)) element)))
    (if
     (null? attr-node)  ; no attributes
     (xlink:construct-xlink-values #f #f #f #f #f #f #f #f #f #f)
     (let loop ((attr-list (cdar attr-node))
                (type #f) (href #f) (role #f) (arcrole #f) (title #f)
                (show #f) (actuate #f) (label #f) (from #f) (to #f))
       (if
        (null? attr-list)
        (xlink:construct-xlink-values
         type href role arcrole title show actuate label from to)
        (let ((attribute-name (symbol->string (caar attr-list)))
              (attribute-value (cadar attr-list)))
          (call-with-values
           (lambda ()
             (cond
               ((string-rindex attribute-name #\:)
                => (lambda (pos)
                     (values
                      (string->symbol (substring attribute-name 0 pos))
                      (string->symbol
                       (substring attribute-name (+ pos 1)
                                  (string-length attribute-name))))))
               (else
                (values #f attribute-name))))
           (lambda (prefix local)
             (if
              (not prefix)   ; this is a non-qualified name
              (loop (cdr attr-list)
                    type href role arcrole title show actuate label from to)
              (let ((namespace-uri
                     (cond
                       ((assoc prefix ns-prefixes)
                        => (lambda (pair)
                             (string->symbol (cadr pair))))
                       (else
                        prefix))))
                (if
                 (not (equal? namespace-uri xlink:namespace-uri))
                 (loop (cdr attr-list)
                       type href role arcrole title show actuate label from to)
                 (case local
                   ((type) (loop (cdr attr-list) attribute-value href role 
                                 arcrole title show actuate label from to))
                   ((href) (loop (cdr attr-list) type attribute-value role
                                 arcrole title show actuate label from to))
                   ((role) (loop (cdr attr-list) type href attribute-value
                                 arcrole title show actuate label from to))
                   ((arcrole)
                    (loop (cdr attr-list) type href role attribute-value title
                          show actuate label from to))
                   ((title) (loop (cdr attr-list) type href role arcrole
                                  attribute-value show actuate label from to))
                   ((show) (loop (cdr attr-list) type href role arcrole title
                                 attribute-value actuate label from to))
                   ((actuate) (loop (cdr attr-list) type href role arcrole title
                                    show attribute-value label from to))
                   ((label) (loop (cdr attr-list) type href role arcrole title
                                  show actuate attribute-value from to))
                   ((from) (loop (cdr attr-list) type href role arcrole title
                                 show actuate label attribute-value to))
                   ((to) (loop (cdr attr-list) type href role arcrole title show 
                               actuate label from attribute-value))
                   (else (loop (cdr attr-list) type href role arcrole title show 
                               actuate label from to))))))))))))))

;------------------------------------------------
; These functions check XLink constrains which limit some attributes' xlink-values

; A helper function which is used by the next one
;  value - a value of an attribute (#f if there is no such attribute)
;  valid-xlink-values - a list of xlink-values which are allowed for this attribute
;  attr-name - a string denotating a name of an attribute (for a message)
;  position - position within a file
; Function always returns #t. 
; Side effects: function "cerr"s a message if 'value' is not #f and not within
; 'valid-xlink-values'
(define (xlink:check-helper value valid-xlink-values attr-name position)
  (cond
    ((not value) )  ; a value is #f - a correct situation
    ((not (member value valid-xlink-values))
     (xlink:parser-error position "unexpected attribute value - " 
                  attr-name "=" value))
    (else #t)))

;  xlink-values = (type href role arcrole show actuate label from to)
; where, for example, 'type' is the value of xlink:type attribute or #f if 
; there is no such attribute (this datatype is a result 
; of 'read-xlink-attributes' function)
;  position - position within a file
;
; The function checks the three similar XLink constraints:
;  1. Constraint: type Value
;  The value of the type attribute must be supplied. The value must be one of
;  "simple", "extended", "locator", "arc", "resource", "title", or "none".
;  2. Constraint: show Value
;  If a value is supplied for a show attribute, it must be one of the xlink-values
;  "new", "replace", "embed", "other", and "none".
;  3. Constraint: actuate Value
;  If a value is supplied for an actuate attribute, it must be be one of the 
;  xlink-values "onLoad", "onRequest", "other", and "none".
; 
; The result is always #t
; Side effects - error messages (printed by
; an 'xlink:check-helper' function above)
(define (xlink:check-type-show-actuate-constraints xlink-values position)
  (xlink:check-helper (xlink:values-type xlink-values)
                      '("simple" "extended" "locator" "arc" "resource"
                        "title" "none")
                      "xlink:type"
                      position)
  (xlink:check-helper (xlink:values-show xlink-values)
                      '("new" "replace" "embed" "other" "none")
                      "xlink:show"
                      position)
  (xlink:check-helper (xlink:values-actuate xlink-values)
                      '("onLoad" "onRequest" "other" "none")
                      "xlink:actuate"
                      position))
    

;=========================================================================
; Functions which perform starting and ending actions for XLink elements
; All these functions have the same signature:
;
; (smth-start position xlink-values xlink:seed)
;  position - position within a file
;  xlink-values = (list  type  href  role  arcrole  show  actuate  label  from  to)
; where, for example, 'type' is the value of xlink:type attribute or #f if 
; there is no such attribute
;  xlink:seed = (list  mode  sxlink-arcs  sxpointer  stack
;                      locators+resources  arcs  declared-labels)
; See a head comment for details
;
; (smth-end xlink:parent-seed xlink:seed element)
; element - the SXML presentation of the current element
;
; All the functions return a new 'xlink:seed'

;------------------------------------------------
; A general element
; It is the element which doesn't have any XLink meaning, but its descendants
; might have such a meaning

(define (xlink:general-start position xlink-values seed)
  (let((sxlink-arcs (xlink:seed-sxlink-arcs seed))       
       (sxpointer (xlink:seed-sxpointer seed))
       (stack (cons (list position xlink-values) (xlink:seed-stack seed))))
    (xlink:make-small-seed
     'general sxlink-arcs (cons 1 sxpointer) stack)))

(define (xlink:general-end parent-seed seed element)
  (let ((mode (xlink:seed-mode parent-seed))
        (sxlink-arcs (xlink:seed-sxlink-arcs seed))
        (sxpointer
             (xlink:sxpointer4sibling (xlink:seed-sxpointer parent-seed)))
        (stack (xlink:seed-stack parent-seed)))
    (xlink:make-small-seed mode sxlink-arcs sxpointer stack)))

;------------------------------------------------
; An element and all its descendants don't have any XLink meaning

(define (xlink:none-start position xlink-values seed)
  (let ((stack (cons (list position xlink-values) (xlink:seed-stack seed))))
    (xlink:make-small-seed 'none '() '() stack)))

(define (xlink:none-end parent-seed seed element)
  parent-seed)
  
;------------------------------------------------
; A simple-link element

(define (xlink:simple-start position xlink-values seed)
  (let ((stack (cons (list position xlink-values) (xlink:seed-stack seed))))
    (xlink:make-small-seed 'none '() '() stack)))

(define (xlink:simple-end parent-seed seed element)
  (let ((stack-element (car (xlink:seed-stack seed))))
    (let ((position (car stack-element))
          (xlink-values (cadr stack-element)))
      (let ((mode (xlink:seed-mode parent-seed))
            (sxlink-arcs (xlink:add-simple 
                             xlink-values element position
                             (xlink:seed-sxpointer parent-seed)
                             (xlink:seed-sxlink-arcs parent-seed)))
            (sxpointer
             (xlink:sxpointer4sibling (xlink:seed-sxpointer parent-seed)))
            (stack (xlink:seed-stack parent-seed)))
        (xlink:make-small-seed
         mode sxlink-arcs sxpointer stack)))))          

;------------------------------------------------
; An extended-link element

(define (xlink:extended-start position xlink-values seed)
  (let ((sxlink-arcs (xlink:seed-sxlink-arcs seed))
        (sxpointer (cons 1 (xlink:seed-sxpointer seed)))
        (stack (cons (list position xlink-values) (xlink:seed-stack seed))))
    (xlink:make-full-seed 'extended sxlink-arcs sxpointer stack
                          '() '() '())))

(define (xlink:extended-end parent-seed seed element)
  (let ((stack-element (car (xlink:seed-stack seed))))
    (let ((position (car stack-element))
          (xlink-values (cadr stack-element)))
      (let ((locators+resources (xlink:seed-locators+resources seed))
            (arcs (xlink:add-default-arc element (xlink:seed-arcs seed)))
            (declared-labels (xlink:seed-declared-labels seed)))           
        (xlink:all-labels-declared arcs declared-labels)      
        (let ((mode (xlink:seed-mode parent-seed))
              (sxlink-arcs
               (xlink:add-extended
                locators+resources arcs (xlink:seed-sxlink-arcs seed)
                `(declaration
                  (uri)  ; declared in this document
                  (nodes ,element)
                  (xpointer ,(xlink:sxpointer->childseq
                              (xlink:seed-sxpointer parent-seed)))
                  (position ,position))))
              (sxpointer
               (xlink:sxpointer4sibling (xlink:seed-sxpointer parent-seed)))
              (stack (xlink:seed-stack parent-seed)))
          (xlink:make-small-seed mode sxlink-arcs sxpointer stack))))))

;------------------------------------------------
; A locator element

(define (xlink:locator-start position xlink-values seed)
  (let ((stack (cons (list position xlink-values) (xlink:seed-stack seed))))
    (xlink:make-small-seed 'none '() '() stack)))  

(define (xlink:locator-end parent-seed seed element)
  (let ((stack-element (car (xlink:seed-stack seed))))
    (let ((position (car stack-element))
          (xlink-values (cadr stack-element)))
      (let ((mode (xlink:seed-mode parent-seed))
            (sxlink-arcs (xlink:seed-sxlink-arcs parent-seed))
            (sxpointer
             (xlink:sxpointer4sibling (xlink:seed-sxpointer parent-seed)))
            (stack (xlink:seed-stack parent-seed))
            (locators+resources 
             (xlink:add-locator xlink-values position element
                                (xlink:seed-locators+resources parent-seed)))
            (arcs (xlink:seed-arcs parent-seed))
            (declared-labels 
             (xlink:add-declared-label 
              xlink-values (xlink:seed-declared-labels parent-seed))))
        (xlink:make-full-seed mode sxlink-arcs sxpointer stack
                              locators+resources arcs declared-labels)))))

;------------------------------------------------
; A resource element

(define (xlink:resource-start position xlink-values seed)
  (let ((stack (cons (list position xlink-values) (xlink:seed-stack seed))))
    (xlink:make-small-seed 'none '() '() stack)))

(define (xlink:resource-end parent-seed seed element)
  (let((stack-element (car (xlink:seed-stack seed))))
    (let ((position (car stack-element))
          (xlink-values (cadr stack-element)))
      (let* ((mode (xlink:seed-mode parent-seed))
             (sxlink-arcs (xlink:seed-sxlink-arcs parent-seed))
             (sxpointer
              (xlink:sxpointer4sibling (xlink:seed-sxpointer parent-seed)))
             (stack (xlink:seed-stack parent-seed))
             (locators+resources 
              (xlink:add-resource xlink-values element sxpointer
                                  (xlink:seed-locators+resources parent-seed)))
             (arcs (xlink:seed-arcs parent-seed))
             (declared-labels
              (xlink:add-declared-label
               xlink-values (xlink:seed-declared-labels parent-seed))))
        (xlink:make-full-seed mode sxlink-arcs sxpointer stack
                              locators+resources arcs declared-labels)))))

;------------------------------------------------
; An arc element

(define (xlink:arc-start position xlink-values seed)
  (let ((stack (cons (list position xlink-values) (xlink:seed-stack seed))))
    (xlink:make-small-seed 'none '() '() stack)))

(define (xlink:arc-end parent-seed seed element)
  (let ((stack-element (car (xlink:seed-stack seed))))
    (let ((position (car stack-element))
          (xlink-values (cadr stack-element)))
      (let ((mode (xlink:seed-mode parent-seed))
            (sxlink-arcs (xlink:seed-sxlink-arcs parent-seed))
            (sxpointer
             (xlink:sxpointer4sibling (xlink:seed-sxpointer parent-seed)))
            (stack (xlink:seed-stack parent-seed))
            (locators+resources (xlink:seed-locators+resources parent-seed))           
            (arcs (xlink:add-arc xlink-values position element
                                 (xlink:seed-arcs parent-seed)))
            (declared-labels
             (xlink:seed-declared-labels parent-seed)))
        (xlink:make-full-seed mode sxlink-arcs sxpointer stack
                              locators+resources arcs declared-labels)))))


;=========================================================================
; Miscellaneous utility functions

;------------------------------------------------
; Functions dealing with position

; Returns posiotion of a port
; NOTE: Specific for different Scheme implementations
(define (xlink:get-port-position port)
  (cond-expand         
   (bigloo
    (string-append "position " (number->string (input-port-position port))))
   (chicken
    (string-append
     "line " (number->string (receive (row col) (port-position port) row))))
   (gambit
    ; DL: was
    ;(string-append "line " (number->string (port-input-line-count port)))
    (string-append "position "
                   (number->string (input-port-byte-position port))))
   (guile
    (string-append "line " (number->string (port-line port))))
   (plt
    (string-append "position " (number->string (file-position port))))
   (else "unknown")))

; This function displays an error message. #t is returned
;  position - position within a file
;  text - a message to display
(define (xlink:parser-error position . text)
  (apply
   cerr
   (if
    (string=? position "unknown")
    (append (list nl "XLink error:" nl) text (list nl))
    (append (list nl "XLink error in " position ":" nl) text (list nl)))))

;------------------------------------------------
; Functions working on branches of an SXML tree

; Helper is used by the following functions in this section
;  action-on-branch ::= (lambda (elem content-nodeset) ...)
;  elem - SXML element that corresponds to the branch
;  content-nodeset - new content
; The lambda should return the new elem
(define (xlink:branch-helper action-on-branch)
  (lambda (document branch-lpath content-nodeset)
    (letrec
        (; Constructs a new branch if it doesn't exist in a document
         (make-new-branch
          (lambda (lpath)
            (if (null? (cdr lpath))  ; lpath consists of a single member
                (cons (car lpath) content-nodeset)
                (list (car lpath) (make-new-branch (cdr lpath))))))
         ; Walks a document
         (tree-walk
          (lambda (elem lpath)
            (if
             (null? lpath)  ; we have reached the desired node
             (action-on-branch elem content-nodeset)
             (let loop ((foll-siblings elem)
                        (prec-siblings '()))
               (cond
                 ((null? foll-siblings)  ; no such branch
                  (cons*
                   (car elem)
                   (make-new-branch lpath)
                   (cdr elem)))
                 ((and (pair? (car foll-siblings))
                       (eq? (caar foll-siblings) (car lpath)))
                  ; match found
                  (append
                   (reverse prec-siblings)
                   (list
                    (tree-walk (car foll-siblings) (cdr lpath)))
                   (cdr foll-siblings)))
                 (else
                  (loop (cdr foll-siblings)
                        (cons (car foll-siblings) prec-siblings)))))))))
      (tree-walk document branch-lpath))))

; Replaces the content of the branch with a new content
;  document - SXML document
;  branch-lpath ::= (listof symbol)
;  branch-lpath - is like an sxpath location path. There must be no more than
; one branch in an SXML tree with this location path. If this branch doesn't
; exist, it will be created as the first branch in a document
;  content-nodeset ::= (listof node)
;  content-nodeset - defines the content of the branch
(define xlink:replace-branch
  (xlink:branch-helper
   (lambda (elem content-nodeset) (cons (car elem) content-nodeset))))

; Appends 'content-nodeset' to the content of the given branch
(define xlink:append-branch
  (xlink:branch-helper
   (lambda (elem content-nodeset) (append elem content-nodeset))))

;------------------------------------------------
; Processing the document URI
; (borrowed from "xlink.scm")

; Given a document, returns its URI (a string)
; #f is returned if there is no "@@/uri" subtree in the document
(define (xlink:get-uri doc)
  (let ((nodeset ((select-kids (ntype?? 'uri))
                  ((select-kids (ntype?? '@@)) doc))))
    (if (null? nodeset)  ; there is no "@@/uri" subtree
        #f
        (cadar nodeset))))

; Adds the URI of the document where the arcs were declared, to sxlink-arcs
; Returns modified sxlink-arcs
(define (xlink:set-uri-for-sxlink-arcs uri sxlink-arcs)
  (letrec
      ((process-arc
        ; uri-alist ::= (listof (cons uri resolved-uri))
        ; association between the URI and the corresponding resolved one
        ; Returns: (values new-node new-uri-alist)
        (lambda (node uri-alist)
          (case (car node)  ; a node is always an SXML element
            ((linkbase simple inbound outbound third-party local-to-local
              from to declaration)
             ; Recursive application to children
             (call-with-values
              (lambda () (process-nodeset (cdr node) uri-alist))
              (lambda (new-children new-uri-alist)
                (values (cons (car node) new-children)
                        new-uri-alist))))
            ((uri)
             (cond
               ((null? (cdr node))  ; no URI is set
                (values `(uri ,uri) uri-alist))
               ((assoc (cadr node) uri-alist)
                => (lambda (pair)
                     (values `(uri ,(cdr pair)) uri-alist)))
               (else
                (let ((resolved-uri
                       (ar:resolve-uri-according-base uri (cadr node))))
                  (values `(uri ,resolved-uri)
                          (cons
                           (cons (cadr node) resolved-uri)
                           uri-alist))))))
            (else
             (values node uri-alist)))))
       ; Applies the previous function to a nodeset
       (process-nodeset
        (lambda (nodeset uri-alist)
          (let loop ((nset nodeset)
                     (res '())
                     (uri-alist uri-alist))
            (if
             (null? nset)
             (values (reverse res) uri-alist)
             (call-with-values
              (lambda () (process-arc (car nset) uri-alist))
              (lambda (new-node new-uri-alist)
                (loop (cdr nset)
                      (cons new-node res)
                      new-uri-alist))))))))
    (call-with-values
     (lambda () (process-nodeset sxlink-arcs '()))
     (lambda (new-sxlink-arcs dummy)
       new-sxlink-arcs))))
                 

;=========================================================================
; Core features of the parser

;------------------------------------------------
; Handler units for SSAX multi-parser
              
; This function is called by the NEW-LEVEL-SEED handler
; A new 'xlink:seed' is returned
(define (xlink:new-level-seed-handler port attributes namespaces seed)
  (let ((position (xlink:get-port-position port))
        (xlink-values (xlink:read-attributes attributes namespaces)))
    (xlink:check-type-show-actuate-constraints xlink-values position)
    (let((mode (xlink:seed-mode seed))
         (type (xlink:values-type xlink-values)))
      (case mode
        ((general)
         (case (if type (string->symbol type) type)
           ((simple) (xlink:simple-start position xlink-values seed))
           ((extended) (xlink:extended-start position xlink-values seed))
           ((none) (xlink:none-start position xlink-values seed))
           (else (xlink:general-start position xlink-values seed))))
        ((extended)
         (case (if type (string->symbol type) type)
           ((locator) (xlink:locator-start position xlink-values seed))
           ((resource) (xlink:resource-start position xlink-values seed))
           ((arc) (xlink:arc-start position xlink-values seed))
           (else (xlink:none-start position xlink-values seed))))
        ((none) (xlink:none-start position xlink-values seed))
        (else 
         (xlink:parser-error position "internal processor error - mode=" 
                      mode)
         (xlink:none-start position xlink-values seed))))))
  
; This function is called by the FINISH-ELEMENT handler
; A new 'xlink:seed' is returned
(define (xlink:finish-element-handler parent-seed seed element)
  (let((xlink-values (cadar (xlink:seed-stack seed))))
    (let((mode (xlink:seed-mode parent-seed))
         (type (xlink:values-type xlink-values)))
      (case mode
        ((general)
         (case (if type (string->symbol type) type)
           ((simple) (xlink:simple-end parent-seed seed element))
           ((extended) (xlink:extended-end parent-seed 
                                           seed element))
           ((none) (xlink:none-end parent-seed seed element))
           (else (xlink:general-end parent-seed seed element))))
        ((extended)
         (case (if type (string->symbol type) type)
           ((locator) (xlink:locator-end parent-seed 
                                         seed element))
           ((resource) (xlink:resource-end parent-seed 
                                           seed element))
           ((arc) (xlink:arc-end parent-seed seed element))
           (else (xlink:none-end parent-seed seed element))))
        ((none) (xlink:none-end parent-seed seed element))
        (else 
         (xlink:parser-error 0 "internal processor error - mode=" 
                      mode)
         (xlink:none-end parent-seed seed element))))))

; Constructs the member of an axuiliary list
(define (xlink:ending-action xlink:seed)
  (let ((sxlink-arcs (reverse (xlink:seed-sxlink-arcs xlink:seed))))    
    `(sxlink
      (declared-here ,@sxlink-arcs))))

;-------------------------------------------------
; The function which adds XLink-related information to the SXML document

;  document - an SXML document
; The function emulates a 'fold-ts' operation.
; A new SXML document is returned. It contains an auxiliary list with an
; 'sxlink' subtree. If the source document already contains such a
; subtree, it will be replaced. Other subtrees in an auxiliary list will
; remain unchanged.
(define (SXML->SXML+xlink document)
  (letrec
      ((fold-ts
        (lambda (node ns-prefixes seed)
          (let ((xlink-values (xlink:read-SXML-attributes node ns-prefixes)))
            (let ((mode (xlink:seed-mode seed))
                  (type (xlink:values-type xlink-values))
                  (pos "unknown"))
              (let rpt
                ((kids ((select-kids (ntype?? '*)) node))
                 (new-seed
                  (case mode
                    ((general)
                     (case (if type (string->symbol type) type)
                       ((simple)
                        (xlink:simple-start pos xlink-values seed))
                       ((extended)
                        (xlink:extended-start pos xlink-values seed))
                       ((none)
                        (xlink:none-start pos xlink-values seed))
                       (else
                        (xlink:general-start pos xlink-values seed))))
                    ((extended)
                     (case (if type (string->symbol type) type)
                       ((locator)
                        (xlink:locator-start pos xlink-values seed))
                       ((resource)
                        (xlink:resource-start pos xlink-values seed))
                       ((arc)
                        (xlink:arc-start pos xlink-values seed))
                       (else
                        (xlink:none-start pos xlink-values seed))))
                    ((none)
                     (xlink:none-start pos xlink-values seed))
                    (else
                     (xlink:parser-error pos "internal processor error - mode=" mode)
                     (xlink:none-start pos xlink-values seed)))))
                (if
                 (not (null? kids))
                 (rpt (cdr kids)
                      (fold-ts (car kids) ns-prefixes new-seed))
                 (case mode
                   ((general)
                    (case (if type (string->symbol type) type)
                      ((simple) (xlink:simple-end seed new-seed node))
                      ((extended) (xlink:extended-end seed new-seed node))
                      ((none) (xlink:none-end seed new-seed node))
                      (else (xlink:general-end seed new-seed node))))
                   ((extended)
                    (case (if type (string->symbol type) type)
                      ((locator) (xlink:locator-end seed new-seed node))
                      ((resource) (xlink:resource-end seed new-seed node))
                      ((arc) (xlink:arc-end seed new-seed node))
                      (else (xlink:none-end seed new-seed node))))
                   ((none) (xlink:none-end seed new-seed node))
                   (else
                    (xlink:parser-error pos
                     "internal processor error - mode=" mode)
                    (xlink:none-end seed new-seed node))))))))))
  (let* ((ns-prefixes
          (let ((ns-node ((select-kids (ntype?? '*NAMESPACES*))
                          ((select-kids (ntype?? '@@)) document))))
            (if (null? ns-node)
                '()
                (cdar ns-node))))
         (sxlink-arcs
          (xlink:seed-sxlink-arcs
           (fold-ts ((select-kids (ntype?? '*)) document)
                    ns-prefixes
                    (xlink:make-small-seed 'general '() '(1) '()))))
         (uri (xlink:get-uri document)))
    (xlink:append-branch
     document
     '(@@ sxlink declared-here)
     (if uri  ; URI for the document supplied
         (xlink:set-uri-for-sxlink-arcs uri sxlink-arcs)
         sxlink-arcs)))))

;-------------------------------------------------
; Adds SXLink arc information to SHTML document

(define (SHTML->SHTML+xlink document)
  (letrec
      ((tree-walk
        ; Returns (listof sxlink-arc)
        (lambda (node sxpointer)          
          (let loop
            ((sxlink-arcs
              (if
               (not (and (pair? node) (eq? (car node) 'a)))
               '()  ; it is not an <A> element
               (let ((href ((select-kids (ntype?? '*text*))
                            ((select-kids (ntype?? 'href))
                             ((select-kids (ntype?? '@)) node)))))
                 (if
                  (null? href)  ; <A> doesn't contain href attribute
                  '()
                  (call-with-values
                   (lambda ()
                     (let ((lst (string-split (car href) (list #\#) 2)))
                       (cond
                         ((null? lst)  ; (car href)=""  - the real situation
                          (values (car href) #f))
                         ((= (length lst) 1)  ; no anchor
                          (values (car lst) #f))
                         ((= (string-length (car lst)) 0)
                          (values #f (cadr lst)))
                         (else
                          (values (car lst) (cadr lst))))))
                   (lambda (uri-ending fragment)
                     `((simple
                        (from
                         (uri)  ; from this document
                         (nodes ,node)
                         (xpointer ,(xlink:sxpointer->childseq sxpointer)))
                        (to
                         (uri ,@(if uri-ending (list uri-ending) '()))
                         ,@(if fragment
                               `((xpointer
                                  ,(string-append
                                    "xpointer(descendant::*[a/@name='"
                                    fragment "'])")))
                               '()))
                        (declaration
                         (uri)
                         (nodes ,node)
                         (xpointer
                          ,(xlink:sxpointer->childseq sxpointer))))))
                   )))))
             (kids ((select-kids (ntype?? '*)) node))
             (kid-pos 1))
            (if (null? kids)  ; every child node processed
                sxlink-arcs
                (loop
                 (append sxlink-arcs
                         (tree-walk (car kids) (cons kid-pos sxpointer)))
                 (cdr kids)
                 (+ kid-pos 1)))))))
    (let ((sxlink-arcs (tree-walk document '()))
          (uri (xlink:get-uri document)))
      (xlink:append-branch
       document
       '(@@ sxlink declared-here)
       (if uri  ; URI for the document supplied
           (xlink:set-uri-for-sxlink-arcs uri sxlink-arcs)
           sxlink-arcs)))))

(provide (all-defined)))
