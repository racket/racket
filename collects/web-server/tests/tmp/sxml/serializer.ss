; Module header is generated automatically
#cs(module serializer mzscheme
(require (lib "ssax.ss" "web-server/tests/tmp/ssax"))

;; SXML serializer into XML and HTML
;
; Partial conformance with
; [1] XSLT 2.0 and XQuery 1.0 Serialization
; W3C Candidate Recommendation 3 November 2005
; http://www.w3.org/TR/2005/CR-xslt-xquery-serialization-20051103/
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lizorkin@ispras.ru    Dmitry Lizorkin

; Prefix for global identifiers in this module is `srl:'
; short for "serialization"

; Requires: function `filter' from SRFI-1
;           syntax `cond-expand' from SRFI-0
; In particular, for PLT, `filter' can be acquired as follows: 
;(require (lib "filter.ss" "srfi/1"))

;==========================================================================
; Basic

; `map' and `append' in a single pass:
; (srl:map-append func lst) = (apply append (map func lst))
; A simplified analogue of `map-union' from "sxpathlib.scm"
(define (srl:map-append func lst)
  (if (null? lst)
      lst
      (append (func (car lst))
              (srl:map-append func (cdr lst)))))

; procedure srl:apply-string-append :: STR-LST -> STRING
; str-lst ::= (listof string)
; Concatenates `str-lst' members into a single string
; (srl:apply-string-append str-lst) = (apply string-append str-lst)
(cond-expand
 (chicken
  ; In Chicken, procedures are generally limited to 126 arguments
  ; http://www.call-with-current-continuation.org/
  ; Due to this Chicken limitation, we cannot apply `string-append' directly
  ; for a potentially long `str-lst'
  
  ; Similar to R5RS 'list-tail' but returns the new list consisting of the
  ; first 'k' members of 'lst'
  (define (srl:list-head lst k)
    (if (or (null? lst) (zero? k))
        '()
        (cons (car lst) (srl:list-head (cdr lst) (- k 1)))))

  ; Because of Chicken 126-argument limitation, I do not care of intermediate
  ; garbage produced in the following solution:
  (define (srl:apply-string-append str-lst)
    (cond
      ((null? str-lst) "")
      ((null? (cdr str-lst)) (car str-lst))
      (else  ; at least two members
       (let ((middle (inexact->exact (round (/ (length str-lst) 2)))))
         (string-append
          (srl:apply-string-append (srl:list-head str-lst middle))
          (srl:apply-string-append (list-tail str-lst middle)))))))
  )
 (else
  (define (srl:apply-string-append str-lst)
    (apply string-append str-lst))
  ))

; Analogue of `assoc'
; However, search is performed by `cdr' of each alist member and `string=?' is
; used for comparison
(define (srl:assoc-cdr-string= item alist)
  (cond
    ((null? alist) #f)
    ((string=? (cdar alist) item) (car alist))
    (else (srl:assoc-cdr-string= item (cdr alist)))))

; Analogue of `member' for strings that uses case insensitive comparison
(define (srl:member-ci str lst)
  (cond
    ((null? lst) #f)
    ((string-ci=? str (car lst)) lst)
    (else (srl:member-ci str (cdr lst)))))

; Analogue of `member'
; The end of the `lst' is returned, from the first member that satisfies
; the `pred?'
(define (srl:mem-pred pred? lst)
  (cond
    ((null? lst) #f)
    ((pred? (car lst)) lst)
    (else (srl:mem-pred pred? (cdr lst)))))

;-------------------------------------------------
; Borrowed from "char-encoding.scm"

; The newline character
(cond-expand
 ((or scheme48 scsh)
  (define srl:char-nl (ascii->char 10)))
 (else
  (define srl:char-nl (integer->char 10))))

; A string consisting of a single newline character
(define srl:newline (string srl:char-nl))

;-------------------------------------------------
; Borrowed from "sxpathlib.scm"

; A simplified implementation of `select-kids' is sufficienf for the serializer
(define (srl:select-kids test-pred?)
  (lambda (node)		; node or node-set
    (cond 
     ((null? node) node)
     ((not (pair? node)) '())   ; No children
     ((symbol? (car node))
      (filter test-pred? (cdr node)))
     (else
      (srl:map-append (srl:select-kids test-pred?) node)))))

;-------------------------------------------------
; Borrowed from "modif.scm"

;  Separates the list into two lists with respect to the predicate
;  Returns:  (values  res-lst1  res-lst2)
; res-lst1 - contains all members from the input lst that satisfy the pred?
; res-lst2 - contains the remaining members of the input lst
(define (srl:separate-list pred? lst)
  (let loop ((lst lst)
             (satisfy '())
             (rest '()))
    (cond
      ((null? lst)
       (values (reverse satisfy) (reverse rest)))
      ((pred? (car lst))   ; the first member satisfies the predicate
       (loop (cdr lst)
             (cons (car lst) satisfy) rest))
      (else
       (loop (cdr lst)
             satisfy (cons (car lst) rest))))))

;-------------------------------------------------
; Borrowed from "fragments.scm"

; A simplified implementation of `sxml:clean-fragments'
(define (srl:clean-fragments fragments)
  (reverse
    (let loop ((fragments fragments) (result '()))
      (cond
	((null? fragments) result)
	((null? (car fragments)) (loop (cdr fragments) result))
	((pair? (car fragments))
	 (loop (cdr fragments) 
	       (loop (car fragments) result)))
	(else
	  (loop (cdr fragments) 
		(cons (car fragments) result)))))))

; A very much simplified analogue of `sxml:display-fragments' for fragments
; that have no more than two levels of nesting
; fragments-level2 ::= (listof fragments-level1)
; fragments-level1 ::= string | (listof string)
(define (srl:display-fragments-2nesting fragments-level2 port)
  (for-each
   (lambda (level1)
     (if (pair? level1)
         (for-each (lambda (x) (display x port))
                   level1)
         (display level1 port)))
   fragments-level2))


;==========================================================================
; Helper SXML utilities

; Splits an SXML `name' into namespace id/uri and local part
; Returns: (cons  namespace-id  local-part)
; local-part - string
; namespace-id - string or #f if the `name' does not have a prefix
(define (srl:split-name name)
  (let* ((name-str (symbol->string name))
         (lng (string-length name-str)))
  (let iter ((i (- lng 1)))
    (cond
      ((< i 0)  ; name scanned, #\: not found
       (cons #f name-str))
      ((char=? (string-ref name-str i) #\:)
       (cons (substring name-str 0 i)
             (substring name-str (+ i 1) lng)))
      (else
       (iter (- i 1)))))))

; Converts SXML atomic object to a string. Keeps non-atomic object unchanged.
; A simplified analogue of applying the XPath `string(.)' function to atomic
; object.
(define (srl:atomic->string obj)
  (cond
    ((or (pair? obj)  ; non-atomic type
         (string? obj)) obj)
    ((number? obj)
     (number->string obj))
    ((boolean? obj)
     (if obj "true" "false"))
    (else  ; unexpected type
     ; ATTENTION: should probably raise an error here
     obj)))

; Whether an SXML element is empty
(define (srl:empty-elem? elem)
  (or (null? (cdr elem))  ; just the name      
      (and (null? (cddr elem))  ; just the name and attributes
           (pair? (cadr elem)) (eq? (caadr elem) '@))
      (and (not (null? (cddr elem)))  ; name, attributes, and SXML 2.X aux-list
           (null? (cdddr elem))
           (pair? (caddr elem)) (eq? (caaddr elem) '@@))))

;-------------------------------------------------
; Handling SXML namespaces
; <namespace-assoc> is defined in the SXML specification as
; <namespace-assoc> ::=  ( <namespace-id> "URI" original-prefix? ) 

; Conventional namespace prefix referred to in XML-related specifications
; These prefixes are used for serializing the corresponding namespace URIs by
; default, unless a different prefix is supplied
(define srl:conventional-ns-prefixes
  '((dc . "http://purl.org/dc/elements/1.1/")
    (fo . "http://www.w3.org/1999/XSL/Format")
    (rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (rng . "http://relaxng.org/ns/structure/1.0")
    (xlink . "http://www.w3.org/1999/xlink")
    (xqx . "http://www.w3.org/2005/XQueryX")
    (xsd . "http://www.w3.org/2001/XMLSchema")
    (xsi . "http://www.w3.org/2001/XMLSchema-instance")
    (xsl . "http://www.w3.org/1999/XSL/Transform")))

; Returns (listof <namespace-assoc>) for the given SXML element
(define (srl:namespace-assoc-for-elem elem)
  ((srl:select-kids (lambda (node) (pair? node)))
   ((srl:select-kids
     (lambda (node) (and (pair? node) (eq? (car node) '*NAMESPACES*))))
    (append
     ((srl:select-kids  ; compatibility with SXML 3.0
       (lambda (node) (and (pair? node) (eq? (car node) '@))))
      ((srl:select-kids
        (lambda (node) (and (pair? node) (eq? (car node) '@))))
       elem))
     ((srl:select-kids  ; compatibility with SXML 2.X
       (lambda (node) (and (pair? node) (eq? (car node) '@@))))
      elem)))))

; Returns (listof <namespace-assoc>) for the SXML document node
(define (srl:ns-assoc-for-top doc)
  ((srl:select-kids (lambda (node) (pair? node)))
   ((srl:select-kids
     (lambda (node) (and (pair? node) (eq? (car node) '*NAMESPACES*))))
    ((srl:select-kids
      (lambda (node)
        ; After sequence normalization [1], the SXML 3.0 aux-list is used
        ; at the top-level
        (and (pair? node) (eq? (car node) '@))))
     doc))))

; Extract original prefix-binding from `namespace-assoc-lst'
; namespace-assoc-lst ::= (listof <namespace-assoc>)
; <namespace-assoc> ::=  ( <namespace-id> "URI" original-prefix? )
; Returns:  (listof (cons original-prefix "URI"))
(define (srl:extract-original-prefix-binding namespace-assoc-lst)
  (map
   (lambda (triple) (cons (caddr triple) (cadr triple)))
   (filter  ; specifies original prefix
    (lambda (memb) (= (length memb) 3))
    namespace-assoc-lst)))

;-------------------------------------------------
; Handling xml:space attribute

; Returns the new value of `space-preserve?' in accordance with the value of
; xml:space attribute probably presented for the given SXML element `elem'
; space-preserve? ::= #t | #f  - whether the SXML subtree inherits the
;  xml:space attribute with the value "preserve"
(define (srl:update-space-specifier elem space-preserve?)
  (let ((xml-space-val
         ((srl:select-kids string?)
          ((srl:select-kids
            (lambda (node) (and (pair? node) (eq? (car node) 'xml:space))))
           ((srl:select-kids
             (lambda (node) (and (pair? node) (eq? (car node) '@))))
            elem)))))
    (cond
      ((null? xml-space-val)  ; no xml:space attribute
       space-preserve?)
      ((string=? (car xml-space-val) "preserve")
       #t)
      ((string=? (car xml-space-val) "default")
       #f)
      (else space-preserve?))))
  

;==========================================================================
; Sequence normalization
; Sect. 2 in [1]

; Performs sequence normalization in accordance with [1]
; Returns the SXML document node
(define (srl:normalize-sequence node-or-sequence)
  (letrec
      ((normaliz-step-1
        ; "If the sequence that is input to serialization is empty, create a
        ; sequence S1 that consists of a zero-length string. Otherwise, copy
        ; each item in the sequence that is input to serialization to create
        ; the new sequence S1." [1]
        (lambda (node-or-seq)
          (cond
            ((null? node-or-seq)  ; empty sequence
             '(""))
            ; Effect of `as-nodeset' from "sxpathlib.scm"
            ((or (not (pair? node-or-seq))  ; single item
                 (symbol? (car node-or-seq)))  ; single node
             (list node-or-seq))
            (else
             node-or-seq))))
       (normaliz-step-2
        ; "For each item in S1, if the item is atomic, obtain the lexical
        ; representation of the item by casting it to an xs:string and copy
        ; the string representation to the new sequence; otherwise, copy the
        ; item, which will be a node, to the new sequence. The new sequence is
        ; S2." [1]
        (lambda (seq)
          (map
           (lambda (item) (srl:atomic->string item))
           seq)))
       (normaliz-step-3
        ; "For each subsequence of adjacent strings in S2, copy a single
        ; string to the new sequence equal to the values of the strings in the
        ; subsequence concatenated in order, each separated by a single space.
        ; Copy all other items to the new sequence. The new sequence is S3."
        (lambda (seq)
          (let loop ((src (reverse seq))
                     (res '()))
            (cond
              ((null? src)
               res)
              ((string? (car src))
               (let adjacent ((src (cdr src))
                              (adj-strs (list (car src))))
                 (cond
                   ((null? src)  ; source sequence is over
                    (cons (srl:apply-string-append adj-strs) res))
                   ((string? (car src))
                    (adjacent (cdr src)
                              (cons (car src) (cons " " adj-strs))))
                   (else
                    (loop (cdr src)
                          (cons (car src)
                                (cons (srl:apply-string-append adj-strs)
                                      res)))))))
              (else
               (loop (cdr src)
                     (cons (car src) res)))))))
       ; Step 4 from [1] is redundant for SXML, since SXML text nodes are not
       ; distinquished from strings
       (normaliz-step-5
        ; "For each item in S4, if the item is a document node, copy its
        ; children to the new sequence; otherwise, copy the item to the new
        ; sequence. The new sequence is S5." [1]
        (lambda (seq)
          (cond            
            ((null? seq)
             seq)
            ((and (pair? (car seq)) (eq? (caar seq) '*TOP*))
             ; Document node
             (append (cdar seq) (normaliz-step-5 (cdr seq))))
            (else
             (cons (car seq) (normaliz-step-5 (cdr seq)))))))
       (normaliz-step-6
        ; "For each subsequence of adjacent text nodes in S5, copy a single
        ; text node to the new sequence equal to the values of the text nodes
        ; in the subsequence concatenated in order. Any text nodes with values
        ; of zero length are dropped. Copy all other items to the new sequence.
        ; The new sequence is S6." [1]
        ; Much like Step 3; however, a space between adjacent strings is not
        ; inserted and the zero-length strings are removed
        (lambda (seq)
          (let loop ((src (reverse seq))
                     (res '()))
            (cond
              ((null? src)
               res)
              ((string? (car src))
               (if
                (string=? (car src) "")  ; empty string
                (loop (cdr src) res)
                (let adjacent ((src (cdr src))
                               (adj-strs (list (car src))))
                  (cond
                    ((null? src)  ; source sequence is over
                     (cons (srl:apply-string-append adj-strs) res))
                    ((string? (car src))
                     ; If it is an empty string, the effect of its presense
                     ; will be removed by string concatenation
                     (adjacent (cdr src)
                               (cons (car src) adj-strs)))
                    (else
                     (loop (cdr src)
                           (cons (car src)
                                 (cons
                                  (srl:apply-string-append adj-strs)
                                  res))))))))
              (else
               (loop (cdr src)
                     (cons (car src) res)))))))
       (normaliz-step-7
        ; "It is a serialization error [err:SENR0001] if an item in S6 is an
        ; attribute node or a namespace node. Otherwise, construct a new
        ; sequence, S7, that consists of a single document node and copy all
        ; the items in the sequence, which are all nodes, as children of that
        ; document node." [1]
        ; On this step, we should take care of SXML aux-lists
        ; ATTENTION: should generally raise an error in the presense of
        ;  attribute nodes in a sequence. By nature of SXML 3.0, however,
        ;  attribute nodes on the top level are treated as aux-nodes
        (lambda (seq)
          (call-with-values
           (lambda ()
             (srl:separate-list
              (lambda (item)
                (and (pair? item)
                     (or (eq? (car item) '@@)  ; aux-list in SXML 2.X
                         (eq? (car item) '@)  ; aux-list in SXML 3.0
                         )))
              seq))
           (lambda (aux-lists body)
             (if
              (null? aux-lists)
              `(*TOP* ,@body)
              `(*TOP*
                (@ ,@(srl:map-append cdr aux-lists))
                ,@body)))))))
    ; TODO: According to [1], if the normalized sequence does not have exactly
    ; one element node node child or has text node children, then the
    ; serialized output should be an XML external general parsed entity.
    ; However, external parsed entities are not currently handled by SSAX
    ; parser. Should think of a compromise between conformance and practical
    ; usability.
    (normaliz-step-7
     (normaliz-step-6
      (normaliz-step-5
       (normaliz-step-3
        (normaliz-step-2
         (normaliz-step-1 node-or-sequence))))))))


;==========================================================================
; Character escaping during string serialization
; Escaping in accordance with [1] and [2]:
;
; [2] Extensible Markup Language (XML) 1.0 (Third Edition)
; W3C Recommendation 04 February 2004
; http://www.w3.org/TR/2004/REC-xml-20040204

;-------------------------------------------------
; CDATA sections

; Returns #f if a given character `ch' is in XML character range [2]
; Otherwise, returns a string representing the character reference for that
; character
(define (srl:xml-char-escaped ch)
  (let ((code (char->integer ch)))
    (if (or (= code 9) (= code 10) (= code 13)
            (and (>= code 32) (<= code 55295))
            (and (>= code 57344) (<= code 65533))
            (>= code 65536))
        #f
        (string-append "&#" (number->string code) ";"
                       ))))
                             
; Represents a given string `str' as a CDATA section
(define (srl:string->cdata-section str)
  (let ((flush-buffer
         ; If a `buffer' is non-empty, converts it to a CDATA string and
         ; cons'es this string to `res'. Returns a new res
         (lambda (buffer res)
           (if (null? buffer)
               res
               (cons
                (string-append
                 "<![CDATA[" (list->string (reverse buffer)) "]]>")
                res)))))
    (let loop ((src (string->list str))
               (buffer '())
               (res '("")))
    (cond
      ((null? src)
       (srl:apply-string-append
        (reverse (flush-buffer buffer res))))
      ((srl:xml-char-escaped (car src))
       => (lambda (charref)
            (loop (cdr src)
                  '()
                  (cons charref (flush-buffer buffer res)))))
      ((and (char=? (car src) #\])
            (not (null? buffer))
            (char=? (car buffer) #\]))
       (loop (cdr src)
             '()
             (cons (string (car buffer) (car src))  ;= "]]"
                   (flush-buffer (cdr buffer) res))))
      (else  ; any other character
       (loop (cdr src)
             (cons (car src) buffer)
             res))))))

;-------------------------------------------------
; Character data and attribute values

; Associative lists of characters to be escaped in XML character data and
; attribute values respectively [2]
(define srl:escape-alist-char-data
  '((#\& . "&amp;") (#\< . "&lt;") (#\> . "&gt;")))
(define srl:escape-alist-att-value
  (append `((#\' . "&apos;") (#\" . "&quot;")
            ; Escaping the newline character in attribute value
            (,srl:char-nl . "&#10;"))
          srl:escape-alist-char-data))
(define srl:escape-alist-html-att
  '((#\& . "&amp;") (#\> . "&gt;") (#\' . "&apos;") (#\" . "&quot;")))

; Escape a string with the `srl:xml-char-escaped' and with the `escape-alist'
; supplied
; escape-alist ::= (listof (cons char string))
; html-method? ::= #t | #f
; Returns the escaped string
(define (srl:string->escaped str escape-alist html-method?)
  (let loop ((src (string->list str))
             (adj-chars '())
             (res '()))
    (cond
      ((null? src)
       (srl:apply-string-append
        (reverse (cons (list->string (reverse adj-chars))
                       res))))
      ((assv (car src) escape-alist)  ; current character matches the alist
       => (lambda (pair)
            (if
             ; Subsect. 7.2 in [1]:
             ; "The HTML output method MUST NOT escape a & character occurring
             ; in an attribute value immediately followed by a { character"
             (and (char=? (car src) #\&)
                  html-method?
                  (not (null? (cdr src))) (char=? (cadr src) #\{))
             (loop (cdr src)
                   (cons (car src) adj-chars)
                   res)
             (loop (cdr src)
                   '()
                   (cons (cdr pair)
                         (cons (list->string (reverse adj-chars))
                               res))))))
      ((srl:xml-char-escaped (car src))
       => (lambda (esc)
            (loop (cdr src)
                  '()
                  (cons esc
                        (cons (list->string (reverse adj-chars))
                              res)))))      
      (else
       (loop (cdr src)
             (cons (car src) adj-chars)
             res)))))
       
(define (srl:string->char-data str)
  (srl:string->escaped str srl:escape-alist-char-data #f))
(define (srl:string->att-value str)
  (srl:string->escaped str srl:escape-alist-att-value #f))
(define (srl:string->html-att str)
  (srl:string->escaped str srl:escape-alist-html-att #t))

;-------------------------------------------------
; Serializing entities produced by HtmlPrag
;
; [3] Neil W. Van Dyke.
; HtmlPrag: Pragmatic Parsing and Emitting of HTML using SXML and SHTML
; Version 0.16, 2005-12-18, http://www.neilvandyke.org/htmlprag/

; "..SHTML adds a special & syntax for non-ASCII (or non-Extended-ASCII)
; characters. The syntax is (& val), where val is a symbol or string naming
; with the symbolic name of the character, or an integer with the numeric
; value of the character." [3]
;  entity ::= `(& ,val)
;  val ::= symbol | string | number
; Returns the string representation for the entity
(define (srl:shtml-entity->char-data entity)
  ; TODO: think of an appropriate error message for an ill-formed entity
  (if
   (= (length entity) 2)
   (let ((val (cadr entity)))
     (cond
       ((symbol? val) (string-append "&" (symbol->string val) ";")
        )
       ((string? val) (string-append "&" val ";")
        )
       ((and (number? val) (integer? val) (> val 0))
        ; to guarantee well-formedness of the result produced
        (string-append "&#" (number->string val) ";")
        )
       (else  ; should signal of an error
        "")))
   ""))


;==========================================================================
; Serialization for markup
; declared-ns-prefixes ::= (listof (cons prefix-string namespace-uri))
; prefix-string, namespace-uri - strings

; Returns the string representation for a QName
; prefix-string ::= string or #f if the name contains no prefix
; TODO: should check names for proper characters
(define (srl:qname->string prefix-string local-part)
  (if prefix-string
      (string-append prefix-string ":" local-part)
      local-part))

;-------------------------------------------------
; Different types of nodes

; Returns the list of strings that constitute the serialized representation
; for the attribute. Inserts a whitespace symbol in the beginning
; method ::= 'xml | 'html
(define (srl:attribute->str-lst prefix-string local-part att-value method)
  (let ((attval (srl:atomic->string att-value)))
    (cond
      (prefix-string
       (list " " prefix-string ":" local-part "=\""
             ((if (eq? method 'html)
                  srl:string->html-att
                  srl:string->att-value) attval)
             "\""))
      ((eq? method 'html)
       (if (string=? local-part attval)  ; boolean attribute
           (list " " local-part)
           (list " " local-part "=\"" (srl:string->html-att attval) "\"")))
      (else  ; unprefixed attribute, XML output method
       (list " " local-part "=\"" (srl:string->att-value attval) "\"")))))

; Returns the list of strings that constitute the serialized representation
; for the namespace declaration. Inserts a whitespace symbol in the beginning
; ATTENTION: character escaping for namespace URI may be improper, study this
;  issue
(define (srl:namespace-decl->str-lst prefix-string namespace-uri)
  (list " xmlns:" prefix-string "=\""
        (srl:string->att-value namespace-uri) "\""))

; According to SXML specification,
;  <comment> ::=  ( *COMMENT* "comment string" )
; ATTENTION: in the case of ill-formed comment, should probably report an error
; instead of recovering
(define (srl:comment->str-lst comment-node)
  (let ((proper-string-in-comment?
         ; Whether a proper string occurs in the comment node. Thus,
         ; "For compatibility, the string '--' (double-hyphen) MUST NOT occur
         ; within comments. ... Note that the grammar does not allow a comment
         ; ending in --->." [2]
         (lambda (str)
           (let ((lng (string-length str)))
             (or
              (zero? lng)  ; empty string allowed in comment [2]
              (and
               (not (char=? (string-ref str 0) #\-))
               (let iter ((i 1)
                          (prev-hyphen? #f))
                 (cond
                   ((>= i lng)
                    (not prev-hyphen?)  ; string must not end with hyphen
                    )
                   ((char=? (string-ref str i) #\-)
                    (if prev-hyphen?
                        #f
                        (iter (+ i 1) #t)))
                   (else
                    (iter (+ i 1) #f))))))))))
    (if (and (= (length comment-node) 2)
             (string? (cadr comment-node))
             (proper-string-in-comment? (cadr comment-node)))
        (list "<!--" (cadr comment-node) "-->")
        (list "<!--" "-->")  ; should probably report of an error
        )))

; According to SXML specification,
; <PI> ::=  ( *PI* pi-target
;                   <annotations>? "processing instruction content string" ) 
; method ::= 'xml | 'html
; Subsect 7.3 in [1]: "The HTML output method MUST terminate processing
; instructions with > rather than ?>." 
; ATTENTION: in the case of ill-formed PI content string, should probably
; report an error instead of recovering
(define (srl:processing-instruction->str-lst pi-node method)
  (let ((string-not-contain-charlist?
         ; Whether `str' does not contain a sequence of characters from
         ; `char-lst' as its substring
         (lambda (str char-lst)
           (let ((lng (string-length str)))
             (or
              (zero? lng)  ; empty string doesn't contain
              (let iter ((i 0)
                         (pattern char-lst))
                (cond                  
                  ((>= i lng) #t)
                  ((char=? (string-ref str i) (car pattern))
                   (if (null? (cdr pattern))  ; it is the last member
                       #f  ; contains
                       (iter (+ i 1) (cdr pattern))))
                  (else
                   (iter (+ i 1) char-lst)))))))))
    (if
     (or (null? (cdr pi-node))
         (not (symbol? (cadr pi-node))))  ; no target => ill-formed PI
     '()  ; should probably raise an error
     (let ((content (filter string? (cddr pi-node))))
       (cond
         ((null? content)  ; PI with no content - correct situation
          (list "<?" (symbol->string (cadr pi-node))
                (if (eq? method 'html) ">" "?>")))
         ; Subsect. 7.3 in [1]: "It is a serialization error to use the HTML
         ; output method when > appears within a processing instruction in
         ; the data model instance being serialized."
         ((and (null? (cdr content))  ; only a single member
               (string-not-contain-charlist?
                (car content)
                (if (eq? method 'html) '(#\>) '(#\? #\>))))
          (list "<?" (symbol->string (cadr pi-node)) " " (car content)
                (if (eq? method 'html) ">" "?>")))
         (else  ; should probably signal of an error
          '()))))))

;-------------------------------------------------
; SXML element

; Returns: (values
;            prefix-string namespace-uri local-part declaration-required?)
; prefix-string - namespace prefix to be given to the serialized name: a string
;  or #f if no prefix is required
; namespace-uri - the namespace URI for the given `name', #f if the name has no
;  namespace URI
; local-part - local part of the name
; declaration-required ::= #t | #f  - whether `prefix' has to be declared
(define (srl:name->qname-components
         name ns-prefix-assig namespace-assoc declared-ns-prefixes)
  (let ((use-ns-id-or-generate-prefix
         (lambda (ns-id)
           (if
            (and ns-id  ; try to use namespace-id as a prefix
                 (not (assq (string->symbol ns-id) ns-prefix-assig))
                 (not (assoc ns-id declared-ns-prefixes)))
            ns-id
            ; Otherwise - generate unique prefix
            ; Returns a prefix-string not presented in ns-prefix-assig and
            ; declared-ns-prefixes
            (let loop ((i 1))
              (let ((candidate (string-append "prfx" (number->string i))))
                (if (or (assoc candidate declared-ns-prefixes)
                        (assq (string->symbol candidate) ns-prefix-assig))
                    (loop (+ i 1))
                    candidate))))))
        (n-parts (srl:split-name name)))
    (cond
      ((not (car n-parts))  ; no namespace-id => no namespace
       (values #f #f (cdr n-parts)  ; name as a string
               #f))
      ((string-ci=? (car n-parts) "xml")  ; reserved XML namespace
       (values (car n-parts) "http://www.w3.org/XML/1998/namespace"
               (cdr n-parts) #f))
      (else
       (call-with-values
        (lambda ()
          (cond
            ((assq (string->symbol (car n-parts))  ; suppose a namespace-id
                   namespace-assoc)
             => (lambda (lst)
                  (values (cadr lst) (car n-parts))))
            (else  ; first part of a name is a namespace URI
             (values (car n-parts) #f))))
        (lambda (namespace-uri ns-id)
          (cond
            ((srl:assoc-cdr-string= namespace-uri declared-ns-prefixes)
             => (lambda (pair)
                  ; Prefix for that namespace URI already declared
                  (values (car pair) namespace-uri (cdr n-parts) #f)))
            (else  ; namespace undeclared
             (values
              (cond
                ((srl:assoc-cdr-string= namespace-uri ns-prefix-assig)
                 => (lambda (pair)
                      ; A candidate namespace prefix is supplied from the user
                      (let ((candidate (symbol->string (car pair))))
                        (if
                         (assoc candidate declared-ns-prefixes)
                         ; The prefix already bound to a different namespace
                         ; Avoid XML prefix re-declaration
                         (use-ns-id-or-generate-prefix ns-id)
                         candidate))))
                (else
                 (use-ns-id-or-generate-prefix ns-id)))
              namespace-uri
              (cdr n-parts)
              #t  ; in any case, prefix declaration is required
              )))))))))

; Constructs start and end tags for an SXML element `elem'
; method ::= 'xml | 'html
; Returns: (values start-tag end-tag
;                  ns-prefix-assig namespace-assoc declared-ns-prefixes)
; start-tag ::= (listof string)
; end-tag ::= (listof string) or #f for empty element
; TODO: escape URI attributes for HTML
; TODO: indentation probably should be made between attribute declarations
(define (srl:construct-start-end-tags
         elem method
         ns-prefix-assig namespace-assoc declared-ns-prefixes)
  (let ((ns-assoc-here (srl:namespace-assoc-for-elem elem))
        (empty? (srl:empty-elem? elem)))
    (let ((ns-prefix-assig
           (append
            (srl:extract-original-prefix-binding ns-assoc-here)
            ns-prefix-assig))
          (namespace-assoc
           (append ns-assoc-here namespace-assoc)))
      (call-with-values
       (lambda ()           
         (srl:name->qname-components  ; element name
          (car elem) ns-prefix-assig namespace-assoc declared-ns-prefixes))
       (lambda (elem-prefix elem-uri elem-local elem-decl-required?)
         (let loop ((attrs
                     (reverse
                      ((srl:select-kids 
                        (lambda (node)  ; not SXML 3.0 aux-list
                          (and (pair? node) (not (eq? (car node) '@)))))
                       ((srl:select-kids
                         (lambda (node)
                           (and (pair? node) (eq? (car node) '@))))
                        elem))))
                    (start-tag
                     (if
                      (or (not empty?)
                          (and (eq? method 'html)
                               (not elem-prefix)
                               (srl:member-ci
                                elem-local
                                ; ATTENTION: should probably move this list
                                ; to a global const
                                '("area" "base" "basefont" "br" "col"
                                  "frame" "hr" "img" "input" "isindex"
                                  "link" "meta" "param"))))
                      '(">") '("/>")))
                    (ns-prefix-assig ns-prefix-assig)
                    (namespace-assoc namespace-assoc)
                    (declared-ns-prefixes
                     ; As if element namespace already declared
                     (if elem-decl-required?
                         (cons (cons elem-prefix elem-uri)
                               declared-ns-prefixes)
                         declared-ns-prefixes)))
           (if
            (null? attrs)  ; attributes scanned
            (let ((elem-name (srl:qname->string elem-prefix elem-local)))
              (values
               (cons "<"
                     (cons elem-name
                           (if
                            elem-decl-required?
                            (cons
                             (srl:namespace-decl->str-lst elem-prefix elem-uri)
                             start-tag)
                            start-tag)))
               (if empty? #f
                   (list "</" elem-name ">"))
               ns-prefix-assig
               namespace-assoc
               declared-ns-prefixes))
            (call-with-values
             (lambda ()
               (srl:name->qname-components
                (caar attrs)  ; attribute name
                ns-prefix-assig namespace-assoc declared-ns-prefixes))
             (lambda (attr-prefix attr-uri attr-local attr-decl-required?)
               (let ((start-tag
                      (cons
                       (srl:attribute->str-lst
                        attr-prefix attr-local
                        ; TODO: optimize for HTML output method
                        (if (null? (cdar attrs))  ; no attribute value
                            attr-local
                            (cadar attrs))
                        method)
                       start-tag)))
                 (loop
                  (cdr attrs)
                  (if attr-decl-required?
                      (cons (srl:namespace-decl->str-lst attr-prefix attr-uri)
                            start-tag)
                      start-tag)
                  ns-prefix-assig
                  namespace-assoc
                  (if attr-decl-required?                      
                      (cons (cons attr-prefix attr-uri) declared-ns-prefixes)
                      declared-ns-prefixes))))))))))))


;==========================================================================
; Recursively walking the tree of SXML elements

; indentation ::= (listof string) or #f  - a list of whitespace strings
;  depending on the node nesting or #f if no indent is required
; space-preserve? ::= #t | #f  - whether the subtree inherits the xml:space
;  attribute with the value "preserve"
; cdata-section-elements ::= (listof symbol)  - list of element names whose
;  child nodes are to be output with CDATA section
; text-node-handler :: string -> string  - a function that performs a proper
;  character escaping for the given node if it is a text node
; TODO: do not insert whitespaces adjacent to HTML %inline elements in HTML
; output method
(define (srl:node->nested-str-lst-recursive
         node method
         ns-prefix-assig namespace-assoc declared-ns-prefixes
         indentation space-preserve?
         cdata-section-elements text-node-handler)
  (if
   (not (pair? node))  ; text node
   (text-node-handler (srl:atomic->string node))
   (case (car node)  ; node name
     ((*COMMENT*)
      (srl:comment->str-lst node))     
     ((*PI*)
      (srl:processing-instruction->str-lst node method))
     ((&)
      (srl:shtml-entity->char-data node))
     ((*DECL*)  ; recovering for non-SXML nodes
      '())
     (else  ; otherwise - an element node
      (call-with-values
       (lambda ()
         (srl:construct-start-end-tags
          node method
          ns-prefix-assig namespace-assoc declared-ns-prefixes))
       (lambda (start-tag end-tag
                          ns-prefix-assig namespace-assoc declared-ns-prefixes)
         (if
          (not end-tag)  ; empty element => recursion stops
          start-tag
          (let ((space-preserve?
                 (srl:update-space-specifier node space-preserve?))
                (text-node-handler
                 (cond
                   ((memq (car node) cdata-section-elements)
                    srl:string->cdata-section)
                   ((and (eq? method 'html)
                         (srl:member-ci (symbol->string (car node))
                                        '("script" "style")))
                    ; No escaping for strings inside these HTML elements
                    (lambda (str) str))
                   (else
                    srl:string->char-data)))
                (content ((srl:select-kids
                           (lambda (node)  ; TODO: support SXML entities
                             (not (and (pair? node)
                                       (memq (car node) '(@ @@ *ENTITY*))))))
                          node)))
            (call-with-values
             (lambda ()
               (cond
                 ((or (not indentation)
                      (and (eq? method 'html)
                           (srl:member-ci
                            (symbol->string (car node))
                            '("pre" "script" "style" "textarea"))))
                  ; No indent - on this level and subsequent levels
                  (values #f #f))
                 ((or space-preserve?
                      (srl:mem-pred  ; at least a single text node
                       (lambda (node) (not (pair? node)))
                       content))
                  ; No indent on this level, possible indent on nested levels
                  (values #f indentation))
                 (else
                  (values (cons srl:newline indentation)
                          (cons (car indentation) indentation)))))
             (lambda (indent-here indent4recursive)
               (if
                indent-here
                (append
                 start-tag
                 (map
                  (lambda (kid)
                    (list
                     indent-here
                     (srl:node->nested-str-lst-recursive
                      kid method
                      ns-prefix-assig namespace-assoc declared-ns-prefixes
                      indent4recursive space-preserve?
                      cdata-section-elements text-node-handler)))
                  content)
                 (cons srl:newline
                       (cons (cdr indentation) end-tag)))
                (append
                 start-tag
                 (map
                  (lambda (kid)
                    (srl:node->nested-str-lst-recursive
                     kid method
                     ns-prefix-assig namespace-assoc declared-ns-prefixes
                     indent4recursive space-preserve?
                     cdata-section-elements text-node-handler))
                  content)
                 end-tag))))))))))))

(define (srl:display-node-out-recursive
         node port method
         ns-prefix-assig namespace-assoc declared-ns-prefixes
         indentation space-preserve?
         cdata-section-elements text-node-handler)
  (if
   (not (pair? node))  ; text node
   (display (text-node-handler (srl:atomic->string node)) port)
   (case (car node)  ; node name
     ((*COMMENT*)
      (for-each
       (lambda (x) (display x port))
       (srl:comment->str-lst node)))
     ((*PI*)
      (for-each
       (lambda (x) (display x port))
       (srl:processing-instruction->str-lst node method)))
     ((&)
      (display (srl:shtml-entity->char-data node) port))
     ((*DECL*)  ; recovering for non-SXML nodes
      #f)
     (else  ; otherwise - an element node
      (call-with-values
       (lambda ()
         (srl:construct-start-end-tags
          node method
          ns-prefix-assig namespace-assoc declared-ns-prefixes))
       (lambda (start-tag end-tag
                          ns-prefix-assig namespace-assoc declared-ns-prefixes)
         (begin
           (srl:display-fragments-2nesting start-tag port)
           (if
            end-tag  ; there exists content
            (let ((space-preserve?
                   (srl:update-space-specifier node space-preserve?))
                  (text-node-handler
                   (cond
                     ((memq (car node) cdata-section-elements)
                      srl:string->cdata-section)
                     ((and (eq? method 'html)
                           (srl:member-ci (symbol->string (car node))
                                          '("script" "style")))
                      ; No escaping for strings inside these HTML elements
                      (lambda (str) str))
                     (else
                      srl:string->char-data)))
                  (content ((srl:select-kids
                             (lambda (node)  ; TODO: support SXML entities
                               (not (and (pair? node)
                                         (memq (car node) '(@ @@ *ENTITY*))))))
                            node)))
              (call-with-values
               (lambda ()
                 (cond
                   ((or (not indentation)
                        (and (eq? method 'html)
                             (srl:member-ci
                              (symbol->string (car node))
                              '("pre" "script" "style" "textarea"))))
                    ; No indent - on this level and subsequent levels
                    (values #f #f))
                   ((or space-preserve?
                        (srl:mem-pred  ; at least a single text node
                         (lambda (node) (not (pair? node)))
                         content))
                    ; No indent on this level, possible indent on nested levels
                    (values #f indentation))
                   (else
                    (values (cons srl:newline indentation)
                            (cons (car indentation) indentation)))))
               (lambda (indent-here indent4recursive)
                 (begin
                   (for-each  ; display content
                    (if
                     indent-here
                     (lambda (kid)
                       (begin
                         (for-each
                          (lambda (x) (display x port))
                          indent-here)
                         (srl:display-node-out-recursive
                          kid port method
                          ns-prefix-assig namespace-assoc declared-ns-prefixes
                          indent4recursive space-preserve?
                          cdata-section-elements text-node-handler)))
                     (lambda (kid)
                       (srl:display-node-out-recursive
                        kid port method
                        ns-prefix-assig namespace-assoc declared-ns-prefixes
                        indent4recursive space-preserve?
                        cdata-section-elements text-node-handler)))
                    content)
                   (if indent-here
                       (begin
                         (display srl:newline port)
                         (for-each
                          (lambda (x) (display x port))
                          (cdr indentation))))
                   (for-each
                    (lambda (x) (display x port))
                    end-tag)))))))))))))

;-------------------------------------------------
; Serializing the document node - start of recursion

; Creates the serialized representation for the XML declaration
; Returns: (listof string)
; version ::= string | number
; standalone ::= 'yes | 'no | 'omit
(define (srl:make-xml-decl version standalone)
  (let ((version (if (number? version) (number->string version) version)))
    (if (eq? standalone 'omit)
        (list "<?xml version='" version "'?>")
        (list "<?xml version='" version "' standalone='"
              (symbol->string standalone) "'?>"))))

; omit-xml-declaration? ::= #t | #f
; standalone ::= 'yes | 'no | 'omit
; version ::= string | number
(define (srl:top->nested-str-lst doc
                                 cdata-section-elements indent
                                 method ns-prefix-assig
                                 omit-xml-declaration? standalone version)
  (let* ((namespace-assoc (srl:ns-assoc-for-top doc))
         (ns-prefix-assig
          (append
           (srl:extract-original-prefix-binding namespace-assoc)
           ns-prefix-assig))
         (serialized-content
          (map
           (if
            indent  ; => output each member from the newline
            (let ((indentation (list indent)))  ; for nested elements
              (lambda (kid)
                (list
                 srl:newline
                 (srl:node->nested-str-lst-recursive
                  kid method
                  ns-prefix-assig namespace-assoc '()
                  indentation #f
                  cdata-section-elements srl:string->char-data))))
            (lambda (kid)
              (srl:node->nested-str-lst-recursive
               kid method
               ns-prefix-assig namespace-assoc '()
               indent #f
               cdata-section-elements srl:string->char-data)))
           ((srl:select-kids  ; document node content
             (lambda (node)  ; TODO: support SXML entities
               (not (and
                     (pair? node) (memq (car node) '(@ @@ *ENTITY*))))))
            doc))))
    (if (or (eq? method 'html) omit-xml-declaration?)
        (if (and indent (not (null? serialized-content)))
            ; Remove the starting newline
            ; ATTENTION: beware of `Gambit cadar bug':
            ; http://mailman.iro.umontreal.ca/pipermail/gambit-list/
            ;   2005-July/000315.html
            (cons (cadar serialized-content) (cdr serialized-content))
            serialized-content)
        (list (srl:make-xml-decl version standalone) serialized-content))))

(define (srl:display-top-out doc port
                             cdata-section-elements indent
                             method ns-prefix-assig
                             omit-xml-declaration? standalone version)  
  (let ((no-xml-decl?  ; no XML declaration was displayed?
         (if (not (or (eq? method 'html) omit-xml-declaration?))
             (begin
               (for-each  ; display xml declaration
                (lambda (x) (display x port))
                (srl:make-xml-decl version standalone))
               #f)
             #t))
        (content  ; document node content
         ((srl:select-kids
           (lambda (node)  ; TODO: support SXML entities
             (not (and
                   (pair? node) (memq (car node) '(@ @@ *ENTITY*))))))
          doc))
        (namespace-assoc (srl:ns-assoc-for-top doc)))
    (let ((ns-prefix-assig
           (append
            (srl:extract-original-prefix-binding namespace-assoc)
            ns-prefix-assig)))
      (cond
        ((null? content)  ; generally a rare practical situation
         #t)  ; nothing more to do
        ((and indent no-xml-decl?)
         ; We'll not display newline before (car content)
         (let ((indentation (list indent)))  ; for nested elements
           (for-each
            (lambda (kid put-newline?)
              (begin
                (if put-newline?
                    (display srl:newline port))
                (srl:display-node-out-recursive
                 kid port method
                 ns-prefix-assig namespace-assoc '()
                 indentation #f
                 cdata-section-elements srl:string->char-data)))
            content
            ; After sequence normalization, content does not contain #f
            (cons #f (cdr content)))))
        (else
         (for-each
          (if
           indent  ; => output each member from the newline
           (let ((indentation (list indent)))  ; for nested elements
             (lambda (kid)
               (begin
                 (display srl:newline port)
                 (srl:display-node-out-recursive
                  kid port method
                  ns-prefix-assig namespace-assoc '()
                  indentation #f
                  cdata-section-elements srl:string->char-data))))
           (lambda (kid)
             (srl:display-node-out-recursive
              kid port method
              ns-prefix-assig namespace-assoc '()
              indent #f
              cdata-section-elements srl:string->char-data)))
          content))))))


;==========================================================================
; Interface
                                   
;-------------------------------------------------
; Calling the serializer with all the serialization parameters supported
; and with no overhead of parameters parsing.
; ATTENTION: As future versions of this library may provide support for
; additional serialization parameters, the functions `srl:sxml->string' and
; `srl:display-sxml' specified in this subsections may have a different number
; of their arguments in the future versions of the library.

; Returns a string that contains the serialized representation for `sxml-obj'.
; cdata-section-elements ::= (listof sxml-name)
; indent ::= #t | #f | whitespace-string
; method = 'xml | 'html
; ns-prefix-assign ::= (listof (cons prefix-symbol namespace-uri-string))
; omit-xml-declaration? ::= #t | #f
; standalone ::= 'yes | 'no | 'omit
; version ::= number | string
(define (srl:sxml->string sxml-obj
                          cdata-section-elements indent
                          method ns-prefix-assig
                          omit-xml-declaration? standalone version)
  (srl:apply-string-append
   (srl:clean-fragments
    (srl:top->nested-str-lst (srl:normalize-sequence sxml-obj)
                             cdata-section-elements
                             (if (and indent (not (string? indent)))
                                 "  " indent)
                             method ns-prefix-assig
                             omit-xml-declaration? standalone version))))

; Writes the serialized representation of the `sxml-obj' to an output port
; `port'. The result returned by the function is unspecified.
(define (srl:display-sxml sxml-obj port-or-filename
                          cdata-section-elements indent
                          method ns-prefix-assig
                          omit-xml-declaration? standalone version)
  (if
   (string? port-or-filename)  ; a filename?
   (let ((out (open-output-file port-or-filename)))
     (begin
       (srl:display-top-out (srl:normalize-sequence sxml-obj) out
                            cdata-section-elements
                            (if (and indent (not (string? indent)))
                                "  " indent)
                            method ns-prefix-assig
                            omit-xml-declaration? standalone version)
       (display srl:newline out)  ; newline at the end of file
       (close-output-port out)))
   (srl:display-top-out (srl:normalize-sequence sxml-obj) port-or-filename
                        cdata-section-elements
                        (if (and indent (not (string? indent))) "  " indent)
                        method ns-prefix-assig
                        omit-xml-declaration? standalone version)))

;-------------------------------------------------
; Generalized serialization procedure, parameterizable with all the
; serialization params supported by this implementation

; procedure srl:parameterizable :: SXML-OBJ [PORT] {PARAM}* ->
;                                    -> STRING|unspecified
; sxml-obj - an SXML object to serialize
; param ::= (cons param-name param-value)
; param-name ::= symbol
; 
; 1. cdata-section-elements
; value ::= (listof sxml-elem-name)
; sxml-elem-name ::= symbol
;
; 2. indent
; value ::= 'yes | #t | 'no | #f | whitespace-string
;
; 3. method
; value ::= 'xml | 'html
;
; 4. ns-prefix-assig
; value ::= (listof (cons prefix namespace-uri))
; prefix ::= symbol
; namespace-uri ::= string
;
; 5. omit-xml-declaration?
; value ::= 'yes | #t | 'no | #f
;
; 6. standalone
; value ::= 'yes | #t | 'no | #f | 'omit
;
; 7. version
; value ::= string | number
;
; ATTENTION: If a parameter name is unexpected or a parameter value is
; ill-formed, the parameter is silently ignored. Probably, a warning message
; in such a case would be more appropriate.
;
; Example:
; (srl:parameterizable 
;   '(tag (@ (attr "value")) (nested "text node") (empty))
;   (current-output-port)
;   '(method . xml)  ; XML output method is used by default
;   '(indent . "\t")  ; use a single tabulation to indent nested elements
;   '(omit-xml-declaration . #f)  ; add XML declaration
;   '(standalone . yes)  ; denote a standalone XML document
;   '(version . "1.0"))  ; XML version
(define (srl:parameterizable sxml-obj . port-or-filename+params)
  (call-with-values
   (lambda ()
     (if (and (not (null? port-or-filename+params))
              (or (output-port? (car port-or-filename+params))
                  (string? (car port-or-filename+params))))
         (values (car port-or-filename+params) (cdr port-or-filename+params))
         (values #f port-or-filename+params)))
   (lambda (port-or-filename params)
     (let loop ((params params)
                (cdata-section-elements '())
                (indent "  ")
                (method 'xml)
                (ns-prefix-assig srl:conventional-ns-prefixes)
                (omit-xml-declaration? #t)
                (standalone 'omit)
                (version "1.0"))
       (cond
         ((null? params)  ; all parameters parsed
          (if port-or-filename
              (srl:display-sxml sxml-obj port-or-filename
                                cdata-section-elements indent
                                method ns-prefix-assig
                                omit-xml-declaration? standalone version)
              (srl:sxml->string sxml-obj
                                cdata-section-elements indent
                                method ns-prefix-assig
                                omit-xml-declaration? standalone version)))
         ((or (not (pair? (car params)))  ; not a pair or has no param value
              (null? (cdar params)))
          (loop (cdr params)
                cdata-section-elements indent
                method ns-prefix-assig
                omit-xml-declaration? standalone version))
         (else
          (let ((prm-value (cdar params)))
            (case (caar params)
              ((cdata-section-elements)
               (loop (cdr params)
                     (if (list? prm-value) prm-value cdata-section-elements)
                     indent method ns-prefix-assig
                     omit-xml-declaration? standalone version))
              ((indent)
               (loop (cdr params)
                     cdata-section-elements
                     (cond
                       ((boolean? prm-value)
                        (if prm-value "  " prm-value))
                       ((string? prm-value) prm-value)
                       ((eq? prm-value 'yes) "  ")
                       ((eq? prm-value 'no) #f)
                       (else indent))
                     method ns-prefix-assig
                     omit-xml-declaration? standalone version))
              ((method)
               (loop (cdr params)
                     cdata-section-elements indent
                     (if (or (eq? prm-value 'xml) (eq? prm-value 'html))
                         prm-value method)
                     ns-prefix-assig
                     omit-xml-declaration? standalone version))
              ((ns-prefix-assig)
               (loop (cdr params)
                     cdata-section-elements indent method
                     (if (and (list? prm-value)
                              (not (srl:mem-pred  ; no non-pair members
                                    (lambda (x) (not (pair? x)))
                                    prm-value)))
                         (append prm-value ns-prefix-assig)
                         ns-prefix-assig)
                     omit-xml-declaration? standalone version))
              ((omit-xml-declaration)
               (loop (cdr params)
                     cdata-section-elements indent
                     method ns-prefix-assig
                     (cond
                       ((boolean? prm-value) prm-value)                       
                       ((eq? prm-value 'yes) #t)
                       ((eq? prm-value 'no) #f)
                       (else indent))
                     standalone version))
              ((standalone)
               (loop (cdr params)
                     cdata-section-elements indent
                     method ns-prefix-assig omit-xml-declaration?
                     (cond
                       ((memv prm-value '(yes no omit))
                        prm-value)
                       ((boolean? prm-value)
                        (if prm-value 'yes 'no))
                       (else standalone))
                     version))
              ((version)
               (loop (cdr params)
                     cdata-section-elements indent
                     method ns-prefix-assig
                     omit-xml-declaration? standalone
                     (if (or (string? prm-value) (number? prm-value))
                         prm-value version)))
              (else
               (loop (cdr params)
                     cdata-section-elements indent
                     method ns-prefix-assig
                     omit-xml-declaration? standalone version))))))))))

;-------------------------------------------------
; High-level functions for popular serialization use-cases
; These functions use only a subset of serializer functionality, however, this
; subset seems sufficient for most practical purposes.

; procedure srl:sxml->xml :: SXML-OBJ [PORT-OR-FILENAME] -> STRING|unspecified
;
; Serializes the `sxml-obj' into XML, with indentation to facilitate
; readability by a human.
;
; sxml-obj - an SXML object (a node or a nodeset) to be serialized
; port-or-filename - an output port or an output file name, an optional
;  argument
; If `port-or-filename' is not supplied, the functions return a string that
; contains the serialized representation of the `sxml-obj'.
; If `port-or-filename' is supplied and is a port, the functions write the
; serialized representation of `sxml-obj' to this port and return an
; unspecified result.
; If `port-or-filename' is supplied and is a string, this string is treated as
; an output filename, the serialized representation of `sxml-obj' is written to
; that filename and an unspecified result is returned. If a file with the given
; name already exists, the effect is unspecified.
(define (srl:sxml->xml sxml-obj . port-or-filename)
  (if (null? port-or-filename)
      (srl:sxml->string sxml-obj '() #t 'xml
                        srl:conventional-ns-prefixes #t 'omit "1.0")
      (srl:display-sxml sxml-obj (car port-or-filename) '() #t 'xml
                        srl:conventional-ns-prefixes #t 'omit "1.0")))

; procedure srl:sxml->xml-noindent :: SXML-OBJ [PORT-OR-FILENAME] ->
;                                      -> STRING|unspecified
;
; Serializes the `sxml-obj' into XML, without indentation.
(define (srl:sxml->xml-noindent sxml-obj . port-or-filename)
  (if (null? port-or-filename)
      (srl:sxml->string sxml-obj '() #f 'xml
                        srl:conventional-ns-prefixes #t 'omit "1.0")
      (srl:display-sxml sxml-obj (car port-or-filename) '() #f 'xml
                        srl:conventional-ns-prefixes #t 'omit "1.0")))

; procedure srl:sxml->html :: SXML-OBJ [PORT-OR-FILENAME] -> STRING|unspecified
;
; Serializes the `sxml-obj' into HTML, with indentation to facilitate
; readability by a human.
;
; sxml-obj - an SXML object (a node or a nodeset) to be serialized
; port-or-filename - an output port or an output file name, an optional
;  argument
; If `port-or-filename' is not supplied, the functions return a string that
; contains the serialized representation of the `sxml-obj'.
; If `port-or-filename' is supplied and is a port, the functions write the
; serialized representation of `sxml-obj' to this port and return an
; unspecified result.
; If `port-or-filename' is supplied and is a string, this string is treated as
; an output filename, the serialized representation of `sxml-obj' is written to
; that filename and an unspecified result is returned. If a file with the given
; name already exists, the effect is unspecified.
(define (srl:sxml->html sxml-obj . port-or-filename)
  (if (null? port-or-filename)
      (srl:sxml->string sxml-obj '() #t 'html '() #t 'omit "4.0")
      (srl:display-sxml sxml-obj (car port-or-filename)
                        '() #t 'html '() #t 'omit "4.0")))

; procedure srl:sxml->html-noindent :: SXML-OBJ [PORT-OR-FILENAME] ->
;                                       -> STRING|unspecified
;
; Serializes the `sxml-obj' into HTML, without indentation.
(define (srl:sxml->html-noindent sxml-obj . port-or-filename)
  (if (null? port-or-filename)
      (srl:sxml->string sxml-obj '() #f 'html '() #t 'omit "4.0")
      (srl:display-sxml sxml-obj (car port-or-filename)
                        '() #f 'html '() #t 'omit "4.0")))

(provide (all-defined)))
