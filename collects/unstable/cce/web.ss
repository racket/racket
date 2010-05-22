#lang scheme
(require xml
         "define.ss"
         "function.ss"
         "text.ss")

;; css/c : FlatContract
;; Recognizes representations of Cascading Style Sheets.
(define css/c (listof (cons/c text/c (listof (list/c text/c text/c)))))

(provide/contract
 [css/c flat-contract?]
 [css? (-> any/c boolean?)]
 [write-css (->* [css/c] [output-port?] void?)])

;; A Cascading Style Sheet (CSS) is a (Listof StyleDefn)
;; A Style Definition (StyleDefn) is a (cons Selectors (Listof PropDefn))
;; A Selectors is a Selector or a (NonEmptyListof Selector)
;; A Selector is a Symbol or String
;; A Property Definition (PropDefn) is a (list PropName PropVal)
;; A Property Name (PropName) is a Symbol or String
;; A Property Value (PropVal) is a Symbol or String

;; css? : Any -> Boolean
;; Reports whether a value is a CSS.
(define css? (flat-contract-predicate css/c))

;; write-css : CSS [OutputPort] -> Void
;; Writes a CSS datastructure as a proper text Cascading Style Sheet.
(define write-css
  (lambda/parameter (css [output #:param current-output-port])
    (for-each write-style-defn css)))

;; write-style-defn : StyleDefn [OutputPort] -> Void
;; Writes a style definition to a Cascading Style Sheet.
(define write-style-defn
  (lambda/parameter (style-defn [output #:param current-output-port])
    (write-selector (car style-defn))
    (display " {")
    (for-each write-prop-defn (cdr style-defn))
    (display " }\n")))

;; write-text : Text [OutputPort] -> Void
;; Writes a text field to a Cascading Style Sheet.
(define write-text
  (lambda/parameter (text [output #:param current-output-port])
    (display (text->string text))))

;; write-selector : Selector [OutputPort] -> Void
;; Writes a selector to a Cascading Style Sheet.
(define write-selector write-text)

;; write-prop-defn : PropDefn [OutputPort] -> Void
;; Writes a property definition to a Cascading Style Sheet.
(define write-prop-defn
  (lambda/parameter (prop-defn [output #:param current-output-port])
    (display " ")
    (write-prop-name (car prop-defn))
    (display " : ")
    (write-prop-val (cadr prop-defn))
    (display ";")))

;; write-prop-name : PropName [OutputPort] -> Void
;; Writes a property name to a Cascading Style Sheet.
(define write-prop-name write-text)

;; write-prop-val : PropVal [OutputPort] -> Void
;; Writes a property value to a Cascading Style Sheet.
(define write-prop-val write-text)

(define-if-unbound xexpr/c
  (flat-named-contract "Xexpr" xexpr?))

(provide xexpr/c)
(provide/contract
 [write-xexpr (->* [xexpr/c] [output-port?] void?)])

(define write-xexpr
  (lambda/parameter (xexpr [output #:param current-output-port])
    (write-xml/content (xexpr->xml xexpr))))

(provide/contract
 [create-webpage (string? xexpr/c . -> . void?)]
 [create-stylesheet (string? css/c . -> . void?)])

;; create-stylesheet : String CSS -> Void
;; Writes an individual stylesheet to a file.
(define (create-stylesheet filename css)
  (let* ([out-port (open-output-file filename #:exists 'replace)])
    (write-css css out-port)
    (close-output-port out-port)))

;; create-webpage : String XExpr -> Void
;; Writes an individual webpage to a file.
(define (create-webpage filename xexpr)
  (let* ([out-port (open-output-file filename #:exists 'replace)])
    (write-xexpr xexpr out-port)
    (close-output-port out-port)))
