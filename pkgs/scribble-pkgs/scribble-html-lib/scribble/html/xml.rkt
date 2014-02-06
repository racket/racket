#lang racket/base

;; XML-like objects and functions, with rendering

(require scribble/text racket/port)

;; ----------------------------------------------------------------------------
;; Represent attribute names as `foo:' symbols.  They are made self-quoting in
;; the language.  A different option would be to use the usual racket keyword
;; arguments, but that tends to have problems like disallowing repeated uses of
;; the same keyword, sorting the keywords alphabetically, and ambiguity when
;; some keywords are meant to do the usual thing (customize a function) instead
;; of representing an attribute.  It's more convenient to just have a separate
;; mechanism for this, so racket keywords are still used in the same way, and
;; orthogonal to specifying attributes.  Another possibility is to have a new
;; type, with `foo:' evaluating to instances -- but it's often convenient to
;; pass them around as quoted lists.

(define attribute->symbol
  (let ([t (make-weak-hasheq)])
    (lambda (x)
      (and (symbol? x)
           (hash-ref! t x
             (lambda ()
               (define m (regexp-match #rx"^(.*):$" (symbol->string x)))
               (and m (string->symbol (cadr m)))))))))

(provide attribute?)
(define attribute? attribute->symbol)

(provide attributes+body)
(define (attributes+body xs)
  (let loop ([xs xs] [as '()])
    (define a (and (pair? xs) (attribute->symbol (car xs))))
    (cond [(not a) (values (reverse as) xs)]
          [(null? (cdr xs)) (error 'attriubtes+body
                                   "missing attribute value for `~s:'" a)]
          [else (loop (cddr xs) (cons (cons a (cadr xs)) as))])))

;; similar, but keeps the attributes as a list, useful to build new functions
;; that accept attributes without knowing about the xml structs.
(provide split-attributes+body)
(define (split-attributes+body xs)
  (let loop ([xs xs] [as '()])
    (if (and (pair? xs) (pair? (cdr xs)) (attribute->symbol (car xs)))
      (loop (cddr xs) (list* (cadr xs) (car xs) as))
      (values (reverse as) xs))))

;; ----------------------------------------------------------------------------
;; An output that handles xml quoting, customizable

;; TODO: make this more conveniently customizable and extensible
(define (write-string/xml-quote str p [start 0] [end (string-length str)])
  (let loop ([start start])
    (when (< start end)
      (define m (regexp-match-positions #rx"[&<>\"]" str start end p))
      (when m
        (write-string (case (string-ref str (caar m))
                        [(#\&) "&amp;"]
                        [(#\<) "&lt;"]
                        [(#\>) "&gt;"]
                        [(#\") "&quot;"])
                      p)
        (loop (cdar m))))))

(provide xml-writer)
(define xml-writer (make-parameter write-string/xml-quote))

(provide output-xml)
(define (output-xml content [p (current-output-port)])
  (output (disable-prefix (with-writer (xml-writer) content)) p))

(provide xml->string)
(define (xml->string content)
  (with-output-to-string (lambda () (output-xml content))))

;; ----------------------------------------------------------------------------
;; Structs for xml data: elements, literals, entities

(provide make-element)
(struct element (tag attrs body [cache #:auto #:mutable])
  #:constructor-name make-element
  #:transparent #:omit-define-syntaxes #:auto-value #f
  #:property prop:procedure
  (lambda (e)
    (unless (element-cache e) (set-element-cache! e (element->output e)))
    (element-cache e)))

(provide element)
(define (element tag . args)
  (define-values [attrs body] (attributes+body args))
  (make-element tag attrs body))

;; similar to element, but will always have a closing tag instead of using the
;; short syntax (see also `element->output' below)
(provide element/not-empty)
(define (element/not-empty tag . args)
  (define-values [attrs body] (attributes+body args))
  (make-element tag attrs (if (null? body) '(#f) body)))

;; convert an element to something output-able
(define (element->output e)
  (define tag   (element-tag   e))
  (define attrs (element-attrs e))
  (define body  (element-body  e))
  ;; null body means a lone tag, tags that should always have a closer will
  ;; have a '(#f) as their body (see below)
  (list (with-writer #f "<" tag)
        (map (lambda (attr)
               (define name (car attr))
               (define val (cdr attr))
               (cond [(not val) #f]
                     ;; #t means just mention the attribute
                     [(eq? #t val) (with-writer #f (list " " name))]
                     [else (list (with-writer #f (list " " name "=\""))
                                 val
                                 (with-writer #f "\""))]))
             attrs)
        (if (null? body)
          (with-writer #f " />")
          (list (with-writer #f ">")
                body
                (with-writer #f "</" tag ">")))))

;; ----------------------------------------------------------------------------
;; Literals

;; literal "struct" for things that are not escaped
(provide literal)
(define (literal . contents) (with-writer #f contents))

;; entities are implemented as literals
(provide entity)
(define (entity x) (literal "&" (and (number? x) "#") x ";"))

;; comments and cdata
(provide comment)
(define (comment #:newlines? [newlines? #f] . body)
  (define newline (and newlines? "\n"))
  (literal "<!--" newline body newline "-->"))
(provide cdata)
(define (cdata #:newlines? [newlines? #t] #:line-prefix [pfx #f] . body)
  (define newline (and newlines? "\n"))
  (literal pfx "<![CDATA[" newline body newline pfx "]]>"))

;; ----------------------------------------------------------------------------
;; Template definition forms

(provide define/provide-elements/empty
         define/provide-elements/not-empty
         define/provide-entities)
(define-syntax-rule (define/provide-elements/empty tag ...)
  (begin (provide tag ...)
         (define (tag . args) (apply element 'tag args)) ...))
(define-syntax-rule (define/provide-elements/not-empty tag ...)
  (begin (provide tag ...)
         (define (tag . args) (apply element/not-empty 'tag args)) ...))
(define-syntax-rule (define/provide-entities ent ...)
  (begin (provide ent ...)
         (define ent ; use string-append to make it a little faster
           (literal (string-append "&" (symbol->string 'ent) ";")))
         ...))
