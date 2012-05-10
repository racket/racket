#lang racket/base
(require racket/contract
         racket/match
         racket/list
         racket/pretty
         xml)

(define-struct (exn:pretty exn) (xexpr))

(provide/contract
 [struct (exn:pretty exn) ([message string?]
                           [continuation-marks continuation-mark-set?]
                           [xexpr xexpr/c])]
 [format-xexpr/errors (any/c . -> . string?)]
 [pretty-xexpr/c contract?])

(define pretty-xexpr/c
  (make-contract
   #:name 'pretty-xexpr/c
   #:projection
   (lambda (blame)
     (lambda (val)
       (define marks (current-continuation-marks))
       (with-handlers ([exn:fail:contract?
                        (lambda (exn)
                          (raise
                           (make-exn:pretty
                            (exn-message exn)
                            marks
                            `(span ,(drop-after "Context:\n" (exn-message exn)) "\n"
                                   ,(make-cdata #f #f (format-xexpr/errors val))))))])
         (((contract-projection xexpr/c) blame) val))))))

(define (drop-after delim str)
  (match (regexp-match-positions (regexp-quote delim) str)
    [(list-rest (list-rest start end) _rst)
     (substring str 0 end)]
    [_
     str]))

; Formating Xexprs
(define (format-xexpr/errors v)
  (pretty-format (format-xexpr v) 80))

(define-struct xexpr-error (message content)
  #:property prop:custom-write
  (lambda (v port write?)
    (display "<a title=\"" port)
    (display (xexpr-error-message v) port)
    (display "\" class=\"error\">" port)
    ; XXX Can this be XML escaped?
    (print (xexpr-error-content v) port)
    (display "</a>" port)))
(define mark-error make-xexpr-error)

(define (xexpr-datum? v)
  (or (string? v)
      (symbol? v)
      (exact-nonnegative-integer? v)
      (comment? v)
      (p-i? v)
      (pcdata? v)
      (cdata? v)))

(define (format-xexpr v)
  (cond
    [(pair? v)
     (cond
       [(empty? v)
        (mark-error "Not a valid Xexpr element (No tag; Tag must be a symbol.)" v)]
       [(symbol? (car v))
        (list* (car v)
               (format-elements+attributes (cdr v)))]
       [else
        (list* (mark-error "Not a valid Xexpr element tag (Must be a symbol.)" (car v))
               (format-elements+attributes (cdr v)))])]
    [(xexpr-datum? v) v]
    [else
     (mark-error "Not a valid Xexpr datum (Must be a string, symbol, exact nonnegative integer, comment, PI, pcdata, or cdata.)" v)]))

(define (format-elements+attributes l)
  (match l
    ; ()
    [(list) empty]
    ; (datum ...)
    [(list-rest (? xexpr-datum?) other-elems)
     (format-elements l)]
    ; ((p ...) ...)
    [(list-rest (list-rest (? symbol?) inner-elems) other-elems)
     (format-elements l)]
    ; (attrs ...)
    [(list-rest attrs elems)
     (list* (format-attributes attrs)
            (format-elements elems))]
    [else
     (mark-error 
      "Not a valid Xexpr tag content list. (Must be either (1) a list of Xexprs or (2) An attribute list followed by a list of Xexprs.)"
      l)]))

(define (format-elements l)
  (if (list? l)
      (map format-xexpr l)
      (mark-error
       "Expected a list of elements"
       l)))

(define (format-attributes l)
  (match l
    [(list) empty]
    [(list-rest attr attrs)
     (list* (format-attribute attr)
            (format-attributes attrs))]
    [else
     (mark-error 
      "Not a valid attribute list (Must be list of attributes. An attribute is a list containing a symbol and a string.)"
      l)]))

(define (format-attribute l)
  (match l
    [(list (? symbol? attr) (? string? val))
     l]
    [(list (? symbol? attr) val)
     (list attr (mark-error "Not a valid attribute value (Must be string.)" val))]
    [(list attr (? string? val))
     (list (mark-error "Not a valid attribute name (Must be symbol.)" attr) val)]
    [else
     (mark-error "Not a valid attribute (Must be a list of a symbol and a string.)" l)]))
