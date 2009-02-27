#lang scheme
(require scheme/pretty
         xml)

(provide/contract
 [format-xexpr/errors (any/c . -> . string?)]
 [reformat-xexpr-exn (string? . -> . xexpr/c)])

; Formating Xexprs
(define (format-xexpr/errors v)
  (pretty-format (format-xexpr v)))

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
     (mark-error "Not a valid Xexpr datum (Must be a string, symbol, exact nonnegative integer, comment, PI, or cdata.)" v)]))

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
  (map format-xexpr l))

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

; Reformating Xexpr errors
(define (parse-xexpr-error s)
  (with-input-from-string 
   s (lambda ()
       (define violator (read))
       (define c:broke (read))
       (define c:the (read))
       (define c:contract (read))
       (define contract-expr (read))
       (define c:on (read))
       (define contracted (read))
       (define c:semi (read-char))
       (define xml:msg (read-line))
       (define blank (read-line))
       (define c:context (read-line))
       (define not-xexpr (read))
       (when 
           (or (ormap eof-object? 
                    (list violator c:broke c:the c:contract contract-expr
                          c:on contracted c:semi xml:msg blank c:context not-xexpr))
               (not (andmap symbol=?
                            (list 'broke 'the 'contract 'on '|;| 'Context:)
                            (list c:broke c:the c:contract c:on c:semi c:context))))
         (error 'parse-xexpr-error "Not Xexpr error"))
       (values violator contract-expr contracted xml:msg not-xexpr))))

(define (reformat-xexpr-exn m)
  (with-handlers ([exn? (lambda _ m)])
    (define-values (violator contract-expr contracted xml:msg not-xexpr)
      (parse-xexpr-error m))
    `(span ,(format "~a broke the contract~n~a~non ~a;~a~n~nContext:~n"
                    violator (pretty-format contract-expr) contracted
                    xml:msg)
           ,(make-cdata #f #f (format-xexpr/errors not-xexpr)))))
