#lang racket/base

(require "uri-codec.rkt")

(provide

 ;; -- cgi methods --
 get-bindings
 get-bindings/post
 get-bindings/get
 output-http-headers
 generate-html-output
 generate-error-output
 bindings-as-html
 extract-bindings
 extract-binding/single
 get-cgi-method

 ;; -- general HTML utilities --
 string->html
 generate-link-text)

;; type bindings = list ((symbol . string))

;; --------------------------------------------------------------------

;; string->html : string -> string
;; -- the input is raw text, the output is HTML appropriately quoted

(define (string->html s)
  (apply string-append
         (map (lambda (c)
                (case c
                  [(#\<) "&lt;"]
                  [(#\>) "&gt;"]
                  [(#\&) "&amp;"]
                  [else (string c)]))
              (string->list s))))

(define default-text-color  "#000000")
(define default-bg-color    "#ffffff")
(define default-link-color  "#cc2200")
(define default-vlink-color "#882200")
(define default-alink-color "#444444")

;; generate-html-output :
;; html-string x list (html-string) x ... -> ()

(define (generate-html-output title body-lines
                              [text-color default-text-color]
                              [bg-color default-bg-color]
                              [link-color default-link-color]
                              [vlink-color default-vlink-color]
                              [alink-color default-alink-color])
  (let ([sa string-append])
    (for ([l `("Content-type: text/html"
               ""
               "<html>"
               "<!-- The form was processed, and this document was generated,"
               "     using the CGI utilities for Racket.  For more information"
               "     on Racket, see"
               "       http://racket-lang.org/"
               "     and for the CGI utilities, contact"
               "     (sk@cs.brown.edu). -->"
               "<head>"
               ,(sa "<title>" title "</title>")
               "</head>"
               ""
               ,(sa "<body bgcolor=\"" bg-color "\" text=\"" text-color "\"")
               ,(sa "      link=\"" link-color "\"")
               ,(sa "      vlink=\"" vlink-color "\" alink=\"" alink-color "\">")
               ""
               ,@body-lines
               ""
               "</body>"
               "</html>")])
      (display l)
      (newline))))

;; output-http-headers : -> void
(define (output-http-headers)
  (printf "Content-type: text/html\r\n\r\n"))

;; delimiter->predicate : symbol -> regexp
;; returns a regexp to read a chunk of text up to a delimiter (excluding it)
(define (delimiter->rx delimiter)
  (case delimiter
    [(amp)         #rx#"^[^&]*"]
    [(semi)        #rx#"^[^;]*"]
    [(amp-or-semi) #rx#"^[^&;]*"]
    [else (error 'delimiter->rx
                 "internal-error, unknown delimiter: ~e" delimiter)]))

;; get-bindings* : iport -> (listof (cons symbol string))
;; Reads all bindings from the input port.  The strings are processed to
;; remove the CGI spec "escape"s.
;; This code is _slightly_ lax: it allows an input to end in
;; (current-alist-separator-mode).  It's not clear this is legal by the
;; CGI spec, which suggests that the last value binding must end in an
;; EOF.  It doesn't look like this matters.
;; ELI: * Keeping this behavior for now, maybe better to remove it?
;;      * Looks like `form-urlencoded->alist' is doing almost exactly
;;        the same job this code does.
(define (get-bindings* method ip)
  (define (err fmt . xs)
    (generate-error-output
     (list (format "Server generated malformed input for ~a method:" method)
           (apply format fmt xs))))
  (define value-rx (delimiter->rx (current-alist-separator-mode)))
  (define (process str) (form-urlencoded-decode (bytes->string/utf-8 str)))
  (let loop ([bindings '()])
    (if (eof-object? (peek-char ip))
      (reverse bindings)
      (let ()
        (define name (car (or (regexp-match #rx"^[^=]+" ip)
                              (err "Missing field name before `='"))))
        (unless (eq? #\= (read-char ip))
          (err "No binding for `~a' field." name))
        (define value (car (regexp-match value-rx ip)))
        (read-char ip) ; consume the delimiter, possibly eof (retested above)
        (loop (cons (cons (string->symbol (process name)) (process value))
                    bindings))))))

;; get-bindings/post : () -> bindings
(define (get-bindings/post)
  (get-bindings* "POST" (current-input-port)))

;; get-bindings/get : () -> bindings
(define (get-bindings/get)
  (get-bindings* "GET" (open-input-string (getenv "QUERY_STRING"))))

;; get-bindings : () -> bindings
(define (get-bindings)
  (if (string=? (get-cgi-method) "POST")
    (get-bindings/post)
    (get-bindings/get)))

;; generate-error-output : list (html-string) -> <exit>
(define (generate-error-output error-message-lines)
  (generate-html-output "Internal Error" error-message-lines)
  (exit))

;; bindings-as-html : bindings -> list (html-string)
;; -- formats name-value bindings as HTML appropriate for displaying
(define (bindings-as-html bindings)
  `("<code>"
    ,@(map (lambda (bind)
             (string-append (symbol->string (car bind))
                            "&nbsp;--&gt;&nbsp;"
                            (cdr bind)
                            "<br>"))
           bindings)
    "</code>"))

;; extract-bindings : (string + symbol) x bindings -> list (string)
;; -- Extracts the bindings associated with a given name.  The semantics of
;; forms states that a CHECKBOX may use the same NAME field multiple times.
;; Hence, a list of strings is returned.  Note that the result may be the
;; empty list.
(define (extract-bindings field-name bindings)
  (let ([field-name (if (symbol? field-name)
                      field-name (string->symbol field-name))])
    (let loop ([found null] [bindings bindings])
      (if (null? bindings)
        found
        (if (equal? field-name (caar bindings))
          (loop (cons (cdar bindings) found) (cdr bindings))
          (loop found (cdr bindings)))))))

;; extract-binding/single : (string + symbol) x bindings -> string
;; -- used in cases where only one binding is supposed to occur
(define (extract-binding/single field-name bindings)
  (let* ([field-name (if (symbol? field-name)
                       field-name (string->symbol field-name))]
         [result (extract-bindings field-name bindings)])
    (cond
      [(null? result)
       (generate-error-output
        (cons (format "No binding for field `~a':<br>" field-name)
              (bindings-as-html bindings)))]
      [(null? (cdr result))
       (car result)]
      [else
       (generate-error-output
        (cons (format "Multiple bindings for field `~a' where one expected:<br>"
                      field-name)
              (bindings-as-html bindings)))])))

;; get-cgi-method : () -> string
;; -- string is either GET or POST (though future extension is possible)
(define (get-cgi-method)
  (or (getenv "REQUEST_METHOD")
      (error 'get-cgi-method "no REQUEST_METHOD environment variable")))

;; generate-link-text : string x html-string -> html-string
(define (generate-link-text url anchor-text)
  (string-append "<a href=\"" url "\">" anchor-text "</a>"))
