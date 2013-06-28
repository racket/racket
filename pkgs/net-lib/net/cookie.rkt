;;;
;;; <cookie.rkt> ---- HTTP cookies library
;;; Time-stamp: <03/04/25 10:50:05 noel>
;;;
;;; Copyright (C) 2002 by Francisco Solsona.
;;;
;;; This file is part of net.

;;; net is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; net is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with net; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA.

;;; Author: Francisco Solsona <solsona@acm.org>
;;
;;
;; Commentary:
;;
;; The PLT Web server offers functionality to keep state beyond the
;; capacity of HTTP cookies.  Believe or not, sometimes you may want
;; to keep stuff on the client end, like the theme the (anonymous)
;; user likes, a session id, etc., so I wrote this wanna be library.
;;
;; It uses some of the surfies you see on the require section below.
;; It doesn't actually "send" the Set-Cookie header to the browser, it
;; doesn't even add the string "Set-Cookie: " to the will-be cookie it
;; generates.  Why? well because you may want to use this code with
;; very different Web browsers, and the way to send them may vary
;; drastically.  For instance, if you are writing a CGI to be executed
;; from a non-cooperative (from the PLT Scheme point of view) web
;; server, like Apache, then you will `display' de cookie right to the
;; standard output.  If you use FastCGI, you will `display' it trhough
;; the `output' communication stream between your CGI, and the Web
;; browser, but if you are using PLT Scheme Web browser, you will use
;; `make-response/full', or similar, chances are you will add cookies
;; to the `extras' parameter (extra headers), as cons pairs... as in
;; `(("Set-Cookie" . "cookie=\"as_returned_by_this_library";etc"))'.
;;
;; You should think of this procedures as a `format' for cookies.
#lang racket/base

(require srfi/13/string
         srfi/14/char-set
         racket/serialize)
(provide
 cookie-name?
 cookie-value?
 cookie?
 valid-domain?
 set-cookie
 cookie:add-comment
 cookie:add-domain
 cookie:add-max-age
 cookie:add-expires
 cookie:add-path
 cookie:secure
 cookie:version
 ;; To actually return a cookie (string formated as a cookie):
 print-cookie
 ;; To parse the Cookies header:
 get-cookie
 get-cookie/single
 ;; exceptions
 (struct-out cookie-error))

(define-serializable-struct cookie
  (name value comment domain max-age path secure version expires) #:mutable)
(define-struct (cookie-error exn:fail) ())

;; error* : string args ... -> raises a cookie-error exception
;; constructs a cookie-error struct from the given error message
;; (added to fix exceptions-must-take-immutable-strings bug)
(define (error* fmt . args)
  (raise (make-cookie-error (apply format fmt args)
                            (current-continuation-marks))))

;; The syntax for the Set-Cookie response header is
;; set-cookie      =       "Set-Cookie:" cookies
;; cookies         =       1#cookie
;; cookie          =       NAME "=" VALUE *(";" cookie-av)
;; NAME            =       attr
;; VALUE           =       value
;; cookie-av       =       "Comment" "=" value
;;                  |       "Domain" "=" value
;;                  |       "Max-Age" "=" value
;;                  |       "Expires" "=" value
;;                  |       "Path" "=" value
;;                  |       "Secure"
;;                  |       "Version" "=" 1*DIGIT
(define (set-cookie name pre-value)
  (let ([value (to-rfc2109:value pre-value)])
    (unless (rfc2068:token? name)
      (error* "invalid cookie name: ~a / ~a" name value))
    (make-cookie name value
                 #f ; comment
                 #f ; current domain
                 #f ; at the end of session
                 #f ; current path
                 #f ; normal (non SSL)
                 #f ; default version
                 #f ; doesn't expire
                 )))

;;!
;;
;; (function (print-cookie cookie))
;;
;; (param cookie Cookie-structure "The cookie to return as a string")
;;
;; Formats the cookie contents in a string ready to be appended to a
;; "Set-Cookie: " header, and sent to a client (browser).
(define (print-cookie cookie)
  (define (format-if fmt val) (and val (format fmt val)))
  (unless (cookie? cookie) (error* "cookie expected, received: ~a" cookie))
  (string-join
   (filter values
           (list (format "~a=~a" (cookie-name cookie) (cookie-value cookie))
                 (format-if "Comment=~a" (cookie-comment cookie))
                 (format-if "Domain=~a" (cookie-domain cookie))
                 (format-if "Max-Age=~a" (cookie-max-age cookie))
                 (format-if "Path=~a" (cookie-path cookie))
                 (and (cookie-secure cookie) "Secure")
                 (format-if "Version=~a" (cookie-version cookie))
                 (format-if "expires=~a" (cookie-expires cookie))))
   "; "))

(define (cookie:add-comment cookie pre-comment)
  (let ([comment (to-rfc2109:value pre-comment)])
    (unless (cookie? cookie)
      (error* "cookie expected, received: ~a" cookie))
    (set-cookie-comment! cookie comment)
    cookie))

(define (cookie:add-domain cookie domain)
  (unless (valid-domain? domain)
    (error* "invalid domain: ~a" domain))
  (unless (cookie? cookie)
    (error* "cookie expected, received: ~a" cookie))
  (set-cookie-domain! cookie domain)
  cookie)

(define (cookie:add-expires cookie expires)
  (unless (string? expires)
    (error* "invalid expires: ~a" expires))
  (unless (cookie? cookie)
    (error* "cookie expected, received: ~a" cookie))
  (set-cookie-expires! cookie expires)
  cookie)

(define (cookie:add-max-age cookie seconds)
  (unless (and (integer? seconds) (not (negative? seconds)))
    (error* "invalid Max-Age for cookie: ~a" seconds))
  (unless (cookie? cookie)
    (error* "cookie expected, received: ~a" cookie))
  (set-cookie-max-age! cookie seconds)
  cookie)

(define (cookie:add-path cookie pre-path)
  (let ([path pre-path])
    (unless (cookie? cookie)
      (error* "cookie expected, received: ~a" cookie))
    (set-cookie-path! cookie path)
    cookie))

(define (cookie:secure cookie secure?)
  (unless (boolean? secure?)
    (error* "invalid argument (boolean expected), received: ~a" secure?))
  (unless (cookie? cookie)
    (error* "cookie expected, received: ~a" cookie))
  (set-cookie-secure! cookie secure?)
  cookie)

(define (cookie:version cookie version)
  (unless (integer? version)
    (error* "unsupported version: ~a" version))
  (unless (cookie? cookie)
    (error* "cookie expected, received: ~a" cookie))
  (set-cookie-version! cookie version)
  cookie)


;; Parsing the Cookie header:

(define char-set:all-but=
  (char-set-difference char-set:full (string->char-set "=")))

(define char-set:all-but-semicolon
  (char-set-difference char-set:full (string->char-set ";")))

;;!
;;
;; (function (get-all-results name cookies))
;;
;; Auxiliar procedure that returns all values associated with
;; `name' in the association list (cookies).
(define (get-all-results name cookies)
  (let loop ([c cookies])
    (if (null? c)
        '()
        (let ([pair (car c)])
          (if (string=? name (car pair))
              ;; found an instance of cookie named `name'
              (cons (cadr pair) (loop (cdr c)))
              (loop (cdr c)))))))

;; which typically looks like:
;;   (cookie . "test5=\"5\"; test1=\"1\"; test0=\"0\"; test1=\"20\"")
;; note that it can be multi-valued: `test1' has values: "1", and "20".  Of
;; course, in the same spirit, we only receive the "string content".
(define (get-cookie name cookies)
  (let ([cookies (map (lambda (p)
                        (map string-trim-both
                             (string-tokenize p char-set:all-but=)))
                      (string-tokenize cookies char-set:all-but-semicolon))])
    (get-all-results name cookies)))

;;!
;;
;; (function (get-cookie/single name cookies))
;;
;; (param name String "The name of the cookie we are looking for")
;; (param cookies String "The string (from the environment) with the content of the cookie header.")
;;
;; Returns the first name associated with the cookie named `name', if any, or #f.
(define (get-cookie/single name cookies)
  (let ([cookies (get-cookie name cookies)])
    (and (not (null? cookies)) (car cookies))))


;;;;;
;; Auxiliary procedures
;;;;;

;; token          = 1*<any CHAR except CTLs or tspecials>
;;
;; tspecials      = "(" | ")" | "<" | ">" | "@"
;;                | "," | ";" | ":" | "\" | <">
;;                | "/" | "[" | "]" | "?" | "="
;;                | "{" | "}" | SP | HT
(define char-set:tspecials
  (char-set-union (string->char-set "()<>@,;:\\\"/[]?={}")
                  char-set:whitespace
                  (char-set #\tab)))

(define char-set:control
  (char-set-union char-set:iso-control
                  (char-set (integer->char 127))));; DEL
(define char-set:token
  (char-set-difference char-set:ascii char-set:tspecials char-set:control))

;; token? : string -> boolean
;;
;; returns #t if s is an RFC 2068 token (see definition above), #f otherwise.
(define (rfc2068:token? s)
  (string-every char-set:token s))

;;!
;;
;; (function (quoted-string? s))
;;
;; (param s String "The string to check")
;;
;; Returns s if the string is an RFC 2068 quoted-string, #f otherwise. As in:
;; quoted-string  = ( <"> *(qdtext) <"> )
;; qdtext         = <any TEXT except <">>
;;
;; The backslash character ("\") may be used as a single-character quoting
;; mechanism only within quoted-string and comment constructs.
;;
;; quoted-pair    = "\" CHAR
;;
;; implementation note: I have chosen to use a regular expression rather than
;; a character set for this definition because of two dependencies: CRLF must
;; appear as a block to be legal, and " may only appear as \"
(define (rfc2068:quoted-string? s)
  (and (regexp-match?
        #rx"^\"([^\"\u0000-\u001F]| |\r\n|\t|\\\\\")*\"$"
        s)
       s))

;; value: token | quoted-string
(define (rfc2109:value? s)
  (or (rfc2068:token? s) (rfc2068:quoted-string? s)
      (rfc2068:quoted-string? (convert-to-quoted s))))

;; convert-to-quoted : string -> quoted-string?
;; takes the given string as a particular message, and converts the given
;; string to that representatation
(define (convert-to-quoted str)
  (string-append "\"" (regexp-replace* #rx"\"" str "\\\\\"") "\""))

;; string -> rfc2109:value?
(define (to-rfc2109:value s)
  (cond
    [(not (string? s))
     (error* "expected string, given: ~e" s)]
    
    ;; for backwards compatibility, just use the given string if it will work
    [(rfc2068:token? s) s]
    [(rfc2068:quoted-string? s) s]
    
    ;; ... but if it doesn't work (i.e., it's just a normal message) then try
    ;; to convert it into a representation that will work
    [(rfc2068:quoted-string? (convert-to-quoted s))
     => (λ (x) x)]
    [else
     (error* "could not convert the given string to an acceptable RFC 2109 value: ~s" s)]))

;;!
;;
;; (function (cookie-string? s))
;;
;; (param s String "String to check")
;;
;; Returns whether this is a valid string to use as the value or the
;; name (depending on value?) of an HTTP cookie.
(define (cookie-value? s)
  (and (string? s)
       (rfc2109:value? s)))

(define (cookie-name? s)
  (and (string? s) 
       ;; name:  token
       (rfc2068:token? s)))

;; Host names as per RFC 1123 and RFC952, more or less, anyway. :-)
(define char-set:hostname
  (let ([a-z-lowercase (ucs-range->char-set #x61 #x7B)]
        [a-z-uppercase (ucs-range->char-set #x41 #x5B)])
    (char-set-adjoin!
     (char-set-union char-set:digit a-z-lowercase a-z-uppercase)
     #\.)))

(define (valid-domain? dom)
  (and (string? dom)
       ;; Domain must start with a dot (.)
       (string=? (string-take dom 1) ".")
       ;; The rest are tokens-like strings separated by dots
       (string-every char-set:hostname dom)
       (<= (string-length dom) 76)))

(define (valid-path? v)
  (and (string? v) (rfc2109:value? v)))
