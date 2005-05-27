;;;
;;; <cookie-unit.ss> ---- HTTP cookies library
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
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

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

(module cookie-unit mzscheme
  (require (lib "unitsig.ss")
           (lib "etc.ss")
	   (lib "list.ss")
	   (lib "string.ss" "srfi" "13")
	   (lib "char-set.ss" "srfi" "14")
           "cookie-sig.ss")

  (provide cookie@)


  (define cookie@
    (unit/sig net:cookie^
      (import)
  
      (define-struct cookie (name value comment domain max-age path secure version))
      (define-struct (cookie-error exn) ())
  
      ;; The syntax for the Set-Cookie response header is
      ;; set-cookie      =       "Set-Cookie:" cookies
      ;; cookies         =       1#cookie
      ;; cookie          =       NAME "=" VALUE *(";" cookie-av)
      ;; NAME            =       attr
      ;; VALUE           =       value
      ;; cookie-av       =       "Comment" "=" value
      ;; 		      |       "Domain" "=" value
      ;; 		      |       "Max-Age" "=" value
      ;; 		      |       "Path" "=" value
      ;; 		      |       "Secure"
      ;; 		      |       "Version" "=" 1*DIGIT
      (define set-cookie
        (lambda (name value)
          (unless (and (cookie-string? name #f)
                       (cookie-string? value))
            (raise (make-cookie-error (format "Invalid NAME/VALUE pair: ~a / ~a" name value) (current-continuation-marks))))
          (make-cookie name value
                       #f;; comment
                       #f;; current domain
                       #f;; at the end of session
                       #f;; current path
                       #f;; normal (non SSL)
                       #f;; default version
                       )))

      ;;!
      ;;
      ;; (function (print-cookie cookie))
      ;;
      ;; (param cookie Cookie-structure "The cookie to return as a string")
      ;;
      ;; Formats the cookie contents in a string ready to be appended to a
      ;; "Set-Cookie: " header, and sent to a client (browser).
      (define print-cookie
        (lambda (cookie)
          (unless (cookie? cookie)
            (raise (make-cookie-error (format "Cookie expected, received: ~a" cookie) (current-continuation-marks))))
          (string-join
           (filter (lambda (s)
                     (not (string-null? s)))
                   (list (format "~a=~a" (cookie-name cookie) (cookie-value cookie))
                         (let ((c (cookie-comment cookie))) (if c (format "Comment=~a" c) ""))
                         (let ((d (cookie-domain cookie))) (if d (format "Domain=~a" d) ""))
                         (let ((age (cookie-max-age cookie))) (if age (format "Max-Age=~a" age) ""))
                         (let ((p (cookie-path cookie))) (if p (format "Path=~a" p) ""))
                         (let ((s (cookie-secure cookie))) (if s "Secure" ""))
                         (let ((v (cookie-version cookie))) (format "Version=~a" (if v v 1)))))
           "; ")))

      (define cookie:add-comment
        (lambda (cookie comment)
          (unless (cookie-string? comment)
            (raise (make-cookie-error (format "Invalid comment: ~a" comment) (current-continuation-marks))))
          (unless (cookie? cookie)
            (raise (make-cookie-error (format "Cookie expected, received: ~a" cookie) (current-continuation-marks))))
          (set-cookie-comment! cookie comment)
          cookie))

      (define cookie:add-domain
        (lambda (cookie domain)
          (unless (valid-domain? domain)
            (raise (make-cookie-error (format "Invalid domain: ~a" domain) (current-continuation-marks))))
          (unless (cookie? cookie)
            (raise (make-cookie-error (format "Cookie expected, received: ~a" cookie) (current-continuation-marks))))
          (set-cookie-domain! cookie domain)
          cookie))

      (define cookie:add-max-age
        (lambda (cookie seconds)
          (unless (and (integer? seconds) (not (negative? seconds)))
            (raise (make-cookie-error (format "Invalid Max-Age for cookie: ~a" seconds) (current-continuation-marks))))
          (unless (cookie? cookie)
            (raise (make-cookie-error (format "Cookie expected, received: ~a" cookie) (current-continuation-marks))))
          (set-cookie-max-age! cookie seconds)
          cookie))

      (define cookie:add-path
        (lambda (cookie path)
          (unless (string? path)
            (raise (make-cookie-error (format "Invalid path: ~a" path) (current-continuation-marks))))
          (unless (cookie? cookie)
            (raise (make-cookie-error (format "Cookie expected, received: ~a" cookie) (current-continuation-marks))))
          (set-cookie-path! cookie path)
          cookie))
   
      (define cookie:secure
        (lambda (cookie secure?)
          (unless (boolean? secure?)
            (raise (make-cookie-error (format "Invalid argument (boolean expected), received: ~a" secure?) (current-continuation-marks))))
          (unless (cookie? cookie)
            (raise (make-cookie-error (format "Cookie expected, received: ~a" cookie) (current-continuation-marks))))
          (set-cookie-secure! cookie secure?)
          cookie))

      (define cookie:version
        (lambda (cookie version)
          (unless (integer? version)
            (raise (make-cookie-error (format "Unsupported version: ~a" version) (current-continuation-marks))))
          (unless (cookie? cookie)
            (raise (make-cookie-error (format "Cookie expected, received: ~a" cookie) (current-continuation-marks))))
          (set-cookie-version! cookie version)
          cookie))


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
      (define get-all-results
        (lambda (name cookies)
          (let loop ((c cookies))
            (cond ((null? c) ())
                  (else
                   (let ((pair (car c)))
                     (if (string=? name (car pair))
                         ;; found an instance of cookie named `name'
                         (cons (cadr pair) (loop (cdr c)))
                         (loop (cdr c)))))))))
   
      ;; which tipically looks like: (cookie . "test5=\"5\"; test1=\"1\"; test0=\"0\"; test1=\"20\"")
      ;; note that it can be multi-valued: `test1' has values: "1", and "20".
      ;; Of course, in the same spirit, we only receive the "string content".
      (define get-cookie
        (lambda (name cookies)
          (let ((cookies (map (lambda (p)
                                (map string-trim-both
                                     (string-tokenize p char-set:all-but=)))
                              (string-tokenize cookies char-set:all-but-semicolon))))
            (get-all-results name cookies))))

      ;;!
      ;;
      ;; (function (get-cookie/single name cookies))
      ;;
      ;; (param name String "The name of the cookie we are looking for")
      ;; (param cookies String "The string (from the environment) with the content of the cookie header.")
      ;;
      ;; Returns the first name associated with the cookie named `name', if any, or #f.
      (define get-cookie/single
        (lambda (name cookies)
          (let ((cookies (get-cookie name cookies)))
            (and (not (null? cookies))
                 (car cookies)))))

   
   
   ;;;;;
      ;; Auxiliar procedures
   ;;;;;
   

      ;; token          = 1*<any CHAR except CTLs or tspecials>
      ;; 
      ;; tspecials      = "(" | ")" | "<" | ">" | "@"
      ;;	            | "," | ";" | ":" | "\" | <">
      ;;	            | "/" | "[" | "]" | "?" | "="
      ;;	            | "{" | "}" | SP | HT
      (define char-set:tspecials
        (char-set-union
         (char-set-difference char-set:punctuation (string->char-set "_"))
         char-set:whitespace))
      (define char-set:control (char-set-union char-set:iso-control
                                               (char-set (integer->char 127))));; DEL
      (define char-set:token (char-set-difference char-set:ascii char-set:tspecials char-set:control))

      ;;!
      ;;
      ;; (function (quoted-string? s))
      ;;
      ;; (param s String "The string to check")
      ;;
      ;; Returns #t only if the string is surrounded by double quotes.  As in:
      ;; quoted-string  = ( <"> *(qdtext) <"> )
      ;; qdtext         = <any TEXT except <">>
      (define quoted-string?
        (lambda (s)
          (and (string=? (string-take s 1) "\"")
               (string=? (string-take-right s 1) "\""))))

      ;;!
      ;;
      ;; (function (cookie-string? s))
      ;;
      ;; (param s String "String to check")
      ;;
      ;; Returns whether this is a valid string to use as the value or the
      ;; name (depending on value?) of an HTTP cookie.
      (define cookie-string?
        (opt-lambda (s (value? #t))
          (unless (string? s)
            (raise (make-cookie-error (format "String expected, received: ~a" s) (current-continuation-marks))))
          (if value?
              ;; value: token | quoted-string
              (or (string-every char-set:token s)
                  (quoted-string? s))
              ;; name:  token
              (string-every char-set:token s))))

      ;; Host names as per RFC 1123 and RFC952, more or less, anyway. :-)
      (define char-set:hostname
        (let ((a-z-lowercase (ucs-range->char-set #x61 #x7B))
              (a-z-uppercase (ucs-range->char-set #x41 #x5B)))
          (char-set-adjoin!
           (char-set-union char-set:digit a-z-lowercase a-z-uppercase)
           #\. )))
  
      (define valid-domain?
        (lambda (dom)
          (and
           ;; Domain must start with a dot (.)
           (string=? (string-take dom 1) ".")
           ;; The rest are tokens-like strings separated by dots
           (string-every char-set:hostname dom)
           (<= (string-length dom) 76))))
      ))
  )

;;; cookie-unit.ss ends here