#lang racket/base

#|
People used to wonder why semicolons were the default.  We then
decided to switch the default back to ampersands --

  http://www.w3.org/TR/html401/appendix/notes.html#h-B.2.2

  We recommend that HTTP server implementors, and in particular, CGI
  implementors support the use of ";" in place of "&" to save authors
  the trouble of escaping "&" characters in this manner.

See more in PR8831.
|#

;;; <uri-codec.rkt> ---- En/Decode URLs and form-urlencoded data
;;; Time-stamp: <03/04/25 10:31:31 noel>
;;;
;;; Copyright (C) 2002 by Noel Welsh.
;;;
;;; This file is part of Net.

;;; Net is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; Net is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with Net; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA.

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;; Commentary:

;; The module provides functions to encode and decode strings using
;; the URI encoding rules given in RFC 2396, and to encode and decode
;; name/value pairs using the application/x-www-form-urlencoded
;; mimetype given the in HTML 4.0 specification.  There are minor
;; differences between the two encodings.

;; The URI encoding uses allows a few characters to be represented `as
;; is': a-Z, A-Z, 0-9, -, _, ., !, ~, *, ', ( and ).  The remaining
;; characters are encoded as %xx, where xx is the hex representation
;; of the integer value of the character (where the mapping
;; character<->integer is determined by US-ASCII if the integer is
;; <128).

;; The encoding, inline with RFC 2396's recommendation, represents a
;; character as is, if possible.  The decoding allows any characters
;; to be represented by their hex values, and allows characters to be
;; incorrectly represented `as is'.

;; The rules for the application/x-www-form-urlencoded mimetype given
;; in the HTML 4.0 spec are:

;;   1. Control names and values are escaped. Space characters are
;;   replaced by `+', and then reserved characters are escaped as
;;   described in [RFC1738], section 2.2: Non-alphanumeric characters
;;   are replaced by `%HH', a percent sign and two hexadecimal digits
;;   representing the ASCII code of the character. Line breaks are
;;   represented as "CR LF" pairs (i.e., `%0D%0A').

;;   2. The control names/values are listed in the order they appear
;;   in the document. The name is separated from the value by `=' and
;;   name/value pairs are separated from each other by `&'.

;; NB: RFC 2396 supersedes RFC 1738.

;; This differs slightly from the straight encoding in RFC 2396 in
;; that `+' is allowed, and represents a space.  We follow this
;; convention, encoding a space as `+' and decoding `+' as a space.
;; There appear to be some brain-dead decoders on the web, so we also
;; encode `!', `~', `'', `(' and ')' using their hex representation.
;; This is the same choice as made by the Java URLEncoder.

;; Draws inspiration from encode-decode.scm by Kurt Normark and a code
;; sample provided by Eli Barzilay

(require racket/match racket/string racket/list)

(provide uri-encode
         uri-decode
         uri-path-segment-encode
         uri-path-segment-decode
         uri-userinfo-encode
         uri-userinfo-decode
         uri-unreserved-encode
         uri-unreserved-decode
         uri-path-segment-unreserved-encode
         uri-path-segment-unreserved-decode
         form-urlencoded-encode
         form-urlencoded-decode
         alist->form-urlencoded
         form-urlencoded->alist
         current-alist-separator-mode)

(define (self-map-char ch) (cons ch ch))
(define (self-map-chars str) (map self-map-char (string->list str)))

;; The characters that always map to themselves
(define alphanumeric-mapping
  (self-map-chars
   "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

;; Characters that sometimes map to themselves
;; called 'mark' in RFC 3986
(define safe-mapping (self-map-chars "-_.!~*'()"))

;; The strict URI mapping
(define uri-mapping (append alphanumeric-mapping safe-mapping))

;; The uri path segment mapping from RFC 3986
(define path-segment-extra-mapping (self-map-chars "@+,=$&:"))
(define uri-path-segment-mapping
  (append uri-mapping
          path-segment-extra-mapping))

;; from RFC 3986
(define unreserved-mapping
  (append alphanumeric-mapping
          (self-map-chars "-._~")))

;; The uri path segment mapping from RFC 3986
(define uri-path-segment-unreserved-mapping
  (append unreserved-mapping
          path-segment-extra-mapping))

;; from RFC 3986
(define sub-delims-mapping
  (self-map-chars "!$&'()*+,;="))

;; The uri userinfo mapping from RFC 3986
(define uri-userinfo-mapping
  (append unreserved-mapping
          sub-delims-mapping
          (self-map-chars ":")))

;; The form-urlencoded mapping
(define form-urlencoded-mapping
  `(,@(self-map-chars ".-*_") (#\space . #\+) ,@alphanumeric-mapping))

(define (number->hex-string number)
  (define (hex n) (string-ref "0123456789ABCDEF" n))
  (string #\% (hex (quotient number 16)) (hex (modulo number 16))))

(define ascii-size 128)

;; (listof (cons char char)) -> (values (vectorof string) (vectorof string))
(define (make-codec-tables alist)
  (let ([encoding-table (build-vector ascii-size number->hex-string)]
        [decoding-table (build-vector ascii-size values)])
    (for-each (match-lambda
               [(cons orig enc)
                (vector-set! encoding-table
                             (char->integer orig)
                             (string enc))
                (vector-set! decoding-table
                             (char->integer enc)
                             (char->integer orig))])
              alist)

    (values encoding-table decoding-table)))

(define-values (uri-encoding-vector uri-decoding-vector)
  (make-codec-tables uri-mapping))

(define-values (uri-path-segment-encoding-vector
                uri-path-segment-decoding-vector)
  (make-codec-tables uri-path-segment-mapping))

(define-values (uri-userinfo-encoding-vector
                uri-userinfo-decoding-vector)
  (make-codec-tables uri-userinfo-mapping))

(define-values (uri-unreserved-encoding-vector
                uri-unreserved-decoding-vector)
  (make-codec-tables unreserved-mapping))

(define-values (uri-path-segment-unreserved-encoding-vector
                uri-path-segment-unreserved-decoding-vector)
  (make-codec-tables uri-path-segment-unreserved-mapping))

(define-values (form-urlencoded-encoding-vector
                form-urlencoded-decoding-vector)
  (make-codec-tables form-urlencoded-mapping))

;; vector string -> string
(define (encode table str)
  (apply string-append (map (lambda (byte)
                              (if (< byte ascii-size)
                                (vector-ref table byte)
                                (number->hex-string byte)))
                            (bytes->list (string->bytes/utf-8 str)))))

;; vector string -> string
(define (decode table str)
  (define internal-decode
    (match-lambda [(list) (list)]
                  [(list* #\% (? hex-digit? char1) (? hex-digit? char2) rest)
                   ;; This used to consult the table again, but I think that's
                   ;;  wrong. For example %2b should produce +, not a space.
                   (cons (string->number (string char1 char2) 16)
                         (internal-decode rest))]
                  [(cons (? ascii-char? char) rest)
                   (cons (vector-ref table (char->integer char))
                         (internal-decode rest))]
                  [(cons char rest)
                   ;; JBC : this appears to handle strings containing
                   ;; non-ascii characters; shouldn't this just be an 
                   ;; error?
                   (append
                    (bytes->list (string->bytes/utf-8 (string char)))
                    (internal-decode rest))]))
  (bytes->string/utf-8 (apply bytes (internal-decode (string->list str)))))

(define (ascii-char? c)
  (< (char->integer c) ascii-size))

(define (hex-digit? c)
  (or (char<=? #\0 c #\9)
      (char<=? #\a c #\f)
      (char<=? #\A c #\F)))

;; string -> string
(define (uri-encode str)
  (encode uri-encoding-vector str))

;; string -> string
(define (uri-decode str)
  (decode uri-decoding-vector str))

;; string -> string
(define (uri-path-segment-encode str)
  (encode uri-path-segment-encoding-vector str))

;; string -> string
(define (uri-path-segment-decode str)
  (decode uri-path-segment-decoding-vector str))

;; string -> string
(define (uri-userinfo-encode str)
  (encode uri-userinfo-encoding-vector str))

;; string -> string
(define (uri-userinfo-decode str)
  (decode uri-userinfo-decoding-vector str))

;; string -> string
(define (uri-unreserved-encode str)
  (encode uri-unreserved-encoding-vector str))

;; string -> string
(define (uri-unreserved-decode str)
  (decode uri-unreserved-decoding-vector str))

;; string -> string
(define (uri-path-segment-unreserved-encode str)
  (encode uri-path-segment-unreserved-encoding-vector str))

;; string -> string
(define (uri-path-segment-unreserved-decode str)
  (decode uri-path-segment-unreserved-decoding-vector str))

;; string -> string
(define (form-urlencoded-encode str)
  (encode form-urlencoded-encoding-vector str))

;; string -> string
(define (form-urlencoded-decode str)
  (decode form-urlencoded-decoding-vector str))

;; listof (cons string string) -> string
;; http://www.w3.org/TR/html401/appendix/notes.html#ampersands-in-uris
;; listof (cons symbol string) -> string
(define (alist->form-urlencoded args)
  (let* ([sep (if (memq (current-alist-separator-mode) '(semi semi-or-amp))
                ";" "&")]
         [format-one
          (lambda (arg)
            (let* ([name  (car arg)]
                   [value (cdr arg)]
                   [name  (form-urlencoded-encode (symbol->string name))]
                   [value (and value (form-urlencoded-encode value))])
              (if value (string-append name "=" value) name)))]
         [strs (if (null? args)
                 '()
                 (cons (format-one (car args))
                       (apply append
                              (map (lambda (a) (list sep (format-one a)))
                                   (cdr args)))))])
    (apply string-append strs)))

;; string -> listof (cons string string)
;; http://www.w3.org/TR/html401/appendix/notes.html#ampersands-in-uris
(define (form-urlencoded->alist str)
  (define keyval-regexp #rx"=")
  (define value-regexp
    (case (current-alist-separator-mode)
      [(semi) #rx"[;]"]
      [(amp) #rx"[&]"]
      [else #rx"[&;]"]))
  (define (parse-keyval keyval)
    (let (;; m = #f => no "=..." part
          [m (regexp-match-positions keyval-regexp keyval)])
      (cons (string->symbol (form-urlencoded-decode
                             (if m (substring keyval 0 (caar m)) keyval)))
            (and m (form-urlencoded-decode (substring keyval (cdar m)))))))
  (if (equal? "" str) '() (map parse-keyval (regexp-split value-regexp str))))

(define current-alist-separator-mode
  (make-parameter 'amp-or-semi
    (lambda (s)
      (unless (memq s '(amp semi amp-or-semi semi-or-amp))
        (raise-type-error 'current-alist-separator-mode
                          "'amp, 'semi, 'amp-or-semi, or 'semi-or-amp"
                          s))
      s)))

;;; uri-codec.rkt ends here
