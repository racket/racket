;; 1/2/2006: Added a mapping for uri path segments 
;; that allows more characters to remain decoded 
;; -robby

;;;
;;; <uri-codec-unit.ss> ---- En/Decode URLs and form-urlencoded data
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
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
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

(module uri-codec-unit mzscheme

  (require (lib "unitsig.ss")
           (lib "match.ss")
           (lib "string.ss")
           (lib "list.ss")
           (lib "etc.ss")
           "uri-codec-sig.ss")

  (provide uri-codec@)

  (define uri-codec@
    (unit/sig net:uri-codec^
      (import)

      (define (self-map-char ch) (cons ch ch))
      (define (self-map-chars str) (map self-map-char (string->list str)))

      ;; The characters that always map to themselves
      (define alphanumeric-mapping
        (self-map-chars
         "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

      ;; Characters that sometimes map to themselves
      (define safe-mapping (self-map-chars "-_.!~*'()"))

      ;; The strict URI mapping
      (define uri-mapping (append alphanumeric-mapping safe-mapping))

      ;; The uri path segment mapping from RFC 3986
      (define uri-path-segment-mapping
        (append alphanumeric-mapping
                safe-mapping
                (map (λ (c) (cons c c)) (string->list "@+,=$&:"))))

      ;; The form-urlencoded mapping
      (define form-urlencoded-mapping
        `(,@(self-map-chars ".-*_") (#\space . #\+) ,@alphanumeric-mapping))

      (define (number->hex-string number)
        (define (hex n) (string-ref "0123456789ABCDEF" n))
        (string #\% (hex (quotient number 16)) (hex (modulo number 16))))

      (define (hex-string->number hex-string)
        (string->number (substring hex-string 1 3) 16))

      (define ascii-size 128)

      ;; (listof (cons char char)) -> (values (vectorof string) (vectorof string))
      (define (make-codec-tables alist)
        (let ([encoding-table (build-vector ascii-size number->hex-string)]
              [decoding-table (build-vector ascii-size values)])
          (for-each (match-lambda
                     [(orig . enc)
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

      (define-values (form-urlencoded-encoding-vector
                      form-urlencoded-decoding-vector)
        (make-codec-tables form-urlencoded-mapping))

      ;; vector string -> string
      (define (encode table str)
        (apply string-append
               (map (lambda (byte) 
                      (cond
                        [(< byte ascii-size)
                         (vector-ref table byte)]
                        [else (number->hex-string byte)]))
                    (bytes->list (string->bytes/utf-8 str)))))

      ;; vector string -> string
      (define (decode table str)
        (define internal-decode
          (match-lambda
           [() (list)]
           [(#\% (? hex-digit? char1) (? hex-digit? char2) . rest)
            ;; This used to consult the table again, but I think that's
            ;;  wrong. For example %2b should produce +, not a space.
            (cons (string->number (string char1 char2) 16)
                  (internal-decode rest))]
           [((? ascii-char? char) . rest)
            (cons
             (vector-ref table (char->integer char))
             (internal-decode rest))]
           [(char . rest)
            (append
             (bytes->list (string->bytes/utf-8 (string char)))
             (internal-decode rest))]))
	(bytes->string/utf-8
	 (apply bytes (internal-decode (string->list str)))))
      
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
      (define (form-urlencoded-encode str)
        (encode form-urlencoded-encoding-vector str))

      ;; string -> string
      (define (form-urlencoded-decode str)
        (decode form-urlencoded-decoding-vector str))

      ;; listof (cons string string) -> string
      ;; http://www.w3.org/TR/html401/appendix/notes.html#ampersands-in-uris
      ;; listof (cons symbol string) -> string
      (define (alist->form-urlencoded args)
        (let* ([mode (current-alist-separator-mode)]
               [format-one
                (lambda (arg)
                  (let* ([name (car arg)]
                         [value (cdr arg)])
                    (string-append (form-urlencoded-encode (symbol->string name))
                                   "="
                                   (form-urlencoded-encode value))))]
               [strs (let loop ([args args])
                       (cond
                        [(null? args) null]
                        [(null? (cdr args)) (list (format-one (car args)))]
                        [else (list* (format-one (car args))
                                     (if (eq? mode 'amp) "&" ";")
                                     (loop (cdr args)))]))])
          (apply string-append strs)))

      ;; string -> listof (cons string string)
      ;; http://www.w3.org/TR/html401/appendix/notes.html#ampersands-in-uris
      (define (form-urlencoded->alist str)
	(define key-regexp #rx"[^=]*")
        (define value-regexp (case (current-alist-separator-mode)
                               [(semi) #rx"[^;]*"]
                               [(amp) #rx"[^&]*"]
                               [else #rx"[^&;]*"]))
        (define (next-key str start)
          (and (< start (string-length str))
               (match (regexp-match-positions key-regexp str start)
                 [((start . end))
                  (vector (let ([s (form-urlencoded-decode
                                    (substring str start end))])
                            (string->symbol s))
                          (add1 end))]
                 [#f #f])))
        (define (next-value str start)
          (and (< start (string-length str))
               (match (regexp-match-positions value-regexp str start)
                 [((start . end))
                  (vector (form-urlencoded-decode (substring str start end))
                          (add1 end))]
                 [#f #f])))
        (define (next-pair str start)
          (match (next-key str start)
            [#(key start)
             (match (next-value str start)
               [#(value start)
                (vector (cons key value) start)]
               [#f
                (vector (cons key "") (string-length str))])]
            [#f #f]))
        (let loop ([start 0]
                   [end (string-length str)]
                   [make-alist (lambda (x) x)])
          (if (>= start end)
            (make-alist '())
            (match (next-pair str start)
              [#(pair next-start)
               (loop next-start end (lambda (x) (make-alist (cons pair x))))]
              [#f (make-alist '())]))))

      (define current-alist-separator-mode
	(make-parameter 'amp-or-semi
                        (lambda (s)
                          (unless (memq s '(amp semi amp-or-semi))
                            (raise-type-error 'current-alist-separator-mode
                                              "'amp, 'semi, or 'amp-or-semi"
                                              s))
                          s))))))

;;; uri-codec-unit.ss ends here
