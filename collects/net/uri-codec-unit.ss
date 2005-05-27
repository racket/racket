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
	   (lib "etc.ss")
           "uri-codec-sig.ss")

  (provide uri-codec@)

  ;; Macro to loop over integers from n (inclusive) to m (exclusive)
  ;; Extracted from iteration.ss in macro package at Schematics
  (define-syntax for
        (syntax-rules ()
          ((for (i n m) forms ...)
           (let ((fixed-m m))
                 (let loop ((i n))
                   (if (< i fixed-m)
                           (begin
                                 forms ...
                                 (loop (+ i 1)))))))))

  (define uri-codec@
    (unit/sig net:uri-codec^
      (import)

      ;; The characters that always map to themselves
      (define alphanumeric-mapping
        (map (lambda (char)
               (cons char char))
             '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
               #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
               #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
               #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d
               #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n
               #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x
               #\y #\z)))

      ;; Characters that sometimes map to themselves
      (define safe-mapping
        (map (lambda (char)
               (cons char char))
             '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\))))

      ;; The strict URI mapping
      (define uri-mapping
        (append alphanumeric-mapping safe-mapping))

      ;; The form-urlencoded mapping
      (define form-urlencoded-mapping
        (append '((#\. . #\.)
                  (#\- . #\-)
                  (#\* . #\*)
                  (#\_ . #\_)
                  (#\space . #\+))
                alphanumeric-mapping))

      (define (number->hex-string number)
        (let ((hex (number->string number 16)))
          (string-append "%"
                         (if (= (string-length hex) 1)
                             (string-append "0" hex)
                             hex))))

      (define (hex-string->number hex-string)
        (string->number (substring hex-string 1 3) 16))

      ;; (listof (cons char char)) -> (values (vectorof string) (vectorof string))
      (define (make-codec-tables alist)
        (let ((encoding-table (make-vector 256 ""))
              (decoding-table (make-vector 256 0)))
          (for (i 0 256)
               (vector-set! encoding-table i (number->hex-string i))
               (vector-set! decoding-table i i))
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

      (define-values (form-urlencoded-encoding-vector
                      form-urlencoded-decoding-vector)
        (make-codec-tables form-urlencoded-mapping))

      ;; vector string -> string
      (define (encode table str)
        (apply string-append
               (map (lambda (byte)
                      (vector-ref table byte))
                    (bytes->list (string->bytes/utf-8 str)))))

      ;; vector string -> string
      (define (decode table str)
        (define internal-decode
          (match-lambda
           [() (list)]
           [(#\% (? hex-digit? char1) (? hex-digit? char2) . rest)
            ;; This used to consult the table again, but I think that's
            ;;  wrong. For exmaple %2b should produce +, not a space.
            (cons (string->number (string char1 char2) 16)
                  (internal-decode rest))]
           [(char . rest)
            (cons
             (vector-ref table
                         (char->integer char))
             (internal-decode rest))]))
	(bytes->string/utf-8
	 (apply bytes (internal-decode (string->list str)))))

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
      (define (form-urlencoded-encode str)
        (encode form-urlencoded-encoding-vector str))

      ;; string -> string
      (define (form-urlencoded-decode str)
        (decode form-urlencoded-decoding-vector str))

      ;; listof (cons string string) -> string
      ;; http://www.w3.org/TR/html401/appendix/notes.html#ampersands-in-uris
      ;; listof (cons symbol string) -> string
      (define alist->form-urlencoded
        (opt-lambda (args)
          (let* ([mode (current-alist-separator-mode)]
		 [format-one
                  (lambda (arg)
                    (let* ([name (car arg)]
                           [value (cdr arg)])
                      (string-append 
                       (form-urlencoded-encode (symbol->string name))
                       "="
                       (form-urlencoded-encode value))))]
                 [strs (let loop ([args args])
                         (cond
                           [(null? args) null]
                           [(null? (cdr args)) (list (format-one (car args)))]
                           [else (list* (format-one (car args))
                                        (if (eq? mode 'amp)
					    "&"
					    ";")
                                        (loop (cdr args)))]))])
            (apply string-append strs))))

      ;; string -> listof (cons string string)  
      ;; http://www.w3.org/TR/html401/appendix/notes.html#ampersands-in-uris
      (define form-urlencoded->alist
	(opt-lambda (str)
	  (define key-regexp (regexp "[^=]*"))
	  (define value-regexp (case (current-alist-separator-mode)
				 [(semi) (regexp "[^;]*")]
				 [(amp) (regexp "[^&]*")]
				 [else (regexp "[^&;]*")]))
	  (define (next-key str start)
	    (if (>= start (string-length str))
		#f
		(match (regexp-match-positions key-regexp str start)
		  [((start . end))
		   (vector (let ([s (form-urlencoded-decode (substring str start end))])
			     (string-lowercase! s)
			     (string->symbol s))
			   (add1 end))]
		  [#f #f])))
	  (define (next-value str start)
	    (if (>= start (string-length str))
		#f
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
	    (cond
	     [(>= start end) (make-alist '())]
	     [else
	      (match (next-pair str start)
		[#(pair next-start)
		 (loop next-start end (lambda (x) (make-alist (cons pair x))))]
		[#f (make-alist '())])]))))

      (define current-alist-separator-mode
	(make-parameter 'amp-or-semi (lambda (s)
				       (unless (memq s '(amp semi amp-or-semi))
					 (raise-type-error 'current-alist-separator-mode
							   "'amp, 'semi, or 'amp-or-semi"
							   s))
				       s)))

      ))
  )

;;; uri-codec-unit.ss ends here
