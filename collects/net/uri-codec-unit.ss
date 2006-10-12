;; 1/2/2006: Added a mapping for uri path segments 
;; that allows more characters to remain decoded 
;; -robby


#|

People often seem to wonder why semicolons are the default in this code, 
and not ampersands. Here's are the best answers we have:

From: Doug Orleans <dougorleans@gmail.com>
To: plt-scheme@list.cs.brown.edu
Subject: Re: [plt-scheme] Problem fetching a URL
Date: Wed, 11 Oct 2006 16:18:40 -0400
X-Mailer: VM 7.19 under 21.4 (patch 19) "Constant Variable" XEmacs Lucid

Robby Findler writes:
 > Do you (or does anyone else) have a reference to an rfc or similar that
 > actually says what the syntax for queries is supposed to be?
 > rfc3986.txt (the latest url syntax rfc I know of) doesn't seem to say.

The HTML 4.01 spec defines the MIME type application/x-www-form-urlencoded:

http://www.w3.org/TR/html401/interact/forms.html#form-content-type

See also XForms, which uses a "separator" attribute whose default
value is a semicolon:

http://www.w3.org/TR/xforms/slice11.html#serialize-urlencode
http://www.w3.org/TR/xforms/slice3.html#structure-model-submission

--dougorleans@gmail.com
_________________________________________________
  For list-related administrative tasks:
  http://list.cs.brown.edu/mailman/listinfo/plt-scheme






From: John David Stone <stone@math.grinnell.edu>
To: plt-scheme@list.cs.brown.edu
Subject: Re: [plt-scheme] Problem fetching a URL
Date: Wed, 11 Oct 2006 11:36:14 -0500
X-Mailer: VM 7.19 under Emacs 21.4.1

-----BEGIN PGP SIGNED MESSAGE-----
Hash: SHA1

        Danny Yoo:

 > > Just out of curiosity, why is current-alist-separator-mode using 
 > > semicolons by default rather than ampersands?  I understand that 
 > > flexibility is nice, but this is the fifth time I've seen people hit this 
 > > as a roadblock; shouldn't the default be what's most commonly used?

        Robby Findler:

 > It is my understanding that semi-colons are more standards compliant.
 > That's why it is the default.

        According to the RFC1738 (http://www.ietf.org/rfc/rfc1738.txt),
semicolons and ampersands are equally acceptable in URLs that begin with
`http://' (section 5, page 17), but semicolons are ``reserved'' characters
(section 3.3, page 8) and so are allowed to appear unencoded in the URL
(section 2.2, page 3), whereas ampersands are not reserved in such URLs and
so must be encoded (as, say, &#38;).  RFC2141
(http://www.ietf.org/rfc/rfc2141.txt) extends this rule to Uniform Resource
Names generally, classifying ampersands as ``excluded'' characters that
must be encoded whenever used in URNs (section 2.4, page 3).

        The explanation and rationale is given in Appendix B
(``Performance, implementation, and design notes,''
http://www.w3.org/TR/html401/appendix/notes.html) of the World Wide Web
Consortium's technical report defining HTML 4.01.  Section B.2.2
(``Ampersands in URI attribute values'') of that appendix notes that the
use of ampersands in URLs to carry information derived from forms is, in
practice, a serious glitch and source of errors, since it tempts careless
implementers and authors of HTML documents to insert those ampersands in
URLs without encoding them.  This practice conflicts with the simple and
otherwise standard convention, derived from SGML, that an ampersand is
always the opening delimiter of a character entity reference.  So the World
Wide Web Consortium encourages implementers to use and recognize semicolons
rather than ampersands in URLs that carry information derived from forms.

-----BEGIN PGP SIGNATURE-----
Version: GnuPG v1.4.5 (GNU/Linux)
Comment: Processed by Mailcrypt 3.5.8+ <http://mailcrypt.sourceforge.net/>

iD4DBQFFLR16bBGsCPR0ElQRAizVAJddgT63LKc6UWqRyHh57aqWjSXGAJ4wyseS
JALQefhDMCATcl2/bZL0bw==
=W2uS
-----END PGP SIGNATURE-----

|#


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
