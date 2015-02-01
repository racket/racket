#lang racket

(provide (contract-out
          [cookie-name? (-> any/c boolean?)]
          [cookie-value? (-> any/c boolean?)]
          [path/extension-value? (-> any/c boolean?)]
          [domain-value? (-> any/c boolean?)]
          ))

(require racket/match)

;;;;;;;;; Cookie names ;;;;;;;;;

(require srfi/13 srfi/14) ; for charsets, and testing strings against them

;; cookie-name? : Any -> Bool
;; true iff s is a token, per RFC6265; see below
(define (cookie-name? s)
  (or (and (bytes? s)
           (not (zero? (bytes-length s)))
           (for/and ([b (in-bytes s)]) (token-byte? b)))
      (and (string? s)
           (not (zero? (string-length s)))
           (string-every char-set:token s))))

;; token          = 1*<any CHAR except CTLs or separators>
;; separator      = "(" | ")" | "<" | ">" | "@"
;;                | "," | ";" | ":" | "\" | <">
;;                | "/" | "[" | "]" | "?" | "="
;;                | "{" | "}" | SP | HT
;; see also RFC2616 Sec 2.2

(define (token-byte? b)
  (and (< 31 b 127) (not (separator-byte? b)))) ; exclude CTLs and seps
(define (separator-byte? b)
  (member b (bytes->list #"()<>@,;:\\\"/[]?={} \t")))

(define char-set:separators
  (char-set-union (string->char-set "()<>@,;:\\\"/[]?={}")
                  char-set:whitespace
                  (char-set #\tab)))
(define char-set:control
  (char-set-union char-set:iso-control (char-set (integer->char 127))));; DEL
(define char-set:token
  (char-set-difference char-set:ascii char-set:separators char-set:control))

;;;;;;;;; Cookie values ;;;;;;;;;

;; cookie-value? : Any -> Boolean
;; true iff x is a valid cookie value, per RFC6265. From the RFC:
;;   cookie-value    = *cookie-octet
;;                   / ( DQUOTE *cookie-octet DQUOTE )
;; where cookie-octet is defined below
(define (cookie-value? x)
  (or (and (bytes? x)
           (let ([len (bytes-length x)])
             (or (and (>= len 2)
                      (= (bytes-ref x 0) DQUOTE)
                      (= (bytes-ref x (- len 1)) DQUOTE)
                      (all-cookie-octets? (subbytes x 1 (- len 1))))
                 (all-cookie-octets? x))))
      (and (string? x)
           (or (string-every char-set:cookie-octets x)
               (let ([m (regexp-match #rx"^\"(.*)\"$" x)])
                 (match m
                   [(list _ quoted-text)
                    (string-every char-set:cookie-octets quoted-text)]
                   [_ #f]))))))

(define (all-cookie-octets? x)
  (for/and ([b (in-bytes x)]) (cookie-octet-byte? b)))
(define DQUOTE #x22)
;; From the RFC:
;;      path-value = *av-octet
;;    extension-av = *av-octet
;; where av-octet is defined below.
(define (path/extension-value? x) ; : Any -> Boolean
  (and (string? x) (string-every char-set:av-octets x)))


;; Per RFC1034.3.5 (with the RFC1123 revision to allow domain name
;;   components to start with a digit):
;; subdomain = label *("." label)
;; label     = ( ALPHA / DIGIT ) [ *ldh (ALPHA / DIGIT) ]
;; ldh       = ALPHA / DIGIT / "-"

(define domain-label-rx
  ;; Regexp matching one component of a domain name:
  #px"^[[:alnum:]][[:alnum:]-]*[[:alnum:]]$")

;; Test if dom is a valid domain name. From the RFC:
;;      domain-value = <subdomain>
;;                     ; as def'd in RFC1034 Sec 3.5
;;                     ; and enhanced by RFC1123 Sec 2.1
(define (domain-value? dom) ; Any -> Boolean
  (or (and (string? dom)
       (let ([parts (string-split dom "." #:trim? #f)])
         (and (not (null? parts))
              (for/and ([part parts])
                (regexp-match domain-label-rx part))))
       #t)))

;;;; Underlying charsets

;; From the RFC:
;;      cookie-octet = <US-ASCII chars excluding CTLs, whitespace, DQUOTE,
;;                      comma, semicolon, and backslash>
;;          av-octet = <any CHAR except CTLs or #\;>
;;               CTL = ASCII octets 0-31 and 127

;; Charset used in cookie values includes the following chars:
;; ( ) ! # $ % & '  * + - . / 0 1 2 3 4 5 6 7 8 9 : < = > ? @ [ ] ^ _ `
;; { | } ~ A-Z a-z

(define (cookie-octet-byte? x)
  (and (< 31 x 127) (not (memv x non-cookie-octet-bytes))))
(define non-cookie-octet-bytes (map char->integer (string->list " \t\",;\\")))

(define char-set:cookie-octets
  (char-set-difference char-set:ascii
                       char-set:control char-set:whitespace
                       (string->char-set "\",;\\")))

;; Chars used in path-av and extension-av values:
#;
(define (cookie-av-octet-byte? x)
  (and (< 31 x 127) (not (= x #x3B)))) ; #x3B is #\;
(define char-set:av-octets
  (char-set-difference char-set:ascii char-set:control (char-set #\;)))
