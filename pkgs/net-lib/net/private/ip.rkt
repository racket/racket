#lang racket/base

;; A library for manipulating IP Addresses

(require racket/bool
         racket/contract
         racket/format
         racket/list
         racket/match
         racket/string
         racket/sequence)

(provide
  (contract-out
    ;; check if a given value is an IP address
    [ip-address? (-> any/c boolean?)]

    ;; check if a given string is a valid representation of an IP address
    [ip-address-string? (-> any/c boolean?)]

    ;; construct an IP address from various inputs
    [make-ip-address
     (-> (or/c ip-address-string?
               (bytes-of-length 4)
               (bytes-of-length 16))
         ip-address?)]

    ;; construct a string representation of the address
    [ip-address->string (-> ip-address? string?)]

    ;; return a byte string representation of the address
    [ip-address->bytes (-> ip-address? bytes?)]

    (struct ipv4 ([bytes (bytes-of-length 4)]))
    (struct ipv6 ([bytes (bytes-of-length 16)]))))

;; data definitions

;; An IPAddress is one of
;;   (ipv4 4Bytes)
;;   (ipv6 16Bytes)
;;
;; interp. an IPv4 address represented as four bytes
;;         an IPv6 address represented as sixteen bytes

(define (ip-address? x) (or (ipv4? x) (ipv6? x)))

(struct ipv4 (bytes)
        #:transparent
        #:guard (λ (bytes _) (bytes->immutable-bytes bytes))
        #:methods gen:equal+hash
        [(define (equal-proc addr1 addr2 rec)
           (equal? (ipv4-bytes addr1) (ipv4-bytes addr2)))
         (define (hash-proc addr rec) (rec (ipv4-bytes addr)))
         (define (hash2-proc addr rec) (rec (ipv4-bytes addr)))])

(struct ipv6 (bytes)
        #:transparent
        #:guard (λ (bytes _) (bytes->immutable-bytes bytes))
        #:methods gen:equal+hash
        [(define (equal-proc addr1 addr2 rec)
           (equal? (ipv6-bytes addr1) (ipv6-bytes addr2)))
         (define (hash-proc addr rec) (rec (ipv6-bytes addr)))
         (define (hash2-proc addr rec) (rec (ipv6-bytes addr)))])

(define (make-ip-address input)
  (match input
    ;; TODO: make more efficient by not double checking
    [(? ipv4-string?) (ipv4 (ipv4-string->bytes input))]
    [(? ipv6-string?) (ipv6 (ipv6-string->bytes input))]
    [(? (bytes-of-length 4)) (ipv4 input)]
    [(? (bytes-of-length 16)) (ipv6 input)]))

(define (ip-address-string? val)
  (and (string? val)
       (or (ipv4-string? val)
           (ipv6-string? val))))

;; String -> Boolean
;; Check if the input string represents an IPv4 address
(define (ipv4-string? str)
  ;; String -> Boolean
  ;; check if the given string has leading zeroes
  (define (has-leading-zeroes? str)
    (and (> (string-length str) 1)
         (char=? (string-ref str 0) #\0)))
  (define matches
    (regexp-match #px"^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$"
                  str))
  (and matches
       (= (length matches) 5)
       ;; check that each octet field is an octet
       (andmap byte? (map string->number (cdr matches)))
       ;; leading zeroes lead to query errors
       (not (ormap has-leading-zeroes? (cdr matches)))))

;; String -> Boolean
;; Check if the input string represents an IPv6 address
;; TODO: support dotted quad notation
(define (ipv6-string? str)
  (define re-::/: #px"^([0-9a-fA-F]{1,4})(::|:)")
  (define re-:: #px"^()(::)")
  (define re-: #px"^([0-9a-fA-F]{1,4})(:)")
  (define re-end #px"^[0-9a-fA-F]{1,4}$")
  (or (regexp-match? #px"^::$" str) ; special case
      (let loop ([octet-pairs '()]  ; keep octet-pairs to count
                 [::? #f]           ; seen a :: in the string yet?
                 [str str])
        ;; match digit groups and a separator
        (define matches
          (if ::?
              (regexp-match re-: str)
              (or (regexp-match re-:: str)
                  (regexp-match re-::/: str))))
        (cond [matches
               (match-define (list match digits sep) matches)
               (define rest (substring str (string-length match)))
               ;; we need to make sure there is only one :: at most
               (if (or ::? (string=? sep "::"))
                   (loop (cons digits octet-pairs) #t rest)
                   (loop (cons digits octet-pairs) #f rest))]
              [else
               (and ;; if there isn't a ::, we need 7+1 octet-pairs
                    (implies (not ::?) (= (length octet-pairs) 7))
                    ;; this is the +1 octet pair
                    (regexp-match? re-end str))]))))

;; String -> Bytes
;; converts a string representating an IPv4 address to bytes
(define (ipv4-string->bytes ip)
  (let ([result (regexp-match #rx"^([0-9]+)\\.([0-9]+)\\.([0-9]+)\\.([0-9]+)$"
                              ip)])
    (bytes (string->number (list-ref result 1))
           (string->number (list-ref result 2))
           (string->number (list-ref result 3))
           (string->number (list-ref result 4)))))

;; String -> Bytes
;; converts a string representing an IPv6 address to bytes
(define (ipv6-string->bytes ip)
  ;; String -> Bytes of length 2
  ;; turn a string of two octets and write two bytes
  (define (octet-pair-string->bytes two-octs)
    (define n (string->number two-octs 16))
    (define byte1 (arithmetic-shift (bitwise-and #xff00 n) -8))
    (define byte2 (bitwise-and #x00ff n))
    (bytes byte1 byte2))

  (define has-::? (regexp-match? #rx"::" ip))
  (define splitted (regexp-split #rx":" ip))
  (define not-empty-str (filter (λ (s) (not (string=? "" s))) splitted))
  (define pad-amount (* 2 (- 8 (length not-empty-str))))
  (let loop ([result #""] [splitted splitted])
    (cond [(empty? splitted) result]
          [(string=? (car splitted) "")
           (loop (bytes-append result (make-bytes pad-amount 0))
                 (remove* '("") (cdr splitted)))]
          [else
           (loop (bytes-append result (octet-pair-string->bytes (car splitted)))
                 (cdr splitted))])))

;; IPAddress -> Bytestring
;; Turn an ip struct into a byte string
(define (ip-address->bytes ip)
  (match ip
    [(? ipv4?) (ipv4-bytes ip)]
    [(? ipv6?) (ipv6-bytes ip)]))

;; IPAddress -> String
;; Convert an IP address to a string
(define (ip-address->string ip)
  (match ip
    [(? ipv4?) (ipv4->string (ipv4-bytes ip))]
    [(? ipv6?) (ipv6->string (ipv6-bytes ip))]))

;; Bytes -> String
;; Convert a bytestring for an IPv4 address to a string
(define (ipv4->string bytes)
  (string-join (for/list ([b (in-bytes bytes)]) (~r b)) "."))

;; Bytes -> String
;; Convert a bytestring representing an IPv6 address to a string
(define (ipv6->string bytes)
  (define two-octets
    (for/list ([oct-pair (in-slice 2 (in-bytes bytes))])
      (define oct1 (car oct-pair))
      (define oct2 (cadr oct-pair))
      (+ (arithmetic-shift oct1 8) oct2)))
  (define compressed (compress two-octets))
  ;; add an extra "" if :: is at the start
  (define compressed-strs
    (for/list ([elem (in-list compressed)])
      (if (eq? elem '::)
          "" ; string-join will turn this into ::
          (~r elem #:base 16))))
  (define compressed-strs*
    (if (string=? (car compressed-strs) "")
        (cons "" compressed-strs)
        compressed-strs))
  (string-join compressed-strs* ":"))

;; (Listof Number) -> (Listof (U Number '::))
;; Compress an IPv6 address to its shortest representation
(define (compress lon)
  (let loop ([acc '()] [lon lon])
    (cond [(empty? lon) (reverse acc)]
          [else
           (define zeroes
             (for/list ([n (in-list lon)] #:break (not (zero? n))) n))
           (define num-zs (length zeroes))
           (if (<= num-zs 1)
               (loop (cons (car lon) acc) (cdr lon))
               (append (reverse acc) '(::) (drop lon num-zs)))])))

;; contract helper
(define (bytes-of-length n)
  (flat-named-contract
    `(bytes-of-length ,n)
    (λ (bs) (and (bytes? bs) (= (bytes-length bs) n)))))
