#lang racket/base

;; DNS query library for Racket

(require racket/bool
         racket/contract
         racket/format
         racket/match
         racket/string
         racket/system
         racket/udp
         (only-in unstable/sequence in-slice))

(provide (contract-out
          [dns-get-address
           (->* (ip-address-string? string?)
                (#:ipv6? any/c)
                ip-address-string?)]
          [dns-get-name
           (-> ip-address-string? ip-address-string? string?)]
          [dns-get-mail-exchanger
           (-> ip-address-string? string? (or/c bytes? string?))]
          [dns-find-nameserver
           (-> (or/c ip-address-string? #f))]))

(module+ test (require rackunit))

;; UDP retry timeout:
(define INIT-TIMEOUT 50)

;; Contract utilities and Data Definitions
;;
;; An LB is a (Listof Bytes)
;;
;; An IPAddressString passes the following predicate
;;
;; Any -> Boolean
;; check if the input string represents an IPv4 address
(define (ip-address-string? val)
  ;; String -> Boolean
  ;; check if the given string has leading zeroes
  (define (has-leading-zeroes? str)
    (and (> (string-length str) 1)
         (char=? (string-ref str 0) #\0)))
  (define (ipv4-string? str)
    (define matches
      (regexp-match #px"^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$"
                    str))
    (and matches
         (= (length matches) 5)
         ;; check that each octet field is an octet
         (andmap byte? (map string->number (cdr matches)))
         ;; leading zeroes lead to query errors
         (not (ormap has-leading-zeroes? matches))))
  ;; TODO: support dotted quad notation
  (define (ipv6-string? str)
    (define re-::/: #px"^([0-9a-fA-F]{1,4})(::|:)")
    (define re-:: #px"^()(::)")
    (define re-: #px"^([0-9a-fA-F]{1,4})(:)")
    (define re-end #px"^[0-9a-fA-F]{1,4}$")
    (or (regexp-match? #px"^::$" str)   ; special case
        (let loop ([octet-pairs '()]    ; keep octet-pairs to count
                   [::? #f]             ; seen a :: in the string yet?
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
  (and (string? val)
       (or (ipv4-string? val)
           (ipv6-string? val))))

(module+ test
  (check-true (ip-address-string? "8.8.8.8"))
  (check-true (ip-address-string? "12.81.255.109"))
  (check-true (ip-address-string? "192.168.0.1"))
  (check-true (ip-address-string? "2001:0db8:85a3:0000:0000:8a2e:0370:7334"))
  (check-true (ip-address-string? "2001:200:dff:fff1:216:3eff:feb1:44d7"))
  (check-true (ip-address-string? "2001:db8:85a3:0:0:8a2e:370:7334"))
  (check-true (ip-address-string? "2001:db8:85a3::8a2e:370:7334"))
  (check-true (ip-address-string? "0:0:0:0:0:0:0:1"))
  (check-true (ip-address-string? "0:0:0:0:0:0:0:0"))
  (check-true (ip-address-string? "::"))
  (check-true (ip-address-string? "::0"))
  (check-true (ip-address-string? "::ffff:c000:0280"))
  (check-true (ip-address-string? "2001:db8::2:1"))
  (check-true (ip-address-string? "2001:db8:0:0:1::1"))
  (check-false (ip-address-string? ""))
  (check-false (ip-address-string? ":::"))
  (check-false (ip-address-string? "::0::"))
  (check-false (ip-address-string? "2001::db8::2:1"))
  (check-false (ip-address-string? "2001:::db8:2:1"))
  (check-false (ip-address-string? "52001:db8::2:1"))
  (check-false (ip-address-string? "80.8.800.8"))
  (check-false (ip-address-string? "80.8.800.0"))
  (check-false (ip-address-string? "080.8.800.8"))
  (check-false (ip-address-string? "vas8.8.800.8"))
  (check-false (ip-address-string? "80.8.128.8dd"))
  (check-false (ip-address-string? "0.8.800.008"))
  (check-false (ip-address-string? "0.8.800.a8"))
  (check-false (ip-address-string? "potatoes"))
  (check-false (ip-address-string? "127.0.0")))

;; A Type is one of the following
(define types
  '((a 1)
    (ns 2)
    (md 3)
    (mf 4)
    (cname 5)
    (soa 6)
    (mb 7)
    (mg 8)
    (mr 9)
    (null 10)
    (wks 11)
    (ptr 12)
    (hinfo 13)
    (minfo 14)
    (mx 15)
    (txt 16)
    (aaaa 28)))

;; A Class is one of the following
(define classes
  '((in 1)
    (cs 2)
    (ch 3)
    (hs 4)))

;;;

(define (cossa i l)
  (cond [(null? l) #f]
        [(equal? (cadar l) i) (car l)]
        [else (cossa i (cdr l))]))

(define (number->octet-pair n)
  (list (arithmetic-shift n -8)
        (modulo n 256)))

(define (octet-pair->number a b)
  (+ (arithmetic-shift a 8) b))

(define (octet-quad->number a b c d)
  (+ (arithmetic-shift a 24)
     (arithmetic-shift b 16)
     (arithmetic-shift c 8)
     d))

;; Bytes -> LB
;; Convert the domain name into a sequence of labels, where each
;; label is a length octet and then that many octets
(define (name->octets s)
  (let ([do-one (lambda (s) (cons (bytes-length s) (bytes->list s)))])
    (let loop ([s s])
      (let ([m (regexp-match #rx#"^([^.]*)[.](.*)" s)])
        (if m
            (append (do-one (cadr m)) (loop (caddr m)))
            ;; terminate with zero length octet
            (append (do-one s) (list 0)))))))

;; The query header. See RFC1035 4.1.1 for details
;;
;; The opcode & flags are set as:
;; QR | OPCODE  | AA | TC | RD | RA | Z     | RCODE   |
;; 0  | 0 0 0 0 | 0  | 0  | 1  | 0  | 0 0 0 | 0 0 0 0 |
;;
(define (make-std-query-header id question-count)
  (append (number->octet-pair id)             ; 16-bit random identifier
          (list 1 0)                          ; Opcode & flags
          (number->octet-pair question-count) ; QDCOUNT
          (number->octet-pair 0)              ; ANCOUNT
          (number->octet-pair 0)              ; NSCOUNT
          (number->octet-pair 0)))            ; ARCOUNT

;; Int16 Bytes Type Class -> LB
;; Construct a DNS query message
(define (make-query id name type class)
  (append (make-std-query-header id 1)
          ;; Question section. See RF1035 4.1.2
          (name->octets name)                 ; QNAME
          (number->octet-pair                 ; QTYPE
           (cadr (assoc type types)))
          (number->octet-pair                 ; QCLASS
           (cadr (assoc class classes)))))

(define (add-size-tag m)
  (append (number->octet-pair (length m)) m))

(define (rr-data rr)
  (cadddr (cdr rr)))

(define (rr-type rr)
  (cadr rr))

(define (rr-name rr)
  (car rr))

(define (parse-name start reply)
  (let ([v (car start)])
    (cond
      [(zero? v)
       ;; End of name
       (values #f (cdr start))]
      [(zero? (bitwise-and #xc0 v))
       ;; Normal label
       (let loop ([len v][start (cdr start)][accum null])
         (if (zero? len)
           (let-values ([(s start) (parse-name start reply)])
             (let ([s0 (list->bytes (reverse accum))])
               (values (if s (bytes-append s0 #"." s) s0)
                       start)))
           (loop (sub1 len) (cdr start) (cons (car start) accum))))]
      [else
       ;; Compression offset
       (let ([offset (+ (arithmetic-shift (bitwise-and #x3f v) 8)
                        (cadr start))])
         (let-values ([(s ignore-start)
                       (parse-name (list-tail reply offset) reply)])
           (values s (cddr start))))])))

(define (parse-rr start reply)
  (let-values ([(name start) (parse-name start reply)])
    (let* ([type (car (cossa (octet-pair->number (car start) (cadr start))
                             types))]
           [start (cddr start)]
           ;;
           [class (car (cossa (octet-pair->number (car start) (cadr start))
                              classes))]
           [start (cddr start)]
           ;;
           [ttl (octet-quad->number (car start) (cadr start)
                                    (caddr start) (cadddr start))]
           [start (cddddr start)]
           ;;
           [len (octet-pair->number (car start) (cadr start))]
           [start (cddr start)])
      ;; Extract next len bytes for data:
      (let loop ([len len] [start start] [accum null])
        (if (zero? len)
          (values (list name type class ttl (reverse accum))
                  start)
          (loop (sub1 len) (cdr start) (cons (car start) accum)))))))

(define (parse-ques start reply)
  (let-values ([(name start) (parse-name start reply)])
    (let* ([type (car (cossa (octet-pair->number (car start) (cadr start))
                             types))]
           [start (cddr start)]
           ;;
           [class (car (cossa (octet-pair->number (car start) (cadr start))
                              classes))]
           [start (cddr start)])
      (values (list name type class) start))))

(define (parse-n parse start reply n)
  (let loop ([n n][start start][accum null])
    (if (zero? n)
      (values (reverse accum) start)
      (let-values ([(rr start) (parse start reply)])
        (loop (sub1 n) start (cons rr accum))))))

;; NameServer String Type Class -> (Values Boolean LB LB LB LB LB)
(define (dns-query nameserver addr type class)
  (unless (assoc type types)
    (raise-type-error 'dns-query "DNS query type" type))
  (unless (assoc class classes)
    (raise-type-error 'dns-query "DNS query class" class))

  (let* ([query (make-query (random 256) (string->bytes/latin-1 addr)
                            type class)]
         [udp (udp-open-socket)]
         [reply
          (dynamic-wind
            void
            (lambda ()
              (let ([s (make-bytes 512)])
                (let retry ([timeout INIT-TIMEOUT])
                  (udp-send-to udp nameserver 53 (list->bytes query))
                  (sync (handle-evt (udp-receive!-evt udp s)
                                    (lambda (r)
                                      (bytes->list (subbytes s 0 (car r)))))
                        (handle-evt (alarm-evt (+ (current-inexact-milliseconds)
                                                  timeout))
                                    (lambda (v)
                                      (retry (* timeout 2))))))))
            (lambda () (udp-close udp)))])

    ;; First two bytes must match sent message id:
    (unless (and (= (car reply) (car query))
                 (= (cadr reply) (cadr query)))
      (error 'dns-query "bad reply id from server"))

    (let ([v0 (caddr reply)]
          [v1 (cadddr reply)])
      ;; Check for error code:
      (let ([rcode (bitwise-and #xf v1)])
        (unless (zero? rcode)
          (error 'dns-query "error from server: ~a"
                 (case rcode
                   [(1) "format error"]
                   [(2) "server failure"]
                   [(3) "name error"]
                   [(4) "not implemented"]
                   [(5) "refused"]))))

      (let ([qd-count (octet-pair->number (list-ref reply 4) (list-ref reply 5))]
            [an-count (octet-pair->number (list-ref reply 6) (list-ref reply 7))]
            [ns-count (octet-pair->number (list-ref reply 8) (list-ref reply 9))]
            [ar-count (octet-pair->number (list-ref reply 10) (list-ref reply 11))])

        (let ([start (list-tail reply 12)])
          (let*-values ([(qds start) (parse-n parse-ques start reply qd-count)]
                        [(ans start) (parse-n parse-rr start reply an-count)]
                        [(nss start) (parse-n parse-rr start reply ns-count)]
                        [(ars start) (parse-n parse-rr start reply ar-count)])
            (unless (null? start)
              (error 'dns-query "error parsing server reply"))
            (values (positive? (bitwise-and #x4 v0))
                    qds ans nss ars reply)))))))

;; A cache for DNS query data
;; Stores a (List Boolean LB LB LB LB LB)
(define cache (make-hasheq))

;; NameServer Address Type Class -> (Values Boolean LB LB LB LB LB)
;; Execute a DNS query and cache it
(define (dns-query/cache nameserver addr type class)
  (let ([key (string->symbol (format "~a;~a;~a;~a" nameserver addr type class))])
    (let ([v (hash-ref cache key (lambda () #f))])
      (if v
        (apply values v)
        (let-values ([(auth? qds ans nss ars reply)
                      (dns-query nameserver addr type class)])
          (hash-set! cache key (list auth? qds ans nss ars reply))
          (values auth? qds ans nss ars reply))))))

(define (ip->string s)
  (format "~a.~a.~a.~a"
          (list-ref s 0) (list-ref s 1) (list-ref s 2) (list-ref s 3)))

;; Convert a list of bytes representing an IPv6 address to a string
(define (ipv6->string lob)
  (define two-octet-strings
    (for/list ([oct-pair (in-slice 2 (in-list lob))])
      (define oct1 (car oct-pair))
      (define oct2 (cadr oct-pair))
      (~r (+ (arithmetic-shift oct1 8) oct2)
          #:base 16)))
  (string-join two-octet-strings ":"))

;; (NameServer -> (Values Any LB Boolean)) NameServer -> Any
;; Run the given query function, trying until an answer is found
(define (try-forwarding k nameserver)
  (let loop ([nameserver nameserver] [tried (list nameserver)])
    ;; Normally the recusion is done for us, but it's technically optional
    (let-values ([(v ars auth?) (k nameserver)])
      (or v
          (and (not auth?)
               (let* ([ns (ormap (lambda (ar)
                                   (and (eq? (rr-type ar) 'a)
                                        (ip->string (rr-data ar))))
                                 ars)])
                 (and ns
                      (not (member ns tried))
                      (loop ns (cons ns tried)))))))))

(define (ip->in-addr.arpa ip)
  (let ([result (regexp-match #rx"^([0-9]+)\\.([0-9]+)\\.([0-9]+)\\.([0-9]+)$"
                              ip)])
    (format "~a.~a.~a.~a.in-addr.arpa"
            (list-ref result 4)
            (list-ref result 3)
            (list-ref result 2)
            (list-ref result 1))))

(define (get-ptr-list-from-ans ans)
  (filter (lambda (ans-entry) (eq? (list-ref ans-entry 1) 'ptr)) ans))

(define (dns-get-name nameserver ip)
  (or (try-forwarding
       (lambda (nameserver)
         (let-values ([(auth? qds ans nss ars reply)
                       (dns-query/cache nameserver (ip->in-addr.arpa ip) 'ptr 'in)])
           (values (and (positive? (length (get-ptr-list-from-ans ans)))
                        (let ([s (rr-data (car (get-ptr-list-from-ans ans)))])
                          (let-values ([(name null) (parse-name s reply)])
                            (bytes->string/latin-1 name))))
                   ars auth?)))
       nameserver)
      (error 'dns-get-name "bad ip address")))

;; Get resource records corresponding to the given type
(define (get-records-from-ans ans type)
  (for/list ([ans-entry ans]
             #:when (eq? (list-ref ans-entry 1) type))
    ans-entry))

(define (dns-get-address nameserver addr #:ipv6? [ipv6? #f])
  (define type (if ipv6? 'aaaa 'a))
  (define (get-address nameserver)
    (define-values (auth? qds ans nss ars reply)
      (dns-query/cache nameserver addr type 'in))
    (define answer-records (get-records-from-ans ans type))
    (define address
      (and (positive? (length answer-records))
           (let ([data (rr-data (car answer-records))])
             (if ipv6?
                 (ipv6->string data)
                 (ip->string data)))))
    (values address ars auth?))
  (or (try-forwarding get-address nameserver)
      (error 'dns-get-address "bad address")))

(define (dns-get-mail-exchanger nameserver addr)
  (or (try-forwarding
       (lambda (nameserver)
         (let-values ([(auth? qds ans nss ars reply) (dns-query/cache nameserver addr 'mx 'in)])
           (values (let loop ([ans ans][best-pref +inf.0][exchanger #f])
                     (cond
                       [(null? ans)
                        (or exchanger
                            ;; Does 'soa mean that the input address is fine?
                            (and (ormap (lambda (ns) (eq? (rr-type ns) 'soa))
                                        nss)
                                 addr))]
                       [else
                        (let ([d (rr-data (car ans))])
                          (let ([pref (octet-pair->number (car d) (cadr d))])
                            (if (< pref best-pref)
                              (let-values ([(name start) (parse-name (cddr d) reply)])
                                (loop (cdr ans) pref name))
                              (loop (cdr ans) best-pref exchanger))))]))
                   ars auth?)))
       nameserver)
      (error 'dns-get-mail-exchanger "bad address")))

(define (dns-find-nameserver)
  (case (system-type)
    [(unix macosx)
     (with-handlers ([void (lambda (x) #f)])
       (with-input-from-file "/etc/resolv.conf"
         (lambda ()
           (let loop ()
             (let ([l (read-line)])
               (or (and (string? l)
                        (let ([m (regexp-match
                                  #rx"nameserver[ \t]+([0-9]+[.][0-9]+[.][0-9]+[.][0-9]+)"
                                  l)])
                          (and m (cadr m))))
                   (and (not (eof-object? l))
                        (loop))))))))]
    [(windows)
     (let ([nslookup (find-executable-path "nslookup.exe" #f)])
       (and nslookup
            (let-values ([(pin pout pid perr proc)
                          (apply
                           values
                           (process/ports
                            #f (open-input-file "NUL") (current-error-port)
                            nslookup))])
              (let loop ([name #f] [ip #f] [try-ip? #f])
                (let ([line (read-line pin 'any)])
                  (cond [(eof-object? line)
                         (close-input-port pin)
                         (proc 'wait)
                         (or ip name)]
                        [(and (not name)
                              (regexp-match #rx"^Default Server: +(.*)$" line))
                         => (lambda (m) (loop (cadr m) #f #t))]
                        [(and try-ip?
                              (regexp-match #rx"^Address: +(.*)$" line))
                         => (lambda (m) (loop name (cadr m) #f))]
                        [else (loop name ip #f)]))))))]
    [else #f]))
