#lang racket/base

;; DNS query library for Racket

(require "private/ip.rkt"
         "private/rr-generic.rkt"
         "private/rr-srv.rkt"
         racket/contract
         racket/format
         racket/string
         racket/system
         racket/udp)

(provide (struct-out srv-rr)
         (contract-out
          [dns-get-address
           (->* ((or/c ip-address? ip-address-string?) string?)
                (#:ipv6? any/c)
                ip-address-string?)]
          [dns-get-srv
           (->* ((or/c ip-address? ip-address-string?)
                 string?
                 string?)
                (string?)
                (listof (struct/c srv-rr
                                  (integer-in 0 65535)
                                  (integer-in 0 65535)
                                  (integer-in 0 65535)
                                  string?)))]
          ;; N.B. We should eventually expose this *kind* of
          ;; functionality, but this interface is very unfriendly. We
          ;; should make the interface less unfriendly before we
          ;; expose this function. It should also be documented when
          ;; it is exposed.
          ;;
          ;; A second option, suitable for use in the interim, would
          ;; be to move dns-lookup to be a provided identifier (with
          ;; this contract) from some module in `private/`, but this
          ;; would involve moving most of the code in this file into
          ;; `private/`, which... ehhh. Could do I suppose. At least
          ;; that way people would be able to use dns-lookup in
          ;; extremis, even though it has an ugly API and no
          ;; documentation.
          ;;
          ;; [dns-lookup
          ;;  (-> (or/c ip-address? ip-address-string?)
          ;;      string?
          ;;      #:rr-type symbol?
          ;;      #:rr-parser (-> (listof (list/c bytes? ;; name
          ;;                                      symbol? ;; type
          ;;                                      symbol? ;; class
          ;;                                      integer? ;; ttl
          ;;                                      (listof byte?))) ;; rdata
          ;;                      (listof byte?) ;; the whole packet
          ;;                      any/c)
          ;;      any/c)]
          [dns-get-name
           (-> (or/c ip-address? ip-address-string?)
               (or/c ip-address? ip-address-string?)
               string?)]
          [dns-get-mail-exchanger
           (-> (or/c ip-address? ip-address-string?)
               string?
               string?)]
          [dns-find-nameserver
           (-> (or/c ip-address-string? #f))]))

;; UDP retry timeout:
(define INIT-TIMEOUT 50)

;; Data Definitions
;; An LB is a (Listof Bytes)

;; A Type is one of the following
(define types
  '((a      1)
    (ns     2)
    (md     3)
    (mf     4)
    (cname  5)
    (soa    6)
    (mb     7)
    (mg     8)
    (mr     9)
    (null  10)
    (wks   11)
    (ptr   12)
    (hinfo 13)
    (minfo 14)
    (mx    15)
    (txt   16)
    (aaaa  28)
    (srv   33)))

;; A Class is one of the following
(define classes
  '((in 1)
    (cs 2)
    (ch 3)
    (hs 4)))

;;;

(define (cossa i l)
  (cond [(null? l)            #f]
        [(equal? (cadar l) i) (car l)]
        [else                 (cossa i (cdr l))]))

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
  (let loop ([n n] [start start] [accum null])
    (if (zero? n)
      (values (reverse accum) start)
      (let-values ([(rr start) (parse start reply)])
        (loop (sub1 n) start (cons rr accum))))))

;; NameServer String Type Class -> (Values Boolean LB LB LB LB LB)
(define (dns-query nameserver-ip addr type class)
  (unless (assoc type types)
    (raise-type-error 'dns-query "DNS query type" type))
  (unless (assoc class classes)
    (raise-type-error 'dns-query "DNS query class" class))

  (define nameserver (ip-address->string nameserver-ip))
  (define query (make-query (random 256) (string->bytes/latin-1 addr)
                            type class))
  (define udp (udp-open-socket nameserver 53))
  (define reply
    (dynamic-wind
      void
      (lambda ()
        (define s (make-bytes 512))
        (let retry ([timeout INIT-TIMEOUT])
          (udp-send-to udp nameserver 53 (list->bytes query))
          (sync (handle-evt (udp-receive!-evt udp s)
                            (lambda (r) (bytes->list (subbytes s 0 (car r)))))
                (handle-evt (alarm-evt (+ (current-inexact-milliseconds)
                                          timeout))
                            (lambda (v) (retry (* timeout 2)))))))
      (lambda () (udp-close udp))))

  (parse-reply query reply))

;; Parse a DNS query reply
(define (parse-reply query reply)
  ;; First two bytes must match sent message id:
  (unless (and (= (car reply)  (car query))
               (= (cadr reply) (cadr query)))
    (error 'dns-query "bad reply id from server"))
  (define v0 (caddr reply))
  (define v1 (cadddr reply))
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
  (define qd-count (octet-pair->number (list-ref reply 4) (list-ref reply 5)))
  (define an-count (octet-pair->number (list-ref reply 6) (list-ref reply 7)))
  (define ns-count (octet-pair->number (list-ref reply 8) (list-ref reply 9)))
  (define ar-count (octet-pair->number (list-ref reply 10) (list-ref reply 11)))
  (define start (list-tail reply 12))
  (let*-values ([(qds start) (parse-n parse-ques start reply qd-count)]
                [(ans start) (parse-n parse-rr start reply an-count)]
                [(nss start) (parse-n parse-rr start reply ns-count)]
                [(ars start) (parse-n parse-rr start reply ar-count)])
    (unless (null? start)
      (error 'dns-query "error parsing server reply"))
    (values (positive? (bitwise-and #x4 v0))
            qds ans nss ars reply)))

;; A cache for DNS query data
;; Stores a (List Boolean LB LB LB LB LB)
(define cache (make-hasheq))

;; NameServer Address Type Class -> (Values Boolean LB LB LB LB LB)
;; Execute a DNS query and cache it
(define (dns-query/cache nameserver addr type class)
  (define key (string->symbol (format "~a;~a;~a;~a" nameserver addr type class)))
  (define v (hash-ref cache key (lambda () #f)))
  (if v
      (apply values v)
      (let-values ([(auth? qds ans nss ars reply)
                    (dns-query nameserver addr type class)])
        (hash-set! cache key (list auth? qds ans nss ars reply))
        (values auth? qds ans nss ars reply))))

;; Convert a list of bytes representing an IPv4 address to a string
(define (ip->string lob)
  (ip-address->string (ipv4 (list->bytes lob))))

;; Convert a list of bytes representing an IPv6 address to a string
(define (ipv6->string lob)
  (ip-address->string (ipv6 (list->bytes lob))))

;; (NameServer -> (Values Any LB Boolean)) NameServer -> Any
;; Run the given query function, trying until an answer is found
(define (try-forwarding k nameserver)
  (let loop ([nameserver nameserver] [tried (list nameserver)])
    ;; Normally the recusion is done for us, but it's technically optional
    (let-values ([(v ars auth?) (k nameserver)])
      (or v
          (and (not auth?)
               (let ([ns (ormap (lambda (ar)
                                  (and (eq? (rr-type ar) 'a)
                                       (ip->string (rr-data ar))))
                                ars)])
                 (and ns
                      (not (member ns tried))
                      (loop ns (cons ns tried)))))))))

;; IPAddress -> String
;; Convert an IP address to a suitable format for a reverse lookup
(define (ip->query-domain ip)
  (if (ipv4? ip)
      (ip->in-addr.arpa ip)
      (ip->ip6.arpa ip)))

;; Convert an IPv4 address for reverse lookup
(define (ip->in-addr.arpa ip)
  (apply format "~a.~a.~a.~a.in-addr.arpa"
         (reverse (bytes->list (ipv4-bytes ip)))))

;; Convert an IPv6 address for reverse lookup
(define (ip->ip6.arpa ip)
  (string-join
   (for/fold ([nibbles '()])
             ([byte (in-bytes (ipv6-bytes ip))])
     (define nib1 (arithmetic-shift (bitwise-and #xf0 byte) -4))
     (define nib2 (bitwise-and #x0f byte))
     (append (list (~r nib2 #:base 16) (~r nib1 #:base 16)) nibbles))
   "." #:after-last ".ip6.arpa"))

(define (get-ptr-list-from-ans ans)
  (filter (λ (ans-entry) (eq? (list-ref ans-entry 1) 'ptr)) ans))

(define (dns-get-name nameserver-ip-or-string ip-or-string)
  (define nameserver (if (ip-address? nameserver-ip-or-string)
                         nameserver-ip-or-string
                         (make-ip-address nameserver-ip-or-string)))
  (define ip (if (ip-address? ip-or-string)
                 ip-or-string
                 (make-ip-address ip-or-string)))

  (or (try-forwarding
       (lambda (nameserver)
         (let-values ([(auth? qds ans nss ars reply)
                       (dns-query/cache nameserver (ip->query-domain ip) 'ptr 'in)])
           (values (and (positive? (length (get-ptr-list-from-ans ans)))
                        (let ([s (rr-data (car (get-ptr-list-from-ans ans)))])
                          (let-values ([(name null) (parse-name s reply)])
                            (bytes->string/latin-1 name))))
                   ars auth?)))
       nameserver)
      (error 'dns-get-name "bad ip address")))

;; Get resource records corresponding to the given type
(define (get-records-from-ans ans type)
  (for/list ([ans-entry (in-list ans)]
             #:when (eq? (list-ref ans-entry 1) type))
    ans-entry))

(define (dns-get-address nameserver-ip-or-string addr #:ipv6? [ipv6? #f])
  (or (dns-lookup nameserver-ip-or-string addr
                  #:rr-type (if ipv6? 'aaaa 'a)
                  #:rr-parser (lambda (answer-records reply)
                                (and (positive? (length answer-records))
                                     ((if ipv6? ipv6->string ip->string)
                                      (rr-data (car answer-records))))))
      (error 'dns-lookup "bad address")))

(define (dns-get-srv nameserver-ip-or-string name service [proto "tcp"])
  (dns-lookup nameserver-ip-or-string
              (format "_~a._~a.~a" service proto name)
              #:rr-type 'srv
              #:rr-parser parse-srv-rr))

(define (dns-lookup nameserver-ip-or-string addr
                    #:rr-type type
                    #:rr-parser rr-parser)
  (define nameserver (if (ip-address? nameserver-ip-or-string)
                         nameserver-ip-or-string
                         (make-ip-address nameserver-ip-or-string)))
  (define (get-address nameserver)
    (define-values (auth? qds ans nss ars reply)
      (dns-query/cache nameserver addr type 'in))
    (define answer-records (get-records-from-ans ans type))
    (define result (rr-parser answer-records reply))
    (values result ars auth?))
  (try-forwarding get-address nameserver))

(define (dns-get-mail-exchanger nameserver-ip-or-string addr)
  (define nameserver (if (ip-address? nameserver-ip-or-string)
                         nameserver-ip-or-string
                         (make-ip-address nameserver-ip-or-string)))
  (or (try-forwarding
       (lambda (nameserver)
         (let-values ([(auth? qds ans nss ars reply) (dns-query/cache nameserver addr 'mx 'in)])
           (values (parse-mx-response ans nss reply addr) ars auth?)))
       nameserver)
      (error 'dns-get-mail-exchanger "bad address")))

;; helper that parses a response for MX queries
(define (parse-mx-response ans nss reply addr)
  (let loop ([ans ans] [best-pref +inf.0] [exchanger #f])
    (cond
     [(null? ans)
      (or (and exchanger (bytes->string/latin-1 exchanger))
          ;; FIXME: Does 'soa mean that the input address is fine?
          (and (ormap (lambda (ns) (eq? (rr-type ns) 'soa))
                      nss)
               addr))]
     [else
      (define type (rr-type (car ans)))
      (define d (rr-data (car ans)))
      (cond [(not (eq? type 'mx)) ; not MX record, keep going
             (loop (cdr ans) best-pref exchanger)]
            [else
             (define pref (octet-pair->number (car d) (cadr d)))
             (if (< pref best-pref)
                 (let-values ([(name start) (parse-name (cddr d) reply)])
                   (loop (cdr ans) pref name))
                 (loop (cdr ans) best-pref exchanger))])])))

(define (dns-find-nameserver)
  (case (system-type)
    [(unix macosx)
     (with-handlers ([void (lambda (x) #f)])
       (with-input-from-file "/etc/resolv.conf"
         (lambda ()
           (let loop ()
             (define line (read-line))
             (or (and (string? line)
                      (let ([m (regexp-match
                                #rx"nameserver[ \t]+([0-9]+[.][0-9]+[.][0-9]+[.][0-9]+)"
                                line)])
                        (and m (cadr m))))
                 (and (not (eof-object? line))
                      (loop)))))))]
    [(windows)
     (define nslookup (find-executable-path "nslookup.exe" #f))
     (and nslookup
          (let-values ([(pin pout pid perr proc)
                        (apply
                         values
                         (process/ports
                          #f (open-input-file "NUL") (current-error-port)
                          nslookup))])
            (let loop ([name #f] [ip #f] [try-ip? #f])
              (define line (read-line pin 'any))
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
                    [else (loop name ip #f)]))))]
    [else #f]))
