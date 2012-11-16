#lang racket/base

(require racket/match
         racket/contract
         web-server/http/request-structs)

(define framing-mode (make-parameter 'old))

(struct ws-conn ([closed? #:mutable] line headers ip op)
        #:property prop:evt (struct-field-index ip))
(define (open-ws-conn? x)
  (and (ws-conn? x) (not (ws-conn-closed? x))))
(provide/contract
 [framing-mode (parameter/c (symbols 'old 'new))]
 [ws-conn (false/c bytes? (listof header?) input-port? output-port? . -> . open-ws-conn?)]
 [ws-conn? (any/c . -> . boolean?)]
 [open-ws-conn? (any/c . -> . boolean?)]
 [ws-conn-line (ws-conn? . -> . bytes?)]
 [ws-conn-closed? (ws-conn? . -> . boolean?)]
 [ws-conn-headers (ws-conn? . -> . (listof header?))]
 [ws-send! (-> open-ws-conn? string? void)]
 [ws-recv (-> open-ws-conn? (or/c string? eof-object?))]
 [ws-close! (-> open-ws-conn? void)])

(define (write-ws-frame! t s op)
  (define bs (string->bytes/utf-8 s))
  (case (framing-mode)
    [(new)
     (write-byte t op)
     (write-bytes (integer->integer-bytes (bytes-length bs) 8 #f #t) op)
     (write-bytes bs op)]
    [(old)
     (write-byte #x00 op)
     (write-bytes bs op)
     (write-byte #xff op)])
  (flush-output op))  

(define (read-ws-frame ip)
  (case (framing-mode)
    [(new)
     (let ()
       (define frame (read-byte ip))
       (when (eof-object? frame) (error 'read-ws-frame "Premature connection close"))
       (define len-bs (read-bytes 8 ip))
       (when (eof-object? len-bs) (error 'read-ws-frame "Premature connection close"))
       (define len (integer-bytes->integer len-bs #f #t))
       (define data-bs (read-bytes len ip))
       (when (eof-object? data-bs) (error 'read-ws-frame "Premature connection close"))
       (values frame (bytes->string/utf-8 data-bs)))]
    [(old)
     (let ()
       (define l (read-byte ip))
       (cond [(eof-object? l) (values #x00 #"")]
             [(= #xff l)
              (read-byte ip)
              (values #x00 #"")]
             [else
              (values #xff (bytes->string/utf-8 (read-until-byte #xff ip)))]))]))

(define (read-until-byte b ip)
  (define ob (open-output-bytes))
  (let loop ()
    (define n (read-byte ip))
    (unless (or (eof-object? n) (= n b))
      (write-byte n ob)
      (loop)))
  (get-output-bytes ob))

(define (ws-send! wsc s)
  (match-define (ws-conn _ _ _ _ op) wsc)
  (write-ws-frame! #xff s op))

(define (ws-recv wsc)
  (match-define (ws-conn _ _ _ ip _) wsc)
  (define-values (ft m) (read-ws-frame ip))
  (if (= #x00 ft)
      eof
      m))

(define (ws-close! wsc)
  (match-define (ws-conn _ _ _ ip op) wsc)
  
  (case (framing-mode)
    [(new)
     (write-ws-frame! #x00 "" op)]
    [(old)
     (write-byte #xff op)
     (write-byte #x00 op)
     (flush-output op)])
  
  (close-input-port ip)
  (close-output-port op)
  (set-ws-conn-closed?! wsc #t))
