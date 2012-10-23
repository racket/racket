#lang racket/base
(require racket/tcp
         racket/list
         racket/match
         racket/port
         racket/contract)

(define to-client #"0")
(define to-server #"1")
(define (tcp-localhost-available?)
  (with-handlers
      ([exn? (λ (x) #f)])
    (define the-listener
      (tcp-listen 0 4 #t #f))
    (define-values (local-host port end-host end-port)
      (tcp-addresses the-listener #t))
    (let loop ([listener the-listener]
               [sip #f] [sop #f]
               [connected? #f]
               [cip #f] [cop #f])
      (if (and (not listener)
               (not sip)
               (not sop)
               connected?
               (not cip)
               (not cop))
        #t
        (sync
         (if listener
           (handle-evt
            (tcp-accept-evt listener)
            (match-lambda
             [(list sip sop)
              (tcp-close listener)
              (loop #f sip sop connected? cip cop)]))
           never-evt)
         (if sop
           (handle-evt
            (write-bytes-avail-evt to-client sop)
            (λ (written-bs-n)
              (tcp-abandon-port sop)
              (loop #f sip #f connected? cip cop)))
           never-evt)
         (if sip
           (handle-evt
            (read-bytes-evt 1 sip)
            (λ (read-bs)
              (unless (bytes=? to-server read-bs)
                (error 'wrong))
              (tcp-abandon-port sip)
              (loop #f #f sop connected? cip cop)))
           never-evt)
         (if connected?
           never-evt
           (handle-evt
            always-evt
            (λ (_)
              (define-values (cip cop)
                (tcp-connect "localhost" port))
              (loop listener sip sop #t cip cop))))
         (if cop
           (handle-evt
            (write-bytes-avail-evt to-server cop)
            (λ (written-bs-n)
              (tcp-abandon-port cop)
              (loop listener sip sop connected? cip #f)))
           never-evt)
         (if cip
           (handle-evt
            (read-bytes-evt 1 cip)
            (λ (read-bs)
              (unless (bytes=? to-client read-bs)
                (error 'wrong))
              (tcp-abandon-port cip)
              (loop listener sip sop connected? #f cop)))
           never-evt))))))

(provide
 (contract-out
  [tcp-localhost-available? (-> boolean?)]))

(module+ main
  (tcp-localhost-available?))
