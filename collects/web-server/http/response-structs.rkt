#lang racket/base
(require racket/contract
         web-server/http/request-structs)

(define TEXT/HTML-MIME-TYPE #"text/html; charset=utf-8")

(struct response (code message seconds mime headers output))

(define (response/full code message seconds mime headers body)
  (response code message seconds mime
            (list* (make-header #"Content-Length" 
                                (string->bytes/utf-8
                                 (number->string
                                  (for/fold ([len 0])
                                    ([b (in-list body)])
                                    (+ len (bytes-length b))))))
                   headers)
            (lambda (op)
              (for ([b (in-list body)])
                (write-bytes b op)))))

(provide/contract
 [struct response
         ([code number?]
          [message bytes?]
          [seconds number?]
          [mime (or/c false/c bytes?)]
          [headers (listof header?)]
          [output (output-port? . -> . void)])]
 [response/full (-> number? bytes? number? (or/c false/c bytes?) (listof header?) (listof bytes?) response?)]
 [TEXT/HTML-MIME-TYPE bytes?])
