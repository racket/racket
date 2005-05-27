;; Default choice for writing module servlets
(module servlet mzscheme
  (require (lib "contract.ss")
           (all-except "request-parsing.ss" request-bindings)
           "servlet-tables.ss"
           "response.ss"
           "servlet-helpers.ss"
           "xexpr-callback.ss")

  ;; Weak contracts: the input is checked in output-response, and a message is
  ;; sent directly to the client (Web browser) instead of the terminal/log.
  (provide/contract
    (send/back (any/c . -> . any))
    (send/finish (any/c . -> . any))
    (send/suspend ((string? . -> . any/c) . -> . request?))
    (send/forward ((string? . -> . any/c) . -> . request?))
    ;;; validate-xexpr/callback is not checked anywhere:
    (send/suspend/callback (xexpr/callback? . -> . any))
    )

  (provide
    (all-from "servlet-helpers.ss")
    (all-from "xexpr-callback.ss")
    )


  ;; ************************************************************
  ;; EXPORTS

  ;; send/back: response -> void
  ;; send a response and don't clear the continuation table
  (define (send/back resp)
    (let ([ctxt (servlet-instance-context (current-servlet-instance))])
      (output-response (execution-context-connection ctxt) resp)
      ((execution-context-suspend ctxt))))

  ;; send/finish: response -> void
  ;; send a response and clear the continuation table
  (define (send/finish resp)
    (clear-continuations! (current-servlet-instance))
    (send/back resp))

  ;; send/suspend: (url -> response) -> request
  ;; send a response and apply the continuation to the next request
  (define (send/suspend response-generator)
    (let/cc k
      (let* ([inst (current-servlet-instance)]
             [ctxt (servlet-instance-context inst)])
        (output-response
         (execution-context-connection ctxt)
         (response-generator
          (store-continuation!
           k (request-uri (execution-context-request ctxt))
           inst)))
        ((execution-context-suspend ctxt)))))

  ;; send/forward: (url -> response) -> request
  ;; clear the continuation table, then behave like send/suspend
  (define (send/forward response-generator)
    (clear-continuations! (current-servlet-instance))
    (send/suspend response-generator))

  ;; send/suspend/callback : xexpr/callback? -> void
  ;; send/back a response with callbacks in it; send/suspend those callbacks.
  (define (send/suspend/callback p-exp)
    (let/cc k0
      (send/back
        (replace-procedures
          p-exp (lambda (proc)
                  (let/cc k1 (k0 (proc (send/suspend k1)))))))))


  ;; ************************************************************
  ;; HELPERS

  ;; replace-procedures : xexpr/callbacks? (xexpr/callbacks? -> xexpr?) -> xexpr?
  ;; Change procedures to the send/suspend of a k-url
  (define (replace-procedures p-exp p->a)
    (cond
      ((list? p-exp) (map (lambda (p-e) (replace-procedures p-e p->a))
                          p-exp))
      ((procedure? p-exp) (p->a p-exp))
      (else p-exp)))

)
