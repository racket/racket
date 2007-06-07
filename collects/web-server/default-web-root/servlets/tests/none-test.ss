(module none-test mzscheme
  (require (lib "none.ss" "web-server" "managers"))
  (provide (all-defined))
  
  (define interface-version 'v2-transitional)
  (define manager (create-none-manager
                   (lambda (failed-request)
                     `(html (body (h2 "Error"))))))
  (define (start initial-request)
    `(html (body (h2 "Look Ma, No Instance!")))))
