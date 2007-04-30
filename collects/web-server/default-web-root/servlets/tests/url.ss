(module url mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "url.ss" "net"))
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define count 0)
  
  (define (start initial-request)
    (set! count (add1 count))
    `(html (head (title "URL Test"))
           (body (p "The method requested is: " ,(format "~s" (request-method initial-request)))
                 (p "The URL requested is: " ,(url->string (request-uri initial-request)))
                 (p "count is: " ,(number->string count))))))