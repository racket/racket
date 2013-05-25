#lang racket/base
(require rackunit
         mzlib/etc
         mzlib/list
         mzlib/pretty
         net/url
         web-server/servlet-env
         web-server/servlet)
(provide servlet-env-tests)

#;(define (call u bs)
    (define sx (ssax:xml->sxml (get-pure-port (string->url u)) empty))
    (pretty-print sx)
    sx)

(define servlet-env-tests
  (test-suite
   "Servlet Environment"
   
   ; XXX At least just check whether the server can be started by the servlet-env guts
   
   ; XXX Broken
   #;(test-not-exn "Add two numbers"
                   (lambda () 
                     (sleep 2)
                     (parameterize ([send-url
                                     (lambda (a-url sep?)
                                       (let* ([k0 (first ((sxpath "//form/@action/text()") (call a-url empty)))]
                                              [k1 (first ((sxpath "//form/@action/text()") (call k0 (list (make-binding:form #"number" #"23")))))]
                                              [n (first ((sxpath "//p/text()") (call k1 (list (make-binding:form #"number" #"12")))))])
                                         n)
                                       (void))])
                       (example))))))

; request-number : str -> num
(define (request-number which-number)
  (string->number
   (extract-binding/single
    'number
    (request-bindings (send/suspend (build-request-page which-number))))))

; build-request-page : str -> str -> response
(define (build-request-page which-number)
  (lambda (k-url)
    `(html (head (title "Enter a Number to Add"))
           (body ([bgcolor "white"])
                 (form ([action ,k-url] [method "post"])
                       "Enter the " ,which-number " number to add: "
                       (input ([type "text"] [name "number"] [value ""]))
                       (input ([type "submit"] [name "enter"] [value "Enter"])))))))
(define (example)
  (serve/servlet
   (lambda (request)
     `(html (head (title "Sum"))
            (body ([bgcolor "white"])
                  (p "The sum is "
                     ,(number->string (+ (request-number "first") (request-number "second")))))))
   #:port 9999))
