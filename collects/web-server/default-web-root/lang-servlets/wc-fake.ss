(module wc-fake (lib "lang.ss" "web-server")
  (require (lib "url.ss" "net"))
  (provide start)
    
  (define (start initial-request)
    (define counter1 0)
    (define counter2 0)
    (send/suspend/dispatch
     (lambda (embed/url)
       (let*-values ([(inc1 next-counter1 next-counter2) (include-counter counter1 counter2 embed/url)]
                     [(inc2 next-counter2 next-counter1) (include-counter next-counter2 next-counter1 embed/url)])
         `(html 
           (body (h2 "Web Cell Test")
                 (div (h3 "First") ,(inc1 next-counter1 next-counter2))
                 (div (h3 "Second") ,(inc2 next-counter2 next-counter1))))))))
  
  (define (include-counter my-counter other-counter embed/url)
    (let/cc k
      (letrec ([include
                (lambda (next-my-counter next-other-counter)
                  `(div (h3 ,(number->string next-my-counter))
                        (a ([href 
                             ,(url->string
                               (embed/url
                                (lambda _
                                  (k include
                                     (add1 next-my-counter)
                                     next-other-counter))))])
                           "Increment")))])
        (values include
                my-counter
                other-counter)))))
