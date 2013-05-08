#lang racket
(require rackunit
         net/url
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/http/cookie
         web-server/http/cookie-parse)
(provide cookies-tests)

(define (header-equal? h1 h2)
  (and (bytes=? (header-field h1)
                (header-field h2))
       (bytes=? (header-value h1)
                (header-value h2))))

(define (set-header->read-header h)
  (make-header #"Cookie" (header-value h)))

(define cookies-tests
  (test-suite
   "Cookies"
   
   (test-suite
    "cookie.rkt"
    
    (test-suite
     "cookie->header and make-cookie"
     (test-check "Simple" header-equal?
                 (cookie->header (make-cookie "name" "value"))
                 (make-header #"Set-Cookie" #"name=value"))
     
     (test-equal? "Comment"
                  (header-value (cookie->header (make-cookie "name" "value" #:comment "comment")))
                  #"name=value; Comment=comment")
     
     (test-equal? "Domain"
                  (header-value (cookie->header (make-cookie "name" "value" #:domain ".domain")))
                  #"name=value; Domain=.domain")
     
     (test-equal? "max-age"
                  (header-value (cookie->header (make-cookie "name" "value" #:max-age 24)))
                  #"name=value; Max-Age=24")
     
     (test-equal? "path"
                  (header-value (cookie->header (make-cookie "name" "value" #:path "path")))
                  #"name=value; Path=path")
     
     (test-equal? "secure? #t"
                  (header-value (cookie->header (make-cookie "name" "value" #:secure? #t)))
                  #"name=value; Secure")
     
     (test-equal? "secure? #f"
                  (header-value (cookie->header (make-cookie "name" "value" #:secure? #f)))
                  #"name=value")))
   
   (test-suite
    "cookie-parse.rkt"
    
    (test-equal? "None"
                 (request-cookies 
                  (make-request 
                   #"GET" (string->url "http://test.com/foo")
                   empty (delay empty) #f
                   "host" 80 "client"))
                 empty)
    
    (test-equal? "Simple"
                 (request-cookies 
                  (make-request 
                   #"GET" (string->url "http://test.com/foo")
                   (list (make-header #"Cookie" #"$Version=\"1\"; name=\"value\""))
                   (delay empty) #f
                   "host" 80 "client"))
                 (list (make-client-cookie "$Version" "1" #f #f)
                       (make-client-cookie "name" "value" #f #f)))
    
    (test-equal? "Path"
                 (request-cookies 
                  (make-request 
                   #"GET" (string->url "http://test.com/foo")
                   (list (make-header #"Cookie" #"$Version=\"1\"; name=\"value\"; $Path=\"/acme\""))
                   (delay empty) #f
                   "host" 80 "client"))
                 (list (make-client-cookie "$Version" "1" #f #f)
                       (make-client-cookie "name" "value" #f "/acme")))
    
    (test-equal? "Domain"
                 (request-cookies 
                  (make-request 
                   #"GET" (string->url "http://test.com/foo")
                   (list (make-header #"Cookie" #"$Version=\"1\"; name=\"value\"; $Domain=\".acme\""))
                   (delay empty) #f
                   "host" 80 "client"))
                 (list (make-client-cookie "$Version" "1" #f #f)
                       (make-client-cookie "name" "value" ".acme" #f)))
    
    (test-equal? "Multiple"
                 (request-cookies 
                  (make-request 
                   #"GET" (string->url "http://test.com/foo")
                   (list (make-header #"Cookie" #"$Version=\"1\"; key1=\"value1\"; key2=\"value2\""))
                   (delay empty) #f
                   "host" 80 "client"))
                 (list (make-client-cookie "$Version" "1" #f #f)
                       (make-client-cookie "key1" "value1" #f #f)
                       (make-client-cookie "key2" "value2" #f #f)))
    
    (test-equal? "Multiple w/ paths & domains"
                 (request-cookies 
                  (make-request 
                   #"GET" (string->url "http://test.com/foo")
                   (list (make-header #"Cookie" #"$Version=\"1\"; key1=\"value1\"; $Path=\"/acme\"; key2=\"value2\"; $Domain=\".acme\""))
                   (delay empty) #f
                   "host" 80 "client"))
                 (list (make-client-cookie "$Version" "1" #f #f)
                       (make-client-cookie "key1" "value1" #f "/acme")
                       (make-client-cookie "key2" "value2" ".acme" #f)))
    
     (test-equal? "phpBB. PR10689"
                 (request-cookies 
                  (make-request 
                   #"GET" (string->url "http://test.com/foo")
                   (list (make-header #"Cookie" 
                                      #"style_cookie=null; phpbb3_e1p9b_u=54; phpbb3_e1p9b_k=; phpbb3_e1p9b_sid=3fa8d7a7b65fbabcbe9b345861dc079a"))
                   (delay empty) #f
                   "host" 80 "client"))
                 (list (make-client-cookie "style_cookie" "null" #f #f)
                       (make-client-cookie "phpbb3_e1p9b_u" "54" #f #f)
                       (make-client-cookie "phpbb3_e1p9b_k" "" #f #f)
                       (make-client-cookie "phpbb3_e1p9b_sid" "3fa8d7a7b65fbabcbe9b345861dc079a" #f #f)))
     
     (test-equal? "Google"
                  (request-cookies 
                   (make-request 
                    #"GET" (string->url "http://test.com/foo")
                    (list (make-header #"Cookie" 
                                       #"teaching-order=course;
__utmz=165257760.1272597702.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none)\r\n"))
                    (delay empty) #f
                    "host" 80 "client"))
                  (list (make-client-cookie "teaching-order" "course" #f #f)
                        (make-client-cookie "__utmz" "165257760.1272597702.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none)" #f #f)))
                  
    
    )))
