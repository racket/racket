#lang racket/base
(require rackunit
         (only-in mzlib/file
                  make-temporary-file)
         net/url
         mzlib/list
         web-server/http
         web-server/dispatchers/dispatch
         (prefix-in host: web-server/dispatchers/dispatch-host))
(provide dispatch-host-tests)

(require/expose web-server/dispatchers/dispatch-host
                (get-host))

(define lower-url (make-url #f #f "www.racket-lang.org" #f #t empty empty #f))
(define upper-url (make-url #f #f "www.RACKET-lang.org" #f #t empty empty #f))
(define no-host-url (make-url #f #f #f #f #t empty empty #f))

(define dispatch-host-tests
  (test-suite
   "Host"
   
   (test-equal? "get-host - uri - lower"
                'www.racket-lang.org
                (get-host lower-url empty))
   
   (test-equal? "get-host - uri - upper"
                'www.racket-lang.org
                (get-host upper-url empty))
   
   (test-equal? "get-host - headers - lower key - lower val"
                'www.racket-lang.org
                (get-host no-host-url (list (make-header #"host" #"www.racket-lang.org"))))
   (test-equal? "get-host - headers - lower key - upper val"
                'www.racket-lang.org
                (get-host no-host-url (list (make-header #"host" #"www.RACKET-lang.org"))))

   (test-equal? "get-host - headers - upper key - lower val"
                'www.racket-lang.org
                (get-host no-host-url (list (make-header #"Host" #"www.racket-lang.org"))))
   (test-equal? "get-host - headers - upper key - upper val"
                'www.racket-lang.org
                (get-host no-host-url (list (make-header #"Host" #"www.RACKET-lang.org"))))

   (test-equal? "get-host - headers - caps key - lower val"
                'www.racket-lang.org
                (get-host no-host-url (list (make-header #"HOST" #"www.racket-lang.org"))))
   (test-equal? "get-host - headers - caps key - upper val"
                'www.racket-lang.org
                (get-host no-host-url (list (make-header #"HOST" #"www.RACKET-lang.org"))))
   
   (test-equal? "get-host - none"
                'none
                (get-host no-host-url empty))
   
   ))
