#lang racket/base
(require web-server/http
         web-server/servlet-env
         "basic-index.rkt"
         "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests
 (with-fake-root
  (shelly-case
   "A lock is used to guarantee serial access to the package database"

   ;; Step 1: Start a special server that waits for our signal to respond
   (initialize-catalogs)

   (define succeed-catalog (make-channel))
   (define fail-catalog (make-channel))

   (thread
    (λ ()
      (define first-time? #t)
      (serve/servlet (pkg-index/basic
                      (λ (pkg-name)
                        ;; only do the synchronization protocol once:
                        ;;  `pkg-index/basic` can decide to return 500
                        ;;  which triggers a retry, and since no one is
                        ;;  posting a second time to these channels, we
                        ;;  would get stuck.
                        (when first-time?
                          (channel-put fail-catalog 'go)
                          (define v (sync fail-catalog)) ;; => 'continue
                          (set! first-time? #f))
                        (define r (hash-ref *index-ht-1* pkg-name #f))
                        r)
                      (λ () *index-ht-1*))
                     #:command-line? #t
                     #:servlet-regexp #rx""
                     #:port 9967)))

   ;; Step 2: Assign it as our server
   $ "raco pkg config --set catalogs http://localhost:9967"

   ;; Step 3: Start an installation request in the background
   (thread
    (λ ()
      (shelly-begin
       $ "raco pkg install pkg-test1")
      (channel-put succeed-catalog 'done)))
   (sync fail-catalog) ;; => 'go

   ;; Step 4: Start the installation request that will fail
   $ "raco pkg install pkg-test1" =exit> 1

   ;; Step 5: Free the other one
   (channel-put fail-catalog 'continue)
   (sync succeed-catalog) ;; => 'done
   )))
