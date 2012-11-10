#lang racket/base
(require web-server/http
         web-server/servlet-env
         meta/planet2-index/basic/main
         "shelly.rkt"
         "util.rkt")

(pkg-tests
 (with-fake-root
  (shelly-case
   "A lock is used to guarantee serial access to the package database"

   ;; Step 1: Start a special server that waits for our signal to respond
   (initialize-indexes)
   (define okay-to-respond?-sema (make-semaphore))
   (thread
    (λ ()
      (serve/servlet (planet2-index/basic
                      (λ (pkg-name)
                        (semaphore-wait okay-to-respond?-sema)
                        (define r (hash-ref *index-ht-1* pkg-name #f))
                        r))
                     #:command-line? #t
                     #:servlet-regexp #rx""
                     #:port 9967)
      (sleep 1)))

   ;; Step 2: Assign it as our server
   $ "raco pkg config --set indexes http://localhost:9967"

   ;; Step 3: Start an installation request in the background
   (thread
    (λ ()
      (shelly-begin
       $ "raco pkg install planet2-test1")))
   (sleep 1)

   ;; Step 4: Start the installation request that will fail
   $ "raco pkg install planet2-test1" =exit> 1

   ;; Step 5: Free the other one
   (semaphore-post okay-to-respond?-sema))))
