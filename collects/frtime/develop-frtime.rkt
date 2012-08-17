#lang racket
 (require setup/link)
 
 
 #|Update this to point to your racket installation directory|#
 (define install-path "C:/Program Files/Racket/collects/frtime")
 
 #|Update this to point to your github clone|#
 (define dev-path "C:/Users/user/Documents/GitHub/racket/collects/frtime")
 
 #|Then call one of these functions to begin developing frtime, or to halt development.|#
 (define start-developing-frtime
   (lambda ()
     (start-developing-collection dev-path install-path)))
 
 
 (define stop-developing-frtime
   (lambda ()
     (stop-developing-collection dev-path install-path)))
     
  (define start-developing-collection
    (lambda (dev-coll-path install-coll-path)
      (links install-coll-path #:remove? #t)
      (links dev-coll-path)))
  
  (define stop-developing-collection
    (lambda (dev-coll-path install-coll-path)
      (start-developing-collection install-coll-path dev-coll-path)))
 