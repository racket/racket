#lang scheme/gui
(require profile)
(profile-thunk
 (λ ()
   (parameterize ([current-eventspace (make-eventspace)])
     (let ([s (make-semaphore 0)])
       (queue-callback
        (λ ()
          (dynamic-require "perform-robby.ss" #f)
          (semaphore-post s)))
       (semaphore-wait s))))
 #:threads #t)


