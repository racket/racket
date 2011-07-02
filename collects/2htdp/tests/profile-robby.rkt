#lang scheme/gui
(require profile
         scheme/runtime-path)

(define-runtime-path perform-robby "perform-robby.rkt")

(profile-thunk
 (λ ()
   (parameterize ([current-eventspace (make-eventspace)])
     (let ([s (make-semaphore 0)])
       (queue-callback
        (λ ()
          (dynamic-require perform-robby #f)
          (semaphore-post s)))
       (semaphore-wait s))))
 #:threads #t)


