#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         "utils.rkt"
         "types.rkt")

(provide 
 (protect-out play-sound))

(import-class NSSound)

(define-objc-class RacketSound NSSound
  [result
   sema]
  [-a _void (sound: [_id sound] didFinishPlaying: [_BOOL ok?])
      (set! result ok?)
      (semaphore-post sema)
      (tellv self release)])

(define (play-sound path async?)
  (let ([s (as-objc-allocation
            (tell (tell RacketSound alloc)
                  initWithContentsOfFile: #:type _NSString (path->string 
                                                            (path->complete-path path))
                  byReference: #:type _BOOL #t))]
        [sema (make-semaphore)])
    (tellv s setDelegate: s)
    (set-ivar! s sema sema)

    ; use the `retain' method instead of the `retain' function, because we
    ; don't want a finalization-triggered release:
    (tellv s retain)
    
    (and (tell #:type _BOOL s play)
         (if async?
             #t
             (begin
               (semaphore-wait sema)
               (get-ivar s result))))))
