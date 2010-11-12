#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         "utils.rkt"
         "types.rkt")

(provide 
 (protect-out play-sound))

(import-class NSSound)

(define-objc-class MySound NSSound
  [result
   sema]
  [-a _void (sound: [_id sound] didFinishPlaying: [_BOOL ok?])
      (set! result ok?)
      (semaphore-post sema)
      (tellv self release)])

(define (play-sound path async?)
  (let ([s (as-objc-allocation
            (tell (tell MySound alloc)
                  initWithContentsOfFile: #:type _NSString (if (path? path)
                                                               (path->string path)
                                                               path)
                  byReference: #:type _BOOL #t))]
        [sema (make-semaphore)])
    (tellv s setDelegate: s)
    (set-ivar! s sema sema)
    (tellv s retain) ; don't use `retain', because we dont' want auto-release
    (tellv s play)
    (if async?
        (begin
          (semaphore-wait sema)
          (get-ivar s result))
        #t)))
