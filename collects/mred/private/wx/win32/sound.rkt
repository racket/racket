#lang racket/base
(require ffi/unsafe
         racket/class
	 "../../lock.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt")

(provide
 (protect-out play-sound))

(define-winmm PlaySoundW (_wfun _string/utf-16 _pointer _DWORD -> _BOOL))

(define SND_SYNC   #x0000)
(define SND_ASYNC  #x0001)
(define SND_NOSTOP #x0010)

(define previous-done-sema #f)

(define (play-sound path async?)
  (let ([path (simplify-path path #f)]
	[done (make-semaphore)])
    (and (let ([p (path->string
		   (cleanse-path (path->complete-path path)))])
	   (atomically
	    (when previous-done-sema (semaphore-post previous-done-sema))
	    (set! previous-done-sema done)
	    (PlaySoundW p #f SND_ASYNC)))
	 (or async?
	     ;; Implement synchronous playing by polling, where
	     ;; PlaySound with no sound file and SND_NOSTOP polls.
	     (let loop ()
	       (sleep 0.1)
	       (or (semaphore-try-wait? done)
		   (PlaySoundW #f #f (bitwise-ior SND_ASYNC SND_NOSTOP))
		   (loop)))))))
