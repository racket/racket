#lang racket/base
(require ffi/unsafe
         racket/class
         "utils.rkt"
         "types.rkt"
         "const.rkt")

(provide play-sound)

(define-winmm PlaySoundW (_wfun _string/utf-16 _pointer _DWORD -> _BOOL))

(define SND_SYNC   #x0000)
(define SND_ASYNC  #x0001)

(define (play-sound path async?)
  (let ([path (simplify-path path #f)])
    ;; FIXME: sync sound play blocks all Racket threads
    (PlaySoundW (if (path? path) (path->string path) path)
                #f
                (if async? SND_ASYNC SND_SYNC))))
