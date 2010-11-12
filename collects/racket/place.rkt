#lang scheme/base
(require '#%place)
(require '#%futures)

(define (place-channel-send/recv ch msg)
  (place-channel-send ch msg)
  (place-channel-recv ch))

(provide place
         place-sleep
         place-wait 
         place-channel
         place-channel-send
         place-channel-recv
         place-channel?
         place?
         place-channel-send/recv
         processor-count)
