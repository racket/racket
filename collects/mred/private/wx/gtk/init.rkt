#lang scheme/base
(require scheme/foreign
         "utils.rkt"
         "types.rkt"
         "queue.rkt")
(unsafe!)

(define-gtk gtk_init (_fun (_ptr io _int) (_ptr io _pointer) -> _void))

(gtk_init 0 #f)
(define pump-thread (gtk-start-event-pump))

