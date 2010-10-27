#lang racket/base
(require ffi/unsafe
         "utils.rkt"
         "types.rkt"
         "queue.rkt")

(define-gtk gtk_rc_parse_string (_fun _string -> _void))
(define-gtk gtk_rc_add_default_file (_fun _path -> _void))

(when (eq? 'windows (system-type))
  (let ([dir (simplify-path (build-path (collection-path "racket") 'up 'up "lib"))])
    (gtk_rc_parse_string (format "module_path \"~a\"\n" dir))
    (gtk_rc_add_default_file (build-path dir "gtkrc"))))

(define pump-thread (gtk-start-event-pump))

