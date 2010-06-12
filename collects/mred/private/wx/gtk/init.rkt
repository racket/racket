#lang scheme/base
(require scheme/foreign
         "utils.rkt"
         "types.rkt"
         "queue.rkt")
(unsafe!)

(define-gtk gtk_init (_fun (_ptr io _int) (_ptr io _pointer) -> _void))

(define-gtk gtk_rc_parse_string (_fun _string -> _void))
(define-gtk gtk_rc_add_default_file (_fun _path -> _void))
(define-gtk gtk_rc_find_module_in_path (_fun _path -> _path))
(define-gtk gtk_rc_get_module_dir (_fun -> _path))

(when (eq? 'windows (system-type))
  (let ([dir (simplify-path (build-path (collection-path "scheme") 'up 'up "lib"))])
    (gtk_rc_parse_string (format "module_path \"~a\"\n" dir))
    (gtk_rc_add_default_file (build-path dir "gtkrc"))))

(gtk_init 0 #f)
(define pump-thread (gtk-start-event-pump))

