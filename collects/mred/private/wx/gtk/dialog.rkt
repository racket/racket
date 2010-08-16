#lang scheme/base
(require scheme/class
         ffi/unsafe
          "../../syntax.rkt"
          "../common/queue.rkt"
          "types.rkt"
          "utils.rkt"
          "frame.rkt")

(provide dialog%)

(define GTK_WIN_POS_CENTER 1)
(define GTK_WIN_POS_CENTER_ON_PARENT 4)

(define GDK_WINDOW_TYPE_HINT_DIALOG 1)

(define-gtk gtk_window_set_position (_fun _GtkWidget _int -> _void))
(define-gtk gtk_window_set_transient_for (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_window_set_type_hint (_fun _GtkWidget _int -> _void))

(define dialog-level-counter 0)

(defclass dialog% frame%
  (inherit get-gtk
           get-parent)

  (super-new [is-dialog? #t])

  (define close-sema #f)

  (gtk_window_set_type_hint (get-gtk) GDK_WINDOW_TYPE_HINT_DIALOG)

  (let ([p (get-parent)])
    (when p
      (gtk_window_set_transient_for (get-gtk) (send p get-gtk))))

  (define dialog-level 0)
  (define/override (get-dialog-level) dialog-level)
  
  (define/override (frame-relative-dialog-status win) 
    (let ([dl (send win get-dialog-level)])
      (cond
       [(= dl dialog-level) 'same]
       [(dl . > . dialog-level) #f]
       [else 'other])))
  
  (define/override (direct-show on?)
    (when on?
      (set! dialog-level-counter (add1 dialog-level-counter))
      (set! dialog-level dialog-level-counter))
    (unless on?
      (set! dialog-level 0))
    (unless on?
      (when close-sema
        (semaphore-post close-sema)
        (set! close-sema #f)))
    (super direct-show on?))

  (define/override (center dir wrt)
    (if (eq? dir 'both)
        (gtk_window_set_position (get-gtk) 
                                 (if (get-parent)
                                     GTK_WIN_POS_CENTER_ON_PARENT
                                     GTK_WIN_POS_CENTER))
        (super center dir wrt)))

  (define/override (show on?)
    (if on?
        (unless close-sema
          (let ([s (make-semaphore)])
            (set! close-sema s)
            (super show on?)
            (yield s)))
        (super show on?))))
