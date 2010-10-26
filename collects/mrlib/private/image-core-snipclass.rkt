#lang racket/base
(require racket/class)
(provide fetch image<%>
         get-shape set-shape get-bb get-pinhole 
         get-normalized? set-normalized get-normalized-shape)

(define-local-member-name
  get-shape set-shape get-bb get-pinhole 
  get-normalized? set-normalized get-normalized-shape)

(define image<%>
  (interface ()
    get-shape set-shape get-bb get-pinhole 
    get-normalized? get-normalized-shape))

(define (fetch bytes)
  (let* ([str 
          (and bytes
               (with-handlers ((exn:fail? (λ (x) #f)))
                 (bytes->string/utf-8 bytes)))]
         [lst (and str
                   (with-handlers ((exn:fail:read? (λ (x) #f)))
                     (racket/base:read
                      (open-input-string
                       str))))])
    (cond
      [(and (list? lst)
            (= 2 (length lst)))
       ;; backwards compatibility for saved images that didn't have a pinhole
       (list (list-ref lst 0)
             (list-ref lst 1)
             #f)]
      [else
       lst])))

(define racket/base:read read)
