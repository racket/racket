#lang scheme/base 

(require scheme/class
         (prefix-in scheme: (only-in scheme/base read))
         mrlib/cache-image-snip)

(provide visible-matrix%
         matrix-snip-class%
         (rename-out (matrix-snip-class snip-class))
         v?
         v-m
         matrix<%>
         build-bitmap ;; from cache-image-snip
         overlay-bitmap
         )

(define matrix<%> 
  (interface ()
    ->rectangle ;; -> [Listof [Listof X]]
    ))

(define visible-matrix<%> 
  (interface ()
    get-M ;; -> matrix<%> u [Listof [Listof X]]
    ))

;; ---------------------------------------------------------------------------

(define (v-m VM) (send VM get-M))
(define (v? VM) (is-a? VM visible-matrix%))

;; representing a matrix that renders itself as an image, as in "image.rkt"
(define visible-matrix%
  (class cache-image-snip%
    (inherit set-snipclass get-argb)
    (inherit-field dc-proc argb-proc width height argb px py)
    
    (init M_0)
    (field 
     [M (if (is-a? M_0 matrix<%>) M_0 #f)]
     [R (cond
          [M #f]
          [(pair? M_0) M_0]
          [else
           (error 'visible% "expects matrix or rectangle, given: ~e" M_0)])])

    (define/public (get-M) (if M M R))
    
    ;; create a matrix from this instance 
    (define/override (copy)
      (new visible-matrix%
           (M_0 (get-M)) (dc-proc dc-proc) (argb-proc argb-proc)
           (width width) (height height) (px px) (py py) (argb argb)))

    (define/private (->s-expr)
      (list (if R R (send M ->rectangle))
            (list (argb-vector (get-argb)) width px py)))
    
    (define/override (write f)
      (define x (format "~s" (->s-expr)))
      (define y (string->bytes/utf-8 x))
      (send f put y))
    
    (super-new)
    (set-snipclass matrix-snip-class)))

;; the snip class for matrices
(define matrix-snip-class% 
  (class cache-image-snip-class% 
    (super-new)
    (define/override (read f)
      (define b (send f get-bytes))
      (data->snip 
       (and b
            (not (equal? b #""))
            (with-handlers ((exn:fail:read? (λ (x) #f)))
              (scheme:read (open-input-bytes b))))))
    (define/override (data->snip data) 
      (define _ (unless data (error 'read "in matrix-snip-class% failed")))
      (define new-cache-image-snip (super data->snip (cadr data)))
      (define-values (w h) (send new-cache-image-snip get-size))
      (define M (car data))
      ;; .. but we need to produce a visible-matrix% instead
      (new visible-matrix% 
           (M_0 M)
           (dc-proc (send new-cache-image-snip get-dc-proc))
           (argb-proc (send new-cache-image-snip get-argb-proc))
           (width w)
           (height h)
           (argb (get-argb new-cache-image-snip))
           (px (get-px new-cache-image-snip))
           (py (get-py new-cache-image-snip))))))
(define get-argb (class-field-accessor cache-image-snip% argb))
(define get-px (class-field-accessor cache-image-snip% px))
(define get-py (class-field-accessor cache-image-snip% py))

;; setting up the 'snip class' 
(define matrix-snip-class (new matrix-snip-class%))
(send matrix-snip-class set-version 1)
(send matrix-snip-class set-classname (format "~s" `(lib "matrix.ss" "htdp")))
(send the-drscheme-snip-class #;(get-the-snip-class-list) add matrix-snip-class)
