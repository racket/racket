#lang racket/base
(require racket/class
         wxme
         "private/image-core-snipclass.rkt"
         "private/regmk.rkt")
(provide reader image<%>)

(define guiless-image%
  (class* object% (equal<%> image<%>)
    (init-field pinhole bb)
    (define/public (equal-to? that eq-recur)
      (cond
        [(eq? this that) #t]
        [else (error 'image% "cannot do equality comparison without gui libraries")]))
    (define/public (equal-hash-code-of y) 42)
    (define/public (equal-secondary-hash-code-of y) 3)

    (define/public (get-shape)
      (error 'image% "cannot get-shape without gui libraries"))
    (define/public (set-shape s)
      (error 'image% "cannot get-shape without gui libraries"))
    (define/public (get-bb) bb)
    (define/public (get-pinhole) pinhole)
    (define/public (get-normalized?) #f)
    (define/public (set-normalized? n?) (void))
    
    (define/public (get-normalized-shape)
      (error 'image% "cannot get-normalized-shape without gui libraries"))

    (super-new)))

(define reader
  (new
   (class* object% (snip-reader<%>)
     (define/public (read-header vers stream)
       (void))
     (define/public (read-snip text? cvers stream)
       (let* ([lst (fetch (send stream read-raw-bytes '2htdp/image))])
         (if text?
             #"."
             (let ([marshalled-img (list-ref lst 0)]
                   [marshalled-bb (list-ref lst 1)]
                   [marshalled-pinhole (list-ref lst 2)])
               (new guiless-image%
                    [bb (if (and (vector? marshalled-bb)
                                 (= 4 (vector-length marshalled-bb))
                                 (eq? (vector-ref marshalled-bb 0) 'struct:bb)
                                 (number? (vector-ref marshalled-bb 1))
                                 (number? (vector-ref marshalled-bb 2))
                                 (number? (vector-ref marshalled-bb 3)))
                            (apply make-bb (cdr (vector->list marshalled-bb)))
                            (make-bb 100 100 100))]
                    [pinhole 
                     (if (and (vector? marshalled-pinhole)
                              (= 3 (vector-length marshalled-pinhole))
                              (eq? (vector-ref marshalled-pinhole 0) 'struct:point)
                              (number? (vector-ref marshalled-pinhole 1))
                              (number? (vector-ref marshalled-pinhole 2)))
                         (make-point (vector-ref marshalled-pinhole 1)
                                     (vector-ref marshalled-pinhole 2))
                         #f)])))))
    (super-new))))
