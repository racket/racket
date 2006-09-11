
(module cursor mzscheme
  (provide (all-defined))
  
  ;; Cursors
  
;;  (define-struct cursor (v n))
;;  
;;  (define (cursor:new items)
;;    (if (pair? items)
;;        (make-cursor (list->vector items) 0)
;;        (make-cursor #f #f)))
;;
;;  (define (cursor:current c)
;;    (when (cursor-n c)
;;      (vector-ref (cursor-v c) (cursor-n c))))
;;  (define (cursor:move-next c)
;;    (when (cursor:can-move-next? c)
;;      (set-cursor-n! c (add1 (cursor-n c)))))
;;  (define (cursor:move-previous c)
;;    (when (cursor:can-move-previous? c)
;;      (set-cursor-n! c (sub1 (cursor-n c)))))
;;  (define (cursor:move-to-start c)
;;    (when (cursor-n c)
;;      (set-cursor-n! c 0)))
;;  (define (cursor:move-to-end c)
;;    (when (cursor-n c)
;;      (set-cursor-n! c (sub1 (vector-length (cursor-v c))))))
;;  
;;  (define (cursor:can-move-next? c)
;;    (and (cursor-n c) (< (cursor-n c) (sub1 (vector-length (cursor-v c))))))
;;
;;  (define (cursor:can-move-previous? c)
;;    (and (cursor-n c) (> (cursor-n c) 0)))
  
  
  (define-struct cursor (prefix suffixp))
  
  (define (cursor-suffix c)
    (if (promise? (cursor-suffixp c))
        (force (cursor-suffixp c))
        (cursor-suffixp c)))
  (define set-cursor-suffix! set-cursor-suffixp!)
  
  (define (cursor:new items)
    (make-cursor null items))

  (define (cursor:current c)
    (let ([suffix (cursor-suffix c)])
      (if (pair? suffix)
          (car suffix)
          #f)))

  (define (cursor:move-to-start c)
    (when (cursor:can-move-previous? c)
      (cursor:move-previous c)
      (cursor:move-to-start c)))
  
  (define (cursor:move-to-end c)
    (when (cursor:can-move-next? c)
      (cursor:move-next c)
      (cursor:move-to-end c)))
  
  (define (cursor:move-previous c)
    (when (pair? (cursor-prefix c))
      (let ([old-prefix-cell (cursor-prefix c)])
        (set-cursor-prefix! c (cdr old-prefix-cell))
        (set-cdr! old-prefix-cell (cursor-suffix c))
        (set-cursor-suffix! c old-prefix-cell))))
  
  (define (cursor:move-next c)
    (when (cursor:can-move-next? c)
      (let ([old-suffix-cell (cursor-suffix c)])
        (set-cursor-suffix! c (cdr old-suffix-cell))
        (set-cdr! old-suffix-cell (cursor-prefix c))
        (set-cursor-prefix! c old-suffix-cell))))
  
  (define (cursor:can-move-next? c)
    (pair? (cursor-suffix c)))
  
  (define (cursor:can-move-previous? c)
    (pair? (cursor-prefix c)))
  
  
  
  )