
(module pretty-range mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
           "interfaces.ss")
  (provide ranges%
           (struct range (obj start end)))
  
  ;; A range contains
  ;;   - obj : datum, stand-in for syntax object
  ;;   - start : number
  ;;   - end : number
  (define-struct range (obj start end))
  
  ;; ranges%
  (define ranges%
    (class* object% (range<%>)
      (define starts (make-hash-table))
      (define ranges (make-hash-table))
      
      (define/public (get-start obj)
        (hash-table-get starts obj (lambda _ #f)))
      (define/public (get-ranges obj)
        (hash-table-get ranges obj (lambda _ null)))
      
      (define/public (all-ranges)
        (sort 
         (apply append 
                (hash-table-map
                 ranges
                 (lambda (k vs)
                   (map (lambda (v) (make-range k (car v) (cdr v))) vs))))
         (lambda (x y)
           (>= (- (range-end x) (range-start x))
               (- (range-end y) (range-start y))))))
      
      (define/public (set-start obj n)
        (hash-table-put! starts obj n))
      (define/private (set-ranges obj x)
        (hash-table-put! ranges obj x))
      (define/public (add-range obj range)
        (set-ranges obj (cons range (get-ranges obj))))
      
      (super-new)))

  )
