;;; finite-map.scm  --  Jens Axel Soegaard

(module finite-map mzscheme
  (require (prefix set: "red-black-tree-set.scm")
           (lib "67.ss" "srfi")
           (lib "42.ss" "srfi")
           (only (lib "list.ss") foldl))
  
  ;; REPRESENTATION
  ;;   - a BINDING is a pair of a key and a value
  ;;   - an FMAP is a set of bindings
  
  (define-struct finite-map (compare bindings))
  
  (define key car)
  (define value cdr)
  (define bindings  finite-map-bindings)
  
  (define empty
    (case-lambda 
      [()    (empty (current-compare))]
      [(cmp) (make-finite-map cmp
                              (set:empty (lambda (p1 p2)
                                           (cmp (key p1) (key p2)))))]))
  
  (define (empty? m)
    (set:empty? (bindings m)))
  
  (define (insert x v m)
    (make-finite-map (finite-map-compare m)
                     (set:insert (cons x v)
                                 (set:delete (cons x 'dummy) (bindings m)))))
  
  (define (insert* xvs m)
    (foldl (lambda (key-value-pair m)
             (insert (key key-value-pair) (value key-value-pair) m))
           (empty)
           xvs))
  
  (define (fold f init m)
    ; combine all the values of the bindings,
    ; fold ignores the keys
    ; f : elm alpha -> alpha  
    (set:fold (lambda (b a) (f (value b) a))
              init (bindings m)))
  
  (define (fold/key f init m)
    ; combine all the values of the bindings,
    ; f : elm key alpha -> alpha  
    (set:fold (lambda (b a) (f (key b) (value b) a))
              init (bindings m)))
  
  
  (define (elements m)
    (fold cons '() m))
  
  (define (keys m)
    (fold/key (lambda (k v a) (cons k a))
              '() m))
  
  
  (define (get x m)
    (let ([y (set:get (cons x 'ignore) (bindings m))])
      (if y
          (cons (key y) (value y))
          #f)))
  
  (define (lookup x m)
    (let ([kv (get x m)])
      (if kv
          (cdr kv)
          #f)))
  
  (define (lookup/default x m d)
    (let ([kv (get x m)])
      (if kv
          (cdr kv)
          d)))
  
  (define (member? x b)
    (if (get x b)
        #t
        #f))
  
  (define (delete x m)
    (make-finite-map (finite-map-compare m)
                     (set:delete (cons x 'dummy) (bindings m))))
  
  ; remove all elements in the list xs
  (define (delete* xs m)
    (foldl delete m xs))
  
  ; remove all occurences of x 
  (define (delete-all x m)
    (delete x m))
  
  (define (union m1 m2)
    (make-finite-map (finite-map-compare m1)
                     (set:union (bindings m1) (bindings m2))))
  
  (define (union* ms)
    (foldl union (empty) ms))
  
  (define (from kvs)
    (foldl (lambda (p m)
             (insert (key p) (value p)))
           (empty) kvs))
  
  (define (difference m1 m2)
    (make-finite-map (finite-map-compare m1)
                     (set:difference (bindings m1) (bindings m2))))
  
  (define (intersection m1 m2)
    (make-finite-map (finite-map-compare m1)
                     (set:intersection (bindings m1) (bindings m2))))
  
  (define singleton
    (case-lambda
      [(x v)      (insert x v (empty))]
      [(cmp x v)  (insert x v (empty cmp))]))
  
  (define (size m)
    (set:size (bindings m)))
  
  (define (count x m)
    (if (set:member? (cons x 'dummy) (bindings m))
        1
        0))
  
  (define (select m)
    (when (empty? m)
      (error 'select "can't select element from an empty finite map"))
    (key (set:select (bindings m))))
  
  (define (equal=? m1 m2)
    ; TODO: Improve efficency of this
    (and (set:equal=? (bindings m1) (bindings m2))
         (let ([cmp (finite-map-compare m1)])
           (let loop ([ks (keys m1)])
             (or (null? ks)
                 (and (zero? (cmp (lookup (car ks) m1) (lookup (car ks) m2)))
                      (loop (cdr ks))))))))
  
  ;; support for srfi-42
  
;  (define-syntax finite-map-ec
;    (syntax-rules ()
;      [(_ cmp etc1 etc ...)
;       (fold-ec (empty cmp) etc1 etc ... insert)]))
;  
;  (define-syntax :finite-map
;    (syntax-rules (index)
;      ((_ cc var (index i) arg)
;       (:parallel cc (:stack var arg) (:integers i)) )
;      ((_ cc var arg)
;       (:do cc
;            (let ())
;            ((t (elements arg)))
;            (not (null? t))
;            (let ((var (car t))))
;            #t
;            ((cdr t)) ))))
;  
;  (define (:finite-map-dispatch args)
;    (cond
;      [(null? args)
;       'bag]
;      [(and (= (length args) 1)
;            (finite-map? (car args)))
;       (:generator-proc (:finite-map (car args)))]
;      [else
;       #f]))
;  
;  (:-dispatch-set! 
;   (dispatch-union (:-dispatch-ref) :finite-map-dispatch))
  
  (require "signatures/finite-map-signature.scm")
  (provide-finite-map)
)  
