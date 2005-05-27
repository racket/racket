;; This library is used by match.ss

;;!(function proper-hash-table-pattern?
;;          (form (proper-hash-table-pattern? pat-list) -> bool)
;;          (contract list-of-syntax -> bool))
;; This function returns true if there is no ddk in the list of
;; patterns or there is only a ddk at the end of the list.
(define (proper-hash-table-pattern? pat-list)
  (cond ((null? pat-list) #t)
        (else
         (let ((ddk-list (ddk-in-list? pat-list)))
           (or (not ddk-list)
               (and ddk-list
                    (ddk-only-at-end-of-list? pat-list)))))))

;;!(function ddk-in-list?
;;          (form (ddk l) -> bool)
;;          (contract list-of-syntax -> bool))
;; This is a predicate that returns true if there is a ddk in the
;; list.
(define (ddk-in-list? l)
  (not (andmap (lambda (x) (not (stx-dot-dot-k? x))) l)))

;;!(function ddk-only-at-end-of-list?
;;          (form (ddk-only-at-end-of-list? l) -> bool)
;;          (contract list-of-syntax -> bool))
;; This is a predicate that returns true if there is a ddk at the
;; end of the list and the list has at least one item before the ddk.
(define ddk-only-at-end-of-list?
  (lambda (l)
    '(match
      l
      (((not (? stx-dot-dot-k?)) ..1 a) (stx-dot-dot-k? a)))
    (let ((x l))
      (if (list? x)
          (let ddnnl26305 ((exp26306 x) (count26307 0))
            (if (and (not (null? exp26306))
                     ((lambda (exp-sym) (if (stx-dot-dot-k? exp-sym) #f #t))
                      (car exp26306)))
                (ddnnl26305 (cdr exp26306) (add1 count26307))
                (if (>= count26307 1)
                    (if (and (pair? exp26306) (null? (cdr exp26306)))
                        ((lambda (a) (stx-dot-dot-k? a)) (car exp26306))
                        #f)
                    #f)))
          #f))))

;;!(function ddk-only-at-end-of-vector?
;;          (form (ddk-only-at-end-of-vector? vec) -> bool)
;;          (contract vector -> bool))
;; This is a predicate that returns true if there is a ddk at the
;; end of the vector and the list has at least one item before the ddk.
(define ddk-only-at-end-of-vector?
  (lambda (vec)
    '(match
      vec
      (#((not (? stx-dot-dot-k?)) ..1 a) #t))
    ;; the following is expanded from the above match expression
    (let ((x vec))
      (let ((match-failure
             (lambda () #f)))
        (if (vector? x)
            (let ((lv32956 (vector-length x)))
              (if (>= lv32956 2)
                  (let ((curr-ind32957 0))
                    (let vloop32958 ((count32959 curr-ind32957))
                      (let ((fail32961
                             (lambda (count-offset32962 index32963)
                               (if (>= count-offset32962 1)
                                   (if (= index32963 lv32956)
                                       (match-failure)
                                       (let ((cindnm32965 (add1 index32963)))
                                         (if (>= cindnm32965 lv32956)
                                             ((lambda (a) #t) 
                                              (vector-ref x index32963))
                                             (match-failure))))
                                   (match-failure)))))
                        (if (= lv32956 count32959)
                            (fail32961 (- count32959 curr-ind32957) count32959)
                            (if (stx-dot-dot-k? (vector-ref x count32959))
                                (fail32961 (- count32959 curr-ind32957) 
                                           count32959)
                                (let ((arglist (list)))
                                  (apply vloop32958 (add1 count32959) 
                                         arglist)))))))
                  (match-failure)))
            (match-failure))))))

;;!(function ddk-in-vec?
;;          (form (ddk-in-vec? vec stx) -> (integer or #f))
;;          (contract (vector syntax) -> (integer or bool)))
;; this function returns the total of the k's in a vector of syntax
;; it also insure that the ..k's are not consecutive
(define ddk-in-vec?
  (lambda (vec stx)
    ;; make sure first element is not ddk
    (if (stx-dot-dot-k? (vector-ref vec 0))
        (match:syntax-err
         stx
         "vector pattern cannot start with ..k syntax")
        (let ((vlength (vector-length vec))
              (flag #f))
          (letrec ((check-vec
                    (lambda (last-stx index)
                      (if (= index vlength)
                          0
                          (let ((k-prev (stx-dot-dot-k? last-stx))
                                (k-curr (stx-dot-dot-k? (vector-ref vec 
                                                                    index))))
                            (cond
                             ((and k-prev k-curr)
                              (match:syntax-err
                               stx
                               "consecutive ..k markers are not allowed"))
                             (k-curr
                              (begin
                                (set! flag #t)
                                (+ (- k-curr 2) (check-vec (vector-ref vec 
                                                                       index) 
                                                           (add1 index)))))
                             (else
                              (check-vec (vector-ref vec index) 
                                         (add1 index)))))))))
            (let ((res (check-vec (vector-ref vec 0) 1)))
              (if flag res #f)))))))
