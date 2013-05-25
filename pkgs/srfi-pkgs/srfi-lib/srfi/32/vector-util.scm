(module vector-util mzscheme

  (provide (all-defined))
  
  (define (vector-portion-copy vec start end)
    (let* ((len (vector-length vec))
           (new-len (- end start))
           (new (make-vector new-len)))
      (do ((i start (+ i 1))
           (j 0 (+ j 1)))
	((= i end) new)
        (vector-set! new j (vector-ref vec i)))))
  
  (define (vector-copy vec)
    (vector-portion-copy vec 0 (vector-length vec)))
  
  (define (vector-portion-copy! target src start end)
    (let ((len (- end start)))
      (do ((i (- len 1) (- i 1))
           (j (- end 1) (- j 1)))
	((< i 0))
        (vector-set! target i (vector-ref src j)))))
  
  (define (has-element list index)
    (cond
      ((zero? index)
       (if (pair? list)
           (values #t (car list))
           (values #f #f)))
      ((null? list)
       (values #f #f))
      (else
       (has-element (cdr list) (- index 1)))))
  
  (define (list-ref-or-default list index default)
    (call-with-values
     (lambda () (has-element list index))
     (lambda (has? maybe)
       (if has?
           maybe
           default))))
  
  (define (vector-start+end vector maybe-start+end)
    (let ((start (list-ref-or-default maybe-start+end
                                      0 0))
          (end (list-ref-or-default maybe-start+end
                                    1 (vector-length vector))))
      (values start end)))
  
  (define (vectors-start+end-2 vector-1 vector-2 maybe-start+end)
    (let ((start-1 (list-ref-or-default maybe-start+end
                                        0 0))
          (end-1 (list-ref-or-default maybe-start+end
                                      1 (vector-length vector-1)))
          (start-2 (list-ref-or-default maybe-start+end
                                        2 0))
          (end-2 (list-ref-or-default maybe-start+end
                                      3 (vector-length vector-2))))
      (values start-1 end-1
              start-2 end-2)))
  )
