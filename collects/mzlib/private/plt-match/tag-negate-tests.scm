(define (tag-neg-test ls target-set)
  (easy-tag ls #f target-set))

(define (easy-tag ls last-shape target-set)
  (cond ((null? ls) #f)
        ((let ((tst (car ls)))
           (and ;(not (action-test? tst))
                (not (or (shape-test? tst) (action-test? tst)))
                (equal? target-set (test-used-set-neg tst))))
         (begin 
           (when (and last-shape (not (shape-test? (car ls))))
             (set-test-closest-shape-tst! (car ls) last-shape) 
             (set-test-used-set! (car ls) last-shape)
             (set-test-times-used! (car ls) (length last-shape)))
           #t))
        ((shape-test? (car ls))
         (easy-tag (cdr ls) (test-used-set (car ls)) target-set))
        (else 
         (easy-tag (cdr ls) last-shape target-set))))

(define (tag-negate-tests ls-of-ls)
  (letrec ((gen-target-set-help
            (lambda (init length)
              (if (zero? length)
                  '()
                  (cons init 
                        (gen-target-set-help (add1 init) 
                                             (sub1 length))))))
           (gen-target-set
            (lambda (length)
              (gen-target-set-help 2 length)))
           (tag-help 
            (lambda (ls target-set)
              (if (null? target-set) 
                  '()
                  (begin 
                    (tag-neg-test (car ls) 
                                  (reverse target-set))
                    (tag-help
                     (cdr ls)
                     (cdr target-set)))))))
    (tag-help (map car ls-of-ls) (gen-target-set (sub1 (length ls-of-ls))))))
           

; (define (move-negates-to-tags ls-of-ls)
;   (map (lambda (l) (cons (move-neg-to-tag (car l))
;                          (cdr l)))
;        ls-of-ls))
                         

; (define (move-neg-to-tag ls)
;   (let-values (((list-without-neg-tests neg-tests)
;                 (let loop ((l ls)
;                            (ntsf '()))
;                   (cond ((null? l) (values '() ntsf))
;                         ((negate-test? (car l))
;                          (loop (cdr l) (append ntsf (list (car l)))))
;                         (else 
;                          (let-values (((lwnt ntsf) (loop (cdr l) ntsf)))
;                            (values (cons (car l) lwnt)
;                                    ntsf)))))))
; ;(write 'lwnt--)(pretty-print list-without-neg-tests)
; ;(write 'neg-test)(pretty-print neg-tests)
;     (letrec ((insert-negtest 
;               (lambda (t-list neg-test)
;                 (cond ((null? t-list)
;                        '())
;                       ((and (equal? (test-used-set (car t-list))
;                                     (test-closest-shape-tst neg-test))
;                             (or (null? (cdr t-list))
;                                 (not (equal? (test-used-set (cadr t-list))
;                                     (test-closest-shape-tst neg-test)))))
;                        (cons (car t-list)
;                              (cons neg-test
;                                    (cdr t-list))))
;                      ;  ((equal? (test-tst (car t-list))
; ;                                (test-closest-shape-tst neg-test))
; ;                        (cons (car t-list)
; ;                              (cons neg-test
; ;                                    (cdr t-list))))
;                       (else 
;                        (cons (car t-list) 
;                              (insert-negtest (cdr t-list)
;                                              neg-test)))))))
;       (let loop2 ((t-list list-without-neg-tests)
;                   (ntst neg-tests))
;         ;(write 't-list)(pretty-print t-list)
;         ;(write 'ntst ) (pretty-print ntst)
;         ;(write 'insert) (pretty-print (insert-negtest t-list (car ntst)) )
; (cond ((null? ntst) t-list)
;       (else (insert-negtest t-list (car ntst))))))))
;        ;  (cond ((null? ntst)
; ;                t-list)
; ;               (loop2 (insert-negtest t-list (car ntst)) 
; ;                      (cdr ntst)))))))
  
    
         
  