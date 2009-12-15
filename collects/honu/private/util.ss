#lang scheme


(provide delim-identifier=?
         extract-until
         call-values)

(require syntax/stx)

(define (delim-identifier=? a b)
  (eq? (syntax-e a) (syntax-e b)))

(define extract-until
  (case-lambda
    [(r ids keep?)
     (let loop ([r r][val-stxs null])
       (cond
         [(stx-null? r)
          (values #f #f #f)]
         [(and (identifier? (stx-car r))
               (ormap (lambda (id)
                        (delim-identifier=? id (stx-car r)))
                      ids))
          (values (reverse (if keep?
                             (cons (stx-car r) val-stxs) 
                             val-stxs))
                  r
                  (stx-car r))]
         [else
           (loop (stx-cdr r) (cons (stx-car r) val-stxs))]))]
    [(r ids) (extract-until r ids #f)]))

(define-syntax-rule (call-values function values-producing)
  (call-with-values (lambda () values-producing) function))

(define (test)
  (let* ([original #'(a b c d e)]
         [delimiter #'c]
         [expected-before #'(a b)]
         [expected-rest #'(c d e)]
         [expected-delimiter #'c]
         )
    (let-values ([(before rest hit) (extract-until original (list delimiter))])
      ;; is there a better way to test equality between two syntaxes?
      (when (not (and (equal? (syntax->datum expected-before)
                              (map syntax->datum before))
                      (equal? (syntax->datum expected-rest)
                              (map syntax->datum rest))
                      (equal? (syntax->datum expected-delimiter)
                              (syntax->datum hit))))
        (printf "failure: original ~a until ~a\n" (syntax->datum original) (map syntax->datum (list delimiter)))
        (printf " before expected ~a actual ~a\n" (syntax->datum expected-before) (map syntax->datum before))
        (printf " rest expected ~a actual ~a\n" (syntax->datum expected-rest) (map syntax->datum rest))
        (printf " delimiter expected ~a actual ~a\n" (syntax->datum expected-delimiter) (syntax->datum hit))
        ))))

(test)
