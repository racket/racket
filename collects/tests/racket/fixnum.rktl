(load-relative "loadtest.rktl")
(Section 'fixnum)
(require scheme/fixnum
         scheme/unsafe/ops
         (prefix-in r6: rnrs/arithmetic/fixnums-6))

(define unary-table 
  (list (list fxnot unsafe-fxnot)
        (list fxabs unsafe-fxabs)
        (list fx->fl unsafe-fx->fl)
        (list (lambda (v) (fl->fx (exact->inexact x)))
              (lambda (v) (unsafe-fl->fx (exact->inexact x))))))

(define binary-table
  (list (list fx+ unsafe-fx+)
        (list fx- unsafe-fx-)
        (list fx* unsafe-fx*)

        (list fxquotient unsafe-fxquotient)
        (list fxremainder unsafe-fxremainder)
        (list fxmodulo unsafe-fxmodulo)
        
        (list fxand unsafe-fxand)
        (list fxior unsafe-fxior)
        (list fxxor unsafe-fxxor)

        (list fx>= unsafe-fx>=)
        (list fx> unsafe-fx>)
        (list fx= unsafe-fx=)
        (list fx<= unsafe-fx<=)
        (list fx< unsafe-fx<)
        (list fxmin unsafe-fxmin)
        (list fxmax unsafe-fxmax)))

(define binary/small-second-arg-table
  (list (list fxlshift unsafe-fxlshift)
        (list fxrshift unsafe-fxrshift)))

(define nary-table
  (list))

(define table (append binary/small-second-arg-table binary-table unary-table nary-table))

(define (normalize-arity a)
  (cond
    [(list? a)
     (let ([at-least (ormap (λ (x) (and (arity-at-least? x) x)) a)])
       (if at-least
           (let ([new-a
                  (filter (λ (x) (or (not (number? x))
                                     (< x (arity-at-least-value at-least))))
                          a)])
             (if (pair? (cdr new-a))
                 new-a
                 (car new-a)))
           (if (pair? (cdr a))
               a
               (car a))))]
    [else a]))
  
(define (check-arity fx unsafe-fx)
  (let ([same-arities? (λ (x y) (equal? (normalize-arity (procedure-arity x))
                                        (normalize-arity (procedure-arity y))))])
    (test #t
          same-arities?
          fx
          unsafe-fx)))


;; same-results : (fixnum ... -> any) (fixnum ... -> any) (listof fixnum) -> #t
;; applies fx to args; if it raises an error, the function returns #t.
;;                     if it returns a result, the function applies args
;;                     to unsafe-fx and makes sure the results are either eq? or
;;                     (if the results are flonums), =
;; raises an exception when it finds a bug.
(define (same-results fx unsafe-fx args) 
  (let/ec k
    (let* ([fx-result (with-handlers ((exn:fail? (λ (x) (k #t))))
                        (apply fx args))]
           [unsafe-result (apply unsafe-fx args)]
           [ans
            (or (eq? fx-result unsafe-result)
                (and (flonum? fx-result)
                     (flonum? unsafe-result)
                     (= fx-result unsafe-result)))])
      (unless ans
        (newline)
        (error 'same-results "better die now, rather than continue, what with unsafe ops around:\n     fx-result ~s\n unsafe-result ~s"
               fx-result
               unsafe-result))
      #t)))

(define (flonum? x) (inexact-real? x))

(define (same-results/range/table)
  (for ([line (in-list unary-table)])
    (for ([i (in-range (- (expt 2 8)) (expt 2 8))])
      (test #t same-results (list-ref line 0) (list-ref line 1) (list i))))
  
  (for ([line (in-list (append binary/small-second-arg-table 
                               binary-table
                               nary-table))])
    (for ([i (in-range (- (expt 2 4)) (expt 2 4))])
      (for ([j (in-range (- (expt 2 4)) (expt 2 4))])
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i j))))))

(define (same-results/extremum)
  (let ([interesting-values (list (r6:least-fixnum) -1 0 1 (r6:greatest-fixnum))])
    (for ([line (in-list unary-table)])
      (for ([i (in-list interesting-values)])
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i))))
    
    (for ([line (in-list (append binary/small-second-arg-table
                                 binary-table
                                 nary-table))])
      (for ([i (in-list interesting-values)])
        (for ([j (in-list interesting-values)])
          (test #t same-results (list-ref line 0) (list-ref line 1) (list i j)))))))

(define (same-results/random/table)
  (for ([ignore (in-range 0 800)])
    (let ([i (random-fixnum)]
          [j (random-fixnum)]
          [k (inexact->exact (floor (* (random) (+ 1 (r6:fixnum-width)))))]
          [more-fixnums (build-list (random 20) (λ (i) (random-fixnum)))])
      (for ([line (in-list unary-table)])
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i)))
      (for ([line (in-list binary-table)])
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i j)))
      (for ([line (in-list binary/small-second-arg-table)])
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i k)))
      (for ([line (in-list nary-table)])
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i))
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i j))
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i j k))
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i k j))
        (test #t same-results (list-ref line 0) (list-ref line 1) more-fixnums)))))

(define (random-fixnum)
  (inexact->exact (floor (+ (r6:least-fixnum) (* (random) (+ (- (r6:greatest-fixnum) (r6:least-fixnum)) 1))))))

;; check the arities
(for-each (λ (x) (apply check-arity x)) table)

;; check the extreme values (against themselves and few other values)
(same-results/extremum)

;; check randomly
(same-results/random/table)

;; check a small range
(same-results/range/table)

(report-errs)
