(module xelda-lib mzscheme
  
  (require (lib "list.ss"))
  
  (provide 
   average
   gen-equal-units
   gen-mult-units
   gen-div-units
   gen-exp-units
   split-list
   check-empty-units
   check-equal-units
   is-error-unit?
   empty-unit?
   empty-unit
   canonicalize-units
   verify-equal-units)

  (define (empty-unit) (list (list 'empty_unit 1)))

  (define (canonicalize-units us)
    (filter (lambda (u) (not (zero? (cadr u))))
            (sort us (lambda (u1 u2)
                       (string<=? (symbol->string (car u1))
                                  (symbol->string (car u2)))))))

  (define (empty-unit? u)
    (and (= (length u) 1)
         (eq? (car (first u)) 'empty_unit)))

  (define (split-list lst p?)
    (let loop ([lst lst]
	       [yes null]
	       [no null])
      (if (null? lst)
	  (list (reverse yes) (reverse no))
	  (if (p? (car lst))
	      (loop (cdr lst) (cons (car lst) yes) no)
	      (loop (cdr lst) yes (cons (car lst) no))))))
  
  (define (average . ns)
    (/ (apply + ns) (length ns)))
  
  (define (gen-equal-units . us)
    (let ([u1 (car us)])
      (if (andmap (lambda (u) (equal? u u1))
		  (cdr us))
	  u1
	  '((error/equality 1)))))
  
  (define (is-error-unit? u)
    (if (and (list? u) (pair? (first u)))
        (let ([str (symbol->string (car (first u)))])
          (cond
            ((and (> (string-length str) 6)
                  (string=? (substring str 0 6) "error/")) #t)
            (else #f)))
        #f))
  
  (define (check-equal-units unts sym)
    (let* ([us (map (lambda (_us)
                      (map (lambda (u)
                             (cond ((eq? (car u) 'empty_unit)
                                    (list 'empty_unit 1))
                                   (else u))) _us)) unts)]
           [base-unit (first us)]
           [err-us (filter (lambda (u) (is-error-unit? u)) us)])
      (if (not (empty? err-us))
          (if (eq? sym (second (first (first err-us))))
              (first err-us)
              (list (list 'error/propagated (second (first (first err-us))))))
          (if (andmap (lambda (u) (equal? u base-unit)) us)
              base-unit
              (list (list 'error/equality sym))))))
  
  (define (verify-equal-units cell-name u1 u2)
    (if (equal? u1 u2)
	(printf "Verified units for cell ~a~n" cell-name)
	(printf (string-append 
		 "Units problem with cell ~a:~n"
		 "Cell annotated with ~a, formula yields ~a~n")
		cell-name u1 u2)))
  
  (define (gen-mult-units u1 u2 sym)
    (cond [(is-error-unit? u1)
           (cond [(eq? sym (second (first u1))) u1]
                 [else (list (list 'error/propagated
                                   (second (first u1))))])]
          [(is-error-unit? u2)
           (cond [(eq? sym (second (first u2))) u2]
                 [else (list (list 'error/propagated
                                   (second (first u2))))])]
          [(empty-unit? u1) u2]
          [(empty-unit? u2) u1]
          [else
           (let ([raw-us (append u1 u2)])
             (let loop ([new-us raw-us]
                        [result null])
               (if (null? new-us)
                   (canonicalize-units result)
                   (let* ([u (car new-us)]
                          [u-name (car u)]
                          [u-exp (cadr u)]
                          [u-and-non-u 
                           (split-list (cdr new-us) 
                                       (lambda (x) 
                                         (eq? (car x) u-name)))]
                          [new-u-exp (+ u-exp 
                                        (apply + (map cadr (car u-and-non-u))))]
                          [non-u (cadr u-and-non-u)])
                     (loop non-u (cons (list u-name new-u-exp) result))))))]))
  
  (define (gen-div-units u1 u2 sym)
    (cond [(is-error-unit? u1)  
           (cond [(eq? sym (second (first u1))) u1]
                 [else (list (list 'error/propagated
                                   (second (first u1))))])]
          [(is-error-unit? u2)
           (cond [(eq? sym (second (first u2))) u2]
                 [else (list (list 'error/propagated
                                   (second (first u2))))])]
          [(empty-unit? u2) u1]
          [else
           (gen-mult-units u1 (map (lambda (u)
                                     (let ([u-name (car u)]
                                           [u-exp (cadr u)])
                                       (list u-name (- 0 u-exp)))) u2) sym)]))
  
  (define (gen-exp-units u exp sym)
    (cond [(is-error-unit? u)
           (cond [(eq? sym (second (first u))) u]
                 [else (list (list 'error/propagated (second (first u))))])]
          [else
           (if (not (integer? exp))
               '((error/exponentiation sym))
               (cond [(empty-unit? u) u]
                     [else
                      (map (lambda (u_)
                             (let ([u-name (car u_)]
                                   [u-exp (cadr u_)])
                               (list u-name (inexact->exact 
                                             (* exp u-exp)))))
                           u)]))]))
  
  (define (check-empty-units . us)
    (if (andmap null? us)
	null
	'((error/empty-unit 1)))))

