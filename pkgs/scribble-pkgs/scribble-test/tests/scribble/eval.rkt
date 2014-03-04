#lang racket/base
(require scribble/eval scribble/core rackunit racket/match)

(check-not-exn (λ () (make-base-eval)))
(check-not-exn (λ () (make-base-eval #:pretty-print? #t #:lang 'racket/base)))
(check-not-exn (λ () (make-base-eval #:pretty-print? #t #:lang 'racket)))
(check-not-exn (λ () (make-base-eval #:pretty-print? #t #:lang 'typed/racket)))
(check-not-exn (λ () (make-base-eval #:pretty-print? #t #:lang 'lazy)))
(check-not-exn (λ () (make-base-eval #:pretty-print? #f #:lang 'racket/base)))
(check-not-exn (λ () (make-base-eval #:pretty-print? #f #:lang 'racket)))
(check-not-exn (λ () (make-base-eval #:pretty-print? #f #:lang 'typed/racket)))
(check-not-exn (λ () (make-base-eval #:pretty-print? #f #:lang 'lazy)))

(check-not-exn (λ () ((make-base-eval-factory '() #:pretty-print? #t))))
(check-not-exn (λ () ((make-base-eval-factory '() #:pretty-print? #t #:lang 'racket/base))))
(check-not-exn (λ () ((make-base-eval-factory '() #:pretty-print? #t #:lang 'racket))))
(check-not-exn (λ () ((make-base-eval-factory '() #:pretty-print? #t #:lang 'typed/racket))))
(check-not-exn (λ () ((make-base-eval-factory '() #:pretty-print? #t #:lang 'lazy))))

(check-not-exn (λ () ((make-eval-factory '() #:pretty-print? #t))))
(check-not-exn (λ () ((make-eval-factory '() #:pretty-print? #t #:lang 'racket/base))))
(check-not-exn (λ () ((make-eval-factory '() #:pretty-print? #t #:lang 'racket))))
(check-not-exn (λ () ((make-eval-factory '() #:pretty-print? #t #:lang 'typed/racket))))
(check-not-exn (λ () ((make-eval-factory '() #:pretty-print? #t #:lang 'lazy))))

(define (get-result-blocks nf)
  (match (nested-flow-blocks nf) [(list (table _ (list _ res))) res]))

(define filter-datum '(define (filter p? lst) 
                        (if (null? lst) 
                            null 
                            (let ([x (car lst)]) 
                              (if (p? x)
                                  (cons x (filter p? (cdr lst)))
                                  (filte p? (cdr lst)))))))
;; check that pretty printing is working
(define pp-blocks
  (car
   (get-result-blocks
    (interaction #:eval (make-base-eval #:pretty-print? #t #:lang 'racket)
                 '(define (filter p? lst) 
                    (if (null? lst) 
                        null 
                        (let ([x (car lst)])
                          (if (p? x) 
                              (cons x (filter p? (cdr lst)))
                              (filter p? (cdr lst))))))))))
(check-true (table? pp-blocks)) ; multiple line result gets put in a table of paragraphs
(check-equal? (length (table-blockss pp-blocks)) 5) ;; pretty printed into 5 lines

(define non-pp-blocks
  (car
   (get-result-blocks
    (interaction #:eval (make-base-eval #:pretty-print? #f #:lang 'racket) 
                 '(define (filter p? lst)
                    (if (null? lst) 
                        null 
                        (let ([x (car lst)]) 
                          (if (p? x) 
                              (cons x (filter p? (cdr lst)))
                              (filter p? (cdr lst))))))))))
(check-true (paragraph? non-pp-blocks)) ;; single line result is just 1 paragraph

;; check that different evaluators do not share a single namespace
(define e1 (make-base-eval))
(define e2 (make-base-eval))
(check-exn exn:fail:contract:variable? (λ () (e1 '(current-date))))
(check-exn exn:fail:contract:variable? (λ () (e2 '(current-date))))
(e1 '(require racket/date))
(check-not-exn (λ () (e1 '(current-date))))
(check-exn exn:fail:contract:variable? (λ () (e2 '(current-date))))
