#!r6rs

;; implementation mostly from Mike Sperber
; The (rnrs lists (6)) library.

(library (rnrs lists (6))
  (export find
          for-all exists
 
          filter
          partition
          fold-left
          fold-right
          remp remove remv remq
          memp member memv memq
          assp assoc assv assq
          cons*)
  (import (rnrs base (6))
          (rnrs control (6)))

  (define (assert-procedure who obj)
    (if (not (procedure? obj))
        (assertion-violation who "not a procedure" obj)))

  (define (find proc list)
    (assert-procedure 'find proc)
    (let loop ((list list))
      (cond
       ((null? list) #f)
       ((proc (car list)) (car list))
       (else (loop (cdr list))))))

  (define (check-nulls who the-list the-lists lists)
    (for-each (lambda (list)
                (if (not (null? list))
                    (apply assertion-violation who
                           "argument lists don't have the same size"
                           list lists)))
              lists))

  (define for-all 
    (case-lambda
     [(proc list)
      (assert-procedure 'for-all proc)
      (for-all1 proc list)]
     [(proc list . lists)
      (assert-procedure 'for-all proc)
      (cond
       ((null? list)
        (check-nulls 'for-all list lists lists)
        #t)
       (else
        (let loop ((list list) (lists lists))
          (let ((next (cdr list)))
            (cond
             ((null? next)
              (apply proc (car list) (map car lists))) 
             ((apply proc (car list) (map car lists))
              (loop next (map cdr lists)))
             (else #f))))))]))

  (define (for-all1 proc list)
    (if (null? list)
        #t
        (let loop ((list list))
          (let ((next (cdr list)))
            (cond
             ((null? next) (proc (car list))) 
             ((proc (car list)) (loop next))
             (else #f))))))

  (define exists
    (case-lambda
     [(proc list)
      (assert-procedure 'exists proc)
      (exists1 proc list)]
     [(proc list . lists)
      (assert-procedure 'exists proc)
      (cond
       ((null? list)
        (check-nulls 'exists list lists lists)
        #f)
       (else
        (let loop ((list list) (lists lists))
          (let ((next (cdr list)))
            (if (null? next)
                (apply proc (car list) (map car lists))
                (or (apply proc (car list) (map car lists))
                    (loop next (map cdr lists))))))))]))

  (define (exists1 proc list)
    (if (null? list)
        #f
        (let loop ((list list))
          (let ((next (cdr list)))
            (if (null? next)
                (proc (car list))
                (or (proc (car list))
                    (loop next)))))))

  (define (filter proc list)
    (assert-procedure 'filter proc)
    (let loop ((list list))
      (cond ((null? list) '())
            ((proc (car list))
             (let ([r (loop (cdr list))])
               (if (eq? r (cdr list))
                   list
                   (cons (car list) r))))
            (else
             (loop (cdr list))))))

  (define (partition proc list)
    (assert-procedure 'partition proc)
    (let loop ((list list) (yes '()) (no '()))
      (cond ((null? list)
             (values (reverse yes) (reverse no)))
            ((proc (car list))
             (loop (cdr list) (cons (car list) yes) no))
            (else
             (loop (cdr list) yes (cons (car list) no))))))

  (define (fold-left combine nil the-list . the-lists)
    (assert-procedure 'fold-left combine)
    (if (null? the-lists)
        (fold-left1 combine nil the-list)
        (let loop ((accum nil) (list the-list) (lists the-lists))
          (if (null? list)
              (begin
                (check-nulls 'fold-left the-list the-lists lists)
                accum)
              (loop (apply combine accum (car list) (map car lists))
                    (cdr list)
                    (map cdr lists))))))

  (define (fold-left1 combine nil list)
    (let loop ((accum nil) (list list))
      (if (null? list)
          accum
          (loop (combine accum (car list))
                (cdr list)))))


  (define (fold-right combine nil the-list . the-lists)
    (assert-procedure 'fold-right combine)
    (if (null? the-lists)
        (fold-right1 combine nil the-list)
        (let recur ((list the-list) (lists the-lists))
          (if (null? list)
              (begin
                (check-nulls 'fold-right the-list the-lists lists)
                nil)
              (apply combine
                     (car list)
                     (append (map car lists)
                             (cons (recur (cdr list) (map cdr lists))
                                   '())))))))

  (define (fold-right1 combine nil list)
    (let recur ((list list))
      (if (null? list)
          nil
          (combine (car list) (recur (cdr list))))))

  (define (remp proc list)
    (assert-procedure 'remp proc)
    (let recur ((list list))
      (cond ((null? list) '())
            ((proc (car list))
             (recur (cdr list)))
            (else
             (let ([r (recur (cdr list))])
               (if (eq? r (cdr list))
                   list
                   (cons (car list) r)))))))

  ;; Poor man's inliner
  (define-syntax define-remove-like
    (syntax-rules ()
      ((define-remove-like ?name ?equal?)
       (define (?name obj list)
         (let recur ((list list))
           (cond ((null? list) '())
                 ((?equal? obj (car list))
                  (recur (cdr list)))
                 (else
                  (let ([r (recur (cdr list))])
                    (if (eq? r (cdr list))
                        list
                        (cons (car list) r))))))))))

  (define-remove-like remove equal?)
  (define-remove-like remv eqv?)
  (define-remove-like remq eq?)

  (define (memp proc list)
    (assert-procedure 'member proc)
    (let loop ((list list))
      (cond ((null? list) #f)
            ((proc (car list)) list)
            (else (loop (cdr list))))))

  (define-syntax define-member-like
    (syntax-rules ()
      ((define-member-like ?name ?equal?)
       (define (?name obj list)
         (let loop ((list list))
           (cond ((null? list) #f)
                 ((?equal? obj (car list)) list)
                 (else (loop (cdr list)))))))))

  (define-member-like member equal?)
  (define-member-like memv eqv?)
  (define-member-like memq eq?)

  (define (assp proc alist)
    (assert-procedure 'assp proc)
    (let loop ((alist alist))
      (if (null? alist)
          #f
          (let ((p (car alist)))
            (if (proc (car p))
                p
                (loop (cdr alist)))))))

  (define-syntax define-assoc-like
    (syntax-rules ()
      ((define-assoc-like ?name ?equal?)
       (define (?name obj alist)
         (let loop ((alist alist))
           (if (null? alist)
               #f
               (let ((p (car alist)))
                 (if (?equal? obj (car p))
                     p
                     (loop (cdr alist))))))))))

  (define-assoc-like assoc equal?)
  (define-assoc-like assq eq?)
  (define-assoc-like assv eqv?)

  (define (cons* obj . objs)
    (if (null? objs)
        obj
        (cons obj (apply cons* objs)))))
