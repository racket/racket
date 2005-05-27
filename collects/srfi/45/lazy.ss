; SRFI 45
; Zhu Chongkai   mrmathematica@yahoo.com
; 25-May-2005
(module lazy mzscheme
  
  (provide lazy
           eager
           s:delay
           s:force
           srfi-45-promise?)
  
  (define-struct srfi-45-promise (content))
  
  (define-syntax lazy
    (syntax-rules ()
      ((_ exp)
       (make-srfi-45-promise (cons #f (lambda () exp))))))
  
  (define (eager x)
    (make-srfi-45-promise (cons #t x)))
  
  (define-syntax s:delay
    (syntax-rules ()
      ((_ exp) (lazy (eager exp)))))
  
  (define (s:force promise)
    (if (srfi-45-promise? promise)
        (let ((content (srfi-45-promise-content promise)))
          (if (car content)
              (cdr content)
              (let* ((promise* ((cdr content)))
                     (content (srfi-45-promise-content promise)))
                (unless (car content)
                  (set-car! content (car (srfi-45-promise-content promise*)))
                  (set-cdr! content (cdr (srfi-45-promise-content promise*)))
                  (set-srfi-45-promise-content! promise* content))
                (s:force promise))))
        (raise-type-error 'force "srfi-45-promise" promise))))
