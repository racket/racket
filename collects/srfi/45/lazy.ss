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
       (make-srfi-45-promise (mcons #f (lambda () exp))))))
  
  (define (eager x)
    (make-srfi-45-promise (mcons #t x)))
  
  (define-syntax s:delay
    (syntax-rules ()
      ((_ exp) (lazy (eager exp)))))
  
  (define (s:force promise)
    (if (srfi-45-promise? promise)
        (let ((content (srfi-45-promise-content promise)))
          (if (mcar content)
              (mcdr content)
              (let* ((promise* ((mcdr content)))
                     (content (srfi-45-promise-content promise)))
                (unless (mcar content)
                  (set-mcar! content (mcar (srfi-45-promise-content promise*)))
                  (set-mcdr! content (mcdr (srfi-45-promise-content promise*)))
                  (set-srfi-45-promise-content! promise* content))
                (s:force promise))))
        (raise-type-error 'force "srfi-45-promise" promise))))
