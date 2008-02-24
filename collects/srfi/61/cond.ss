;;SRFI 61  A more general COND clause
;;Chongkai Zhu  mrmathematica@yahoo.com
;;12-18-2005
(module cond mzscheme
  
  (provide (rename my-cond srfi:cond))
  
  (define-syntax my-cond
    (syntax-rules (=> else)
      ((_ (else else1 else2 ...))
       (if #t (begin else1 else2 ...)))
      ((_ (test => receiver) more-clause ...)
       (let ((t test))
         (if t
             (receiver t)
             (my-cond more-clause ...))))
      ((_ (generator guard => receiver) more-clause ...)
       (call-with-values (lambda () generator)
                         (lambda t
                           (if (apply guard    t)
                               (apply receiver t)
                               (my-cond more-clause ...)))))
      ((_ (test) more-clause ...)
       (or test (my-cond more-clause ...)))
      ((_ (test body ...) more-clause ...)
       (if test
           (begin body ...)
           (my-cond more-clause ...)))
      ((_)
       (void)))))
