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
         (cond/maybe-more t
                          (receiver t)
                          more-clause ...)))
      ((_ (generator guard => receiver) more-clause ...)
       (call-with-values (lambda () generator)
                         (lambda t
                           (cond/maybe-more (apply guard    t)
                                            (apply receiver t)
                                            more-clause ...))))
      ((_ (test) more-clause ...)
       (let ((t test))
         (cond/maybe-more t t more-clause ...)))
      ((_ (test body1 body2 ...) more-clause ...)
       (cond/maybe-more test
                        (begin body1 body2 ...)
                        more-clause ...))
      ((_)
       (cond))))
  
  (define-syntax cond/maybe-more
    (syntax-rules ()
      ((_ test consequent)
       (if test
           consequent))
      ((_ test consequent clause ...)
       (if test
           consequent
           (my-cond clause ...))))))