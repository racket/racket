;;SRFI 61  A more general COND clause
;;Chongkai Zhu  mrmathematica@yahoo.com
;;12-18-2005
;; * Updated by Chris Jester-Young 2013-02-28
#lang racket/base
(provide (prefix-out srfi: (combine-out cond => else)))

(define-syntax cond
  (syntax-rules (=> else)
    ((_ (else else1 else2 ...))
     (when #t (begin else1 else2 ...)))
    ((_ (test => receiver) more-clause ...)
     (let ((t test))
       (if t
           (receiver t)
           (cond more-clause ...))))
    ((_ (generator guard => receiver) more-clause ...)
     (call-with-values (lambda () generator)
                       (lambda t
                         (if (apply guard    t)
                             (apply receiver t)
                             (cond more-clause ...)))))
    ((_ (test) more-clause ...)
     (or test (cond more-clause ...)))
    ((_ (test body ...) more-clause ...)
     (if test
         (begin body ...)
         (cond more-clause ...)))
    ((_)
     (void))))
