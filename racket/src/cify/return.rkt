#lang racket/base
(require "out.rkt")

(provide (struct-out tail-return)
         (struct-out multiple-return)
         (struct-out only-multiple-return)
         (struct-out multiple-return/suffix)
         return
         return-can-omit?
         return-can-omit-single?)

(struct tail-return (function-id lam self-args leaf?))
(struct multiple-return (prefix))
(struct only-multiple-return multiple-return ())
(struct multiple-return/suffix multiple-return (generate-suffix))

(define (return ret runstack s
                #:can-omit? [can-omit? #f]
                #:can-pre-pop? [can-pre-pop? #f])
  (unless (and can-omit? (return-can-omit? ret))
    (let loop ([ret ret])
      (cond
        [(tail-return? ret)
         (cond
           [(tail-return-leaf? ret)
            (out "return ~a;" s)]
           [can-pre-pop?
            (out "c_current_runstack = c_orig_runstack;")
            (out "return ~a;" s)]
           [else
            (out-open "{")
            (out "Scheme_Object *c_retval = ~a;" s)
            (out "c_current_runstack = c_orig_runstack;")
            (out "return c_retval;")
            (out-close "}")])]
        [(multiple-return? ret) (loop (multiple-return-prefix ret))]
        [(procedure? ret) (ret s)]
        [else (out "~a ~a;" ret s)]))
    (when (multiple-return/suffix? ret)
      ((multiple-return/suffix-generate-suffix ret)))))

(define (return-can-omit? ret)
  (or (equal? ret "")
      (and (multiple-return? ret)
           (equal? (multiple-return-prefix ret) ""))))

(define (return-can-omit-single? ret)
  (only-multiple-return? ret))
