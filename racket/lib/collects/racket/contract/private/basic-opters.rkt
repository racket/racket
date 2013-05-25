#lang racket/base

(require "guts.rkt"
         "blame.rkt"
         "opt.rkt"
         "misc.rkt")
(require (for-syntax racket/base
                     "opt-guts.rkt"))


;;
;; any/c
;;
(define/opter (any/c opt/i opt/info stx)
  (syntax-case stx (any/c)
    [any/c 
     (build-optres #:exp (opt/info-val opt/info)
                   #:lifts null
                   #:superlifts null
                   #:partials null
                   #:flat #'#t
                   #:opt #f
                   #:stronger-ribs null
                   #:chaperone #t
                   #:name #''any/c)]))

;;
;; false/c
;;
(define/opter (false/c opt/i opt/info stx) (opt/pred opt/info #'not #:name '#f))
(define/opter (not opt/i opt/info stx) (opt/pred opt/info #'not))
(define/opter (contract? opt/i opt/info stx) (opt/pred opt/info #'contract?))

;;
;; flat-contract helper
;;
(define-for-syntax (opt/flat-ctc opt/info pred checker)
  (syntax-case pred (null? number? integer? boolean? string? pair? not)
    ;; Better way of doing this?
    [pred
     (let* ((lift-vars (generate-temporaries (syntax (pred error-check))))
            (lift-pred (car lift-vars)))
       (with-syntax ((val (opt/info-val opt/info))
                     (ctc (opt/info-contract opt/info))
                     (blame (opt/info-blame opt/info))
                     (lift-pred lift-pred))
         (build-optres
          #:exp (syntax (if (lift-pred val)
                            val
                            (raise-blame-error
                             blame
                             val
                             '(expected: "~s" given: "~e")
                             (contract-name ctc)
                             val)))
          #:lifts
          (interleave-lifts
           lift-vars
           (list #'pred (cond [(eq? checker 'check-flat-contract) #'(check-flat-contract lift-pred)]
                              [(eq? checker 'check-flat-named-contract) #'(check-flat-named-contract lift-pred)])))
          #:superlifts null
          #:partials null
          #:flat (syntax (lift-pred val))
          #:opt #f
          #:stronger-ribs null
          #:chaperone #t
          #:name #'(object-name lift-pred))))]))

;;
;; flat-contract and flat-named-contract
;;
(define/opter (flat-contract opt/i opt/info stx)
  (syntax-case stx (flat-contract)
    [(flat-contract pred) (opt/flat-ctc opt/info #'pred 'check-flat-contract)]))
(define/opter (flat-named-contract opt/i opt/info stx)
  (syntax-case stx (flat-named-contract)
    [(flat-named-contract name pred) (opt/flat-ctc opt/info #'pred 'check-flat-named-contract)]))
