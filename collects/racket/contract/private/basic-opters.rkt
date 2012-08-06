#lang racket/base

(require "guts.rkt"
         "blame.rkt"
         "opt.rkt"
         "misc.rkt")
(require (for-syntax racket/base
                     "opt-guts.rkt"))

;;
;; opt/pred helper
;;
(define-for-syntax (opt/pred opt/info pred)
  (with-syntax ((pred pred))
    (build-optres
     #:exp
     (with-syntax ((val (opt/info-val opt/info))
                   (ctc (opt/info-contract opt/info))
                   (blame (opt/info-blame opt/info)))
       (syntax (if (pred val)
                   val
                   (raise-opt/pred-error blame val 'pred))))
     #:lifts null
     #:superlifts null
     #:partials null
     #:flat (syntax (pred val))
     #:opt #f
     #:stronger-ribs null
     #:chaperone #t)))

(define (raise-opt/pred-error blame val pred-name)
  (raise-blame-error
   blame
   val
   '(expected: "~a")
   pred-name))

;;
;; built-in predicate opters
;;
(define/opter (null? opt/i opt/info stx) (opt/pred opt/info #'null?))
(define/opter (boolean? opt/i opt/info stx) (opt/pred opt/info #'boolean?))
(define/opter (string? opt/i opt/info stx) (opt/pred opt/info #'string?))
(define/opter (integer? opt/i opt/info stx) (opt/pred opt/info #'integer?))
(define/opter (char? opt/i opt/info stx) (opt/pred opt/info #'char?))
(define/opter (number? opt/i opt/info stx) (opt/pred opt/info #'number?))
(define/opter (pair? opt/i opt/info stx) (opt/pred opt/info #'pair?))
(define/opter (not opt/i opt/info stx) (opt/pred opt/info #'not))
(define/opter (real? opt/i opt/info stx) (opt/pred opt/info #'real?))

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
                   #:chaperone #t)]))

;;
;; false/c
;;
(define/opter (false/c opt/i opt/info stx) (opt/pred opt/info #'not))

;;
;; flat-contract helper
;;
(define-for-syntax (opt/flat-ctc opt/info pred checker)
  (syntax-case pred (null? number? integer? boolean? string? pair? not)
    ;; Better way of doing this?
    [null? (opt/pred opt/info pred)]
    [number? (opt/pred opt/info pred)]
    [integer? (opt/pred opt/info pred)]
    [boolean? (opt/pred opt/info pred)]
    [string? (opt/pred opt/info pred)]
    [pair? (opt/pred opt/info pred)]
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
          #:chaperone #t)))]))

;;
;; flat-contract and friends
;;
(define/opter (flat-contract opt/i opt/info stx)
  (syntax-case stx (flat-contract)
    [(flat-contract pred) (opt/flat-ctc opt/info #'pred 'check-flat-contract)]))
(define/opter (flat-named-contract opt/i opt/info stx)
  (syntax-case stx (flat-named-contract)
    [(flat-named-contract name pred) (opt/flat-ctc opt/info #'pred 'check-flat-named-contract)]))
