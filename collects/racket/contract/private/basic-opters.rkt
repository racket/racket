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
    (values
     (with-syntax ((val (opt/info-val opt/info))
                   (ctc (opt/info-contract opt/info))
                   (blame (opt/info-blame opt/info)))
       (syntax (if (pred val)
                   val
                   (raise-blame-error
                    blame
                    val
                    "expected: ~s, given: ~e"
                    (contract-name ctc)
                    val))))
     null
     null
     null
     (syntax (pred val))
     #f
     null
     #t)))

;;
;; built-in predicate opters
;;
(define/opter (null? opt/i opt/info stx)
  (syntax-case stx (null?)
    [null? (opt/pred opt/info #'null?)]))
(define/opter (boolean? opt/i opt/info stx)
  (syntax-case stx (boolean?)
    [boolean? (opt/pred opt/info #'boolean?)]))
(define/opter (string? opt/i opt/info stx)
  (syntax-case stx (string?)
    [string? (opt/pred opt/info #'string?)]))
(define/opter (integer? opt/i opt/info stx)
  (syntax-case stx (integer?)
    [integer? (opt/pred opt/info #'integer?)]))
(define/opter (char? opt/i opt/info stx)
  (syntax-case stx (char?)
    [char? (opt/pred opt/info #'char?)]))
(define/opter (number? opt/i opt/info stx)
  (syntax-case stx (number?)
    [number? (opt/pred opt/info #'number?)]))
(define/opter (pair? opt/i opt/info stx)
  (syntax-case stx (pair?)
    [pair? (opt/pred opt/info #'pair?)]))
(define/opter (not opt/i opt/info stx)
  (syntax-case stx (not)
    [not (opt/pred opt/info #'not)]))

;;
;; any/c
;;
(define/opter (any/c opt/i opt/info stx)
  (syntax-case stx (any/c)
    [any/c (values
            (opt/info-val opt/info)
            null
            null
            null
            #'#t
            #f
            null
            #t)]))

;;
;; false/c
;;
(define/opter (false/c opt/i opt/info stx)
  (syntax-case stx (false/c)
    [false/c (opt/pred opt/info #'not)]))

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
         (values
          (syntax (if (lift-pred val)
                      val
                      (raise-blame-error
                       blame
                       val
                       "expected: ~s, given: ~e"
                       (contract-name ctc)
                       val)))
          (interleave-lifts
           lift-vars
           (list #'pred (cond [(eq? checker 'check-flat-contract) #'(check-flat-contract lift-pred)]
                              [(eq? checker 'check-flat-named-contract) #'(check-flat-named-contract lift-pred)])))
          null
          null
          (syntax (lift-pred val))
          #f
          null
          #t)))]))

;;
;; flat-contract and friends
;;
(define/opter (flat-contract opt/i opt/info stx)
  (syntax-case stx (flat-contract)
    [(flat-contract pred) (opt/flat-ctc opt/info #'pred 'check-flat-contract)]))
(define/opter (flat-named-contract opt/i opt/info stx)
  (syntax-case stx (flat-named-contract)
    [(flat-named-contract name pred) (opt/flat-ctc opt/info #'pred 'check-flat-named-contract)]))
