#lang racket/base
(require syntax/private/boundmap ;; needs to be the private one, since the public one has contracts
         (for-template racket/base)
         (for-template "guts.rkt"
                       "blame.rkt"
                       "misc.rkt")
         (for-syntax racket/base))

(provide get-opter reg-opter! opter
         interleave-lifts
         
         make-opt/info
         opt/info-contract
         opt/info-val
         opt/info-blame
         opt/info-free-vars
         opt/info-recf
         opt/info-base-pred
         opt/info-this
         opt/info-that
         
         opt/info-swap-blame
         opt/info-change-val
         
         opt/unknown)

;; a hash table of opters
(define opters-table
  (make-module-identifier-mapping))

;; get-opter : syntax -> opter
(define (get-opter ctc)
  (module-identifier-mapping-get opters-table ctc (λ () #f)))

;; opter : (union symbol identifier) -> opter
(define (opter ctc)
  (if (identifier? ctc)
      (get-opter ctc)
      (error 'opter "the argument must be a bound identifier, got ~e" ctc)))

;; reg-opter! : symbol opter ->
(define (reg-opter! ctc opter)
  (module-identifier-mapping-put! opters-table ctc opter))

;; interleave-lifts : list list -> list
;; interleaves a list of variables names and a list of sexps into a list of
;; (var sexp) pairs.
(define (interleave-lifts vars sexps)
  (if (= (length vars) (length sexps))
      (if (null? vars) null
          (cons (cons (car vars) (car sexps))
                (interleave-lifts (cdr vars) (cdr sexps))))
      (error 'interleave-lifts "expected lists of equal length, got ~e and ~e" vars sexps)))


;; struct for color-keeping across opters
(define-struct opt/info
  (contract val blame-id swap-blame? free-vars recf base-pred this that))

(define (opt/info-blame oi)
  (if (opt/info-swap-blame? oi)
    #`(blame-swap #,(opt/info-blame-id oi))
    (opt/info-blame-id oi)))

;; opt/info-swap-blame : opt/info -> opt/info
;; swaps pos and neg
(define (opt/info-swap-blame info)
  (struct-copy opt/info info [swap-blame? (not (opt/info-swap-blame? info))]))

;; opt/info-change-val : identifier opt/info -> opt/info
;; changes the name of the variable that the value-to-be-contracted is bound to
(define (opt/info-change-val val info)
  (struct-copy opt/info info [val val]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  stronger helper functions
;;

;; new-stronger-var : identifier (identifier identifier -> exp) -> stronger-rib
;; the second identifier should be bound (in a lift) to an expression whose value has to be saved. 
;; The ids passed to cogen are expected to be bound to two contracts' values of that expression, when
;; those contracts are being compared for strongerness
(define (new-stronger-var id cogen)
  (with-syntax ([(var-this var-that) (generate-temporaries (list id id))])
    (make-stronger-rib (syntax var-this)
                       (syntax var-that)
                       id
                       (cogen (syntax var-this)
                              (syntax var-that)))))

(define empty-stronger '())

(define-struct stronger-rib (this-var that-var save-id stronger-exp))

(provide new-stronger-var 
         (struct-out stronger-rib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lifting helper functions
;;
(provide lift/binding lift/effect empty-lifts bind-lifts bind-superlifts lifts-to-save)

;; lift/binding : syntax[expression] identifier lifts -> (values syntax lifts)
;; adds a new id to `lifts' that is bound to `e'. Returns the
;; variable that was bound
;; values that are lifted are also saved in the wrapper to make sure that the rhs's are evaluated at the right time.
(define (lift/binding e id-hint lifts)
  (syntax-case e ()
    [x
     (or (identifier? e)
         (number? (syntax-e e))
         (boolean? (syntax-e e)))
     (values e lifts)]
    [else
     (let ([x (car (generate-temporaries (list id-hint)))])
       (values x
               (snoc (cons x e) lifts)))]))

;; lift/effect : syntax[expression] lifts -> lifts
;; adds a new lift to `lifts' that is evaluated for effect. no variable returned
(define (lift/effect e lifts)
  (let ([x (car (generate-temporaries '(lift/effect)))])
    (snoc (cons #f e) lifts)))

(define (snoc x l) (append l (list x)))

;; empty-lifts : lifts
;; the initial lifts
(define empty-lifts '())

(define (bind-lifts lifts stx) (do-bind-lifts lifts stx #'let*))
(define (bind-superlifts lifts stx) (do-bind-lifts lifts stx #'letrec))

(define (do-bind-lifts lifts stx binding-form)
  (if (null? lifts)
      stx
      (with-syntax ([((lifts-x . lift-e) ...) lifts])
        (with-syntax ([(lifts-x ...) (map (λ (x) (if (identifier? x) x (car (generate-temporaries '(junk)))))
                                          (syntax->list (syntax (lifts-x ...))))]
                      [binding-form binding-form])
          #`(binding-form ([lifts-x lift-e] ...)
                          #,stx)))))

(define (lifts-to-save lifts) (filter values (map car lifts)))

;;
;; opt/unknown : opt/i id id syntax
;;
(define (opt/unknown opt/i opt/info uctc)
  (let* ((lift-var (car (generate-temporaries (syntax (lift)))))
         (partial-var (car (generate-temporaries (syntax (partial)))))
         (partial-flat-var (car (generate-temporaries (syntax (partial-flat))))))
    (values
     (with-syntax ((partial-var partial-var)
                   (lift-var lift-var)
                   (uctc uctc)
                   (val (opt/info-val opt/info)))
       (syntax (partial-var val)))
     (list (cons lift-var 
                 ;; FIXME needs to get the contract name somehow
                 (with-syntax ((uctc uctc))
                   (syntax (coerce-contract 'opt/c uctc)))))
     null
     (list (cons
            partial-var
            (with-syntax ((lift-var lift-var)
                          (blame (opt/info-blame opt/info)))
              (syntax ((contract-projection lift-var) blame))))
           (cons
            partial-flat-var
            (with-syntax ((lift-var lift-var))
              (syntax (if (flat-contract? lift-var)
                          (flat-contract-predicate lift-var)
                          (lambda (x) (error 'opt/unknown "flat called on an unknown that had no flat pred ~s ~s"
                                             lift-var
                                             x)))))))
     (with-syntax ([val (opt/info-val opt/info)]
                   [partial-flat-var partial-flat-var])
       #'(partial-flat-var val))
     lift-var
     null
     #f)))
