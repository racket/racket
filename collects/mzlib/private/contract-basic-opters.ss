(module contract-basic-opters mzscheme
  (require "contract-guts.ss"
           "contract-opt.ss")
  (require-for-syntax "contract-opt-guts.ss")
  
  ;;
  ;; opt/pred helper
  ;;
  (define-for-syntax (opt/pred pos pred)
    (let* ((lift-vars (generate-temporaries (syntax (pred))))
           (lift-pred-var (car lift-vars)))
      (with-syntax ((lift-pred lift-pred-var))
        (values
         (with-syntax ((pos pos))
           (syntax (if (lift-pred val)
                       val
                       (raise-contract-error
                        val
                        src-info
                        pos
                        orig-str
                        "expected <~a>, given: ~e"
                        ((name-get ctc) ctc)
                        val))))
         (list (cons lift-pred-var pred))
         null
         (syntax (lift-pred val))
         #f))))
  
  ;;
  ;; built-in predicate opters
  ;;
  (define/opter (null? opt/i pos neg stx)
    (syntax-case stx (null?)
      [null? (opt/pred pos #'null?)]))
  (define/opter (boolean? opt/i pos neg stx)
    (syntax-case stx (boolean?)
      [boolean? (opt/pred pos #'boolean?)]))
  (define/opter (integer? opt/i pos neg stx)
    (syntax-case stx (integer?)
      [integer? (opt/pred pos #'integer?)]))
  (define/opter (char? opt/i pos neg stx)
    (syntax-case stx (char?)
      [char? (opt/pred pos #'char?)]))
  (define/opter (number? opt/i pos neg stx)
    (syntax-case stx (number?)
      [number? (opt/pred pos #'number?)]))
  (define/opter (pair? opt/i pos neg stx)
    (syntax-case stx (pair?)
      [pair? (opt/pred pos #'pair?)]))
  
  ;;
  ;; any/c
  ;;
  (define/opter (any/c opt/i pos neg stx)
    (syntax-case stx (any/c)
      [any/c (values
              #'val
              null
              null
              #'#t
              #f)]))

  ;;
  ;; flat-contract helper
  ;;
  (define-for-syntax (opt/flat-ctc pos pred checker)
    (syntax-case pred (null? number? integer? boolean? pair?)
      ;; Better way of doing this?
      [null? (opt/pred pos pred)]
      [number? (opt/pred pos pred)]
      [integer? (opt/pred pos pred)]
      [boolean? (opt/pred pos pred)]
      [pair? (opt/pred pos pred)]
      [pred
       (let* ((lift-vars (generate-temporaries (syntax (pred error-check))))
              (lift-pred (car lift-vars)))
         (with-syntax ((pos pos)
                       (lift-pred lift-pred))
           (values
            (syntax (if (lift-pred val)
                        val
                        (raise-contract-error
                         val
                         src-info
                         pos
                         orig-str
                         "expected <~a>, given: ~e"
                         ((name-get ctc) ctc)
                         val)))
            (interleave-lifts
             lift-vars
             (list #'pred (cond [(eq? checker 'check-flat-contract) #'(check-flat-contract lift-pred)]
                                [(eq? checker 'check-flat-named-contract) #'(check-flat-named-contract lift-pred)])))
            null
            (syntax (lift-pred val))
            #f)))]))
  
  ;;
  ;; flat-contract and friends
  ;;
  (define/opter (flat-contract opt/i pos neg stx)
    (syntax-case stx (flat-contract)
      [(flat-contract pred) (opt/flat-ctc pos #'pred 'check-flat-contract)]))
  (define/opter (flat-named-contract opt/i pos neg stx)
    (syntax-case stx (flat-named-contract)
      [(flat-named-contract name pred) (opt/flat-ctc pos #'pred 'check-flat-named-contract)]))
  
  ;;
  ;; unknown
  ;;
  ;; BUGS: currently, opt/c reports error on something like
  ;;       (opt/c (or/c (begin (print "side effect") number?) boolean?))
  ;;       because the begin sequence is unrecognized, and we have no idea of
  ;;       knowing that `number?' is a pred that we can opt.
  ;; WORKAROUND: wrap `flat-contract' around the pred, it optimizes to the same
  ;;             thing.
  ;;
  (define/opter (unknown opt/i pos neg stx)
    (define (opt/unknown-ctc uctc)
      (let* ((lift-vars (generate-temporaries (syntax (lift error-check))))
             (lift-var (car lift-vars))
             (partial-var (car (generate-temporaries (syntax (partial))))))
        (values
         (with-syntax ((partial-var partial-var)
                       (lift-var lift-var)
                       (uctc uctc))
           (syntax (partial-var val)))
         (interleave-lifts
          lift-vars
          (list uctc
                (with-syntax ((lift-var lift-var))
                  (syntax
                   (unless (contract? lift-var)
                     (error 'contract "expected contract, given ~e" lift-var))))))
         (list (cons
                partial-var
                (with-syntax ((lift-var lift-var)
                              (pos pos)
                              (neg neg))
                  (syntax (((proj-get lift-var) lift-var) pos neg src-info orig-str)))))
         #f
         lift-var)))
    
    (syntax-case stx ()
      [ctc
       (opt/unknown-ctc #'ctc)])))