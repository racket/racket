(module contract-opt-guts mzscheme
  (require (lib "private/boundmap.ss" "syntax")
           (lib "list.ss"))
  (require-for-template mzscheme)
  
  (provide get-opter reg-opter! opter
           interleave-lifts
           
           make-opt/info
           opt/info-contract
           opt/info-val
           opt/info-pos
           opt/info-neg
           opt/info-src-info
           opt/info-orig-str
           opt/info-free-vars
           opt/info-recf
           opt/info-base-pred
           opt/info-this
           opt/info-that
           
           opt/info-swap-blame
           opt/info-change-val)
  
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
  (define-struct opt/info (contract val pos neg src-info orig-str free-vars recf base-pred this that))
  
  ;; opt/info-swap-blame : opt/info -> opt/info
  ;; swaps pos and neg
  (define (opt/info-swap-blame info)
    (let ((ctc (opt/info-contract info))
          (val (opt/info-val info))
          (pos (opt/info-neg info))
          (neg (opt/info-pos info))
          (src-info (opt/info-src-info info))
          (orig-str (opt/info-orig-str info))
          (free-vars (opt/info-free-vars info))
          (recf (opt/info-recf info))
          (base-pred (opt/info-base-pred info))
          (this (opt/info-this info))
          (that (opt/info-that info)))
      (make-opt/info ctc val pos neg src-info orig-str free-vars recf base-pred this that)))
  
  ;; opt/info-change-val : identifier opt/info -> opt/info
  ;; changes the name of the variable that the value-to-be-contracted is bound to
  (define (opt/info-change-val val info)
    (let ((ctc (opt/info-contract info))
          (pos (opt/info-neg info))
          (neg (opt/info-pos info))
          (src-info (opt/info-src-info info))
          (orig-str (opt/info-orig-str info))
          (free-vars (opt/info-free-vars info))
          (recf (opt/info-recf info))
          (base-pred (opt/info-base-pred info))
          (this (opt/info-this info))
          (that (opt/info-that info)))
      (make-opt/info ctc val neg pos src-info orig-str free-vars recf base-pred this that)))
  
  
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
           (struct stronger-rib (this-var that-var save-id stronger-exp)))
  
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
  
  )