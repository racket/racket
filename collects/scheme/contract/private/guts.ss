#lang scheme/base

(require "helpers.ss"
         scheme/pretty)

(require (for-syntax scheme/base
                     "helpers.ss"))

(provide raise-contract-error
         guilty-party
         exn:fail:contract2?
         exn:fail:contract2-srclocs
                
         contract-violation->string
         
         coerce-contract
         coerce-contracts
         coerce-flat-contract
         coerce-flat-contracts
         coerce-contract/f
         
         flat-contract?
         flat-contract
         flat-contract-predicate
         flat-named-contract
         build-flat-contract
         
         build-compound-type-name
         
         and/c
         any/c
         none/c
         make-none/c 
         
         contract?
         contract-name
         contract-proc
         make-proj-contract
         
         contract-stronger?
         
         contract-first-order-passes?
         
         proj-prop proj-pred? proj-get
         name-prop name-pred? name-get
         stronger-prop stronger-pred? stronger-get
         flat-prop flat-pred? flat-get
         flat-proj
         first-order-prop
         first-order-get
         
         ;; for opters
         check-flat-contract
         check-flat-named-contract
         any)

(define-syntax (any stx)
  (raise-syntax-error 'any "use of 'any' outside of an arrow contract" stx))

(define-values (proj-prop proj-pred? raw-proj-get) 
  (make-struct-type-property 'contract-projection))
(define-values (name-prop name-pred? name-get)
  (make-struct-type-property 'contract-name))
(define-values (stronger-prop stronger-pred? stronger-get)
  (make-struct-type-property 'contract-stronger-than))
(define-values (flat-prop flat-pred? flat-get)
  (make-struct-type-property 'contract-flat))

(define-values (first-order-prop first-order-pred? raw-first-order-get)
  (make-struct-type-property 'contract-first-order))

(define (first-order-get stct)
  (cond
    [(flat-pred? stct) (flat-get stct)]
    [else (raw-first-order-get stct)]))

(define (contract-first-order-passes? c v)
  (let ([ctc (coerce-contract 'contract-first-order-passes? c)])
    (cond
      [(first-order-pred? ctc) (((first-order-get ctc) ctc) v)]
      [(flat-pred? c) (((flat-get c) c) v)]
      [else #t])))

(define (proj-get ctc)
  (cond
    [(proj-pred? ctc)
     (raw-proj-get ctc)]
    [else (error 'proj-get "unknown ~e" ctc)]))

;; contract-stronger? : contract contract -> boolean
;; indicates if one contract is stronger (ie, likes fewer values) than another
;; this is not a total order.
(define (contract-stronger? a b)
  (let ([a-ctc (coerce-contract 'contract-stronger? a)]
        [b-ctc (coerce-contract 'contract-stronger? b)])
    ((stronger-get a-ctc) a-ctc b-ctc)))

;; coerce-flat-contract : symbol any/c -> contract
(define (coerce-flat-contract name x)
  (let ([ctc (coerce-contract/f x)])
    (unless (flat-pred? ctc)
      (error name 
             "expected a flat contract or a value that can be coerced into one, got ~e"
             x))
    ctc))

;; coerce-flat-contacts : symbol (listof any/c) -> (listof flat-contract)
;; like coerce-contracts, but insists on flat-contracts
(define (coerce-flat-contracts name xs) 
  (let loop ([xs xs]
             [i 1])
    (cond
      [(null? xs) '()]
      [else
       (let ([fst (coerce-contract/f (car xs))])
         (unless (flat-pred? fst)
           (error name 
                  "expected all of the arguments to be flat contracts, but argument ~a was not, got ~e" 
                  i
                  (car xs)))
         (cons fst (loop (cdr xs) (+ i 1))))])))

;; coerce-contract : symbol any/c -> contract
(define (coerce-contract name x)
  (or (coerce-contract/f x)
      (error name 
             "expected contract or a value that can be coerced into one, got ~e"
             x)))

;; coerce-contracts : symbols (listof any) -> (listof contract)
;; turns all of the arguments in 'xs' into contracts
;; the error messages assume that the function named by 'name'
;; got 'xs' as it argument directly
(define (coerce-contracts name xs) 
  (let loop ([xs xs]
             [i 1])
    (cond
      [(null? xs) '()]
      [(coerce-contract/f (car xs)) => (λ (x) (cons x (loop (cdr xs) (+ i 1))))]
      [else
       (error name 
              "expected all of the arguments to be contracts, but argument ~a was not, got ~e" 
              i
              (car xs))])))

;; coerce-contract/f : any -> (or/c #f contract?)
;; returns #f if the argument could not be coerced to a contract
(define (coerce-contract/f x)
  (cond
    [(proj-pred? x) x]
    [(and (procedure? x) (procedure-arity-includes? x 1)) 
     (make-predicate-contract (or (object-name x) '???) x)]
    [(or (symbol? x) (boolean? x) (char? x) (null? x)) (make-eq-contract x)]
    [(or (bytes? x) (string? x)) (make-equal-contract x)]
    [(number? x) (make-=-contract x)]
    [(or (regexp? x) (byte-regexp? x)) (make-regexp/c x)]
    [else #f]))

(define-values (make-exn:fail:contract2 
                exn:fail:contract2?
                exn:fail:contract2-srclocs
                guilty-party)
  (let-values ([(exn:fail:contract2 
                 make-exn:fail:contract2 
                 exn:fail:contract2?
                 get
                 set)
                (parameterize ([current-inspector (make-inspector)])
                  (make-struct-type 'exn:fail:contract2
                                    struct:exn:fail:contract
                                    2
                                    0
                                    #f
                                    (list (cons prop:exn:srclocs
                                                (lambda (x)
                                                  (exn:fail:contract2-srclocs x))))))])
    (values
     make-exn:fail:contract2 
     exn:fail:contract2?
     (lambda (x) (get x 0))
     (lambda (x) (get x 1)))))

(define (default-contract-violation->string val src-info to-blame contract-sexp msg)
  (let ([blame-src (src-info-as-string src-info)]
        [formatted-contract-sexp
         (let ([one-line 
                (let ([sp (open-output-string)])
                  (parameterize ([pretty-print-columns 'infinity])
                    (pretty-print contract-sexp sp)
                    (get-output-string sp)))])
           (if (< (string-length one-line) 30)
               one-line
               (let ([sp (open-output-string)])
                 (newline sp)
                 (parameterize ([pretty-print-print-line print-contract-liner]
                                [pretty-print-columns 50])
                   (pretty-print contract-sexp sp))
                 (get-output-string sp))))]
        [specific-blame
         (cond
           [(syntax? src-info)
            (let ([datum (syntax->datum src-info)])
              (if (symbol? datum)
                  (format " on ~a" datum)
                  ""))]
           [(pair? src-info)
            (format " on ~a" (list-ref src-info 1))]
           [else ""])])
    (string-append (format "~a~a broke the contract ~a~a; "
                           blame-src
                           (cond
                             [(not to-blame) "<<unknown>>"]
                             [(and (pair? to-blame)
                                   (pair? (cdr to-blame))
                                   (null? (cddr to-blame))
                                   (equal? 'quote (car to-blame)))
                              (format "'~s" (cadr to-blame))]
                             [else (format "~s" to-blame)])
                           formatted-contract-sexp
                           specific-blame)
                   msg)))

(define contract-violation->string (make-parameter default-contract-violation->string))

(define (raise-contract-error val src-info blame contract-sexp fmt . args)
  (let ([blame (unpack-blame blame)])
    (raise
     (make-exn:fail:contract2
      (string->immutable-string
       ((contract-violation->string) val 
                                     src-info 
                                     blame
                                     contract-sexp 
                                     (apply format fmt args)))
      (current-continuation-marks)
      (cond
        [(syntax? src-info)
         (list (make-srcloc 
                (syntax-source src-info)
                (syntax-line src-info)
                (syntax-column src-info)
                (syntax-position src-info)
                (syntax-span src-info)))]
        [(srcloc? src-info) (list src-info)]
        [else '()])
      (unpack-blame blame)))))

(define print-contract-liner
  (let ([default (pretty-print-print-line)])
    (λ (line port ol cols)
      (+ (default line port ol cols)
         (if line
             (begin (display "  " port)
                    2)
             0)))))

;; src-info-as-string : (union srcloc syntax #f) -> string
(define (src-info-as-string src-info)
  (if (or (syntax? src-info)
          (srcloc? src-info))
      (let ([src-loc-str (build-src-loc-string src-info)])
        (if src-loc-str
            (string-append src-loc-str  ": ")
            ""))
      ""))
       
;                                                      
;                                                      
;                                                      
;                                                      
;                                                      
;                          ;                       ;   
;    ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
;   ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
;  ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
;  ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
;  ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
;   ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
;    ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
;                                                      
;                                                      
;                                                      

;; contract = (make-contract sexp
;;                           (sym
;;                            sym
;;                            (union syntax #f)
;;                            string
;;                            ->
;;                            (alpha -> alpha)))
;; the first arg to make-contract builds the name of the contract. The
;; path records how the violation occurs
;;
;; generic contract container; 
;; the first arg to proc is a symbol representing the name of the positive blame
;; the second arg to proc is the symbol representing the name of the negative blame
;; the third argument to proc is the src-info.
;; the fourth argumet is a textual representation of the original contract
;;
;; the argument to the result function is the value to test.
;; (the result function is the projection)
;;  

(define (flat-proj ctc)
  (let ([pred? ((flat-get ctc) ctc)])
    (λ (pos neg src-info orig-str positive-position?)
      (λ (val)
        (if (pred? val)
            val
            (raise-contract-error
             val
             src-info
             pos
             orig-str
             "expected <~a>, given: ~e"
             ((name-get ctc) ctc)
             val))))))

(define (double-any-curried-proj ctc) double-any-curred-proj2)
(define (double-any-curred-proj2 pos-blame neg-blame src-info orig-str positive-position?) values)


(define-values (make-proj-contract)
  (let ()
    (define-struct proj-contract (the-name proj first-order-proc)
      #:property proj-prop
      (λ (ctc) 
        (let ([raw-proj (proj-contract-proj ctc)])
          (if (procedure-arity-includes? raw-proj 5)
              raw-proj
              (λ (pos neg src-info name positive-position?)
                (raw-proj pos neg src-info name)))))
      
      #:property name-prop
      (λ (ctc) (proj-contract-the-name ctc))
      
      #:property first-order-prop
      (λ (ctc) (or (proj-contract-first-order-proc ctc)
                   (λ (x) #t)))
      #:property stronger-prop
      (λ (this that) 
        (and (proj-contract? that)
             (procedure-closure-contents-eq?
              (proj-contract-proj this)
              (proj-contract-proj that)))))
    
    (values make-proj-contract)))

(define (flat-contract-predicate x) 
  (let ([ctc (coerce-flat-contract 'flat-contract-predicate x)])
    ((flat-get ctc) ctc)))

(define (flat-contract? x) 
  (let ([c (coerce-contract/f x)])
    (and c
         (flat-pred? c))))

(define (contract-name ctc)
  (let ([ctc (coerce-contract 'contract-name ctc)])
    ((name-get ctc) ctc)))

(define (contract? x) (and (coerce-contract/f x) #t))
(define (contract-proc ctc) ((proj-get ctc) ctc))

(define (check-flat-contract predicate) (coerce-flat-contract 'flat-contract predicate))
(define (flat-contract predicate) (coerce-flat-contract 'flat-contract predicate))
(define (check-flat-named-contract predicate) (coerce-flat-contract 'flat-named-contract predicate))
(define (flat-named-contract name predicate)
  (cond
    [(and (procedure? predicate)
          (procedure-arity-includes? predicate 1))
     (make-predicate-contract name predicate)]
    [(flat-contract? predicate)
     (make-predicate-contract name (flat-contract-predicate predicate))]
    [else
     (error 'flat-named-contract 
            "expected a flat contract or procedure of arity 1 as second argument, got ~e" 
            predicate)]))

;; build-compound-type-name : (union contract symbol) ... -> (-> sexp)
(define (build-compound-type-name . fs)
  (let loop ([subs fs])
    (cond
      [(null? subs)
       '()]
      [else (let ([sub (car subs)])
              (cond
                [(name-pred? sub)
                 (let ([mk-sub-name (contract-name sub)])
                   `(,mk-sub-name ,@(loop (cdr subs))))]
                [else `(,sub ,@(loop (cdr subs)))]))])))

(define (and-proj ctc)
  (let ([mk-pos-projs (map (λ (x) ((proj-get x) x)) (and/c-ctcs ctc))])
    (lambda (pos neg src-info orig-str positive-position?)
      (let ([projs (map (λ (c) (c pos neg src-info orig-str positive-position?)) mk-pos-projs)])
        (let loop ([projs (cdr projs)]
                   [proj (car projs)])
          (cond
            [(null? projs) proj]
            [else (loop (cdr projs)
                        (let ([f (car projs)])
                          (λ (v) (proj (f v)))))]))))))


(define-struct and/c (ctcs)
  #:omit-define-syntaxes
  #:property proj-prop and-proj
  #:property name-prop (λ (ctc) (apply build-compound-type-name 'and/c (and/c-ctcs ctc)))
  #:property first-order-prop
  (λ (ctc)
    (let ([tests (map (λ (x) ((first-order-get x) x))
                      (and/c-ctcs ctc))])
      (λ (x) 
        (andmap (λ (f) (f x)) tests))))
  #:property stronger-prop
  (λ (this that)
    (and (and/c? that)
         (let ([this-ctcs (and/c-ctcs this)]
               [that-ctcs (and/c-ctcs that)])
           (and (= (length this-ctcs) (length that-ctcs))
                (andmap contract-stronger?
                        this-ctcs
                        that-ctcs))))))

(define (and/c . raw-fs)
  (let ([contracts (coerce-contracts 'and/c raw-fs)])
    (cond
      [(null? contracts) any/c]
      [(andmap flat-contract? contracts)
       (let* ([pred
               (let loop ([pred (flat-contract-predicate (car contracts))]
                          [preds (cdr contracts)])
                 (cond
                   [(null? preds) pred]
                   [else
                    (let* ([fst (flat-contract-predicate (car preds))])
                      (loop (let ([and/c-contract? (lambda (x) (and (pred x) (fst x)))])
                              and/c-contract?)
                            (cdr preds)))]))])
         (flat-named-contract (apply build-compound-type-name 'and/c contracts) pred))]
      [else (make-and/c contracts)])))

(define-struct any/c ()
  #:omit-define-syntaxes
  #:property proj-prop double-any-curried-proj
  #:property stronger-prop (λ (this that) (any/c? that))
  #:property name-prop (λ (ctc) 'any/c)
  #:property first-order-prop (λ (ctc) (λ (val) #t))
  #:property flat-prop (λ (ctc) (λ (x) #t)))

(define any/c (make-any/c))

(define (none-curried-proj ctc)
  (λ (pos-blame neg-blame src-info orig-str positive-position?) 
    (λ (val) 
      (raise-contract-error
       val
       src-info
       pos-blame
       orig-str
       "~s accepts no values, given: ~e"
       (none/c-name ctc)
       val))))

(define-struct none/c (name)
  #:omit-define-syntaxes
  #:property proj-prop none-curried-proj
  #:property stronger-prop (λ (this that) #t)
  #:property name-prop (λ (ctc) (none/c-name ctc))
  #:property first-order-prop (λ (ctc) (λ (val) #f))
  #:property flat-prop (λ (ctc) (λ (x) #f)))

(define none/c (make-none/c 'none/c))




;                                                                                                                 
;                                                                                                                 
;                                                                                                                 
;                                                                                                                 
;            ;                      ;;;                                       ;                         ;         
;          ;;;                                                              ;;;                       ;;;         
;   ;;;;; ;;;;;   ;;;   ;;; ;; ;;;  ;;;   ;;;         ;;;     ;;;   ;;; ;; ;;;;; ;;; ;;;;;;;    ;;;  ;;;;;  ;;;;  
;  ;;;;;;;;;;;;  ;;;;;  ;;;;;;;;;;; ;;;  ;;;;;       ;;;;;   ;;;;;  ;;;;;;;;;;;; ;;;;;;;;;;;;  ;;;;; ;;;;; ;;; ;; 
;  ;;  ;;; ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;     ;;;  ;; ;;; ;;; ;;; ;;; ;;;  ;;;  ;;  ;;; ;;;  ;; ;;;  ;;;    
;    ;;;;; ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;         ;;;     ;;; ;;; ;;; ;;; ;;;  ;;;    ;;;;; ;;;     ;;;   ;;;;  
;  ;;; ;;; ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;     ;;;  ;; ;;; ;;; ;;; ;;; ;;;  ;;;  ;;; ;;; ;;;  ;; ;;;     ;;; 
;  ;;; ;;; ;;;;  ;;;;;  ;;; ;;; ;;; ;;;  ;;;;;       ;;;;;   ;;;;;  ;;; ;;; ;;;; ;;;  ;;; ;;;  ;;;;;  ;;;; ;; ;;; 
;   ;;;;;;  ;;;   ;;;   ;;; ;;; ;;; ;;;   ;;;         ;;;     ;;;   ;;; ;;;  ;;; ;;;   ;;;;;;   ;;;    ;;;  ;;;;  
;                                                                                                                 
;                                                                                                                 
;                                                                                                                 
;                                                                                                                 

(define-struct eq-contract (val)
  #:property proj-prop flat-proj
  #:property flat-prop (λ (ctc) (λ (x) (eq? (eq-contract-val ctc) x)))
  #:property name-prop (λ (ctc) 
                         (if (symbol? (eq-contract-val ctc))
                             `',(eq-contract-val ctc)
                             (eq-contract-val ctc)))
  #:property stronger-prop (λ (this that) (and (eq-contract? that) (eq? (eq-contract-val this) (eq-contract-val that)))))

(define-struct equal-contract (val)
  #:property proj-prop flat-proj
  #:property flat-prop (λ (ctc) (λ (x) (equal? (equal-contract-val ctc) x)))
  #:property name-prop (λ (ctc) (equal-contract-val ctc))
  #:property stronger-prop (λ (this that) (and (equal-contract? that) (equal? (equal-contract-val this) (equal-contract-val that)))))

(define-struct =-contract (val)
  #:property proj-prop flat-proj
  #:property flat-prop (λ (ctc) (λ (x) (and (number? x) (= (=-contract-val ctc) x))))
  #:property name-prop (λ (ctc) (=-contract-val ctc))
  #:property stronger-prop (λ (this that) (and (=-contract? that) (= (=-contract-val this) (=-contract-val that)))))

(define-struct regexp/c (reg)
  #:property proj-prop flat-proj
  #:property flat-prop (λ (ctc) (λ (x) (and (or (string? x) (bytes? x))
                                            (regexp-match (regexp/c-reg ctc) x)
                                            #t)))
  #:property name-prop (λ (ctc) (regexp/c-reg ctc))
  #:property stronger-prop (λ (this that) (and (regexp/c? that) (eq? (regexp/c-reg this) (regexp/c-reg that)))))


(define-struct predicate-contract (name pred)
  #:property proj-prop flat-proj
  #:property stronger-prop
  (λ (this that) 
    (and (predicate-contract? that)
         (procedure-closure-contents-eq? (predicate-contract-pred this)
                                         (predicate-contract-pred that))))
  #:property name-prop (λ (ctc) (predicate-contract-name ctc))
  #:property flat-prop (λ (ctc) (predicate-contract-pred ctc)))

(define (build-flat-contract name pred) (make-predicate-contract name pred))
