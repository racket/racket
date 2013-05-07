#lang racket/base

(require (for-syntax "unit-syntax.rkt" racket/base))
(provide define-syntax/err-param
         (rename-out [make-a-unit make-unit]) unit-import-sigs unit-export-sigs unit-go unit? unit-deps
         check-unit check-no-imports check-sigs check-deps check-helper)
  
(define-syntax define-syntax/err-param
  (syntax-rules ()
    ((_ (name arg) body)
     (define-syntax (name arg)
       (parameterize ((error-syntax arg))
         body)))))

;; for named structures
(define insp (current-inspector))

;; (make-unit (listof (cons symbol symbol)) (listof (cons symbol symbol)) (listof nat) thunk)
;; Runtime representation of a unit
(define-struct unit (import-sigs export-sigs deps go))

;; For units with inferred names, generate a struct that prints using the name:
(define (make-naming-constructor type name)
  (let-values ([(struct: make- ? -accessor -mutator)
                (make-struct-type name type 0 0 #f null insp)])
    make-))

;; Make a unit value (call by the macro expansion of `unit')
(define (make-a-unit name num-imports exports deps go)
  ((if name 
       (make-naming-constructor 
        struct:unit
        (string->symbol (format "unit:~a" name)))
       make-unit)
   num-imports exports deps go))

;; check-unit : X symbol -> 
;; ensure that u is a unit value
(define (check-unit u name)
  (unless (unit? u)
    (raise
     (make-exn:fail:contract
      (format "~a: result of unit expression was not a unit: ~e" name u)
      (current-continuation-marks)))))

;; check-helper : (vectorof (cons symbol (vectorof (cons symbol symbol)))))
;                 (vectorof (cons symbol (vectorof (cons symbol symbol)))))
;;                symbol symbol -> 
;; ensure that the unit's signatures match the expected signatures.
(define (check-helper sub-sig super-sig name import?)
  (define t (make-hash))
  (let loop ([i (sub1 (vector-length sub-sig))])
    (when (>= i 0)
      (let ([v (cdr (vector-ref sub-sig i))])
        (let loop ([j (sub1 (vector-length v))])
          (when (>= j 0)
            (let ([vj (vector-ref v j)])
              (hash-set! t vj
                         (if (hash-ref t vj #f)
                             'amb
                             #t)))
            (loop (sub1 j)))))
      (loop (sub1 i))))
  (let loop ([i (sub1 (vector-length super-sig))])
    (when (>= i 0)
      (let* ([v0 (vector-ref (cdr (vector-ref super-sig i)) 0)]
             [r (hash-ref t v0 #f)])
        (when (or (eq? r 'amb) (not r))
          (let ([tag (if (pair? v0) (car v0) #f)]
                [sub-name (car (vector-ref super-sig i))]
                [err-str (if r
                             "supplies multiple times"
                             "does not supply")])
            (raise
             (make-exn:fail:contract
              (cond
                [(and import? tag)
                 (format "~a: unit argument expects an import for tag ~a with signature ~a, which this usage context ~a"
                         name
                         tag
                         sub-name
                         err-str)]
                [import?
                 (format "~a: unit argument expects an untagged import with signature ~a, which this usage context ~a"
                         name
                         sub-name
                         err-str)]
                [tag
                 (format "~a: this usage context expects a unit with an export for tag ~a with signature ~a, which the given unit ~a"
                         name
                         tag
                         sub-name
                         err-str)]
                [else
                 (format "~a: this usage context expects a unit with an untagged export with signature ~a, which the given unit ~a"
                         name
                         sub-name
                         err-str)])
              (current-continuation-marks))))))
      (loop (sub1 i)))))

;; check-deps : (hash-tableof (cons symbol (or symbol #f)) (cons symbol symbol)) unit symbol ->
;; The hash table keys are the tag and runtime signature id
;; The values are the name of the signature and the linkage
(define (check-deps dep-table unit name)
  (for-each
   (λ (dep)
     (let ([r (hash-ref dep-table dep #f)])
       (when r
         (raise
          (make-exn:fail:contract
           (if (car dep)
               (format "~a: initialization dependent signature ~a with tag ~a is supplied from a later unit with link ~a"
                       name (car r) (car dep) (cdr r))
               (format "~a: untagged initialization dependent signature ~a is supplied from a later unit with link ~a"
                       name (car r) (cdr r)))
           (current-continuation-marks))))))
   (unit-deps unit)))

;; check-no-imports : unit symbol ->
;; ensures that the unit has no imports
(define (check-no-imports unit name)
  (check-helper (vector) (unit-import-sigs unit) name #t))

;; check-sigs : unit
;;              (vectorof (cons symbol (vectorof (cons symbol symbol)))))
;;              (vectorof (cons symbol (vectorof (cons symbol symbol)))))
;;              symbol ->
;; ensures that unit has the given signatures
(define (check-sigs unit expected-imports expected-exports name)
  (check-helper expected-imports (unit-import-sigs unit) name #t)
  (check-helper (unit-export-sigs unit) expected-exports name #f))
