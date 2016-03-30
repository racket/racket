#lang racket/base

;; Common utilities used by the various arrow combinators.

(require "blame.rkt"
         "guts.rkt"
         "prop.rkt"
         racket/stxparam)
(require (for-syntax racket/base))

(provide blame-add-range-context
         blame-add-nth-arg-context
         check-procedure check-procedure/more
         procedure-accepts-and-more?
         keywords-match
         (for-syntax check-tail-contract)
         matches-arity-exactly?
         raise-no-keywords-arg
         unspecified-dom
         get-blame-party-info
         tail-contract-key tail-marks-match?
         bad-number-of-results
         the-unsupplied-arg unsupplied-arg?
         values/drop)

(define-struct unsupplied-arg ())
(define the-unsupplied-arg (make-unsupplied-arg))

(define (blame-add-range-context blame)
  (blame-add-context blame "the range of"))

(define tail-contract-key (gensym 'tail-contract-key))

(define-for-syntax (check-tail-contract rng-ctcs blame-party-info neg-party rng-checkers call-gen blame+neg-party)
  (unless (identifier? rng-ctcs)
    (raise-argument-error 'check-tail-contract
                          "identifier?"
                          0
                          rng-ctcs rng-checkers call-gen))
  #`(call-with-immediate-continuation-mark
     tail-contract-key
     (λ (m)
       (if (tail-marks-match? m #,rng-ctcs #,blame-party-info #,neg-party #,blame+neg-party)
           #,(call-gen #'())
           #,(call-gen rng-checkers)))))

;; matches-arity-exactly? : procedure number (or/c number #f) (listof keyword?) (listof keyword?) -> boolean
(define (matches-arity-exactly? val min-arity max-arity contract-req-kwds contract-opt-kwds)
  (define proc-arity (procedure-arity val)) 
  (and (let-values ([(vr va) (procedure-keywords val)])
         (and va (equal? vr contract-req-kwds) 
	         (keywords-match? va contract-req-kwds contract-opt-kwds)))
       (cond
         [(number? proc-arity) (and (number? max-arity)
                                    (= min-arity max-arity)
                                    (= proc-arity min-arity))]
         [(arity-at-least? proc-arity) (and (not max-arity)
                                            (= (arity-at-least-value proc-arity)
                                               min-arity))]
         [else
          (let loop ([arity proc-arity]
                     [i min-arity])
            (cond
              [(null? arity) 
               (= i (+ max-arity 1))]
              [else
               (let ([fst (car arity)])
                 (if (arity-at-least? fst)
                     (and (number? max-arity)
                          (= (arity-at-least-value fst)
                             max-arity))
                     (and (= i fst)
                          (loop (cdr arity)
                                (+ i 1)))))]))])))

(define (keywords-match? accepted-keywords contract-req-kwds contract-opt-kwds)
  (let loop ([accepted accepted-keywords]
             [req-kwds contract-req-kwds]
             [opt-kwds contract-opt-kwds])
    (cond
      [(null? accepted) (and (null? opt-kwds) (null? req-kwds))]
      [else
       (let ([kwd (car accepted)])
         (cond
          [(and (pair? req-kwds) (eq? (car req-kwds) kwd))
           (loop (cdr accepted) (cdr req-kwds) opt-kwds)]
          [(and (pair? opt-kwds) (eq? (car opt-kwds) kwd))
           (loop (cdr accepted) req-kwds (cdr opt-kwds))]
          [else #f]))])))

(define (raise-no-keywords-arg blame #:missing-party [missing-party #f] val given-kwds)
  (raise-blame-error (blame-swap blame) val
                     #:missing-party missing-party
                     (list 'expected:
                           "no keywords"
                           'given:
                           (apply
                            string-append
                            (let loop ([kwds given-kwds])
                              (cond
                                [(null? kwds) '()]
                                [(null? (cdr kwds))
                                 (list "#:" (keyword->string (car kwds)))]
                                [else
                                 (list* "#:"
                                        (keyword->string (car kwds))
                                        " "
                                        (loop (cdr kwds)))]))))))

(define unspecified-dom (gensym 'unspecified-keyword))

;; used as part of the information in the continuation mark
;; that records what is to be checked for a pending contract
(define (get-blame-party-info blame)
  (define swapped? (blame-swapped? blame))
  (list (if swapped? (blame-negative blame) (blame-positive blame))
        swapped?))

;; m : (or/c #f (cons/c neg-party (cons/c (list/c pos-party boolean?[blame-swapped?]) (listof ctc))))
;; rng-ctc : (or/c #f (listof ctc))
;; blame-party-info : (list/c pos-party boolean?[blame-swapped?])
;; neg-party : neg-party
;; blame+neg-party : (cons/c blame? neg-party)
(define (tail-marks-match? m rng-ctcs blame-party-info neg-party blame+neg-party)
  (with-contract-continuation-mark
   blame+neg-party
   (and m
        rng-ctcs
        (eq? (car m) neg-party)
        (let ([mark-blame-part-info (cadr m)])
          (and (eq? (car mark-blame-part-info) (car blame-party-info))
               (eq? (cadr mark-blame-part-info) (cadr blame-party-info))))
        (let loop ([m (cddr m)]
                   [rng-ctcs rng-ctcs])
          (cond
            [(null? m) (null? rng-ctcs)]
            [(null? rng-ctcs) (null? m)]
            [else
             (define m1 (car m))
             (define rng-ctc1 (car rng-ctcs))
             (cond
               [(eq? m1 rng-ctc1) (loop (cdr m) (cdr rng-ctcs))]
               [(contract-struct-stronger? m1 rng-ctc1) (loop (cdr m) (cdr rng-ctcs))]
               [else #f])])))))

;; this is to make the expanded versions a little easier to read
(define-syntax (values/drop stx)
  (syntax-case stx ()
    [(_ arg) #'arg]
    [(_ args ...) #'(values args ...)]))


;                                                                                 
;                                                                                 
;                                                                                 
;                                       ;;;;                      ;;;;            
;                                       ;;;;                      ;;;;            
;  ;;;;;;;  ;;; ;;; ;;; ;;;       ;;;;; ;;;; ;;;    ;;;     ;;;;; ;;;; ;;;  ;;;;; 
;  ;;;;;;;; ;;;;;;; ;;;;;;;      ;;;;;; ;;;;;;;;;  ;;;;;   ;;;;;; ;;;; ;;; ;;;;;; 
;      ;;;; ;;;; ;; ;;;; ;;     ;;;;;;; ;;;; ;;;; ;;;; ;; ;;;;;;; ;;;;;;;  ;;;;   
;   ;;;;;;; ;;;;    ;;;;        ;;;;    ;;;; ;;;; ;;;;;;; ;;;;    ;;;;;;;   ;;;;  
;  ;;  ;;;; ;;;;    ;;;;        ;;;;;;; ;;;; ;;;; ;;;;;   ;;;;;;; ;;;; ;;;   ;;;; 
;  ;;;;;;;; ;;;;    ;;;;         ;;;;;; ;;;; ;;;;  ;;;;;;  ;;;;;; ;;;; ;;; ;;;;;; 
;   ;; ;;;; ;;;;    ;;;;          ;;;;; ;;;; ;;;;   ;;;;    ;;;;; ;;;; ;;; ;;;;;  
;                                                                                 
;                                                                                 
;                                                                                 

;; ----------------------------------------
;; Checks and error functions used in macro expansions

;; procedure-accepts-and-more? : procedure number -> boolean
;; returns #t if val accepts dom-length arguments and
;; any number of arguments more than dom-length. 
;; returns #f otherwise.
(define (procedure-accepts-and-more? val dom-length)
  (let ([arity (procedure-arity val)])
    (cond
      [(number? arity) #f]
      [(arity-at-least? arity)
       (<= (arity-at-least-value arity) dom-length)]
      [else
       (let ([min-at-least (let loop ([ars arity]
                                      [acc #f])
                             (cond
                               [(null? ars) acc]
                               [else (let ([ar (car ars)])
                                       (cond
                                         [(arity-at-least? ar)
                                          (if (and acc
                                                   (< acc (arity-at-least-value ar)))
                                              (loop (cdr ars) acc)
                                              (loop (cdr ars) (arity-at-least-value ar)))]
                                         [(number? ar)
                                          (loop (cdr ars) acc)]))]))])
         (and min-at-least
              (begin
                (let loop ([counts (sort (filter number? arity) >)])
                  (unless (null? counts)
                    (let ([count (car counts)])
                      (cond
                        [(= (+ count 1) min-at-least)
                         (set! min-at-least count)
                         (loop (cdr counts))]
                        [(< count min-at-least)
                         (void)]
                        [else (loop (cdr counts))]))))
                (<= min-at-least dom-length))))])))

(define (get-mandatory-keywords f)
  (let-values ([(mandatory optional) (procedure-keywords f)])
    mandatory))

(define (no-mandatory-keywords? f)
  (let-values ([(mandatory optional) (procedure-keywords f)])
    (null? mandatory)))

;; check-procedure : ... (or/c #f blame) -> (or/c boolean? void?)
;; if blame is #f, then just return a boolean indicating that this matched 
;;   (for use in arity checking)
(define (check-procedure val mtd? dom-length optionals mandatory-kwds optional-keywords
                         blame neg-party)
  (define passes?
    (and (procedure? val)
         (procedure-arity-includes?/optionals val (if mtd? (+ dom-length 1) dom-length) optionals)
         (keywords-match mandatory-kwds optional-keywords val)))
  (cond
    [blame
     (unless passes?
       (raise-blame-error
        blame #:missing-party neg-party
        val
        '(expected " a ~a that accepts ~a~a~a argument~a~a~a" given: "~e")
        (if mtd? "method" "procedure")
        (if (zero? dom-length) "no" dom-length)
        (if (null? optionals) "" " mandatory")
        (if (null? mandatory-kwds) "" " ordinary")
        (if (= 1 dom-length) "" "s")
        (if (zero? optionals) ""
            (format " and up to ~a optional argument~a" optionals (if (= 1 optionals) "" "s")))
        (keyword-error-text mandatory-kwds optional-keywords)
        val))]
    [else
     passes?]))

(define (procedure-arity-includes?/optionals f base optionals)
  (cond
    [(zero? optionals) (procedure-arity-includes? f base #t)]
    [else (and (procedure-arity-includes? f (+ base optionals) #t)
               (procedure-arity-includes?/optionals f base (- optionals 1)))]))

(define (keywords-match mandatory-kwds optional-kwds val)
  (let-values ([(proc-mandatory proc-all) (procedure-keywords val)])
    (and ;; proc accepts all ctc's mandatory keywords
         (or (not proc-all)
             (andmap (λ (kwd) (member kwd proc-all))
                     mandatory-kwds))
         ;; proc's mandatory keywords are still mandatory in ctc
         (andmap (λ (kwd) (member kwd mandatory-kwds))
                 proc-mandatory)
         ;; proc accepts (but does not require) ctc's optional keywords
         ;;
         ;; if proc-all is #f, then proc accepts all keywords and thus
         ;; this is triviably true (e.g. result of make-keyword-procedure)
         (or (not proc-all)
             (andmap (λ (kwd) (and (member kwd proc-all)
                                   (not (member kwd proc-mandatory))))
                     optional-kwds)))))

(define (keyword-error-text mandatory-keywords optional-keywords)
  (define (format-keywords-error type kwds)
    (cond
      [(null? kwds) ""]
      [(null? (cdr kwds))
       (format "the ~a keyword ~a" type (car kwds))]
      [else
       (format
        "the ~a keywords ~a~a"
        type
        (car kwds)
        (apply string-append (map (λ (x) (format " ~a" x)) (cdr kwds))))]))
  (cond
    [(and (null? optional-keywords) (null? mandatory-keywords)) " without any keywords"]
    [(null? optional-keywords)
     (string-append " and " (format-keywords-error 'mandatory mandatory-keywords))]
    [(null? mandatory-keywords)
     (string-append " and " (format-keywords-error 'optional optional-keywords))]
    [else
     (string-append ", "
                    (format-keywords-error 'mandatory mandatory-keywords)
                    ", and "
                    (format-keywords-error 'optional optional-keywords))]))

;; check-procedure/more : ... (or/c #f blame) -> (or/c boolean? void?)
;; if blame is #f, then just return a boolean indicating that this matched 
;;   (for use in arity checking)
(define (check-procedure/more val mtd? dom-length mandatory-kwds optional-kwds blame neg-party)
  (define passes?
    (and (procedure? val)
         (procedure-accepts-and-more? val (if mtd? (+ dom-length 1) dom-length))
         (keywords-match mandatory-kwds optional-kwds val)))
  (cond
    [blame
     (unless passes?
       (raise-blame-error
        blame
        val
        '(expected " a ~a that accepts ~a argument~a and arbitrarily more~a" given: "~e")
        (if mtd? "method" "procedure")
        (cond
          [(zero? dom-length) "no"]
          [else dom-length])
        (if (= 1 dom-length) "" "s")
        (keyword-error-text mandatory-kwds optional-kwds)
        val))]
    [else
     passes?]))

(define (bad-number-of-results blame val rng-len args [case-context #f]
                               #:missing-party [missing-party #f])
  (define num-values (length args))
  (define blame-case (if case-context 
                         (blame-add-context blame (format "the ~a case of" (n->th (+ case-context 1))))
                         blame))
  (raise-blame-error (blame-add-range-context blame-case)
                     #:missing-party missing-party
                     val 
                     "expected ~a value~a, returned ~a value~a"
                     rng-len (if (= rng-len 1) "" "s")
                     num-values (if (= num-values 1) "" "s")))

(define (blame-add-nth-arg-context blame n)
  (blame-add-context blame
                     (format "the ~a argument of" (n->th n))))

;; timing & size tests

#;
(begin
  (require (prefix-in mz: mzlib/contract))
  (define (time-test f)
    (time
     (let loop ([n 2000])
       (unless (zero? n)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (loop (- n 1))))))
  
  (define (size stx)
    (let ([sp (open-output-string)])
      (write (compile stx) sp)
      (close-output-port sp)
      (string-length (get-output-string sp))))
  
  'raw
  (size #'(λ (x) x))
  (time-test (λ (x) x))
  
  '->
  (size #'(-> number? number?))
  (time-test (contract (-> number? number?) (λ (x) x) 'pos 'neg))
  
  'mz:->
  (size #'(mz:-> number? number?))
  (time-test (contract (mz:-> number? number?) (λ (x) x) 'pos 'neg))
  
  '->*
  (size #'(->* (number?) () number?))
  (time-test (contract (->* (number?) () number?) (λ (x) x) 'pos 'neg))
  
  'mz:->*
  (size #'(mz:->* (number?) any/c (number?)))
  (time-test (contract (mz:->* (number?) any/c (number?)) (λ (x . y) x) 'pos 'neg))
  
  'case->
  (size #'(case-> (-> number? number?)))
  (time-test (contract (case-> (-> number? number?)) (λ (x) x) 'pos 'neg))
  
  'mz:case->
  (size #'(mz:case-> (-> number? number?)))
  (time-test (contract (mz:case-> (-> number? number?)) (λ (x) x) 'pos 'neg))
  
  '->d
  (size #'(->d ([x number?]) () [r number?]))
  (time-test (contract (->d ([x number?]) () [r number?]) (λ (x) x) 'pos 'neg))
  
  'mz:->r
  (size #'(mz:->r ([x number?]) number?))
  (time-test (contract (mz:->r ([x number?]) number?) (λ (x) x) 'pos 'neg))
  
  'object-contract
  (size #'(object-contract [m (-> number? number?)]))
  (time-test 
   (let ([o (contract (object-contract [m (-> number? number?)])
                      (new (class object% (define/public (m x) x) (super-new)))
                      'pos
                      'neg)])
     (λ (x) (send o m x))))
  
  
  'mz:object-contract
  (size #'(mz:object-contract [m (mz:-> number? number?)]))
  (time-test 
   (let ([o (contract (mz:object-contract [m (mz:-> number? number?)])
                      (new (class object% (define/public (m x) x) (super-new)))
                      'pos
                      'neg)])
     (λ (x) (send o m x)))))
