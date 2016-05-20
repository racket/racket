#lang racket/base
(require "test-util.rkt")
(require (for-syntax racket/base
                     (only-in racket/sequence in-syntax)
                     (only-in racket/syntax format-symbol)))

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/list 'racket/undefined
                                               'racket/class 'racket/sequence)])

  (define-syntax (test-cache stx)
    (syntax-case stx ()
      [(_ [expr result] ...)
       (let ()
         (define the-tests
           (for/list ([expr (in-syntax #'(expr ...))]
                      [result (in-syntax #'(result ...))])
             (define test-name
               (format-symbol "can-cache-contract:~a" (syntax-line expr)))
             #`(test/spec-passed/result
                '#,test-name
                '(can-cache-contract? #,expr)
                #,result)))
         #`(begin #,@the-tests))]))

  (test-cache
   [1 #t]
   ['two #t]
   ["three" #t]
   ['#:four #t]
   [5.0 #t]
   [null #t]
   [(λ (x) #t) #f]
   [(λ (x) #t) #f]
   [integer? #t]
   [number? #t]
   [any/c #t]
   [none/c #t]
   ;[empty? #t]
   [(or/c integer? string?) #t]
   [(or/c integer? (λ (x) #t) string?) #f]
   [(first-or/c integer? string?) #t]
   [(first-or/c integer? (λ (x) #t) string?) #f]
   [(or/c) #t]
   [(first-or/c) #t]
   [(and/c) #t]
   [(and/c integer? positive?) #t]
   [(and/c integer? (λ (x) #t) positive?) #f]
   [(not/c integer?) #t]
   [(not/c (λ (x) #f)) #f]
   [(flat-named-contract 'my/c integer?) #t]
   [(flat-named-contract 'my/c (λ (x) #t)) #f]
   [(flat-contract-with-explanation integer? #:can-cache? #t) #t]
   [(flat-contract-with-explanation integer?) #f]
   [(=/c 3) #t]
   [(</c 3) #t]
   [(>/c 3) #t]
   [(<=/c 3) #t]
   [(>=/c 3) #t]
   [(between/c -3 3) #t]
   [(real-in -3.14 2.71) #t]
   [(integer-in -5 7) #t]
   [(char-in #\a #\z) #t]
   [natural-number/c #t]
   [(string-len/c 5) #t]
   [false/c #t]
   [printable/c #t]
   [(one-of/c (void)) #t]
   [(one-of/c 1'three #\f #t null '#:five) #t]
   [(symbols 'one 'two 'three) #t]
   [(vectorof integer?) #t]
   [(vectorof (λ (x) #t)) #f]
   [(vectorof integer? #:immutable #t) #t]
   [(vectorof integer? #:flat? #t) #t]
   [(vectorof integer? #:eager 5) #t]
   [(vectorof integer? #:immutable #t #:flat? #t) #t]
   [(vectorof integer? #:immutable #t #:eager #t) #t]
   [(vectorof integer? #:eager #t #:flat? #t) #t]
   [(vectorof integer? #:immutable #t #:eager #t #:flat? #t) #t]
   [(vector-immutableof integer?) #t]
   [(vector-immutableof (λ (x) #t)) #f]
   [(vector/c integer? string? boolean?) #t]
   [(vector/c integer? string? (λ (x) #t) boolean?) #f]
   [(vector-immutable/c string? integer? boolean?) #t]
   [(vector-immutable/c string? integer? (λ (x) #t)) #f]
   [(box/c integer?) #t]
   [(box/c integer? real?) #t]
   [(box/c (λ (x) #t)) #f]
   [(box/c integer? (λ (x) #t)) #f]
   [(box/c (λ (x) #t) integer?) #f]
   [(box-immutable/c integer?) #t]
   [(box-immutable/c real?) #t]
   [(box-immutable/c (λ (x) #t)) #f]
   [(listof string?) #t]
   [(listof (λ (x) #t)) #f]
   [(non-empty-listof integer?) #t]
   [(non-empty-listof (λ (x) #t)) #f]
   [(listof (listof integer?)) #t]
   [(listof (listof (listof (λ (x) #t)))) #f]
   [(list*of integer?) #t]
   [(list*of integer? list?) #t]
   [(list*of (λ (x) #t)) #f]
   [(list*of (λ (x) #t) list?) #f]
   [(list*of integer? (λ (x) #t)) #f]
   [(cons/c integer? string?) #t]
   [(cons/c integer? (listof integer?)) #t]
   [(cons/c (λ (x) #t) integer?) #f]
   [(cons/c integer? (λ (x) #t)) #f]
   [(cons/c integer? (listof (λ (x) #t))) #f]
   [(list/c integer? string? (vectorof (listof real?))) #t]
   [(list/c integer? (λ (x) #t)) #f]
   [(list/c integer? (box/c (λ (x) #t))) #f]
   [(*list/c number? integer? integer?) #t]
   [(*list/c any/c integer? integer?) #t]
   [(*list/c (λ (x) #t) integer? integer?) #f]
   [(*list/c number? integer?  (λ (x) #t) integer?) #f]
   [(*list/c any/c (λ (x) #t)) #f]
   [(syntax/c integer?) #t]
   [(syntax/c (λ (x) #t)) #f]
   [(let ()
      (struct foo (x))
      (struct/c foo integer?))
    #t]
   [(let ()
      (struct foo (x))
      (struct/c foo (λ (x) #t)))
    #f]
   [(let ()
      (struct foo (x))
      (struct bar foo (y))
      (struct/c bar string? integer?))
    #t]
   [(let ()
      (struct foo (x))
      (struct bar foo (y))
      (struct/c bar string? (λ (x) #t)))
    #f]
   [(let ()
      (struct foo (x))
      (struct bar foo (y))
      (struct/c bar (λ (x) #t) string?))
    #f]
   [(let ()
      (struct bt (val left right))
      (define (bst/c lo hi)
        (or/c #f
              (struct/dc bt
                         [val (between/c lo hi)]
                         [left (val) #:lazy (bst lo val)]
                         [right (val) #:lazy (bst val hi)])))
      (bst/c -100 100))
    #f]
   [(parameter/c integer?) #t]
   [(parameter/c (λ (x) #t)) #f]
   [(parameter/c integer? real?) #t]
   [(parameter/c integer? (λ (x) #t)) #f]
   [(parameter/c (λ (x) #t) integer?) #f]
   [(procedure-arity-includes/c 0) #t]
   [(procedure-arity-includes/c 1) #t]
   [(procedure-arity-includes/c 3) #t]
   [(hash/c string? integer?) #t]
   [(hash/c string? (λ (x) #t)) #f]
   [(hash/c (λ (x) #t) string?) #f]
   [(channel/c integer?) #t]
   [(channel/c (λ (x) #t)) #f]
   [(prompt-tag/c integer?) #t]
   [(prompt-tag/c integer? #:call/cc any/c) #t]
   [(prompt-tag/c integer? string? #:call/cc (values real? any/c)) #t]
   [(prompt-tag/c (λ (x) #t)) #f]
   [(prompt-tag/c integer? #:call/cc (λ (x) #t)) #f]
   [(prompt-tag/c integer? string? #:call/cc (values real? (λ (x) #t))) #f]
   [(continuation-mark-key/c integer?) #t]
   [(continuation-mark-key/c (λ (x) #t)) #f]
   [(evt/c integer?) #t]
   [(evt/c any/c integer? string?) #t]
   [(evt/c (λ (x) #t)) #f]
   [(evt/c integer? (λ (x) #t) string?) #f]
   [(flat-rec-contract sexp
                       (cons/c sexp sexp)
                       number?
                       symbol?)
    #t]
   [(flat-rec-contract sexp
                       (cons/c sexp sexp)
                       (λ (x) #t)
                       symbol?)
    #f]
   [(flat-rec-contract sexp
                       (cons/c sexp sexp)
                       number?
                       (flat-rec-contract sexp2
                                          (cons/c sexp sexp2)
                                          string?
                                          any/c)
                       symbol?)
    #t]
   [(flat-rec-contract sexp
                       (cons/c sexp sexp)
                       number?
                       (flat-rec-contract sexp2
                                          (cons/c sexp sexp2)
                                          (λ (x) #t)
                                          any/c)
                       symbol?)
    #f]
   ;; TODO: what to do about flat-murec-contract???
   [(promise/c integer?) #t]
   [(promise/c (promise/c integer?)) #t]
   [(promise/c (λ (x) #t)) #f]
   [(promise/c (promise/c (λ (x) #t))) #f]
   [(flat-contract integer?) #t]
   [(flat-contract void?) #t]
   [(flat-contract (λ (x) #t)) #f]
   [(suggest/c integer? "suggestion" "message") #t]
   [(suggest/c (vectorof integer?) "suggestion" "message") #t]
   [(suggest/c (λ (x) #t) "suggestion" "message") #f]
   [(suggest/c (vectorof (λ (x) #t)) "suggestion" "message") #f]

   [(-> integer? integer?) #t]
   [(-> integer? (λ (x) #t)) #f]
   [(-> (λ (x) #t) integer?) #f]
   [(-> integer? any) #t]
   [(-> integer? (values integer? integer?)) #t]
   [(->* (integer?) (integer?) (values integer?)) #t]
   [(->* (integer?) (integer?) #:pre (= 1 2) (values integer?)) #f]
   [(->* (integer?) (integer?) (values integer?) #:post (= 2 3)) #f]
   [(->m integer? integer?) #t]
   [(->i #:can-cache ([x integer?][y (x) integer?]) any) #t]
   [(->i ([x integer?][y (x) integer?]) any) #f]

   [(class/c) #t]
   [(class/c (field f)) #t]
   [(class/c (field [f integer?])) #t]
   [(class/c (field [f (λ (x) #t)])) #f]
   [(class/c m) #t]
   [(class/c [m (-> any/c integer?)]) #t]
   [(class/c [m (-> any/c (λ (x) #f))]) #f]
   [(class/c (override m)) #t]
   [(class/c (override [m (-> any/c integer?)])) #t]
   [(class/c (override [m (-> any/c (λ (x) #f))])) #f]

   [(object/c) #t]
   [(object/c m) #t]
   [(object/c [m (-> integer? integer?)]) #t]
   [(object/c [m (-> integer? (λ (x) #t))]) #f]
   [(object/c (field f)) #t]
   [(object/c (field [f (-> integer? integer?)])) #t]
   [(object/c (field [f (λ (x) #t)])) #f]
   [(dynamic-object/c (list 'm) (list (-> integer? integer?)) (list) (list)) #t]
   [(dynamic-object/c (list 'm) (list (-> (λ (x) #f) integer?)) (list) (list)) #f]
   [(dynamic-object/c (list) (list) (list 'f) (list (-> integer? integer?))) #t]
   [(dynamic-object/c (list) (list) (list 'f) (list (-> (λ (x) #f) integer?))) #f]
   [(instanceof/c (class/c)) #t]
   [(instanceof/c (class/c (field [f (λ (x) #t)]))) #f]
   [(sequence/c integer?) #t]
   [(sequence/c (λ (x) #t)) #f]

   [(let ()
      (struct foo ())
      foo?)
    #t]
   )

  (test/spec-passed
   'recursive-contract-can-cache
   '(contract
       (let ()
         (define tree/c
           (recursive-contract
            (or/c integer? (vector/c tree/c integer? tree/c))
            #:can-cache))
         tree/c)
       5
       'p
       'n))

  (contract-error-test
     'recursive-contract-no-cache
     '(contract
       (let ()
         (define tree/c
           (recursive-contract
            (or/c integer? (vector/c tree/c (λ (x) #t) tree/c))
            #:can-cache))
         tree/c)
       5
       'p
       'n)
     (λ (x)
       (and (exn:fail? x)
            (regexp-match #rx"can-cache-contract[?]" (exn-message x)))))

  (contract-error-test
   '->i-no-cache.1
   '((contract
      (->i #:can-cache ([x (λ (x) #t)]) any)
      (λ (x) 1)
      'pos
      'neg)
     11)
   (λ (x)
     (and (exn:fail? x)
          (regexp-match #rx"can-cache-contract[?]" (exn-message x)))))
  (contract-error-test
   '->i-no-cache.2
   '((contract
      (->i #:can-cache () [res (λ (x) #t)])
      (λ () 1)
      'pos
      'neg))
   (λ (x)
     (and (exn:fail? x)
          (regexp-match #rx"can-cache-contract[?]" (exn-message x)))))

  (contract-error-test
   '->i-no-cache.3
   '((contract
      (->i #:can-cache ([x integer?] [y (x) (λ (x) #t)]) any)
      (λ (x y) 1)
      'pos
      'neg)
     11 12)
   (λ (x)
     (and (exn:fail? x)
          (regexp-match #rx"can-cache-contract[?]" (exn-message x)))))

  (contract-error-test
   '->i-no-cache.4
   '((contract
      (->i #:can-cache ([x integer?] [y (x) (list/c (λ (x) #t))]) any)
      (λ (x y) 1)
      'pos
      'neg)
     11 12)
   (λ (x)
     (and (exn:fail? x)
          (regexp-match #rx"can-cache-contract[?]" (exn-message x)))))

  
  #;(test/pos-blame
     'test-contract-25
     '(contract (letrec ([c (recursive-contract (first-or/c (cons/c any/c c) empty?) 
                                                #:list-contract?)])
                  c)
                (read (open-input-string "#1=(1 . #1#)"))
                'pos 'neg)))

