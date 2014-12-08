#lang racket/base
(require "test-util.rkt" (for-syntax racket/base))

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 
                 'racket/promise
                 'racket/contract
                 'racket/class
                 'racket/contract/private/blame)])
  
  (contract-eval '(define (extract-context-lines thunk)
                    (define str
                      (with-handlers ((exn:fail:contract:blame? exn-message))
                        (thunk)
                        "didn't raise an exception"))
                    (define m (regexp-match #rx".*\n +in: (.*)$" str))
                    (cond
                      [m
                       (define without-prefix (list-ref m 1))
                       (define m2 (regexp-match #rx"(.*)\n *contract from:" without-prefix))
                       (cond
                         [m2
                          (define lines (regexp-split #rx"\n *" (list-ref m2 1)))
                          ;; drop the lines with the contract (keep lines beginning with an article)
                          (let loop ([lines (regexp-split #rx"\n *" (list-ref m2 1))])
                            (cond
                              [(null? lines) '()]
                              [else
                               (define line (car lines))
                               (cond
                                 [(or (regexp-match #rx"^the " line)
                                      (regexp-match #rx"^an " line)
                                      (regexp-match #rx"^a " line))
                                  (cons line (loop (cdr lines)))]
                                 [else
                                  (loop (cdr lines))])]))]
                         [else
                          (string-append "did not find ``contract from:'', so no context in msg: "
                                         str)])]
                      [else
                       (string-append "did not find ``in:'', so no context in msg: "
                                      str)])))
  (define-syntax (context-test stx)
    (syntax-case stx ()
      [(_ x ...)
       (with-syntax ([line (syntax-line stx)])
         #'(context-test/proc line x ...))]))
  
  (define (context-test/proc line context expression)
    (define name (format "line ~a" line))
    (contract-eval
     #:test-case-name name
     `(,test #:test-case-name ',name
             ',context extract-context-lines
             (lambda () ,expression)))
    (let/ec k
      (define rewritten (rewrite-to-add-opt/c expression k))
      (unless (equal? expression rewritten)
        (define opt-name (string-append name "+opt/c"))
        (contract-eval 
         #:test-case-name opt-name
         `(,test #:test-case-name ',opt-name
                 ',context extract-context-lines (lambda () ,rewritten))))))
  
  (context-test '("the 1st argument of")
                '((contract (-> boolean? integer? integer?)
                            (λ (x y) x)
                            'pos
                            'neg)
                  0 1))
  
  (context-test '("the cdr of" "the 1st argument of")
                '((contract (-> (cons/c integer? boolean?) integer? integer?)
                            (λ (x y) x)
                            'pos
                            'neg)
                  (cons 1 2) 1))
  
  (context-test '("the 3rd element of" "the 2nd argument of")
                '((contract (-> integer? (list/c integer? integer? boolean?) integer?)
                            (λ (x y) x)
                            'pos
                            'neg)
                  1 (list 1 2 3)))
  
  (context-test '("the range of" "the 4th element of")
                '((cadddr (contract (list/c integer? integer? boolean? (-> number? number?))
                                    (list 1 2 #f (λ (x) #f))
                                    'pos
                                    'neg))
                  1))
  
  (context-test '("a disjunct of")
                '(contract (or/c 1 (-> number? number?))
                           3
                           'pos
                           'neg))
  
  (context-test '("the range of" "a disjunct of")
                '((contract (or/c 1 (-> number? number?) (-> number? boolean? number?))
                            (λ (x) #f)
                            'pos
                            'neg)
                  1))
  
  (context-test '("the 2nd conjunct of")
                '(contract (and/c procedure? (-> integer? integer?))
                           (λ (x y) 1)
                           'pos
                           'neg))
  
  (context-test '("an element of")
                '(contract (listof number?)
                           (list #f)
                           'pos
                           'neg))
  
  (context-test '("the promise from")
                '(force (contract (promise/c number?)
                                  (delay #f)
                                  'pos
                                  'neg)))
  
  (context-test '("the parameter of")
                '((contract (parameter/c number?)
                            (make-parameter #f)
                            'pos
                            'neg)))
  (context-test '("the parameter of")
                '((contract (parameter/c number?)
                            (make-parameter 1)
                            'pos
                            'neg)
                  #f))
  (context-test '("the #:x argument of")
                '((contract (-> #:x number? #:a char? #:w boolean? any)
                            (λ (#:x x #:a a #:w w) x)
                            'pos
                            'neg)
                  #:a #\a #:w #f #:x 'two))
  
  (context-test '("the #:a argument of")
                '((contract (-> #:x number? #:a char? #:w boolean? any)
                            (λ (#:x x #:a a #:w w) x)
                            'pos
                            'neg)
                  #:a #f #:w #f #:x 2))
  
  (context-test '("the #:w argument of")
                '((contract (-> #:x number? #:a char? #:w boolean? any)
                            (λ (#:x x #:a a #:w w) x)
                            'pos
                            'neg)
                  #:a #\a #:w 'false #:x 2))
  
  (context-test '("the #:x argument of")
                '((contract (->* () (#:x number?) any)
                            (λ (#:x [x 1]) x)
                            'pos
                            'neg)
                  #:x #f))
  
  (context-test '("the #:x argument of")
                '((contract (->* () (#:x number? #:a char? #:w boolean?) any)
                            (λ (#:x [x 1] #:a [a #\a] #:w [w #f]) x)
                            'pos
                            'neg)
                  #:a #\a #:w #f #:x 'two))
  
  (context-test '("the #:a argument of")
                '((contract (->* () (#:x number? #:a char? #:w boolean?) any)
                            (λ (#:x [x 1] #:a [a #\a] #:w [w #f]) x)
                            'pos
                            'neg)
                  #:a #f #:w #f #:x 2))
  
  (context-test '("the #:w argument of")
                '((contract (->* () (#:x number? #:a char? #:w boolean?) any)
                            (λ (#:x [x 1] #:a [a #\a] #:w [w #f]) x)
                            'pos
                            'neg)
                  #:a #\a #:w 'false #:x 2))
  
  (context-test '("the x argument of")
                '((contract (->i ([w integer?] [x boolean?] [a char?]) any)
                            (λ (w x a) x)
                            'pos
                            'neg)
                  1 'true #\a))
  
  (context-test '("the x argument of")
                '((contract (->i ([w integer?]) ([x boolean?] [a char?]) any)
                            (λ (w [x #t] [a #\a]) x)
                            'pos
                            'neg)
                  1 'true #\a))
  
  (context-test '("the y result of")
                '((contract (->i () (values [x integer?] [y integer?]))
                            (λ () (values 1 #f))
                            'pos
                            'neg)))
  
  (context-test '("the x result of")
                '((contract (->i () (values [x integer?] [y integer?]))
                            (λ () (values #f 1))
                            'pos
                            'neg)))
  
  (context-test '("the _ result of")
                '((contract (->i ([x integer?]) [_ (x) (<=/c x)])
                            add1
                            'pos
                            'neg)
                  1))
  
  (context-test '("the a argument of")
                '((contract (->i ([a integer?] #:b [b integer?]) ([c integer?] #:d [d integer?]) any)
                            (λ (a #:b b [c 1] #:d [d 1]) 1)
                            'pos
                            'neg)
                  'one #:b 2 3 #:d 4))
  
  (context-test '("the b argument of")
                '((contract (->i ([a integer?] #:b [b integer?]) ([c integer?] #:d [d integer?]) any)
                            (λ (a #:b b [c 1] #:d [d 1]) 1)
                            'pos
                            'neg)
                  1 #:b 'two 3 #:d 4))
  
  (context-test '("the c argument of")
                '((contract (->i ([a integer?] #:b [b integer?]) ([c integer?] #:d [d integer?]) any)
                            (λ (a #:b b [c 1] #:d [d 1]) 1)
                            'pos
                            'neg)
                  1 #:b 2 'three #:d 4))
  
  (context-test '("the d argument of")
                '((contract (->i ([a integer?] #:b [b integer?]) ([c integer?] #:d [d integer?]) any)
                            (λ (a #:b b [c 1] #:d [d 1]) 1)
                            'pos
                            'neg)
                  1 #:b 2 3 #:d 'four))
  
  ;; indy
  (context-test '("the 2nd argument of" "the x argument of")
                '((contract (->i ([x (-> number? boolean? integer?)] [a (x) (>=/c (x 11 'true))]) any)
                            (λ (x a) x)
                            'pos
                            'neg)
                  (λ (x y) 1) 11))
  
  (context-test '("the 2nd argument of" "the x result of")
                '((contract 
                   (->i () (values [x (-> number? boolean? integer?)] [a (x) (>=/c (x 11 'true))]))
                   (λ () (values (λ (x y) x) 1))
                   'pos
                   'neg)))
  
  (context-test '("the x argument of")
                '((contract (->i ([x () integer?]) any)
                            (λ (x) x)
                            'pos
                            'neg)
                  #f))
  
  (context-test '("the a argument of")
                '((contract (->i ([a integer?] [x (a) integer?]) any)
                            (λ (a x) x)
                            'pos
                            'neg)
                  #f 1))
  
  (context-test '("the 1st result of")
                '((contract (->i () (values [_ integer?] [_ integer?]))
                            (λ () (values #f 1))
                            'pos
                            'neg)))
  
  (context-test '("the result of")
                '((contract (->i () [_ integer?])
                            (λ () (values #f))
                            'pos
                            'neg)))
  
  (context-test '("the domain of")
                '((contract (->d ([x integer?]) [y integer?])
                            (λ (x) #f)
                            'pos
                            'neg)
                  #f))
  
  (context-test '("the range of")
                '((contract (->d ([x integer?]) [y integer?])
                            (λ (x) #f)
                            'pos
                            'neg)
                  1))
  
  (context-test '("the range of")
                '(letrec ([ctc (-> integer? (recursive-contract ctc))])
                   (letrec ([f (λ (x) 'not-f)])
                     ((contract ctc f 'pos 'neg) 1))))
  
  (context-test '("the a field of")
                '(let ()
                   (struct s (a b))
                   (contract (struct/dc s [a (b) (<=/c b)] [b integer?])
                             (s 2 1)
                             'pos
                             'neg)))
  
  (context-test '("the a field of")
                '(let ()
                   (struct s (a b))
                   (contract (struct/dc s [a (<=/c 1)] [b integer?])
                             (s 2 1)
                             'pos
                             'neg)))
  
  (context-test '("an element of" "the 2nd element of")
                '(vector-ref
                  (vector-ref
                   (contract (vector/c (vectorof real?) (vectorof number?) (vectorof boolean?))
                             (vector (vector 1) (vector 1) (vector 1))
                             'pos
                             'neg)
                   2)
                  0))
  
  (context-test '("the 0th element of")
                '(vector-ref (contract (vector/c integer?)
                                       (vector #f)
                                       'pos
                                       'neg)
                             0))
  
  (context-test '("the 0th element of")
                '(vector-ref (contract (vector/c (-> integer? integer?))
                                       (vector #f)
                                       'pos
                                       'neg)
                             0))
  
  (context-test '("the 0th element of")
                '(vector-ref (contract (vector/c (new-∀/c 'α))
                                       (vector #f)
                                       'pos
                                       'neg)
                             0))
  
  (context-test '("an element of")
                '(vector-ref
                  (contract (vectorof integer?)
                            (vector #f)
                            'pos
                            'neg)
                  0))
  
  (context-test '("an element of")
                '(vector-ref (contract (vectorof (-> integer? integer?))
                                       (vector #f)
                                       'pos
                                       'neg)
                             0))
  
  (context-test '("an element of")
                '(vector-ref (contract (vectorof (new-∀/c 'α))
                                       (vector #f)
                                       'pos
                                       'neg)
                             0))
  
  (context-test '("the keys of")
                '(contract (hash/c integer? (-> integer? integer?))
                           (hash #f (λ (x) #f))
                           'pos
                           'neg))
  
  (context-test '("the range of" "the values of")
                '((hash-ref
                   (contract (hash/c integer? (-> integer? integer?))
                             (hash 0 (λ (x) #f))
                             'pos
                             'neg)
                   0)
                  1))
  
  (context-test '("an element of" "the rest argument of")
                '((contract (->* () #:rest (listof number?) number?)
                            +
                            'pos 'neg)
                  1 "a"))
  
  (context-test '("the 2nd argument of")
                '((contract (->* (number? number?) #:rest (listof number?) number?)
                            +
                            'pos 'neg)
                  1 "a"))
  
  (context-test '("an element of" "the rest argument of")
                '((contract (->* (number?) #:rest (listof number?) number?)
                            +
                            'pos 'neg)
                  1 "a"))
  
  (context-test '("the range of" "the 2nd case of")
                '((contract (case-> (-> real? real? real?)
                                    (-> real? (values real? real?)))
                            (case-lambda
                              [(x y) 1]
                              [(x) 1])
                            'pos 'neg)
                  1))
  
  (context-test '("the domain of" "the 2nd case of")
                '((contract (case-> (-> real? real? real?)
                                    (-> real? (values real? real?)))
                            (case-lambda
                              [(x y) 1]
                              [(x) 1])
                            'pos 'neg)
                  #f))
  
  (context-test '("the 1st argument of" "the save-file method in")
                '(send (contract (object/c
                                  (save-file (->m string? string?)))
                                 (new (class object%
                                        (define/public (save-file s . args) #f)
                                        (super-new)))
                                 'pos
                                 'neg)
                       save-file 1))
  
  (context-test '("the f field in")
                '(get-field 
                  f
                  (contract (object/c (field [f string?]))
                            (new (class object%
                                   (field [f 1])
                                   (super-new)))
                            'pos
                            'neg)))
  
  (context-test '("the 1st argument of" "the f field in")
                '((get-field 
                   f
                   (contract (object/c (field [f (-> string? any)]))
                             (new (class object%
                                    (field [f (λ (x) 1)])
                                    (super-new)))
                             'pos
                             'neg))
                  #f))
  
  (context-test '("the content of")
                '(unbox (contract (box/c integer?)
                                  (box #f)
                                  'pos
                                  'neg)))
  
  (context-test '("the content of")
                '(contract (box/c integer? #:immutable #t)
                           (box-immutable #f)
                           'pos
                           'neg))
  
  (context-test '("an element of")
                '(contract (vectorof integer? #:flat? #t)
                           (vector-immutable #f)
                           'pos 'neg))
  
  (context-test '("the range of")
                '((contract (parametric->/c (x) (-> x x))
                            (λ (x) 1)
                            'pos 'neg)
                  1))
  
  (let* ([blame-pos (contract-eval '(make-blame (srcloc #f #f #f #f #f)
                                                #f
                                                (λ () 'integer?)
                                                'positive
                                                'negative
                                                #t))]
         [blame-neg (contract-eval `(blame-swap ,blame-pos))])
    (ctest "something ~a" blame-fmt->-string ,blame-neg "something ~a")
    (ctest "promised: ~s\n  produced: ~e" blame-fmt->-string ,blame-pos '(expected: "~s" given: "~e"))
    (ctest "expected: ~s\n  given: ~e" blame-fmt->-string ,blame-neg '(expected: "~s" given: "~e"))
    (ctest "promised ~s produced ~e" blame-fmt->-string ,blame-pos '(expected "~s" given "~e"))
    (ctest "expected ~s given ~e" blame-fmt->-string ,blame-neg '(expected "~s" given "~e"))))