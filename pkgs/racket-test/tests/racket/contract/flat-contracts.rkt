#lang racket/base
(require "test-util.rkt"
         (for-syntax racket/base))

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace
                 'racket/class
                 'racket/contract/combinator
                 'racket/math)])

  (define-syntax (test-flat-contract stx)
    (syntax-case stx ()
      [(_ contract pass fail more ...)
       #`(test-flat-contract/proc contract pass fail #,(syntax-line stx)
                                  more ...)]))

  (define (test-flat-contract/proc contract pass fail line
                                   #:skip-predicate-checks? [skip-predicate-checks? #f])
    (contract-eval `(,test #t flat-contract? ,contract))
    (contract-eval `(,test #t flat-contract? (opt/c ,contract)))
    (define (run-two-tests maybe-rewrite)
      (define name (if (pair? contract) (car contract) contract))
      (let/ec k
        (test/spec-failed (format "~a fail, line ~a" name line)
                          (maybe-rewrite `(contract ,contract ',fail 'pos 'neg) k)
                          'pos))
      (let/ec k
        (test/spec-passed/result
         (format "~a pass, line ~a" name line)
         (maybe-rewrite `(contract ,contract ',pass 'pos 'neg) k)
         pass))
      (unless skip-predicate-checks?
        (let/ec k
          (test/spec-passed/result (format "~a predicate returns #f, line ~a" name line)
                                   (maybe-rewrite `(,contract ',fail) k)
                                   #f))
        (let/ec k
          (test/spec-passed/result
           (format "~a predicate returns #t, line ~a" name line)
           (maybe-rewrite `(,contract ',pass) k)
           #t))))
    (run-two-tests (λ (x k) x))
    (run-two-tests rewrite-to-add-opt/c))
  
  (define flat-contract-predicate (contract-eval 'flat-contract-predicate))
  
  (test-flat-contract '(and/c number? integer?) 1 3/2)
  (test-flat-contract '(not/c integer?) #t 1)
  (test-flat-contract '(=/c 2) 2 3)
  (test-flat-contract '(>/c 5) 10 5)
  (test-flat-contract '(>=/c 5) 5 0)
  (test-flat-contract '(<=/c 5) 5 10)
  (test-flat-contract '(</c 5) 0 5)
  (test-flat-contract '(=/c 2) 2 0+1i)
  (test-flat-contract '(>/c 5) 10 0+1i)
  (test-flat-contract '(>=/c 5) 5 0+1i)
  (test-flat-contract '(<=/c 5) 5 0+1i)
  (test-flat-contract '(</c 5) 0 0+1i)
  (test-flat-contract '(integer-in 0 10) 0 11)
  (test-flat-contract '(integer-in 0 10) 10 3/2)
  (test-flat-contract '(integer-in 0 10) 1 1.0)
  (test-flat-contract '(integer-in 1 1) 1 1.0)
  (test-flat-contract '(integer-in 1 #f) 1 -1)
  (test-flat-contract '(integer-in #f 1) -1 2)
  (test-flat-contract '(integer-in #f #f) -1 "x")
  (test-flat-contract '(and/c natural? (between/c -10 10)) 0 -1)
  (test-flat-contract '(and/c exact-positive-integer? (between/c -10 10)) 1 0)
  (test-flat-contract '(and/c exact-integer? (between/c -10 10)) 1 11)
  (test-flat-contract '(and/c exact-integer? (between/c -10 10)) -1 -11)
  (test-flat-contract '(and/c exact-integer? (between/c -10.5 10.5)) -10 -11)
  (test-flat-contract '(and/c exact-integer? (between/c -10.5 10.5)) 10 11)
  (test-flat-contract '(and/c exact-integer? (<=/c 0)) -1 -3/2)
  (test-flat-contract '(char-in #\a #\z) #\a #\Z)
  (test-flat-contract '(char-in #\a #\z) #\z #\A)
  (test-flat-contract '(char-in #\a #\z) #\b "b")
  (test-flat-contract '(char-in #\a #\a) #\a #\b)
  (test-flat-contract '(real-in 1 10) 3/2 20)
  (test-flat-contract '(string-len/c 3) "ab" "abc")
  (test-flat-contract 'natural-number/c 5 -1)
  (test-flat-contract 'natural-number/c #e3 #i3.0)
  (test-flat-contract 'natural-number/c 0 -1)
  (test-flat-contract 'false/c #f #t #:skip-predicate-checks? #t)
  (test-flat-contract 'contract? #f (λ (x y) 'whatever))

  (test-flat-contract '(and/c real? negative?) -1 0)
  (test-flat-contract '(and/c real? positive?) 1 0)
  (test-flat-contract '(and/c real? (not/c positive?)) 0 1)
  (test-flat-contract '(and/c real? (not/c negative?)) 0 -1)

  (test-flat-contract '(and/c (flat-named-contract 'Real real?) negative?) -1 0)
  (test-flat-contract '(and/c (flat-named-contract 'Real real?) positive?) 1 0)
  (test-flat-contract '(and/c (flat-named-contract 'Real real?) (not/c positive?)) 0 1)
  (test-flat-contract '(and/c (flat-named-contract 'Real real?) (not/c negative?)) 0 -1)
  
  (test-flat-contract #t #t "x" #:skip-predicate-checks? #t)
  (test-flat-contract #f #f "x" #:skip-predicate-checks? #t)
  (test-flat-contract #\a #\a #\b #:skip-predicate-checks? #t)
  (test-flat-contract #\a #\a 'a #:skip-predicate-checks? #t)
  (test-flat-contract ''a 'a 'b #:skip-predicate-checks? #t)
  (let ([a #\⊢])
    (test-flat-contract a (integer->char (char->integer a)) #\a #:skip-predicate-checks? #t))
  (test-flat-contract ''a 'a #\a #:skip-predicate-checks? #t)
  (test-flat-contract "x" "x" "y" #:skip-predicate-checks? #t)
  (test-flat-contract "x" "x" 'x #:skip-predicate-checks? #t)
  (test-flat-contract 1 1 2 #:skip-predicate-checks? #t)
  (test-flat-contract #e1 #i1.0 'x #:skip-predicate-checks? #t)
  (test-flat-contract +nan.0 +nan.0 +nan.f #:skip-predicate-checks? #t)
  (test-flat-contract +nan.f +nan.f +nan.0 #:skip-predicate-checks? #t)
  (test-flat-contract #rx".x." "axq" "x" #:skip-predicate-checks? #t)
  (test-flat-contract #rx#".x." #"axq" #"x" #:skip-predicate-checks? #t)
  (test-flat-contract #rx".x." #"axq" #"x" #:skip-predicate-checks? #t)
  (test-flat-contract #rx#".x." "axq" "x" #:skip-predicate-checks? #t)
  (test-flat-contract ''() '() #f #:skip-predicate-checks? #t)

  (test-flat-contract '(if/c integer? even? list?) 2 3)
  (test-flat-contract '(if/c integer? even? list?) '() #f)
  
  (test/spec-passed 'any/c '(contract any/c 1 'pos 'neg))
  (test-flat-contract 'printable/c (vector (cons 1 (box #f))) (lambda (x) x))
  (let ()
    (define-struct s (a b) #:prefab)
    (test-flat-contract 'printable/c (make-s 1 2) (λ (x) x)))
  (test-flat-contract 'printable/c (hash 'x 1) (make-hash (list (cons 'x 1))))
  (test-flat-contract 'printable/c 1 (hash (λ (x) x) 1))
  
  (test-flat-contract '(symbols 'a 'b 'c) 'a 'd)
  (test-flat-contract '(one-of/c (expt 2 65)) (expt 2 65) 12)
  (test-flat-contract '(one-of/c '#:x '#:z) '#:x '#:y)

  (let ([c% (contract-eval '(class object% (super-new)))])
    (test-flat-contract `(subclass?/c ,c%) c% (contract-eval `object%))
    (test-flat-contract `(subclass?/c ,c%)
                        (contract-eval `(class ,c%)) (contract-eval `(class object%))))

  (let ([i<%> (contract-eval '(interface ()))])
    (test-flat-contract `(implementation?/c ,i<%>)
                        (contract-eval `(class* object% (,i<%>) (super-new)))
                        (contract-eval 'object%))
    (test-flat-contract `(implementation?/c ,i<%>)
                        (contract-eval `(class* object% (,i<%>) (super-new)))
                        #f))

  (begin
    (contract-eval '(define flat-is-a-test<%> (interface ())))
    (contract-eval '(define flat-is-a-test% (class object% (super-new))))
    (test-flat-contract `(is-a?/c flat-is-a-test<%>)
                        (contract-eval `(new (class* object% (flat-is-a-test<%>) (super-new))))
                        (contract-eval '(new object%)))
    (test-flat-contract `(is-a?/c flat-is-a-test%)
                        (contract-eval `(new flat-is-a-test%))
                        (contract-eval '(new object%)))
    (test-flat-contract `(or/c #f (is-a?/c flat-is-a-test<%>))
                        (contract-eval `(new (class* object% (flat-is-a-test<%>) (super-new))))
                        (contract-eval '(new object%)))
    (test-flat-contract `(or/c #f (is-a?/c flat-is-a-test%))
                        (contract-eval `(new flat-is-a-test%))
                        (contract-eval '(new object%)))
    (test-flat-contract `(first-or/c #f (is-a?/c flat-is-a-test<%>))
                        (contract-eval `(new (class* object% (flat-is-a-test<%>) (super-new))))
                        (contract-eval '(new object%)))
    (test-flat-contract `(first-or/c #f (is-a?/c flat-is-a-test%))
                        (contract-eval `(new flat-is-a-test%))
                        (contract-eval '(new object%))))

  (test-flat-contract '(listof boolean?) (list #t #f) (list #f 3 #t))
  (test-flat-contract '(listof any/c) (list #t #f) 3)

  (test-flat-contract '(vectorof boolean? #:flat? #t) (vector #t #f) (vector #f 3 #t))
  (test-flat-contract '(vectorof any/c #:flat? #t) (vector #t #f) 3)
  (test-flat-contract '(vector-immutableof boolean?) 
                      (vector-immutable #t #f)
                      (vector-immutable #f 3 #t))
  (test-flat-contract '(vector-immutableof any/c) (vector-immutable #t #f) 3)

  (test-flat-contract '(vector/c boolean? (flat-contract integer?) #:flat? #t)
                      (vector #t 1)
                      (vector 1 #f))
  (test-flat-contract '(vector/c boolean? (flat-contract integer?) #:flat? #t) (vector #t 1) #f)
  (test-flat-contract '(vector-immutable/c boolean? (flat-contract integer?))
                      (vector-immutable #t 1) (vector-immutable 1 #f))
  (test-flat-contract '(vector-immutable/c boolean? (flat-contract integer?))
                      (vector-immutable #t 1)
                      #f)

  (test-flat-contract '(cons/c boolean? (flat-contract integer?)) (cons #t 1) (cons 1 #f))
  (test-flat-contract '(cons/c boolean? (flat-contract integer?)) (cons #t 1) #f)
  (test-flat-contract '(list/c boolean? (flat-contract integer?)) (list #t 1) (list 1 #f))
  (test-flat-contract '(list/c boolean? (flat-contract integer?)) (list #t 1) #f)

  (contract-eval '(define (a-predicate-that-wont-be-optimized x) (boolean? x)))
  (test-flat-contract '(cons/c a-predicate-that-wont-be-optimized (flat-contract integer?))
                      (cons #t 1)
                      (cons 1 #f))
  (test-flat-contract '(cons/c a-predicate-that-wont-be-optimized (flat-contract integer?))
                      (cons #t 1)
                      #f)
  (test-flat-contract '(list/c a-predicate-that-wont-be-optimized (flat-contract integer?))
                      (list #t 1)
                      (list 1 #f))
  (test-flat-contract '(list/c a-predicate-that-wont-be-optimized (flat-contract integer?))
                      (list #t 1)
                      #f)

  (test-flat-contract '(cons/dc [hd 1] [tl (hd) 3] #:flat) (cons 1 3) (cons 1 4))
  (test-flat-contract '(cons/dc [hd (tl) 1] [tl 3] #:flat) (cons 1 3) (cons 1 4))
  
  (test-flat-contract '(box/c boolean? #:flat? #t) (box #f) (box 1))
  (test-flat-contract '(box/c (flat-contract boolean?) #:flat? #t) (box #t) #f)
  (test-flat-contract '(box-immutable/c boolean?) (box-immutable #f) (box-immutable 1))
  (test-flat-contract '(box-immutable/c (flat-contract boolean?)) (box-immutable #t) #f)

  (test-flat-contract '(flat-rec-contract sexp (cons/c sexp sexp) number?) '(1 2 . 3) '(1 . #f))
  (test-flat-contract '(flat-murec-contract ([even1 (or/c null? (cons/c number? even2))]
                                             [even2 (cons/c number? even1)])
                                            even1)
                      '(1 2 3 4)
                      '(1 2 3))
  (test-flat-contract '(flat-murec-contract ([even1 (first-or/c null? (cons/c number? even2))]
                                             [even2 (cons/c number? even1)])
                                            even1)
                      '(1 2 3 4)
                      '(1 2 3))

  (test-flat-contract '(hash/c symbol? boolean? #:flat? #t) (make-hash) 1)
  (test-flat-contract '(hash/c symbol? boolean? #:flat? #t)
                      (let ([ht (make-hash)])
                        (hash-set! ht 'x #t)
                        ht)
                      (let ([ht (make-hash)])
                        (hash-set! ht 'x 1)
                        ht))
  (test-flat-contract '(hash/c symbol? boolean? #:flat? #t)
                      (let ([ht (make-hash)])
                        (hash-set! ht 'x #t)
                        ht)
                      (let ([ht (make-hash)])
                        (hash-set! ht 'x 1)
                        ht))
  
  (test-flat-contract '(between/c 1 10) 3 11)
  (test-flat-contract '(between/c 1 10) 4 1+1i)
  (test-flat-contract '(between/c 9 9) 9 10)
  (test-flat-contract '(between/c 9 9) 9.0 10)
  (test-flat-contract '(<=/c 1) 0 1+1i)
  (test-flat-contract '(</c 1) 0 1+1i)
  (test-flat-contract '(>/c 1) 4 1+1i)
  (test-flat-contract '(>=/c 1) 4 1+1i)
  
  (test #t 'malformed-binder
        (with-handlers ((exn? exn:fail:syntax?))
          (contract-eval '(flat-murec-contract ([(x) y]) x))
          'no-err))
  (test #t 'missing-body
        (with-handlers ((exn? exn:fail:syntax?))
          (contract-eval '(flat-murec-contract ([x y])))
          'no-err))

  (test-flat-contract '(or/c (flat-contract integer?) char?) #\a #t)
  (test-flat-contract '(or/c (flat-contract integer?) char?) 1 #t)

  (test-flat-contract '(first-or/c (flat-contract integer?) char?) #\a #t)
  (test-flat-contract '(first-or/c (flat-contract integer?) char?) 1 #t)

  
  ;; test flat-contract-predicate
  (test #t (flat-contract-predicate integer?) 1)
  (test #t (flat-contract-predicate #t) #t)

  (test-flat-contract '(flat-contract-with-explanation even?) 0 1)
  (test-flat-contract '(flat-contract-with-explanation
                        (λ (x)
                          (cond
                            [(even? x) #t]
                            [else (λ (b)
                                    (raise-blame-error b x
                                                       '(expected: "an even number"
                                                                   given:
                                                                   "something else")))])))
                      0 1)


  ;; test that anonymous procedures passed to
  ;; various combinators don't get strange names
  (define (test-anon-name name exp)
    (define (anonymous-name-preserved? exn)
      (define m
        (regexp-match
         #rx"\n *((expected)|(promised)): ([^\n]*)\n"
         (exn-message exn)))
      (define procs-name (and m (list-ref m 4)))
      (define passed?
        (and m (equal? "flat-contracts:1:0" procs-name)))
      (unless passed?
        (printf "~a: procs-name is ~s\n" name procs-name))
      passed?)
    (contract-error-test name
                         (datum->syntax #f exp
                                        (vector "flat-contracts" 1 0 1 0))
                         anonymous-name-preserved?))
  (test-anon-name 'anon-name->dom
                  '((contract (-> (λ (x) (even? x)) any) values 'pos 'neg) 1))
  (test-anon-name 'anon-name->dom-kwd
                  '((contract (-> #:x (λ (x) (even? x)) any) (λ (#:x x) x) 'pos 'neg) #:x 1))
  (test-anon-name 'anon-name->after-...
                  '((contract (-> any/c ... (λ (x) (even? x)) any) values 'pos 'neg) 1 3 5))
  (test-anon-name 'anon-name->before-...
                  '((contract (-> (λ (x) (even? x)) ... any/c any) values 'pos 'neg) 1 3 5))
  (test-anon-name 'anon-name->rng
                  '((contract (-> any/c (λ (x) (even? x))) values 'pos 'neg) 1))
  (test-anon-name 'anon-name->values-rng
                  '((contract (-> any/c any/c (values (λ (x) (even? x)) (λ (x) (even? x)))) values 'pos 'neg) 1 3))
  (test-anon-name 'anon-name->i-dom
                  '((contract (->i ([x (λ (x) (even? x))]) any) values 'pos 'neg) 1))
  (test-anon-name 'anon-name->*-dom
                  '((contract (->* ((λ (x) (even? x))) any) values 'pos 'neg) 1))
  (test-anon-name 'anon-name->*-dom
                  '((contract (->* (#:x (λ (x) (even? x))) any) (λ (#:x x) x) 'pos 'neg) #:x 1))
  (test-anon-name 'anon-name-list/c
                  '(contract (listof (λ (x) (even? x))) '(1) 'pos 'neg))
  (test-anon-name 'anon-name-and
                  '(contract (and/c real? (λ (x) (even? x))) 1 'pos 'neg))

  )
