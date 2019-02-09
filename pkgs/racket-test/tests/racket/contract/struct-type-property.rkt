#lang racket/base

(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract/region)])
  (test/spec-passed
   'struct-type-prop.1
   '(let ()
      (define-values (_prop prop? prop-ref) (make-struct-type-property 'prop))
      (define app-prop (contract (-> prop? integer? boolean?)
                                 (λ (x v) (((prop-ref x) x) v))
                                 'pos1 'neg1))
      (define prop (contract (struct-type-property/c (-> prop? (-> integer? boolean?)))
                             _prop
                             'pos2 'neg2))
      (struct s (f) #:property prop (λ (x) (s-f x)))
      (define s1 (s even?))
      (app-prop s1 5)))

  (test/neg-blame
   'struct-type-prop.2
   '(let ()
      (define-values (_prop prop? prop-ref) (make-struct-type-property 'prop))
      (define app-prop (contract (-> prop? integer? boolean?)
                                 (λ (x v) (((prop-ref x) x) v))
                                 'pos1 'neg1))
      (define prop (contract (struct-type-property/c (-> prop? (-> integer? boolean?)))
                             _prop
                             'pos2 'neg2))
      (struct s (f) #:property prop (λ (x) (s-f x)))
      (define s1 (s even?))
      (app-prop s1 'apple)))

  (test/neg-blame
   'struct-type-prop.3
   '(let ()
      (define-values (_prop prop? prop-ref) (make-struct-type-property 'prop))
      (define app-prop (contract (-> prop? integer? boolean?)
                                 (λ (x v) (((prop-ref x) x) v))
                                 'pos1 'neg1))
      (define prop (contract (struct-type-property/c (-> prop? (-> integer? boolean?)))
                             _prop
                             'pos 'neg))
      (struct s (f) #:property prop (λ (x) (s-f x)))
      (define s2 (s "not a fun"))
      (app-prop s2 5)))

  (test/neg-blame
   'struct-type-prop.4
   '(let ()
      (define-values (_prop prop? prop-ref) (make-struct-type-property 'prop))
      (define app-prop (contract (-> prop? integer? boolean?)
                                 (λ (x v) (((prop-ref x) x) v))
                                 'pos1 'neg1))
      (define prop (contract (struct-type-property/c (-> prop? (-> integer? boolean?)))
                             _prop
                             'pos 'neg))
      (struct s (f) #:property prop (λ (x) (s-f x)))
      (define s3 (s list))
      (app-prop s3 5)))

  (test/pos-blame
   'struct-type-prop.5
   '(let ()
      (define-values (_prop prop? prop-ref) (make-struct-type-property 'prop))
      (define app-prop (contract (-> prop? integer? boolean?)
                                 (λ (x v) (((prop-ref x) x) v))
                                 'pos1 'neg1))
      (define prop (contract (struct-type-property/c (-> prop? (-> integer? boolean?)))
                             _prop
                             'pos2 'neg2))
      (struct s (f) #:property prop (λ (x) (s-f x)))
      (define s3 (s list?))
      ((prop-ref s3) 'apple)))

  (test/spec-failed
   'struct-guard/c.1
   '(let ()
      (with-contract pos
        #:result any/c
        (let ()
          (struct s (x y) #:guard (struct-guard/c integer? boolean?))
          (s 1 1))))
   "(region pos)")

  (test/spec-failed
   'struct-guard/c.2
   '(let ()
      (with-contract pos
        #:result any/c
        (let ()
          (struct s (x y) #:guard (struct-guard/c integer? boolean?))
          (s #t #f))))
   "(region pos)")

  (test/spec-passed/result
   'struct-guard/c.3
   '(with-handlers ([exn:fail? (λ (x) (regexp-match? #rx"^the-name-of-the-struct, field 2:"
                                                     (exn-message x)))])
      (struct the-name-of-the-struct (x y) #:guard (struct-guard/c integer? boolean?))
      (the-name-of-the-struct 1 1))
   #t)
   
  
  )
