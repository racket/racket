#lang scheme/base

(require scheme/contract (for-syntax scheme/base))

(provide memory/c apply/c poly/c)

(with-contract
 poly-internals
 ([memory/c
   (->*
    []
    [ #:name any/c
      #:to any/c
      #:from any/c
      #:weak boolean?
      #:equal (or/c 'eq 'eqv 'equal)
      #:table (-> (and/c hash? (not/c immutable?))) ]
    (values flat-contract? flat-contract?))]
  [apply/c (->* [any/c] [#:name any/c] contract?)])

 (define (memory/c
          #:name [name "memory/c"]
          #:to [to (format "~a:to" name)]
          #:from [from (format "~a:from" name)]
          #:weak [weak? #t]
          #:equal [equal 'eq]
          #:table [make-table
                   (case equal
                     [(eq) (if weak? make-weak-hasheq make-hasheq)]
                     [(eqv) (if weak? make-weak-hasheqv make-hasheqv)]
                     [(equal) (if weak? make-weak-hash make-hash)])])
   (let* ([table (make-table)])
     (values
      (flat-named-contract from
        (lambda (v) (hash-set! table v #t) #t))
      (flat-named-contract to
        (lambda (v) (hash-ref table v #f))))))

 (define (apply/c c
                  #:name [name (build-compound-type-name 'apply/c c)])
   (simple-contract
    #:name name
    #:projection
    (lambda (blame)
      (lambda (p)
        (let* ([ctc (coerce-contract 'apply/c c)]
               [thunk
                (lambda ()
                  (((contract-projection ctc) blame) p))])
          (make-keyword-procedure
           (lambda (keys vals . args) (keyword-apply (thunk) keys vals args))
           (case-lambda
             [() ((thunk))]
             [(a) ((thunk) a)]
             [(a b) ((thunk) a b)]
             [(a b c) ((thunk) a b c)]
             [(a b c d) ((thunk) a b c d)]
             [(a b c d e) ((thunk) a b c d e)]
             [(a b c d e f) ((thunk) a b c d e f)]
             [(a b c d e f g) ((thunk) a b c d e f g)]
             [(a b c d e f g h) ((thunk) a b c d e f g h)]
             [args (apply (thunk) args)])))))
    #:first-order procedure?)))

(define-syntax (poly/c stx)
  (syntax-case stx ()
    [(_ opts ... ([c- c+] ...) c)
     (quasisyntax/loc stx
       (apply/c
        #:name (quote #,stx)
        (recursive-contract
         (let-values ([(c- c+) (memory/c #:from 'c- #:to 'c+ opts ...)] ...)
           c))))]))
