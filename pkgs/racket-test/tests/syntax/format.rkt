#lang racket/base

(require rackunit
         syntax/format
         syntax/srcloc
         (submod syntax/private/format for-test))

(check-equal? (syntax-e (~id #'ab "cd" 'hi '#:jk #\l 0)) 'abcdhijkl0)
(check-equal? (syntax-e (~id/1 #'ab "cd" 'hi '#:jk #\l 0)) 'abcdhijkl0)
(check-equal? (~symbol #'ab "cd" 'hi '#:jk #\l 0) 'abcdhijkl0)

(check-equal? (syntax-e (~id #'more-than "-" #'one-identifier)) 'more-than-one-identifier)
(check-exn exn:fail:contract? (λ () (~id/1 #'more-than "-" #'one-identifier)))

(define (do-check-id=? who actual expected)
  (unless (bound-identifier=? actual expected)
    (with-check-info (['actual-context (hash-ref (syntax-debug-info actual) 'context #f)]
                      ['expected-context (hash-ref (syntax-debug-info expected) 'context #f)])
      (fail-check (format "~a identifiers are not ‘bound-identifier=?’" who))))
  (define actual-src (build-source-location actual))
  (define expected-src (build-source-location expected))
  (unless (equal? actual-src expected-src)
    (with-check-info (['actual-src actual-src]
                      ['expected-src expected-src])
      (fail-check (format "~a source locations differ" who)))))

(define (do-check-sub-range-leaf=? who offset actual expected)
  (do-check-id=? who (vector-ref actual offset) (vector-ref expected offset))
  (unless (for/and ([actual-elem (in-vector actual (+ offset 1) (+ offset 5))]
                    [expected-elem (in-vector expected (+ offset 1) (+ offset 5))])
            (equal? actual-elem expected-elem))
    (fail-check (format "~a offsets do not match" who))))

(define-check (check-~id actual-id expected-id expected-sub-range-binders)
  (unless (identifier? actual-id)
    (fail-check "actual-id is not an identifier"))
  (do-check-id=? 'result actual-id expected-id)
  (define actual-sub-range-binders (syntax-property actual-id 'sub-range-binders))
  (with-check-info (['actual-sub-range-binders actual-sub-range-binders])
    (cond
      [expected-sub-range-binders
       (unless (list? actual-sub-range-binders)
         (fail-check "'sub-range-binders property value is not a list"))
       (unless (= (length actual-sub-range-binders) (length expected-sub-range-binders))
         (fail-check "'sub-range-binders properties differ in length"))
       (for ([actual-leaf (in-list actual-sub-range-binders)]
             [expected-leaf (in-list expected-sub-range-binders)])
         (with-check-info (['actual-leaf actual-leaf]
                           ['expected-leaf expected-leaf])
           (unless (sub-range-binder-leaf? actual-leaf)
             (fail-check "'sub-range-binders property leaf has the wrong shape"))
           (do-check-sub-range-leaf=? 'source 0 actual-leaf expected-leaf)
           (do-check-sub-range-leaf=? 'target 5 actual-leaf expected-leaf)))]
      [else
       (unless (not actual-sub-range-binders)
         (fail-check "'sub-range-binders property has a value"))])))

(let ([some #'some]
      [id #'id]
      [some-new-id (datum->syntax #f 'some-new-id)])
  (check-~id (~id some "-new-" id)
             some-new-id
             (list (vector some-new-id 0 4 0.5 0.5 some 0 4 0.5 0.5)
                   (vector some-new-id 9 2 0.5 0.5 id 0 2 0.5 0.5))))

(let* ([lctx #'lctx]
       [srclocs #'srclocs]
       [id-with-lctx+srclocs (datum->syntax lctx 'id-with-lctx+srclocs srclocs)])
  (check-~id (~id "id-with-" lctx "+" srclocs #:context lctx #:source srclocs)
             id-with-lctx+srclocs
             (list (vector id-with-lctx+srclocs 8 4 0.5 0.5 lctx 0 4 0.5 0.5)
                   (vector id-with-lctx+srclocs 13 7 0.5 0.5 srclocs 0 7 0.5 0.5))))

(let* ([base #'base]
       [prefix-base-suffix (datum->syntax base 'prefix-base-suffix base)])
  (check-~id (~id/1 "prefix-" base "-suffix")
             prefix-base-suffix
             (list (vector prefix-base-suffix 7 4 0.5 0.5 base 0 4 0.5 0.5))))
