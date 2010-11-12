#lang racket/base
(require rackunit
         racket/contract
         racket/dict
         data/skip-list
         data/splay-tree
         data/order)

;; Tests for ordered dictionaries
;;   - skip-list
;;   - splay-tree (both kinds)

(define-syntax-rule (rand-test dicts ordered? idk?
                               (-ref
                                -set!
                                -remove!
                                -count
                                -has-key?
                                -iterate-key
                                -iterate-least/>?
                                -iterate-least/>=?
                                -iterate-greatest/<?
                                -iterate-greatest/<=?))
  (let ([hash (make-hash)])
    (for ([c (in-range 100)])
      (let* ([k (- (random 2000) 1000)]
             [v (- (random 2000) 1000)])
        (hash-set! hash k v)
        (for ([d dicts])
          (-set! d k v))))

    (for ([d dicts])
      (check-equal? (hash-count hash)
                    (-count d)))

    ;; Sequential access
    (for ([i (in-range -1000 1000)])
      (for ([d dicts])
        (let ([vh (hash-ref hash i 'not-there)]
              [vd (-ref d i 'not-there)])
          (check-equal? vh vd (format "dict ~e, key = ~s, value = ~s, expected = ~s"
                                      d i vd vh)))))

    ;; Random removal
    (for ([i (in-range 500)])
      (let ([k (- (random 2000) 1000)])
        (for ([d dicts])
          (hash-remove! hash k)
          (-remove! d k)
          (check-equal? (hash-count hash) (-count d)))))

    (when ordered?
      (for ([c (in-range 100)])
        (let* ([k0 (- (random 2000) 1000)])
          (for ([d dicts])
            (let* ([has? (-has-key? d k0)]
                   [l>i (-iterate-least/>? d k0)]
                   [l>=i (-iterate-least/>=? d k0)]
                   [g<i (-iterate-greatest/<? d k0)]
                   [g<=i (-iterate-greatest/<=? d k0)]
                   [l> (and l>i (-iterate-key d l>i))]
                   [l>= (and l>=i (-iterate-key d l>=i))]
                   [g< (and g<i (-iterate-key d g<i))]
                   [g<= (and g<=i (-iterate-key d g<=i))])
              (when has?
                (check-equal? l>= g<= "has, should be same"))
              (unless has?
                (check-equal? l> l>= "not has, should be same")
                (check-equal? g< g<= "not has, should be same"))
              (when l> (check > l> k0))
              (when l>= (check >= l>= k0))
              (when g< (check < g< k0))
              (when g<= (check <= g<= k0))
              (when idk?
                (for ([k (in-dict-keys d)])
                  (when (and l> (and (> k k0) (< k l>))) (error "l>"))
                  (when (and l>= (and (>= k k0) (< k l>=))) (error "l>="))
                  (when (and g< (and (< k k0) (> k g<))) (error "g<"))
                  (when (and g<= (and (<= k k0) (> k g<=))) (error "g<=")))))))))))

;; Test dict interface

(define (dict-test dicts ordered? [idk? #f])
  (rand-test dicts ordered? idk?
             (dict-ref
              dict-set!
              dict-remove!
              dict-count
              dict-has-key?
              dict-iterate-key
              dict-iterate-least/>?
              dict-iterate-least/>=?
              dict-iterate-greatest/<?
              dict-iterate-greatest/<=?)))

(test-case "skip-list, datum-order, dict interface"
  (dict-test (list (make-skip-list)) #t #t))
(test-case "skip-list, < = order, dict interface"
  (dict-test (list (make-skip-list (order 'my-order any/c = <))) #t #t))
(test-case "adjustable-skip-list, dict interface"
  (dict-test (list (make-adjustable-skip-list)) #t #t))

(test-case "splay-tree, datum-order, dict interface"
  (dict-test (list (make-splay-tree)) #t #t))
(test-case "splay-tree, < = order, dict interface"
  (dict-test (list (make-splay-tree (order 'mine any/c = <))) #t #t))
(test-case "adjustable-splay-tree, dict interface"
  (dict-test (list (make-adjustable-splay-tree)) #t #t))

(provide dict-test)

;; Test splay-tree interface

(define (splay-test dicts ordered? [idk? #f])
  (rand-test dicts ordered? idk?
             (splay-tree-ref
              splay-tree-set!
              splay-tree-remove!
              splay-tree-count
              dict-has-key?
              splay-tree-iterate-key
              splay-tree-iterate-least/>?
              splay-tree-iterate-least/>=?
              splay-tree-iterate-greatest/<?
              splay-tree-iterate-greatest/<=?)))

(test-case "splay-tree, datum-order, custom interface"
  (splay-test (list (make-splay-tree)) #t #t))
(test-case "splay-tree, < = order, custom interface"
  (splay-test (list (make-splay-tree (order 'mine any/c = <))) #t #t))

(test-case "adjustable-splay-tree, custom interface"
  (splay-test (list (make-adjustable-splay-tree)) #t #t))

(provide splay-test)

;; Test skip-list interface

(define (skip-test dicts ordered? [idk? #f])
  (rand-test dicts ordered? idk?
             (skip-list-ref
              skip-list-set!
              skip-list-remove!
              skip-list-count
              dict-has-key?
              skip-list-iterate-key
              skip-list-iterate-least/>?
              skip-list-iterate-least/>=?
              skip-list-iterate-greatest/<?
              skip-list-iterate-greatest/<=?)))

(test-case "skip-list, datum-order, custom interface"
  (skip-test (list (make-skip-list)) #t #t))

(test-case "skip-list, < = order, custom interface"
  (skip-test (list (make-skip-list (order 'mine any/c = <))) #t #t))

(test-case "adjustable-skip-list, custom interface"
  (skip-test (list (make-adjustable-skip-list)) #t #t))

(provide skip-test)

;; Test hash interface
;; for speed comparison only

(define (hash-test dicts ordered?)
  (when ordered?
    (error 'hash-tests "ordered not supported"))
  (rand-test dicts #f #f
             (hash-ref
              hash-set!
              hash-remove!
              hash-count
              hash-has-key?
              '-iterate-key
              '-iterate-least/>?
              '-iterate-least/>=?
              '-iterate-greatest/<?
              '-iterate-greatest/<=?)))

(provide hash-test)

;; ============================================================
;; Performance tests
;; ============================================================

(define (p name testf mkd ordered?)
  (collect-garbage)
  (collect-garbage)
  (let-values ([(_result cpu real gc)
                (time-apply
                 (lambda ()
                   (for ([i (in-range 20)])
                     (testf (list (mkd)) ordered?)))
                 null)])
    (printf "~a : ~s\n" name cpu)))

(define (mksplay) (make-splay-tree))
(define (mkadj) (make-adjustable-splay-tree))
(define (mkcsplay) (make-splay-tree real-order))
(define (mkskip) (make-skip-list))
(define (mkcskip) (make-skip-list real-order))

(define (performance)
  (display "Using ordered-dict interface, w/ search\n")
  (p "splay-tree" dict-test mksplay #t)
  (p "adj splay " dict-test mkadj #t)
  (p "skip-list " dict-test mkskip #t)
  (p "splay w/ c" dict-test mkcsplay #t)
  (p "skip w/ c " dict-test mkcskip #t)
  (newline)
  (display "Using custom interfaces, w/ search\n")
  (p "splay-tree" splay-test mksplay #t)
  (p "adj splay " splay-test mkadj #t)
  (p "skip-list " skip-test mkskip #t)
  (p "splay w/ c" splay-test mkcsplay #t)
  (p "skip w/ c " skip-test mkcskip #t)
  (newline)
  (display "Using custom interfaces, w/o search\n")
  (p "splay-tree" splay-test mksplay #f)
  (p "adj splay " splay-test mksplay #f)
  (p "skip-list " skip-test mkskip #f)
  (p "splay w/ c" splay-test mkcsplay #f)
  (p "skip w/ c " skip-test mkcskip #f)
  (p "hash      " hash-test make-hash #f)
  (newline))

(provide performance)

(define (deep-test [test 'splay] [count 100])
  (let ([mk (case test
              ((splay) mksplay)
              ((adj) mkadj)
              ((skip) mkskip))])
    (for ([seed (in-range count)])
      (random-seed seed)
      (printf "seed = ~s\n" seed)
      (p "test" dict-test mk #t))))

(provide deep-test)

#|
Recent run:

  Using ordered-dict interface, w/ search
  splay-tree : 3745
  adj-splay  : 3640
  skip-list  : 3920
  splay w/ c : 5884
  skip w/ c  : 6297

  Using custom interfaces, w/ search
  splay-tree : 3140
  adj-splay  : 3128
  skip-list  : 3428
  splay w/ c : 3277
  skip w/ c  : 3624

  Using custom interfaces, w/o search
  splay-tree : 2732
  adj-splay  : 2736
  skip-list  : 2976
  splay w/ c : 2809
  skip w/ c  : 3076
  hash       : 3456

Recent run with dict contracts bypassed (impl and client):
(Requires multiple changes to code, not easy to automate.)

  Using ordered-dict interface, w/ search
  splay-tree : 3368
  adj-splay  : 3472
  skip-list  : 3276
  splay w/ c : 4729
  skip w/ c  : 5104

Conclusions:

  - per-inst contracts using dict interface are expensive,
    but per-inst contracts using custom interface not that bad
    (why?)

|#
