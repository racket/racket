#lang racket/base
(require rackunit
         racket/dict
         data/skip-list
         data/splay-tree
         data/private/ordered-dict)

;; Tests for ordered dictionaries
;;   - skip-list
;;   - splay-tree

(test-case "random keys and values"
  (let ([hash (make-hash)]
        [dicts (list (make-skip-list = <)
                     (make-splay-tree = <)
                     (make-integer-splay-tree #:adjust? #t))])
    (for ([c (in-range 100)])
      (let* ([k (- (random 2000) 1000)]
             [v (- (random 2000) 1000)])
        (for ([d (cons hash dicts)])
          (dict-set! d k v))))

    (for ([i (in-range -1000 1000)])
      (for ([d dicts])
        (let ([vh (hash-ref hash i 'not-there)]
              [vd (dict-ref d i 'not-there)])
          (check-equal? vh vd (format "dict ~e, key = ~s, value = ~s, expected = ~s"
                                      d i vd vh)))))

    (for ([c (in-range 100)])
      (let* ([k0 (- (random 2000) 1000)])
        (for ([d dicts])
          (let* ([has? (dict-has-key? d k0)]
                 [l>i (dict-iterate-least/>? d k0)]
                 [l>=i (dict-iterate-least/>=? d k0)]
                 [g<i (dict-iterate-greatest/<? d k0)]
                 [g<=i (dict-iterate-greatest/<=? d k0)]
                 [l> (and l>i (dict-iterate-key d l>i))]
                 [l>= (and l>=i (dict-iterate-key d l>=i))]
                 [g< (and g<i (dict-iterate-key d g<i))]
                 [g<= (and g<=i (dict-iterate-key d g<=i))])
            (when has?
              (check-equal? l>= g<= "has, should be same"))
            (unless has?
              (check-equal? l> l>= "not has, should be same")
              (check-equal? g< g<= "not has, should be same"))
            (when l> (check > l> k0))
            (when l>= (check >= l>= k0))
            (when g< (check < g< k0))
            (when g<= (check <= g<= k0))
            (for ([k (in-dict-keys d)])
              (when (and l> (and (> k k0) (< k l>))) (error "l>"))
              (when (and l>= (and (>= k k0) (< k l>=))) (error "l>="))
              (when (and g< (and (< k k0) (> k g<))) (error "g<"))
              (when (and g<= (and (<= k k0) (> k g<=))) (error "g<=")))))))))
