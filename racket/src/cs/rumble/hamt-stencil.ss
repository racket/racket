;; See also "intmap.ss"

;; A HAMT node is implemented as a Chez Scheme stencil vector. A
;; stencil vector has 26 or 58 slots, so we make each node in the HAMT
;; of size 8 (32-bit platforms) or 16 (64-bit platforms) entries. That
;; way, we have use three bits per node entry: child, key, and value.
;;
;;  * A child bit set means that the trie continues with a child node.
;;
;;  * A key bit set meants that the trie ends with a specific key and
;;    value; the key and child bits are mutually exclusive.
;;
;;  * A value if is set only if the corresponding key bit is set. If
;;    the key bit is not set, then the implicit value for the key is
;;    `#t`.
;;
;; We use one extra slot in the stencil vector to record the tree size
;; and equality type (both packed into a single fixnum). That's first
;; in the stencil vector, so the stencil vector mask always has the
;; low bit set. After that "static field", the order is as above:
;; children, keys, then values.
;;
;; Keys in a bnode are "wrapped". A wrapped key differs from a key
;; only in an `equal?`-based hash table, where if a key is complex
;; enough --- so that keeping a hash code is worthwhile to speed up
;; comparions --- then it is paired with its hash code for future
;; reference. (This is not obviously a good idea, since it trades
;; space for speed, but right choice for `equal?`-based tables may be
;; different than for `eq?` and `eqv?`-based tables.)
;;
;; A "cnode" collision node is an association list. It's never a root
;; node, but it can be a child at any non-root depth.

;; authentic immutable hash
(define (intmap? x) (#%$system-stencil-vector? x))

;; log of node size:
(define BNODE-BITS (if (fx> (stencil-vector-mask-width) 26)
                       4
                       3))
(define BNODE-MASK (fx1- (fxsll 1 BNODE-BITS)))

;; node size:
(define HAMT-WIDTH (fxsll 1 BNODE-BITS))
(define HAMT-GROUP-MASK (fx- (fxsll 1 HAMT-WIDTH) 1))

;; Static fields in stencil vector:
(define HAMT-STATIC-FIELD-COUNT 1)
;;  First field is a count with eqtype encoded as a single fixnum:
(define HAMT-COUNT+EQTYPE-INDEX 0)
(define HAMT-COUNT+EQTYPE-BIT (fxsll 1 HAMT-COUNT+EQTYPE-INDEX))

;; Equality types:
(define HAMT-EQTYPE-EQ 0)
(define HAMT-EQTYPE-EQV 1)
(define HAMT-EQTYPE-EQUAL 2)
(define HAMT-EQTYPE-EQUAL-ALWAYS 3)
(define HAMT-EQTYPE-MASK (fx- (fxsll 1 (integer-length HAMT-EQTYPE-EQUAL-ALWAYS)) 1))

(define HAMT-COUNT-OFFSET (integer-length HAMT-EQTYPE-MASK))
(define ONE-COUNT-IN-COUNT+EQTYPE (fxsll 1 HAMT-COUNT-OFFSET))

(define (count+eqtype c t)
  (fxior (fxsll c HAMT-COUNT-OFFSET) t))
(define (count+eqtype-eqtype x)
  (fxand x HAMT-EQTYPE-MASK))
(define (count+eqtype-count x)
  (fxsrl x HAMT-COUNT-OFFSET))

(define (hamt-count+eqtype h)
  (#%$stencil-vector-ref h HAMT-COUNT+EQTYPE-INDEX))
(define (hamt-eqtype h)
  (count+eqtype-eqtype (hamt-count+eqtype h)))
(define (hamt-count h)
  (count+eqtype-count (hamt-count+eqtype h)))

;; to dispatch on a bnode's equality type:
(define-syntax eqtype-case
  (syntax-rules (eq eqv equal-always else)
    [(_ h [(eq) a] [(eqv) b] [(equal-always) c] [else d])
     (let ([eqt (hamt-eqtype h)])
       (cond
        [(fx= eqt HAMT-EQTYPE-EQ) a]
        [(fx= eqt HAMT-EQTYPE-EQV) b]
        [(fx= eqt HAMT-EQTYPE-EQUAL-ALWAYS) c]
        [else d]))]))

;; Child, key, and value bits in the stencil-vector mask:
(define HAMT-CHILD-OFFSET HAMT-STATIC-FIELD-COUNT)
(define HAMT-KEY-OFFSET (fx+ HAMT-CHILD-OFFSET HAMT-WIDTH))
(define HAMT-VAL-OFFSET (fx+ HAMT-KEY-OFFSET HAMT-WIDTH))

(define HAMT-CHILD-MASK (fxsll HAMT-GROUP-MASK HAMT-CHILD-OFFSET))
(define HAMT-KEY-MASK (fxsll HAMT-GROUP-MASK HAMT-KEY-OFFSET))
(define HAMT-VAL-MASK (fxsll HAMT-GROUP-MASK HAMT-VAL-OFFSET))

(define (hamt-mask->child-count mask)
  (fxpopcount16 (fxsrl (fxand HAMT-CHILD-MASK mask) HAMT-CHILD-OFFSET)))
(define (hamt-mask->key-count mask)
  (fxpopcount16 (fxsrl (fxand HAMT-KEY-MASK mask) HAMT-KEY-OFFSET)))
(define (hamt-mask->val-count mask)
  (fxpopcount16 (fxsrl (fxand HAMT-VAL-MASK mask) HAMT-VAL-OFFSET)))

(define (bnode? x) (#%$system-stencil-vector? x))

(define-record-type cnode
  [fields (immutable hash)
          (immutable content)] ; association list
  [nongenerative #{cnode pfwh0bwrq2nqlke97ikru0ds2-0}]
  [sealed #t])

(define (bnode-bit-pos hash shift)
  (bnode-mask hash shift))

(define (bnode-mask hash shift)
  (fxand (fxsrl hash shift) BNODE-MASK))

(define (bnode-maps-key? node bit)
  (fxbit-set? (#%$stencil-vector-mask node) (fx+ bit HAMT-KEY-OFFSET)))

(define (bnode-maps-child? node bit)
  (fxbit-set? (#%$stencil-vector-mask node) (fx+ bit HAMT-CHILD-OFFSET)))

(define (bnode-down shift)
  (fx+ shift BNODE-BITS))

(define (bnode-child-ref n bit)
  (#%$stencil-vector-ref n (fxpopcount32
                            (fxand (#%$stencil-vector-mask n)
                                   (fx- (fxsll 1 (fx+ bit HAMT-CHILD-OFFSET)) 1)))))

(define (bnode-key-ref n bit)
  (#%$stencil-vector-ref n (fxpopcount32
                            (fxand (#%$stencil-vector-mask n)
                                   (fx- (fxsll 1 (fx+ bit HAMT-KEY-OFFSET)) 1)))))

(define (bnode-val-ref n bit)
  (let ([mask-bit (fxsll 1 (fx+ bit HAMT-VAL-OFFSET))]
        [mask (#%$stencil-vector-mask n)])
    (if (not (fxlogtest mask-bit mask))
        #t ; not stored => #t
        (#%$stencil-vector-ref n (fxpopcount
                                  (fxand mask (fx- mask-bit 1)))))))

;; assumes no children
(define (bnode-only-key-ref n)
  (#%$stencil-vector-ref n HAMT-STATIC-FIELD-COUNT))

;; assumes no children
(define (bnode-only-val-ref n)
  (if (not (fxlogtest (#%$stencil-vector-mask n) HAMT-VAL-MASK))
      #t ; not stored => #t
      (#%$stencil-vector-ref n (fx+ 1 HAMT-STATIC-FIELD-COUNT))))

;; i counts from 0 through children + keys
(define (bnode-child-index-ref n i)
  (#%$stencil-vector-ref n (fx+ HAMT-CHILD-OFFSET i)))

;; i counts from 0 through children + keys, so it encodes the number of preceding children
(define (bnode-key-index-ref n i)
  (#%$stencil-vector-ref n (fx+ HAMT-CHILD-OFFSET i)))

;; i counts from 0 through children + keys
(define (bnode-val-index-ref n i)
  (let* ([mask (#%$stencil-vector-mask n)]
         [val-mask (fxand mask HAMT-VAL-MASK)])
    (cond
     [(fx= 0 val-mask)
      ;; All values in this node are implicitly #t
      #t]
     [(fx= (fxsrl val-mask (fx- HAMT-VAL-OFFSET HAMT-KEY-OFFSET))
           (fxand mask HAMT-KEY-MASK))
      ;; All keys supplied, so the value-relative index
      ;; matches the key-relative index
      (let* ([child-count (hamt-mask->child-count mask)]
             [key-count (hamt-mask->key-count mask)]
             [val-i (fx- i child-count)]) ; same as key index
        (bnode-val-local-index-ref n child-count key-count val-i))]
     [else
      ;; Complicated case that we expect to be rare: figure out how many
      ;; previous keys have values, since we don't know how the key/value
      ;; index maps to a key/value bit
      (let* ([child-count (hamt-mask->child-count mask)]
             [key-count (hamt-mask->key-count mask)]
             [key-i (fx- i child-count)])
        (let loop ([i 0] [val-i 0] [bit HAMT-KEY-OFFSET])
          (cond
           [(fxbit-set? mask bit)
            ;; Found a key
            (if (fxbit-set? mask (fx+ bit (fx- HAMT-VAL-OFFSET HAMT-KEY-OFFSET)))
                ;; Also found a value:
                (if (= i key-i)
                    (bnode-val-local-index-ref n child-count key-count val-i)
                    (loop (fx+ i 1) (fx+ val-i 1) (fx+ bit 1)))
                ;; Did not find a value
                (if (= i key-i)
                    #t ; implicit #t
                    (loop (fx+ i 1) val-i (fx+ bit 1))))]
           [else
            (loop i val-i (fx+ bit 1))])))])))

(define (bnode-val-local-index-ref n child-count key-count val-i)
  (#%$stencil-vector-ref n (fx+ val-i
                                HAMT-STATIC-FIELD-COUNT
                                child-count
                                key-count)))

(define (bnode-add-key node wrapped-key val bit)
  (if (eq? val #t)
      (#%$system-stencil-vector-update node
                                       HAMT-COUNT+EQTYPE-BIT
                                       (fxior HAMT-COUNT+EQTYPE-BIT
                                              (fxsll 1 (fx+ bit HAMT-KEY-OFFSET)))
                                       (fx+ (#%$stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                            ONE-COUNT-IN-COUNT+EQTYPE)
                                       wrapped-key)
      (#%$system-stencil-vector-update node
                                       HAMT-COUNT+EQTYPE-BIT
                                       (fxior HAMT-COUNT+EQTYPE-BIT
                                              (fxsll 1 (fx+ bit HAMT-KEY-OFFSET))
                                              (fxsll 1 (fx+ bit HAMT-VAL-OFFSET)))
                                       (fx+ (#%$stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                            ONE-COUNT-IN-COUNT+EQTYPE)
                                       wrapped-key
                                       val)))

(define (bnode-remove-key node bit)
  (let ([val-bit (fxsll 1 (fx+ bit HAMT-VAL-OFFSET))])
    (#%$system-stencil-vector-update node
                                     (fxior HAMT-COUNT+EQTYPE-BIT
                                            (fxsll 1 (fx+ bit HAMT-KEY-OFFSET))
                                            (fxand (#%$stencil-vector-mask node) val-bit))
                                     HAMT-COUNT+EQTYPE-BIT
                                     (fx- (#%$stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                          ONE-COUNT-IN-COUNT+EQTYPE))))

(define (bnode-replace-val node bit val)
  (let ([val-bit (fxsll 1 (fx+ bit HAMT-VAL-OFFSET))])
     (cond
      [(not (fxlogtest (#%$stencil-vector-mask node) val-bit))
       ;; old value was #t
       (cond
        [(eq? val #t)
         node]
        [else
         (#%$system-stencil-vector-update node 0 val-bit val)])]
      [else
       (cond
        [(eq? val #t)
         (#%$system-stencil-vector-update node val-bit 0)]
        [else
         (#%$system-stencil-vector-update node val-bit val-bit val)])])))

(define (bnode-replace-key+val node bit wrapped-key val)
  (let* ([key-bit (fxsll 1 (fx+ bit HAMT-KEY-OFFSET))]
         [val-bit (fxsll 1 (fx+ bit HAMT-VAL-OFFSET))]
         [key+val-bits (fxior key-bit val-bit)])
    (cond
     [(not (fxlogtest (#%$stencil-vector-mask node) val-bit))
      ;; old value was #t
      (cond
       [(eq? val #t)
        (#%$system-stencil-vector-update node key-bit key-bit wrapped-key)]
       [else
        (#%$system-stencil-vector-update node key-bit key+val-bits wrapped-key val)])]
     [else
      (cond
       [(eq? val #t)
        (#%$system-stencil-vector-update node key+val-bits key-bit wrapped-key)]
       [else
        (#%$system-stencil-vector-update node key+val-bits key+val-bits wrapped-key val)])])))

(define (bnode-remove-key-add-child node child bit)
  (let ([val-bit (fxsll 1 (fx+ bit HAMT-VAL-OFFSET))])
    (#%$system-stencil-vector-update node
                                     (fxior HAMT-COUNT+EQTYPE-BIT
                                            (fxsll 1 (fx+ bit HAMT-KEY-OFFSET))
                                            (fxand (#%$stencil-vector-mask node) val-bit))
                                     (fxior HAMT-COUNT+EQTYPE-BIT
                                            (fxsll 1 (fx+ bit HAMT-CHILD-OFFSET)))
                                     (fx+ (#%$stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                          ONE-COUNT-IN-COUNT+EQTYPE)
                                     child)))

(define (bnode-remove-child-add-key node wrapped-key val bit)
  (cond
   [(eq? val #t)
    (#%$system-stencil-vector-update node
                                     (fxior HAMT-COUNT+EQTYPE-BIT
                                            (fxsll 1 (fx+ bit HAMT-CHILD-OFFSET)))
                                     (fxior HAMT-COUNT+EQTYPE-BIT
                                            (fxsll 1 (fx+ bit HAMT-KEY-OFFSET)))
                                     (fx- (#%$stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                          ONE-COUNT-IN-COUNT+EQTYPE)
                                     wrapped-key)]
   [else
    (#%$system-stencil-vector-update node
                                     (fxior HAMT-COUNT+EQTYPE-BIT
                                            (fxsll 1 (fx+ bit HAMT-CHILD-OFFSET)))
                                     (fxior HAMT-COUNT+EQTYPE-BIT
                                            (fxsll 1 (fx+ bit HAMT-KEY-OFFSET))
                                            (fxsll 1 (fx+ bit HAMT-VAL-OFFSET)))
                                     (fx- (#%$stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                          ONE-COUNT-IN-COUNT+EQTYPE)
                                     wrapped-key
                                     val)]))

(define (bnode-replace-child node old-child new-child bit)
  (let ([child-bit (fxsll 1 (fx+ bit HAMT-CHILD-OFFSET))]
        [delta (cond
                [(and (bnode? old-child)
                      (bnode? new-child))
                 (fx- (hamt-count+eqtype new-child)
                      (hamt-count+eqtype old-child))]
                [else
                 (fxsll (fx- (node-count new-child)
                             (node-count old-child))
                        HAMT-COUNT-OFFSET)])])
    (cond
     [(fx= 0 delta)
      (#%$system-stencil-vector-update node child-bit child-bit new-child)]
     [else
      (let ([bits (fxior child-bit
                         HAMT-COUNT+EQTYPE-BIT)])
        (#%$system-stencil-vector-update node
                                         bits
                                         bits
                                         (fx+ (#%$stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                              delta)
                                         new-child))])))

(define HASHCODE-BITS (fxbit-count (most-positive-fixnum)))

(define (node-merge eqtype
                    wrapped-k1 k1 v1 h1
                    wrapped-k2 k2 v2 h2
                    shift)
  (cond
   [(and (fx< HASHCODE-BITS shift)
         (fx= h1 h2))
    (pariah
     ;; hash collision: make a cnode
     (make-cnode h1
                 (list (cons k1 v1)
                       (cons k2 v2))))]

   [else
    (let ([m1 (bnode-mask h1 shift)]
          [m2 (bnode-mask h2 shift)])
      (cond
       [(fx= m1 m2)
        ;; partial collision: descend
        (let* ([child (node-merge eqtype wrapped-k1 k1 v1 h1 wrapped-k2 k2 v2 h2 (bnode-down shift))]
               [cm (bnode-bit-pos h1 shift)])
          (#%$system-stencil-vector (fxior HAMT-COUNT+EQTYPE-BIT
                                           (fxsll 1 (fx+ cm HAMT-CHILD-OFFSET)))
                                    (count+eqtype 2 eqtype)
                                    child))]

       [else
        ;; no collision, make a bnode
        (let ([bit1 (bnode-bit-pos h1 shift)]
              [bit2 (bnode-bit-pos h2 shift)]
              [k1 wrapped-k1]
              [k2 wrapped-k2])
          (let ([finish
                 (lambda (k1 v1 bit1 k2 v2 bit2)
                   (let ([key-bits (fxior (fxsll 1 (fx+ bit1 HAMT-KEY-OFFSET))
                                          (fxsll 1 (fx+ bit2 HAMT-KEY-OFFSET)))])
                     (cond
                      [(eq? v1 #t)
                       (cond
                        [(eq? v2 #t)
                         (#%$system-stencil-vector (fxior HAMT-COUNT+EQTYPE-BIT
                                                          key-bits)
                                                   (count+eqtype 2 eqtype)
                                                   k1 k2)]
                        [else
                         (#%$system-stencil-vector (fxior HAMT-COUNT+EQTYPE-BIT
                                                          key-bits
                                                          (fxsll 1 (fx+ bit2 HAMT-VAL-OFFSET)))
                                                   (count+eqtype 2 eqtype)
                                                   k1 k2
                                                   v2)])]
                      [else
                       (cond
                        [(eq? v2 #t)
                         (#%$system-stencil-vector (fxior HAMT-COUNT+EQTYPE-BIT
                                                          key-bits
                                                          (fxsll 1 (fx+ bit1 HAMT-VAL-OFFSET)))
                                                   (count+eqtype 2 eqtype)
                                                   k1 k2
                                                   v1)]
                        [else
                         (#%$system-stencil-vector (fxior HAMT-COUNT+EQTYPE-BIT
                                                          key-bits
                                                          (fxsll 1 (fx+ bit1 HAMT-VAL-OFFSET))
                                                          (fxsll 1 (fx+ bit2 HAMT-VAL-OFFSET)))
                                                   (count+eqtype 2 eqtype)
                                                   k1 k2
                                                   v1 v2)])])))])
            (if (fx<= bit1 bit2)
                (finish k1 v1 bit1 k2 v2 bit2)
                (finish k2 v2 bit2 k1 v1 bit1))))]))]))

;; Should only be called three times to create the canonical empty
;; hashes:
(define (make-empty-bnode eqtype)
  (#%$system-stencil-vector HAMT-COUNT+EQTYPE-BIT
                            (count+eqtype 0 eqtype)))

;; intmap interface

(define empty-hasheq (make-empty-bnode HAMT-EQTYPE-EQ))
(define empty-hasheqv (make-empty-bnode HAMT-EQTYPE-EQV))
(define empty-hash (make-empty-bnode HAMT-EQTYPE-EQUAL))
;; hashalw is for equal ALWays, first 3 letters of "always" since "equal" is implicit
(define empty-hashalw (make-empty-bnode HAMT-EQTYPE-EQUAL-ALWAYS))

(define intmap-shell-falses (let loop ([n (fx* 2 HAMT-WIDTH)])
                              (if (fx= n 0)
                                  '()
                                  (cons #f (loop (fx- n 1))))))

(define (make-intmap-shell eqtype-sym)
  ;; a shell is a maximally sized node that claims to have 0 items
  (let ([mask (fx- (fxsll 1 (fx+ HAMT-STATIC-FIELD-COUNT
                                 (fx* 2 HAMT-WIDTH)))
                   1)]
        [eqtype (case eqtype-sym
                  [(eq)  HAMT-EQTYPE-EQ]
                  [(eqv) HAMT-EQTYPE-EQV]
                  [(equal-always) HAMT-EQTYPE-EQUAL-ALWAYS]
                  [else  HAMT-EQTYPE-EQUAL])])
    (#%apply #%$system-stencil-vector mask eqtype intmap-shell-falses)))

(define (intmap-shell-sync! dest src)
  (let ([mask (#%$stencil-vector-mask src)])
    (#%$system-stencil-vector-truncate! dest mask)
    (let loop ([i (fx- (fxpopcount mask) 1)])
      (#%$stencil-vector-set! dest i (#%$stencil-vector-ref src i))
      (unless (fx= i 0)
        (loop (fx- i 1))))))

(define (intmap-eq? h)
  (eq? (hamt-eqtype h) HAMT-EQTYPE-EQ))

(define (intmap-eqv? h)
  (eq? (hamt-eqtype h) HAMT-EQTYPE-EQV))

(define (intmap-equal? h)
  (eq? (hamt-eqtype h) HAMT-EQTYPE-EQUAL))

(define (intmap-equal-always? h)
  (eq? (hamt-eqtype h) HAMT-EQTYPE-EQUAL-ALWAYS))

(define (intmap-count h)
  (hamt-count h))

(define (node-count h)
  (if (bnode? h)
      (hamt-count h)
      (length (cnode-content h))))

(define (intmap-empty? h)
  (fxzero? (hamt-count h)))

(define-syntax (eqtype-dispatch stx)
  (syntax-case stx ()
    [(_ h [id ...] e)
     (let ([prefix (lambda (prefix e)
                     (let loop ([e e])
                       (cond
                        [(#%identifier? e)
                         (if (#%memq (#%syntax->datum e) (map #%syntax->datum #'(id ...)))
                             (datum->syntax e
                                            (#%string->symbol
                                             (string-append (#%symbol->string prefix)
                                                            (#%symbol->string (syntax->datum e)))))
                             e)]
                        [else
                         (syntax-case e ()
                           [(a . b)
                            #`(#,(loop #'a) . #,(loop #'b))]
                           [_ e])])))])
       (with-syntax ([eq:e (prefix 'eq: #'e)]
                     [eqv:e (prefix 'eqv: #'e)]
                     [equal:e (prefix 'equal: #'e)]
                     [equal-always:e (prefix 'equal-always: #'e)])
         #'(let ([et (hamt-eqtype h)])
             (cond
              [(fx= et HAMT-EQTYPE-EQ)
               eq:e]
              [(fx= et HAMT-EQTYPE-EQV)
               eqv:e]
              [(fx= et HAMT-EQTYPE-EQUAL-ALWAYS)
               equal-always:e]
              [else
               equal:e]))))]
    [(_ h (f arg ...))
     #'(eqtype-dispatch h [f] (f arg ...))]))

(define (intmap-has-key? h key)
  (eqtype-dispatch
   h [bnode-has-key? bnode-key-hash-code]
   (bnode-has-key? h key (bnode-key-hash-code key) 0)))

(define (intmap-ref h key default)
  (let ([count+eqtype (hamt-count+eqtype h)])
    (cond
     [(fx= 0 (count+eqtype-count count+eqtype))
      ;; Access on an empty HAMT is common, so don't even hash in that case
      default]
     [else
      (let ([eqtype (count+eqtype-eqtype count+eqtype)])
        (cond
         [(fx= eqtype HAMT-EQTYPE-EQ)
          (eq:bnode-ref h key (eq:bnode-key-hash-code key) 0 default)]
         [(fx= eqtype HAMT-EQTYPE-EQV)
          (eqv:bnode-ref h key (eqv:bnode-key-hash-code key) 0 default)]
         [(fx= eqtype HAMT-EQTYPE-EQUAL-ALWAYS)
          (equal-always:bnode-ref h key (equal-always:bnode-key-hash-code key) 0 default)]
         [else
          (equal:bnode-ref h key (equal:bnode-key-hash-code key) 0 default)]))])))

(define (intmap-ref-key h key default)
  (cond
   [(intmap-empty? h)
    default]
   [else
    (eqtype-dispatch
     h [bnode-ref-key bnode-key-hash-code]
     (bnode-ref-key h key (bnode-key-hash-code key) 0 default))]))

(define (intmap-set h key val)
  (eqtype-dispatch
   h [bnode-set bnode-key-hash-code]
   (bnode-set h key val (bnode-key-hash-code key) 0)))

(define (intmap-remove h key)
  (eqtype-dispatch
   h [bnode-remove bnode-key-hash-code]
   (bnode-remove h key (bnode-key-hash-code key) 0)))

;; ----------------------------------------
;; generic iteration by counting

(define (intmap-iterate-first h)
  (and (not (intmap-empty? h))
       0))

(define (intmap-iterate-next h pos)
  (let ([pos (fx1+ pos)])
    (and (not (fx= pos (intmap-count h)))
         pos)))

(define (intmap-iterate-key h pos fail)
  (eqtype-dispatch
   h
   (bnode-entry-at-position h pos 'key fail)))

(define (intmap-iterate-value h pos fail)
  (eqtype-dispatch
   h
   (bnode-entry-at-position h pos 'val fail)))

(define (intmap-iterate-key+value h pos fail)
  (eqtype-dispatch
   h
   (bnode-entry-at-position h pos 'both fail)))

(define (intmap-iterate-pair h pos fail)
  (eqtype-dispatch
   h
   (bnode-entry-at-position h pos 'pair fail)))

;; ----------------------------------------
;; unsafe iteration; position is a stack
;; of the form
;;   - '()
;;   - (cons indent (cons node stack))
;;   - (cons (box assoc-list) stack)

(define (unsafe-intmap-iterate-first h)
  (and (not (intmap-empty? h))
       (unsafe-node-iterate-first h '())))

(define (unsafe-node-iterate-first n stack)
  (cond
   [(bnode? n)
    (let ([mask (#%$stencil-vector-mask n)])
      (let ([child-count (hamt-mask->child-count mask)]
            [key-count (hamt-mask->key-count mask)])
        (let ([stack (cons (fx+ key-count child-count -1) (cons n stack))])
          (if (fx= key-count 0)
              (unsafe-node-iterate-first (bnode-child-index-ref n (fx- child-count 1)) stack)
              stack))))]
   [(cnode? n)
    (cons (#%box (cnode-content n))
          stack)]))

(define (unsafe-intmap-iterate-next h pos)
  (unsafe-node-iterate-next pos))

(define (unsafe-node-iterate-next pos)
  (cond
   [(null? pos)
    ;; Stack is empty, so we're done
    #f]
   [else
    (let ([i (car pos)]
          [stack (cdr pos)])
      (cond
       [(fixnum? i)
        (let ([n (car stack)])
          (cond
           [(fx= 0 i)
            ;; Exhausted this node, so return to parent node
            (unsafe-node-iterate-next (cdr stack))]
           [else
            ;; Move to next (lower) index in the current node
            (let ([i (fx1- i)])
              (let ([child-count (hamt-mask->child-count (#%$stencil-vector-mask n))]
                    [stack (cons i stack)])
                (if (fx< i child-count)
                    (unsafe-node-iterate-first (bnode-child-index-ref n i) stack)
                    stack)))]))]
       [else
        ;; in a cnode
        (let ([new-p (cdr (#%unbox i))])
          (if (null? new-p)
              ;; Exhausted this node, so return to parent node
              (unsafe-node-iterate-next stack)
              ;; still in cnode:
              (cons (#%box new-p) stack)))]))]))

(define (unsafe-intmap-iterate-key h pos)
  (eqtype-dispatch
   h
   (bnode-unsafe-intmap-iterate-key pos)))

(define (unsafe-intmap-iterate-value h pos)
  (eqtype-dispatch
   h
   (bnode-unsafe-intmap-iterate-value pos)))

(define (unsafe-intmap-iterate-key+value h pos)
  (eqtype-dispatch
   h
   (bnode-unsafe-intmap-iterate-key+value pos)))

(define (unsafe-intmap-iterate-pair h pos)
  (eqtype-dispatch
   h
   (bnode-unsafe-intmap-iterate-pair pos)))

(define (intmap=? a b eql?)
  (and (fx= (hamt-count+eqtype a)
            (hamt-count+eqtype b))
       (or (intmap-empty? a) ; explicit test in case `a` or `b` is a shell
           (eqtype-dispatch
            a
            (bnode=? a b eql? 0)))))

(define (intmap-keys-subset? a b)
  (or (intmap-empty? a)
      (eqtype-dispatch
       a
       (bnode-keys-subset? a b 0))))

(define (intmap-hash-code a hash)
  (cond
   [(intmap-empty? a) ;; explicit test in `a` is a shell
    (hamt-count+eqtype a)]
   [else
    (eqtype-dispatch
     a
     (bnode-hash-code a hash 0))]))

(define (intmap-for-each h proc)
  (eqtype-dispatch
   h
   (bnode-fold h (lambda (k v _) (|#%app| proc k v) (void)) (void))))

(define (intmap-map h proc)
  (eqtype-dispatch
   h [bnode-fold]
   (#%reverse (bnode-fold h (lambda (k v xs) (cons (|#%app| proc k v) xs)) '()))))

;; ----------------------------------------
;; eqtype-paramerized definitions

(define-syntax-rule (define-bnode-for-eqtype
                      
                      ;; exports:
                      bnode-key-hash-code
                      bnode-ref
                      bnode-ref-key
                      bnode-has-key?
                      bnode-set
                      bnode-remove
                      bnode-entry-at-position
                      bnode-unsafe-intmap-iterate-key
                      bnode-unsafe-intmap-iterate-value
                      bnode-unsafe-intmap-iterate-key+value
                      bnode-unsafe-intmap-iterate-pair
                      bnode=?
                      bnode-keys-subset?
                      bnode-hash-code
                      bnode-fold

                      ;; imports:
                      hamt-key-eqtype
                      hamt-wrap-key
                      hamt-unwrap-key
                      hamt-key=?
                      hamt-unwrapped-key=?
                      hamt-wrapped-key=?
                      hamt-key-hash-code
                      hamt-wrapped-key-hash-code)
  
  (begin

    (define (bnode-key-hash-code k)
      (hamt-key-hash-code k))

    (define (bnode-ref node key keyhash shift default)
      (let ([bit (bnode-bit-pos keyhash shift)])
        (cond
         [(bnode-maps-key? node bit)
          (let* ([k (bnode-key-ref node bit)])
            (if (hamt-key=? key keyhash k)
                (bnode-val-ref node bit)
                default))]

         [(bnode-maps-child? node bit)
          (let* ([c (bnode-child-ref node bit)])
            (cond
             [(bnode? c)
              (bnode-ref c key keyhash (bnode-down shift) default)]
             [else
              (cnode-ref c key keyhash default)]))]

         [else
          default])))

    (define (bnode-ref-key node key keyhash shift default)
      (let ([bit (bnode-bit-pos keyhash shift)])
        (cond
         [(bnode-maps-key? node bit)
          (let ([k (bnode-key-ref node bit)])
            (if (hamt-key=? key keyhash k)
                (hamt-unwrap-key k)
                default))]

         [(bnode-maps-child? node bit)
          (let* ([c (bnode-child-ref node bit)])
            (cond
             [(bnode? c)
              (bnode-ref-key c key keyhash (bnode-down shift) default)]
             [else
              (cnode-ref-key c key keyhash default)]))]

         [else
          default])))

    (define (bnode-has-key? node key keyhash shift)
      (let ([bit (bnode-bit-pos keyhash shift)])
        (cond
         [(bnode-maps-key? node bit)
          (let ([k (bnode-key-ref node bit)])
            (hamt-key=? key keyhash k))]

         [(bnode-maps-child? node bit)
          (let* ([c (bnode-child-ref node bit)])
            (cond
             [(bnode? c)
              (bnode-has-key? c key keyhash (bnode-down shift))]
             [else
              (cnode-has-key? c key keyhash)]))]

         [else #f])))

    (define (bnode-set node key val keyhash shift)
      (let ([bit (bnode-bit-pos keyhash shift)])
        (cond
         [(bnode-maps-key? node bit)
          (let* ([k (bnode-key-ref node bit)]
                 [v (bnode-val-ref node bit)])
            (cond
             [(hamt-key=? key keyhash k)
              ;; For consistency, we're required to discard the old key and keep the new one
              (if (eq? key k)
                  (if (eq? val v)
                      node
                      (bnode-replace-val node bit val))
                  (bnode-replace-key+val node bit (hamt-wrap-key key keyhash) val))]
             [else
              (let* ([h (hamt-wrapped-key-hash-code node k)]
                     [child (node-merge hamt-key-eqtype
                                        k (hamt-unwrap-key k) v h
                                        (hamt-wrap-key key keyhash) key val keyhash
                                        (bnode-down shift))])
                (bnode-remove-key-add-child node child bit))]))]

         [(bnode-maps-child? node bit)
          (let* ([child (bnode-child-ref node bit)]
                 [new-child (cond
                             [(bnode? child)
                              (bnode-set child key val keyhash (bnode-down shift))]
                             [else
                              (cnode-set child key val keyhash)])])
            (if (eq? new-child child)
                node
                (bnode-replace-child node child new-child bit)))]

         [else
          (bnode-add-key node (hamt-wrap-key key keyhash) val bit)])))

    (define (bnode-remove node key keyhash shift)
      (let ([bit (bnode-bit-pos keyhash shift)])

        (cond
         [(bnode-maps-key? node bit)
          (let* ([k (bnode-key-ref node bit)])
            (cond
             [(hamt-key=? key keyhash k)
              (let ([mask (#%$stencil-vector-mask node)])
                (cond
                 [(and (fx= (fxand mask HAMT-KEY-MASK) (fxsll 1 (fx+ bit HAMT-KEY-OFFSET)))
                       (fxzero? (fxand mask HAMT-CHILD-MASK)))
                  ;; return canonical empty value
                  (eqtype-case
                   node
                   [(eq)  empty-hasheq]
                   [(eqv) empty-hasheqv]
                   [(equal-always) empty-hashalw]
                   [else  empty-hash])]
                 [else
                  (bnode-remove-key node bit)]))]
             [else
              node]))]

         [(bnode-maps-child? node bit)
          (let* ([child (bnode-child-ref node bit)]
                 [new-child (cond
                             [(bnode? child)
                              (bnode-remove child key keyhash (bnode-down shift))]
                             [else
                              (cnode-remove child key keyhash)])])
            (cond
             [(eq? new-child child) node]
             [(and (bnode? new-child)
                   (fx= 1 (hamt-count new-child)))
              ;; Replace child with its sole key and value
              (bnode-remove-child-add-key node (bnode-only-key-ref new-child) (bnode-only-val-ref new-child) bit)]
             [(and (cnode? new-child)
                   (null? (cdr (cnode-content new-child))))
              ;; Replace child with its sole key and value
              (let ([p (car (cnode-content new-child))])
                (bnode-remove-child-add-key node (hamt-wrap-key (car p) (cnode-hash new-child)) (cdr p) bit))]
             [else
              (bnode-replace-child node child new-child bit)]))]

         [else
          node])))

    (define (cnode-ref node key keyhash default)
      (cond
       [(fx= keyhash (cnode-hash node))
        (let ([p (cnode-assoc (cnode-content node) key)])
          (if p
              (cdr p)
              default))]
       [else default]))

    (define (cnode-ref-key node key keyhash default)
      (cond
       [(fx= keyhash (cnode-hash node))
        (let ([p (cnode-assoc (cnode-content node) key)])
          (if p
              (car p)
              default))]
       [else default]))

    (define (cnode-has-key? n key keyhash)
      (and (fx= keyhash (cnode-hash n))
           (cnode-assoc (cnode-content n) key)
           #t))

    (define (cnode-assoc alist key)
      (cond
       [(null? alist) #f]
       [(hamt-unwrapped-key=? key (caar alist))
        (car alist)]
       [else (cnode-assoc (cdr alist) key)]))

    (define (cnode-set node key val keyhash)
      (make-cnode (cnode-hash node)
                  (cnode-assoc-update (cnode-content node) key (cons key val))))

    (define (cnode-remove node key keyhash)
      (make-cnode (cnode-hash node)
                  (cnode-assoc-update (cnode-content node) key #f)))

    (define (cnode-assoc-update alist key new-p)
      (cond
       [(null? alist) (if new-p
                          (list new-p)
                          null)]
       [(hamt-unwrapped-key=? key (caar alist))
        (if new-p
            (cons new-p (cdr alist))
            (cdr alist))]
       [else
        (cons (car alist)
              (cnode-assoc-update (cdr alist) key new-p))]))

    ;; `a` and `b` must both be cnodes
    (define (cnode=? a b eql?)
       (and
        (cnode-keys-subset/equal? a b eql?)
        (fx= (length (cnode-content a))
             (length (cnode-content b)))))

    ;; `a` and `b` must both be cnodes
    (define (cnode-keys-subset/equal? a b eql?)
      (or
       (eq? a b)
       (and
        (fx= (cnode-hash a) (cnode-hash b))
        (let ([ac (cnode-content a)]
              [bc (cnode-content b)])
          (let loop ([ac ac])
            (cond
             [(null? ac) #t]
             [else
              (let ([p (cnode-assoc bc (caar ac))])
                (and p
                     (or (not eql?)
                         (and
                          (eql? (caar ac) (car p)) ; needed for `equal?/recur`
                          (eql? (cdar ac) (cdr p))))
                     (loop (cdr ac))))]))))))

    (define (bnode-entry-at-position n pos mode fail)
      (let* ([mask (#%$stencil-vector-mask n)]
             [child-count (hamt-mask->child-count mask)]
             [key-count (hamt-mask->key-count mask)])
        (cond
         [(fx< pos key-count)
          (let ([get-key (lambda () (hamt-unwrap-key (bnode-key-index-ref n (fx+ pos child-count))))]
                [get-value (lambda () (bnode-val-index-ref n (fx+ pos child-count)))])
            (case mode
              [(key) (get-key)]
              [(val) (get-value)]
              [(both) (values (get-key) (get-value))]
              [else (cons (get-key) (get-value))]))]
         [else
          (let loop ([i 0] [pos (fx- pos key-count)])
            (cond
             [(fx= i child-count)
              fail]
             [else
              (let ([c (bnode-child-index-ref n i)])
                (cond
                 [(bnode? c)
                  (let ([sz (hamt-count c)])
                    (if (fx>= pos sz)
                        (loop (fx+ i 1) (fx- pos sz))
                        (bnode-entry-at-position c pos mode fail)))]
                 [else
                  (let* ([alist (cnode-content c)]
                         [len (length alist)])
                    (if (fx>= pos len)
                        (loop (fx+ i 1) (fx- pos len))
                        (let ([p (list-ref alist pos)])
                          (case mode
                            [(key) (car p)]
                            [(val) (cdr p)]
                            [(both) (values (car p) (cdr p))]
                            [else p]))))]))]))])))

    (define (bnode-unsafe-intmap-iterate-key pos)
      (let ([i (car pos)])
        (cond
         [(fixnum? i)
          (let ([h (cadr pos)])
            (hamt-unwrap-key (bnode-key-index-ref h i)))]
         [else
          ;; in a cnode
          (caar (#%unbox i))])))

    (define (bnode-unsafe-intmap-iterate-value pos)
      (let ([i (car pos)])
        (cond
         [(fixnum? i)
          (bnode-val-index-ref (cadr pos) i)]
         [else
          ;; in a cnode
          (cdar (#%unbox i))])))

    (define (bnode-unsafe-intmap-iterate-key+value pos)
      (let ([i (car pos)])
        (cond
         [(fixnum? i)
          (let ([n (cadr pos)])
            (values (hamt-unwrap-key (bnode-key-index-ref n i))
                    (bnode-val-index-ref n i)))]
         [else
          ;; in a cnode
          (let ([pr (car (#%unbox i))])
            (values (car pr) (cdr pr)))])))

    (define (bnode-unsafe-intmap-iterate-pair pos)
      (let ([i (car pos)])
        (cond
         [(fixnum? i)
          (let ([n (cadr pos)])
            (cons (hamt-unwrap-key (bnode-key-index-ref n i))
                  (bnode-val-index-ref n i)))]
         [else
          ;; in a cnode
          (car (#%unbox i))])))

    (define (bnode=? a b eql? shift)
      (or
       (eq? a b)
       (and
        (fx= (hamt-count a) (hamt-count b))
        (let ([a-mask (#%$stencil-vector-mask a)]
              [b-mask (#%$stencil-vector-mask b)]) 
          (and
           (fx= a-mask b-mask)
           (let ([child-count (hamt-mask->child-count a-mask)])
             (let loop ([i 0])
               (cond
                [(fx= i child-count)
                 (let ([key-count (hamt-mask->key-count a-mask)])
                   (let loop ([j 0])
                     (cond
                      [(fx= j key-count) #t]
                      [else
                       (let ([i (fx+ j child-count)])
                         (let ([ak (bnode-key-index-ref a i)]
                               [bk (bnode-key-index-ref b i)])
                           (and (hamt-wrapped-key=? ak bk)
                                (eql? (hamt-unwrap-key ak) (hamt-unwrap-key bk)) ; needed for `equal?/recur`
                                (eql? (bnode-val-index-ref a i) (bnode-val-index-ref b i))
                                (loop (fx+ j 1)))))])))]
                [else
                 (let ([an (bnode-child-index-ref a i)]
                       [bn (bnode-child-index-ref b i)])
                   (and (or (eq? an bn)
                            (cond
                             [(bnode? an)
                              (and (bnode? b)
                                   (bnode=? an bn eql? (bnode-down shift)))]
                             [else
                              ;; `bn` must be a cnode, too
                              (cnode=? an bn eql?)]))
                        (loop (fx+ i 1))))]))))))))

    ;; `a` and `b` must both be bnodes
    (define (bnode-keys-subset? a b shift)
      (cond
       [(eq? a b) #t]
       [(fx> (hamt-count a) (hamt-count b)) #f]
       [else
        (let* ([a-mask (#%$stencil-vector-mask a)]
               [akm (fxand (fxsrl a-mask HAMT-KEY-OFFSET) HAMT-GROUP-MASK)]
               [acm (fxand (fxsrl a-mask HAMT-CHILD-OFFSET) HAMT-GROUP-MASK)]
               [abm (fxior acm akm)]
               [b-mask (#%$stencil-vector-mask b)]
               [bcm (fxand (fxsrl b-mask HAMT-CHILD-OFFSET) HAMT-GROUP-MASK)]
               [bkm (fxand (fxsrl b-mask HAMT-KEY-OFFSET) HAMT-GROUP-MASK)]
               [bbm (fxior bcm bkm)])
          (and
           (fx= abm (fxand abm bbm))
           ;; At this point, we know that bbm has a bit for every key/child in
           ;; `b`, and we know that `a` has a key/child only where `b` does
           (let loop ([bm bbm] [aki (fxpopcount16 acm)] [bki (fxpopcount16 bcm)] [aci 0] [bci 0])
             (cond
              [(fxzero? bm) #t]
              [else
               (let ([bm-bit (fxand bm (fxxor bm (fx- bm 1)))]) ; peel off lowest set bit of `bm`
                 (cond
                  [(not (fxlogtest bm-bit abm))
                   ;; No key or child in `a`
                   (cond
                    [(not (fxlogtest bm-bit bcm))
                     ;; Key in `b`
                     (loop (fx- bm bm-bit) aki (fx1+ bki) aci bci)]
                    [else
                     ;; Child in `b`
                     (loop (fx- bm bm-bit) aki bki aci (fx1+ bci))])]
                  [(not (fxlogtest bm-bit acm))
                   ;; Key in `a`
                   (cond
                    [(not (fxlogtest bm-bit bcm))
                     ;; Key in `b`
                     (and
                      (hamt-wrapped-key=? (bnode-key-index-ref a aki) (bnode-key-index-ref b bki))
                      (loop (fx- bm bm-bit) (fx1+ aki) (fx1+ bki) aci bci))]
                    [else
                     ;; Child in `b`
                     (and
                      (let ([akey (bnode-key-index-ref a aki)]
                            [bchild (bnode-child-index-ref b bci)])
                        (cond
                         [(bnode? bchild)
                          (bnode-has-key? bchild (hamt-unwrap-key akey) (hamt-wrapped-key-hash-code a akey) (bnode-down shift))]
                         [else
                          (cnode-has-key? bchild (hamt-unwrap-key akey) (hamt-wrapped-key-hash-code a akey))]))
                      (loop (fx- bm bm-bit) (fx1+ aki) bki aci (fx1+ bci)))])]
                  [else
                   ;; Child in `a`
                   (cond
                    [(not (fxlogtest bm-bit bcm))
                     ;; Key in `b`, and multiple keys in `a` child means `a` is not a subset
                     #f]
                    [else
                     ;; Child in `b`
                     (and (let ([ac (bnode-child-index-ref a aci)]
                                [bc (bnode-child-index-ref b bci)])
                            ;; Because a cnode is always at the end of the deepest
                            ;; possible chain of bnodes, `ac` and `bc` must both be
                            ;; the same kind of node
                            (cond
                             [(bnode? ac)
                              (bnode-keys-subset? ac bc (bnode-down shift))]
                             [else
                              (cnode-keys-subset/equal? ac bc #f)]))
                          (loop (fx- bm bm-bit) aki bki (fx1+ aci) (fx1+ bci)))])]))]))))]))

    (define (bnode-hash-code n hash hc)
      (let* ([mask (#%$stencil-vector-mask n)]
             [hc (hash-code-combine hc mask)]
             [child-count (hamt-mask->child-count mask)]
             [key-count (hamt-mask->key-count mask)]
             [val-count (hamt-mask->val-count mask)])
        (let loop ([i 0] [hc hc])
          (cond
           [(fx< i child-count)
            (loop (fx1+ i)
                  (let ([c (bnode-child-index-ref n i)])
                    (cond
                     [(bnode? c)
                      (bnode-hash-code c hash hc)]
                     [else
                      ;; Hash code needs to be order-independent, so
                      ;; collision nodes are a problem; simplify by just
                      ;; using the hash code and hope that collisions are
                      ;; rare.
                      (hash-code-combine hc (cnode-hash c))])))]
           [else
            (let ([offset (fx+ HAMT-STATIC-FIELD-COUNT child-count key-count)])
              (let loop ([i 0] [hc hc])
                (cond
                  [(fx< i val-count)
                   (loop (fx1+ i)
                         (hash-code-combine hc (hash (#%$stencil-vector-ref n (fx+ offset i)))))]
                  [else hc])))]))))

    (define (bnode-fold n f nil)
      (let* ([mask (#%$stencil-vector-mask n)]
             [child-count (hamt-mask->child-count mask)]
             [key-count (hamt-mask->key-count mask)])
        (let loop ([i 0] [nil nil])
          (cond
           [(fx= i child-count)
            (let loop ([i 0] [nil nil])
              (cond
               [(fx= i key-count)
                nil]
               [else
                (loop (fx+ i 1)
                      (f (hamt-unwrap-key (bnode-key-index-ref n (fx+ i child-count)))
                         (bnode-val-index-ref n (fx+ i child-count))
                         nil))]))]
           [else
            (let ([c (bnode-child-index-ref n i)])
              (cond
               [(bnode? c)
                (loop (fx+ i 1)
                      (bnode-fold c f nil))]
               [else
                (let aloop ([alist (cnode-content c)] [nil nil])
                  (cond
                   [(null? alist) (loop (fx+ i 1) nil)]
                   [else
                    (let ([rest-alist (cdr alist)])
                      (aloop rest-alist
                             (f (caar alist)
                                (cdar alist)
                                nil)))]))]))]))))))

(define-syntax (define-prefixed-bnode-for-eqtype stx)
  (syntax-case stx (hamt-wrap-key
                    hamt-unwrap-key
                    hamt-key=?
                    hamt-unwrapped-key=?
                    hamt-wrapped-key=?
                    hamt-key-hash-code
                    hamt-wrapped-key-hash-code)
    [(_ p:
        hamt-key-eqtype hamt-key-eqtype-impl
        hamt-wrap-key hamt-wrap-key-impl
        hamt-unwrap-key hamt-unwrap-key-impl
        hamt-key=? hamt-key=?-impl
        hamt-unwrapped-key=? hamt-unwrapped-key=?-impl
        hamt-wrapped-key=? hamt-wrapped-key=?-impl
        hamt-key-hash-code hamt-key-hash-code-impl
        hamt-wrapped-key-hash-code hamt-wrapped-key-hash-code-impl)
     (let ([prefixed (lambda (s)
                       (datum->syntax #'p:
                                      (#%string->symbol
                                       (string-append (#%symbol->string (syntax->datum #'p:))
                                                      (#%symbol->string s)))))])
       (with-syntax ([p:bnode-key-hash-code (prefixed 'bnode-key-hash-code)]
                     [p:bnode-ref (prefixed 'bnode-ref)]
                     [p:bnode-ref-key (prefixed 'bnode-ref-key)]
                     [p:bnode-has-key? (prefixed 'bnode-has-key?)]
                     [p:bnode-set (prefixed 'bnode-set)]
                     [p:bnode-remove (prefixed 'bnode-remove)]
                     [p:bnode-entry-at-position (prefixed 'bnode-entry-at-position)]
                     [p:bnode-unsafe-intmap-iterate-key (prefixed 'bnode-unsafe-intmap-iterate-key)]
                     [p:bnode-unsafe-intmap-iterate-value (prefixed 'bnode-unsafe-intmap-iterate-value)]
                     [p:bnode-unsafe-intmap-iterate-key+value (prefixed 'bnode-unsafe-intmap-iterate-key+value)]
                     [p:bnode-unsafe-intmap-iterate-pair (prefixed 'bnode-unsafe-intmap-iterate-pair)]
                     [p:bnode=? (prefixed 'bnode=?)]
                     [p:bnode-keys-subset? (prefixed 'bnode-keys-subset?)]
                     [p:bnode-hash-code (prefixed 'bnode-hash-code)]
                     [p:bnode-fold (prefixed 'bnode-fold)])
         #'(define-bnode-for-eqtype
             ;; exports:
             p:bnode-key-hash-code
             p:bnode-ref
             p:bnode-ref-key
             p:bnode-has-key?
             p:bnode-set
             p:bnode-remove
             p:bnode-entry-at-position
             p:bnode-unsafe-intmap-iterate-key
             p:bnode-unsafe-intmap-iterate-value
             p:bnode-unsafe-intmap-iterate-key+value
             p:bnode-unsafe-intmap-iterate-pair
             p:bnode=?
             p:bnode-keys-subset?
             p:bnode-hash-code
             p:bnode-fold

             ;; imports:
             hamt-key-eqtype-impl
             hamt-wrap-key-impl
             hamt-unwrap-key-impl
             hamt-key=?-impl
             hamt-unwrapped-key=?-impl
             hamt-wrapped-key=?-impl
             hamt-key-hash-code-impl
             hamt-wrapped-key-hash-code-impl)))]))

;; ----------------------------------------

(define-prefixed-bnode-for-eqtype
  eq:
  hamt-key-eqtype HAMT-EQTYPE-EQ
  hamt-wrap-key (lambda (k k-hash) k)
  hamt-unwrap-key (lambda (k) k)
  hamt-key=? (lambda (k1 k1-hash wrapped-k2) (eq? k1 wrapped-k2))
  hamt-unwrapped-key=? (lambda (k1 k2) (eq? k1 k2))
  hamt-wrapped-key=? (lambda (k1 k2) (eq? k1 k2))
  hamt-key-hash-code (lambda (k) (eq-hash-code k))
  hamt-wrapped-key-hash-code (lambda (n k) (eq-hash-code k)))

(define-prefixed-bnode-for-eqtype
  eqv:
  hamt-key-eqtype HAMT-EQTYPE-EQV
  hamt-wrap-key (lambda (k k-hash) k)
  hamt-unwrap-key (lambda (k) k)
  hamt-key=? (lambda (k1 k1-hash wrapped-k2) (eqv? k1 wrapped-k2))
  hamt-unwrapped-key=? (lambda (k1 k2) (eqv? k1 k2))
  hamt-wrapped-key=? (lambda (k1 k2) (eqv? k1 k2))
  hamt-key-hash-code (lambda (k) (eqv-hash-code k))
  hamt-wrapped-key-hash-code (lambda (n k) (eqv-hash-code k)))

(define (equal:hamt-wrap-key k k-hash)
  (if (fast-equal-hash-code? k) ; must not include pairs
      k
      (cons k-hash k)))

(define (equal:hamt-unwrap-key k)
  (if (pair? k) (cdr k) k))

;; second key is wrapped
(define (equal:hamt-key=? k1 k1-hash wrapped-k2)
  (if (pair? wrapped-k2)
      (and (fx= k1-hash (car wrapped-k2))
           (key-equal? k1 (cdr wrapped-k2)))
      (key-equal? k1 wrapped-k2)))

(define (equal:hamt-unwrapped-key=? k1 k2)
  (key-equal? k1 k2))

(define (equal:hamt-wrapped-key=? k1 k2)
  (cond
   [(pair? k1)
    (cond
     [(pair? k2)
      (and (fx= (car k1) (car k2))
           (key-equal? (cdr k1) (cdr k2)))]
     [else (key-equal? (cdr k1) k2)])]
   [else
    (cond
     [(pair? k2)
      (key-equal? k1 (cdr k2))]
     [else (key-equal? k1 k2)])]))

(define (equal:hamt-key-hash-code k)
  (key-equal-hash-code k))

(define (equal:hamt-wrapped-key-hash-code n k)
  (if (pair? k)
      (car k)
      (key-equal-hash-code k)))

(define-prefixed-bnode-for-eqtype
  equal:
  hamt-key-eqtype HAMT-EQTYPE-EQUAL
  hamt-wrap-key equal:hamt-wrap-key
  hamt-unwrap-key equal:hamt-unwrap-key
  hamt-key=? equal:hamt-key=?
  hamt-unwrapped-key=? equal:hamt-unwrapped-key=?
  hamt-wrapped-key=? equal:hamt-wrapped-key=?
  hamt-key-hash-code equal:hamt-key-hash-code
  hamt-wrapped-key-hash-code equal:hamt-wrapped-key-hash-code)

(define equal-always:hamt-wrap-key equal:hamt-wrap-key)
(define equal-always:hamt-unwrap-key equal:hamt-unwrap-key)

;; second key is wrapped
(define (equal-always:hamt-key=? k1 k1-hash wrapped-k2)
  (if (pair? wrapped-k2)
      (and (fx= k1-hash (car wrapped-k2))
           (key-equal-always? k1 (cdr wrapped-k2)))
      (key-equal-always? k1 wrapped-k2)))

(define (equal-always:hamt-unwrapped-key=? k1 k2)
  (key-equal-always? k1 k2))

(define (equal-always:hamt-wrapped-key=? k1 k2)
  (cond
   [(pair? k1)
    (cond
     [(pair? k2)
      (and (fx= (car k1) (car k2))
           (key-equal-always? (cdr k1) (cdr k2)))]
     [else (key-equal-always? (cdr k1) k2)])]
   [else
    (cond
     [(pair? k2)
      (key-equal-always? k1 (cdr k2))]
     [else (key-equal-always? k1 k2)])]))

(define (equal-always:hamt-key-hash-code k)
  (key-equal-always-hash-code k))

(define (equal-always:hamt-wrapped-key-hash-code n k)
  (if (pair? k)
      (car k)
      (key-equal-always-hash-code k)))

(define-prefixed-bnode-for-eqtype
  equal-always:
  hamt-key-eqtype HAMT-EQTYPE-EQUAL-ALWAYS
  hamt-wrap-key equal-always:hamt-wrap-key
  hamt-unwrap-key equal-always:hamt-unwrap-key
  hamt-key=? equal-always:hamt-key=?
  hamt-unwrapped-key=? equal-always:hamt-unwrapped-key=?
  hamt-wrapped-key=? equal-always:hamt-wrapped-key=?
  hamt-key-hash-code equal-always:hamt-key-hash-code
  hamt-wrapped-key-hash-code equal-always:hamt-wrapped-key-hash-code)
