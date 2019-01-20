;; HAMT

;; the absence of something
(define NOTHING (gensym 'nothing))

;; 16-bit popcount
(define (popcount x)
  (let* ([x (fx- x (fxand (fxsrl x 1) #x5555))]
         [x (fx+ (fxand x #x3333) (fxand (fxsrl x 2) #x3333))]
         [x (fxand (fx+ x (fxsrl x 4)) #x0f0f)]
         [x (fx+ x (fxsrl x 8))])
    (fxand x #x1f)))

;; record types
(define-record-type hnode
  [fields (immutable eqtype)
          (mutable count)
          (mutable keys)
          (mutable vals)]
  [nongenerative #{hnode pfwh8wvaevt3r6pcwsqn90ry8-0}])

(meta-cond
 [(> (most-positive-fixnum) (expt 2 32))

  ;; 64-bit bnode (pack the bitmaps into a single fixnum)
  (define-record-type (bnode make-raw-bnode bnode?)
    [parent hnode]
    [fields (mutable bitmap)]
    [nongenerative #{bnode pfwhzqkm2ycuuyedzz2nxjx2e-0}]
    [sealed #t])

  (define (make-bnode eqtype count keys vals keymap childmap)
    (let ([bitmap (fxior keymap (fxsll childmap 16))])
      (make-raw-bnode eqtype count keys vals bitmap)))

  (define (bnode-keymap n)
    (fxand #xffff (bnode-bitmap n)))

  (define (bnode-childmap n)
    (fxsrl (bnode-bitmap n) 16))

  (define (bnode-copy-bitmaps! dest src)
    (bnode-bitmap-set! dest (bnode-bitmap src)))]

 [else

  ;; 32-bit bnode (separate bitmaps)
  (define-record-type bnode
    [parent hnode]
    [fields (mutable keymap)
            (mutable childmap)]
    [nongenerative #{bnode pfwhzqkm2ycuuyedzz2nxjx2e-1}]
    [sealed #t])

  (define (bnode-copy-bitmaps! dest src)
    (bnode-set-keymap! dest (bnode-keymap src))
    (bnode-set-childmap! dest (bnode-childmap src)))])

(define-record-type cnode
  [parent hnode]
  [fields (immutable hash)]
  [nongenerative #{cnode pfwh0bwrq2nqlke97ikru0ds2-0}]
  [sealed #t])

(define (make-empty-bnode eqtype)
  (make-bnode eqtype
              0
              (vector)
              #f
              0
              0))

(define empty-hasheq (make-empty-bnode 'eq))
(define empty-hasheqv (make-empty-bnode 'eqv))
(define empty-hash (make-empty-bnode 'equal))

(define (make-hamt-shell eqtype)
  (make-empty-bnode eqtype))

(define (hamt-shell-sync! dest src)
  (hnode-count-set! dest (hnode-count src))
  (hnode-keys-set! dest (hnode-keys src))
  (hnode-vals-set! dest (hnode-vals src))
  (bnode-copy-bitmaps! dest src))

;; hamt interface
(define hamt? hnode?)
(define immutable-hash? hnode?)

(define (hamt-eq? h)
  (eq? (hnode-eqtype h) 'eq))

(define (hamt-eqv? h)
  (eq? (hnode-eqtype h) 'eqv))

(define (hamt-equal? h)
  (eq? (hnode-eqtype h) 'equal))

(define (hamt-has-key? h key)
  (node-has-key? h key (hash-code h key) 0))

(define (node-has-key? n key keyhash shift)
  (cond [(bnode? n) (bnode-has-key? n key keyhash shift)]
        [else       (cnode-has-key? n key)]))

(define (hamt-ref h key default)
  (cond
   [(hamt-empty? h)
    ;; Access on an empty HAMT is common, so don't even hash in that case
    (if (procedure? default)
        (default)
        default)]
   [else
    (let ([res (bnode-ref h key (hash-code h key) 0)])
      (if (eq? res NOTHING)
          (if (procedure? default)
              (default)
              default)
          res))]))

(define (hamt-set h key val)
  (bnode-set h key val (hash-code h key) 0))

(define (hamt-remove h key)
  (bnode-remove h key (hash-code h key) 0))

(define (hamt-count h)
  (hnode-count h))

(define (hamt-empty? h)
  (fxzero? (hamt-count h)))

(define (hamt=? a b eql?)
  (and (eq? (hnode-eqtype a)
            (hnode-eqtype b))
       (node=? a b eql? 0)))

(define (hamt-hash-code a hash)
  (node-hash-code a hash 0))

(define ignored/hamt
  (begin
    ;; Go through generic `hash` versions to support `a`
    ;; and `b` as impersonated hash tables
    (record-type-equal-procedure (record-type-descriptor bnode)
                                 (lambda (a b eql?)
                                   (hash=? a b eql?)))
    (record-type-hash-procedure (record-type-descriptor bnode)
                                (lambda (a hash)
                                  (hash-hash-code a hash)))))

(define (hamt-keys-subset? a b)
  (or (hamt-empty? a)
      (node-keys-subset? a b 0)))

(define (hamt-foldk h f nil kont)
  (bnode-foldk h f nil kont))

(define (hamt-fold h nil fn)
  (hamt-foldk
   h
   (lambda (key val nil k)
     (k (fn key val nil)))
   nil
   (lambda (x) x)))

(define (hamt->list h)
  (hamt-fold h '() (lambda (k v xs) (cons (cons k v) xs))))

(define (hamt-keys h)
  (hamt-fold h '() (lambda (k _ xs) (cons k xs))))

(define (hamt-values h)
  (hamt-fold h '() (lambda (_ v xs) (cons v xs))))

(define (hamt-for-each h proc)
  (hamt-fold h (void) (lambda (k v _) (proc k v) (void))))

(define (hamt-map h proc)
  (hamt-fold h '() (lambda (k v xs) (cons (proc k v) xs))))

;; generatic iteration by counting
(define (hamt-iterate-first h)
  (and (not (hamt-empty? h))
       0))

(define (hamt-iterate-next h pos)
  (let ([pos (fx1+ pos)])
    (and (not (fx= pos (hamt-count h)))
         pos)))

(define (hamt-iterate-key h pos fail)
  (let ([p (node-entry-at-position h pos)])
    (if p
        (car p)
        fail)))

(define (hamt-iterate-value h pos fail)
  (let ([p (node-entry-at-position h pos)])
    (if p
        (cdr p)
        fail)))

(define (hamt-iterate-key+value h pos fail)
  (let ([p (node-entry-at-position h pos)])
    (if p
        (values (car p) (cdr p))
        fail)))

(define (hamt-iterate-pair h pos fail)
  (let ([p (node-entry-at-position h pos)])
    (or p fail)))

;; unsafe iteration; position is a stack
;; represented by a list of (cons node index)
(define (unsafe-hamt-iterate-first h)
  (and (not (hamt-empty? h))
       (unsafe-node-iterate-first h '())))

(define (unsafe-node-iterate-first n stack)
  (cond
   [(bnode? n)
    (let ([i (fx1- (#%vector-length (hnode-keys n)))]
          [key-count (popcount (bnode-keymap n))])
      (let ([stack (cons (cons n i) stack)])
        (if (fx>= i key-count)
            (unsafe-node-iterate-first (key-ref n i) stack)
            stack)))]
   [(cnode? n)
    (let ([i (fx1- (#%vector-length (hnode-keys n)))])
      (cons (cons n i) stack))]))

(define (unsafe-hamt-iterate-next h pos)
  (unsafe-node-iterate-next pos))

(define (unsafe-node-iterate-next pos)
  (cond
   [(null? pos)
    ;; Stack is empty, so we're done
    #f]
   [else
    (let ([p (car pos)]
          [stack (cdr pos)])
      (let ([n (car p)]
            [i (cdr p)])
        (cond
         [(fx= 0 i)
          ;; Exhausted this node, so return to parent node
          (unsafe-node-iterate-next stack)]
         [else
          ;; Move to next (lower) index in the current node
          (let ([i (fx1- i)])
            (cond
             [(bnode? n)
              (let ([key-count (popcount (bnode-keymap n))]
                    [stack (cons (cons n i) stack)])
                (if (fx>= i key-count)
                    (unsafe-node-iterate-first (key-ref n i) stack)
                    stack))]
             [(cnode? n)
              (cons (cons n i) stack)]))])))]))

(define (unsafe-hamt-iterate-key h pos)
  (let ([p (car pos)])
    (key-ref (car p) (cdr p))))

(define (unsafe-hamt-iterate-value h pos)
  (let ([p (car pos)])
    (val-ref (car p) (cdr p))))

(define (unsafe-hamt-iterate-key+value h pos)
  (let ([p (car pos)])
    (let ([n (car p)]
          [i (cdr p)])
      (values (key-ref n i)
              (val-ref n i)))))

(define (unsafe-hamt-iterate-pair h pos)
  (let ([p (car pos)])
    (let ([n (car p)]
          [i (cdr p)])
      (cons (key-ref n i)
            (val-ref n i)))))

;; constants
(define HASHCODE-BITS (fxbit-count (most-positive-fixnum)))
(define BNODE-BITS 4)
(define BNODE-MASK (fx1- (fxsll 1 BNODE-BITS)))

;; vector operations
(define (vector-insert v i x)
  (let* ([len (#%vector-length v)]
         [new (#%make-vector (fx1+ len))])
    (vector*-copy! new 0 v 0 i)
    (#%vector-set! new i x)
    (vector*-copy! new (fx1+ i) v i len)
    new))

(define (vector-remove v i)
  (let* ([len (#%vector-length v)]
         [new (#%make-vector (fx1- len))])
    (vector*-copy! new 0 v 0 i)
    (vector*-copy! new i v (fx1+ i) len)
    new))

;; hnode operations
(define (key=? n k1 k2)
  (case (hnode-eqtype n)
    [(eq)  (eq? k1 k2)]
    [(eqv) (eqv? k1 k2)]
    [else  (key-equal? k1 k2)]))

(define (hash-code n k)
  (case (hnode-eqtype n)
    [(eq)  (eq-hash-code k)]
    [(eqv) (eqv-hash-code k)]
    [else  (key-equal-hash-code k)]))

(define (key-ref n i)
  (#%vector-ref (hnode-keys n) i))

(define (val-ref n i)
  (let ([vals (hnode-vals n)])
    (or (not vals)
        (#%vector-ref vals i))))

(define (node-ref n key keyhash shift)
  (cond [(bnode? n) (bnode-ref n key keyhash shift)]
        [else       (cnode-ref n key)]))

(define (node-set n key val keyhash shift)
  (cond [(bnode? n) (bnode-set n key val keyhash shift)]
        [else       (cnode-set n key val)]))

(define (node-remove n key keyhash shift)
  (cond [(bnode? n) (bnode-remove n key keyhash shift)]
        [else       (cnode-remove n key keyhash)]))

(define (node-singleton? node)
  (fx= (hnode-count node) 1))

(define (node=? a b eql? shift)
  (or (eq? a b)
      (and (fx= (hnode-count a) (hnode-count b))
           (cond [(bnode? a) (bnode=? a b eql? shift)]
                 [else       (cnode=? a b eql?)]))))

(define (node-hash-code n hash hc)
  (cond
   [(bnode? n)
    (let* ([bm (fxior (bnode-keymap n) (bnode-childmap n))]
           [hc (hash-code-combine hc bm)]
           [len (#%vector-length (hnode-keys n))]
           [key-count (popcount (bnode-keymap n))])
      (let loop ([i 0] [hc hc])
        (cond
         [(fx= i len) hc]
         [else
          (let ([x (key-ref n i)])
            (cond
             [(fx>= i key-count)
              (loop (fx1+ i)
                    (node-hash-code x hash hc))]
             [else
              (loop (fx1+ i)
                    (hash-code-combine hc (hash (val-ref n i))))]))])))]
   [else
    ;; Hash code needs to be order-independent, so
    ;; collision nodes are a problem; simplify by just
    ;; using the hash code and hope that collisions are
    ;; rare.
    (hash-code-combine hc (cnode-hash n))]))

(define (node-keys-subset? a b shift)
  (or (eq? a b)
      (and (fx<= (hnode-count a) (hnode-count b))
           (cond [(bnode? a) (bnode-keys-subset? a b shift)]
                 [else       (cnode-keys-subset? a b shift)]))))

(define (node-entry-at-position n pos)
  (cond [(bnode? n) (bnode-entry-at-position n pos)]
        [else       (cnode-entry-at-position n pos)]))

(define (node-foldk n f nil kont)
  (cond [(bnode? n) (bnode-foldk n f nil kont)]
        [else       (cnode-foldk n f nil kont)]))

;; bnode operations
(define (bnode-ref node key keyhash shift)
  (let ([bit (bnode-bit-pos keyhash shift)])
    (cond
     [(bnode-maps-key? node bit)
      (let* ([ki (bnode-key-index node bit)]
             [k (key-ref node ki)])
        (if (key=? node key k)
            (val-ref node ki)
            NOTHING))]

     [(bnode-maps-child? node bit)
      (let* ([ci (bnode-child-index node bit)]
             [c (child-ref node ci)])
        (node-ref c key keyhash (down shift)))]

     [else
      NOTHING])))

(define (bnode-has-key? n key keyhash shift)
  (not (eq? NOTHING (bnode-ref n key keyhash shift))))

(define (bnode-set node key val keyhash shift)
  (let ([bit (bnode-bit-pos keyhash shift)])

    (cond
     [(bnode-maps-key? node bit)
      (let* ([ki (bnode-key-index node bit)]
             [k (key-ref node ki)]
             [v (val-ref node ki)])
        (cond
         [(key=? node key k)
          (if (eq? val v)
              node
              (bnode-replace-val node ki val))]
         [else
          (let* ([h (hash-code node k)]
                 [eqtype (hnode-eqtype node)]
                 [child (node-merge eqtype k v h key val keyhash (down shift))])
            (bnode-add-child node child ki bit))]))]

     [(bnode-maps-child? node bit)
      (let* ([ci (bnode-child-index node bit)]
             [child (child-ref node ci)]
             [new-child (node-set child key val keyhash (down shift))])
        (if (eq? new-child child)
            node
            (bnode-replace-child node child new-child ci)))]

     [else
      (bnode-add-key node key val bit)])))

(define (bnode-remove node key keyhash shift)
  (let ([bit (bnode-bit-pos keyhash shift)])

    (cond
     [(bnode-maps-key? node bit)
      (let* ([ki (bnode-key-index node bit)]
             [k (key-ref node ki)])
        (cond
         [(key=? node key k)
          (let ([km (bnode-keymap node)]
                [cm (bnode-childmap node)])
            (if (and (fx= (popcount km) 2)
                     (fxzero? cm))
                (bnode-singleton node ki bit keyhash shift)
                (bnode-remove-key node ki bit)))]
         [else
          node]))]

     [(bnode-maps-child? node bit)
      (let* ([ci (bnode-child-index node bit)]
             [child (child-ref node ci)]
             [new-child (node-remove child key keyhash (down shift))])
        (cond
         [(eq? new-child child) node]
         [(node-singleton? new-child)
          (if (and (fxzero? (bnode-childmap node))
                   (fx= (popcount (bnode-keymap node)) 1))
              new-child
              (bnode-remove-child node new-child ci bit))]
         [else
          (bnode-replace-child node child new-child ci)]))]

     [else
      node])))

(define (bnode=? a b eql? shift)
  (and
   (bnode? b)
   (fx= (bnode-keymap a) (bnode-keymap b))
   (fx= (bnode-childmap a) (bnode-childmap b))

   (let* ([keys (hnode-keys a)]
          [len (#%vector-length keys)]
          [key-count (popcount (bnode-keymap a))])
     (let loop ([i 0])
       (cond
        [(fx= i len) #t]
        [else
         (let ([ak (key-ref a i)]
               [bk (key-ref b i)])
           (and
            (cond
             [(fx>= i key-count)
              (node=? ak bk eql? (down shift))]
             [else
              (and (key=? a ak bk)
                   (eql? (val-ref a i) (val-ref b i)))])
            (loop (fx1+ i))))])))))

(define (bnode-keys-subset? a b shift)
  (cond
   [(bnode? b)
    (let* ([akm (bnode-keymap a)]
	   [bkm (bnode-keymap b)]
	   [acm (bnode-childmap a)]
	   [bcm (bnode-childmap b)]
	   [abm (fxior akm acm)]
	   [bbm (fxior bkm bcm)])
      (and
       (fx= abm (fxand abm bbm))

       (let loop ([abm abm] [bit 0] [aki 0] [bki 0] [aci 0] [bci 0])
	 (cond
	  [(fxzero? abm) #t]
	  [(fxbit-set? akm bit)
	   (cond
	    [(fxbit-set? bkm bit)
	     (and
	      (key=? a (key-ref a aki) (key-ref b bki))
	      (loop (fxsrl abm 1) (fx1+ bit) (fx1+ aki) (fx1+ bki) aci bci))]
	    [else
	     (and
	      (let ([akey (key-ref a aki)]
		    [bchild (child-ref b bci)])
		(node-has-key? bchild akey (hash-code a akey) (down shift)))
	      (loop (fxsrl abm 1) (fx1+ bit) (fx1+ aki) bki aci (fx1+ bci)))])]
	  [(fxbit-set? acm bit)
	   (cond
	    [(fxbit-set? bkm bit) #f]
	    [else
	     (and
	      (node-keys-subset? (child-ref a aci) (child-ref b bci) (down shift))
	      (loop (fxsrl abm 1) (fx1+ bit) aki bki (fx1+ aci) (fx1+ bci)))])]
	  [(fxbit-set? bkm bit)
	   (loop (fxsrl abm 1) (fx1+ bit) aki (fx1+ bki) aci bci)]
	  [(fxbit-set? bcm bit)
	   (loop (fxsrl abm 1) (fx1+ bit) aki bki aci (fx1+ bci))]
	  [else
	   (loop (fxsrl abm 1) (fx1+ bit) aki bki aci bci)]))))]

   [else
    (let* ([akeys (hnode-keys a)]
           [len (#%vector-length akeys)])
      (and (fx= len 1)
           (let ([x (#%vector-ref akeys 0)])
             (if (fx= 0 (bnode-keymap a))
                 (node-keys-subset? x b (down shift))
                 (not (not (cnode-index b x)))))))]))

(define (bnode-bit-pos hash shift)
  (fxsll 1 (bnode-mask hash shift)))

(define (bnode-mask hash shift)
  (fxand (fxsrl hash shift) BNODE-MASK))

(define (bnode-maps-key? node bit)
  (bnode-maps-bit? (bnode-keymap node) bit))

(define (bnode-maps-child? node bit)
  (bnode-maps-bit? (bnode-childmap node) bit))

(define (bnode-maps-bit? bitmap bit)
  (not (fxzero? (fxand bitmap bit))))

(define (bnode-index bitmap bit)
  (popcount (fxand bitmap (fx1- bit))))

(define (bnode-key-index node bit)
  (bnode-index (bnode-keymap node) bit))

(define (bnode-child-index node bit)
  (bnode-index (bnode-childmap node) bit))

(define (child-ref n i)
  (let ([keys (hnode-keys n)])
    (#%vector-ref keys (fx- (#%vector-length keys) 1 i))))

(define (down shift)
  (fx+ shift BNODE-BITS))

(define (bnode-add-key node key val bit)
  (let* ([keys (hnode-keys node)]
         [vals (hnode-vals node)]
         [ki (bnode-key-index node bit)]
         [new-keys (vector-insert keys ki key)]
         [new-vals
          (cond
           [vals (vector-insert vals ki val)]
           [(eq? val #t) #f]
           [else ; reify values
            (pariah
             (let* ([pop (popcount (bnode-keymap node))]
                    [v (#%make-vector (fx1+ pop) #t)])
               (#%vector-set! v ki val)
               v))])])

    (make-bnode (hnode-eqtype node)
                (fx1+ (hnode-count node))
                new-keys
                new-vals
                (fxior (bnode-keymap node) bit)
                (bnode-childmap node))))

(define (bnode-remove-key node ki bit)
  (let* ([keys (hnode-keys node)]
         [vals (hnode-vals node)]
         [new-keys (vector-remove keys ki)]
         [new-vals (and vals (vector-remove vals ki))])

    (make-bnode (hnode-eqtype node)
                (fx1- (hnode-count node))
                new-keys
                new-vals
                (fxxor (bnode-keymap node) bit)
                (bnode-childmap node))))

(define (bnode-replace-val node ki val)
  (let* ([vals (hnode-vals node)]
         [new-vals
          (if vals
              (#%vector-copy vals)
              (pariah ; reify values
               (let ([pop (popcount (bnode-keymap node))])
                 (#%make-vector pop #t))))])

    (#%vector-set! new-vals ki val)

    (make-bnode (hnode-eqtype node)
                (hnode-count node)
                (hnode-keys node)
                new-vals
                (bnode-keymap node)
                (bnode-childmap node))))

(define (bnode-add-child node child ki bit)
  ;; We're removing a key from, and adding a child to, node.
  ;; So length stays the same.
  (let* ([keys (hnode-keys node)]
         [vals (hnode-vals node)]
         [len (#%vector-length keys)]
         [new-keys (#%make-vector len)]
         [ci (fx- len 1 (bnode-child-index node bit))])

    (vector*-copy! new-keys 0 keys 0 ki)
    (vector*-copy! new-keys ki keys (fx1+ ki) (fx1+ ci))
    (#%vector-set! new-keys ci child)
    (vector*-copy! new-keys (fx1+ ci) keys (fx1+ ci) len)

    (make-bnode (hnode-eqtype node)
                (fx1+ (hnode-count node))
                new-keys
                (and vals (vector-remove vals ki))
                (fxxor (bnode-keymap node) bit)
                (fxior (bnode-childmap node) bit))))

;; `child` is a singleton.
;; `lci` is the logical child index; the physical index is computed below.
(define (bnode-remove-child node child lci bit)
  (let* ([keys (hnode-keys node)]
         [vals (hnode-vals node)]
         [len (#%vector-length keys)]
         [ci (fx- len 1 lci)]
         [ki (bnode-key-index node bit)]
         [k (key-ref child 0)]
         [v (val-ref child 0)]

         [new-keys
          (let ([cpy (#%make-vector len)])
            (vector*-copy! cpy 0 keys 0 ki)
            (#%vector-set! cpy ki k)
            (vector*-copy! cpy (fx1+ ki) keys ki ci)
            (vector*-copy! cpy (fx1+ ci) keys (fx1+ ci) len)
            cpy)]

         [new-vals
          (cond
           [vals (vector-insert vals ki v)]
           [(eq? v #t) #f]
           [else ; reify values
            (pariah
             (let* ([pop (popcount (bnode-keymap node))]
                    [cpy (#%make-vector (fx1+ pop) #t)])
               (#%vector-set! cpy ki v)
               cpy))])])

    (make-bnode (hnode-eqtype node)
                (fx1- (hnode-count node))
                new-keys
                new-vals
                (fxior (bnode-keymap node) bit)
                (fxxor (bnode-childmap node) bit))))

(define (bnode-replace-child node old-child new-child ci)
  (let* ([keys (hnode-keys node)]
         [len (#%vector-length keys)]
         [new-keys (vector-copy keys)])
    (#%vector-set! new-keys (fx- len 1 ci) new-child)

    (make-bnode (hnode-eqtype node)
                (fx+ (hnode-count node)
                     (fx- (hnode-count new-child)
                          (hnode-count old-child)))
                new-keys
                (hnode-vals node)
                (bnode-keymap node)
                (bnode-childmap node))))

(define (bnode-singleton node ki bit keyhash shift)
  (let* ([km (bnode-keymap node)]
         [new-km
          ;; I'll admit: I do not understand the false arm of this
          ;; conditional. Shouldn't the new keymap use the hash of
          ;; the key that will remain, rather than the one that's
          ;; being removed?
          (if (fxzero? shift)
              (fxxor km bit)
              (bnode-bit-pos keyhash 0))]
         [idx (if (fxzero? ki) 1 0)]
         [val (val-ref node idx)])

    (make-bnode (hnode-eqtype node)
                1
                (vector (key-ref node idx))
                (if (eq? val #t) #f (vector val))
                new-km
                0)))

(define (node-merge eqtype k1 v1 h1 k2 v2 h2 shift)
  (cond
   [(and (fx< HASHCODE-BITS shift)
         (fx= h1 h2))
    (pariah
     ;; hash collision: make a cnode
     (let ([vals
            (if (and (eq? v1 #t) (eq? v2 #t))
                #f
                (vector v1 v2))])
       (make-cnode eqtype 2 (vector k1 k2) vals h1)))]

   [else
    (let ([m1 (bnode-mask h1 shift)]
          [m2 (bnode-mask h2 shift)])
      (cond
       [(fx= m1 m2)
        ;; partial collision: descend
        (let* ([child (node-merge eqtype k1 v1 h1 k2 v2 h2 (down shift))]
               [count (hnode-count child)]
               [cm (bnode-bit-pos h1 shift)])
          (make-bnode eqtype count (vector child) #f 0 cm))]

       [else
        ;; no collision
        (let ([km (fxior (bnode-bit-pos h1 shift)
                         (bnode-bit-pos h2 shift))])
          (if (and (eq? v1 #t) (eq? v2 #t))
              (if (fx< m1 m2)
                  (make-bnode eqtype 2 (vector k1 k2) #f km 0)
                  (make-bnode eqtype 2 (vector k2 k1) #f km 0))
              (if (fx< m1 m2)
                  (make-bnode eqtype 2 (vector k1 k2) (vector v1 v2) km 0)
                  (make-bnode eqtype 2 (vector k2 k1) (vector v2 v1) km 0))))]))]))

(define (bnode-entry-at-position n pos)
  (let ([kpop (popcount (bnode-keymap n))])
    (cond
     [(fx< pos kpop)
      (cons (key-ref n pos) (val-ref n pos))]
     [else
      (let ([cpop (popcount (bnode-childmap n))])
        (let loop ([i 0] [pos (fx- pos kpop)])
          (cond
           [(fx= i cpop) #f]
           [else
            (let* ([child (child-ref n i)]
                   [count (hnode-count child)])
              (if (fx< pos count)
                  (node-entry-at-position child pos)
                  (loop (fx1+ i) (fx- pos count))))])))])))

(define (bnode-foldk n f nil kont)
  (let ([kpop (popcount (bnode-keymap n))])
    (keys-foldk kpop n f nil
                (lambda (nil) (child-foldk kpop n f nil kont)))))

(define (keys-foldk pop n f nil kont)
  (let loop ([i 0] [nil nil] [kont kont])
    (cond
     [(fx= i pop) (kont nil)]
     [else
      (f (key-ref n i) (val-ref n i) nil
         (lambda (nil) (loop (fx1+ i) nil kont)))])))

(define (child-foldk pop n f nil kont)
  (let* ([keys (hnode-keys n)]
         [len  (#%vector-length keys)])
    (let loop ([i pop] [nil nil] [kont kont])
      (cond
       [(fx= i len) (kont nil)]
       [else
        (node-foldk (#%vector-ref keys i) f nil
                    (lambda (nil) (loop (fx1+ i) nil kont)))]))))

;; cnode operations
(define (cnode-index node key)
  (let* ([keys (hnode-keys node)]
         [len (#%vector-length keys)])
    (let loop ([i 0])
      (cond [(fx= i len) #f]
            [(key=? node key (#%vector-ref keys i)) i]
            [else (loop (fx1+ i))]))))

(define (cnode-ref node key)
  (let ([i (cnode-index node key)])
    (if i
        (val-ref node i)
        NOTHING)))

(define (cnode-has-key? n key)
  (not (not (cnode-index n key))))

(define (cnode-set node key val)
  (let* ([i (cnode-index node key)]
         [keys (hnode-keys node)]
         [vals (hnode-vals node)]
         [len (#%vector-length keys)])
    (if i
        (cnode-replace-val node i val)
        (cnode-add-key node key val))))

(define (cnode-remove node key keyhash)
  (let ([ki (cnode-index node key)]
        [eqtype (hnode-eqtype node)])
    (cond
     [ki
      (case (hnode-count node)
        [(1)
         (make-empty-bnode eqtype)]
        [(2)
         (let ([empty (make-empty-bnode eqtype)]
               [i (if (fx= ki 0) 1 0)])
           (bnode-set empty (key-ref node i) (val-ref node i) keyhash 0))]
        [else
         (make-cnode eqtype
                     (fx1- (hnode-count node))
                     (vector-remove (hnode-keys node) ki)
                     (let ([vals (hnode-vals node)])
                       (and vals (vector-remove vals ki)))
                     (cnode-hash node))])]
     [else
      node])))

(define (cnode=? a b eql?)
  (and
   (cnode? b)
   (fx= (cnode-hash a) (cnode-hash b))
   (let* ([akeys (hnode-keys a)]
          [alen (#%vector-length akeys)])
     (and (let loop ([i 0])
            (cond
             [(fx= i alen) #t]
             [else
              (let* ([akey (key-ref a i)]
                     [bval (cnode-ref b akey)])
                (and
                 (eql? (val-ref a i) bval)
                 (loop (fx1+ i))))]))))))

(define (cnode-keys-subset? a b shift)
  (cond
   [(cnode? b)
    (and (fx= (cnode-hash a) (cnode-hash b))
         (let loop ([i (hnode-count a)])
           (cond
            [(fxzero? i) #t]
            [else
             (and (cnode-index b (key-ref a (fx1- i)))
                  (loop (fx1- i)))])))]
   [else
    (let loop ([i (hnode-count a)])
      (cond
       [(fxzero? i) #t]
       [else
        (let ([k (key-ref a (fx1- i))])
          (and (bnode-has-key? b k (hash-code a k) shift)
               (loop (fx1- i))))]))]))

(define (cnode-replace-val node i val)
  (let ([v (val-ref node i)])
    (cond
     [(eq? v val)
      node]

     [else
      (let* ([keys (hnode-keys node)]
             [vals (hnode-vals node)]
             [len (#%vector-length keys)]
             [new-vals
              (if vals
                  (#%vector-copy vals)
                  (#%make-vector len #t))])
        (#%vector-set! new-vals i val)

        (make-cnode (hnode-eqtype node)
                    (hnode-count node)
                    keys
                    new-vals
                    (cnode-hash node)))])))

(define (cnode-add-key node key val)
  (let* ([keys (hnode-keys node)]
         [vals (hnode-vals node)]
         [len (#%vector-length keys)]
         [new-vals
          (cond
           [vals (vector-insert vals len val)]
           [(eq? val #t) #f]
           [else
            (let ([vec (#%make-vector (fx1+ len) #t)])
              (#%vector-set! vec len val)
              vec)])])

    (make-cnode (hnode-eqtype node)
                (fx1+ (hnode-count node))
                (vector-insert keys len key)
                new-vals
                (cnode-hash node))))

(define (cnode-entry-at-position n pos)
  (and (fx< pos (hnode-count n))
       (cons (key-ref n pos) (val-ref n pos))))

(define (cnode-foldk n f nil kont)
  (keys-foldk (hnode-count n) n f nil kont))
