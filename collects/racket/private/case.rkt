;;----------------------------------------------------------------------
;; case: based on Clinger, "Rapid Case Dispatch in Scheme"
;;       [http://scheme2006.cs.uchicago.edu/07-clinger.pdf]

(module case '#%kernel
  (#%require "small-scheme.rkt" "define.rkt" '#%paramz '#%unsafe
             (for-syntax '#%kernel "stx.rkt" "small-scheme.rkt" "stxcase-scheme.rkt"
                         "qqstx.rkt" "struct.rkt" "define.rkt" "sort.rkt"))

  (#%provide case)

  (define-syntax (case stx)
    (syntax-case stx (else)
      [(_ v)
       (syntax/loc stx (#%expression (begin v (void))))]
      [(_ v [else e1 e2 ...])
       (syntax/loc stx (#%expression (begin v (let-values () e1 e2 ...))))]
      [(self v [(k ...) e1 e2 ...] ...)
       (syntax/loc stx (self v [(k ...) e1 e2 ...] ... [else (void)]))]
      [(_ v [(k ...) e1 e2 ...] ... [else x1 x2 ...])
       (let*-values ([(constant-stx)       #'((k ...) ...)]
                     [(then-stx)           #'([e1 e2 ...] ...)]
                     [(else-stx)           #'(x1 x2 ...)]
                     [(arms constant-hash) (arm-syntax->arms+hash constant-stx)])
         (quasisyntax/loc
             stx
           (let ([tmp v])
             #,(if (< (hash-count constant-hash) *sequential-threshold*)
                   (sequential-case arms #'tmp then-stx else-stx)
                   (dispatch-case constant-hash #'tmp then-stx else-stx)))))]
      [(_ v (bad e1 e2 ...) . rest)
       (raise-syntax-error 
        #f
        "bad syntax (not a datum sequence)"
        stx
        (syntax bad))]
      [(_ v clause . rest)
       (raise-syntax-error
        #f
        "bad syntax (missing expression after datum sequence)"
        stx
        (syntax clause))]
      [(_ . v)
       (not (null? (syntax-e (syntax v))))
       (raise-syntax-error 
        #f
        "bad syntax (illegal use of `.')"
        stx)]))

  (begin-for-syntax
    (define *sequential-threshold* 12)
    (define *hash-threshold*       10)

    (struct interval     (lo hi index))

    (define nothing (gensym))

    (define (arm-syntax->arms+hash stx)
      (define hash (make-hasheqv))
      (define xs   (map syntax->list (syntax->list stx)))
      (define arms
        (let loop ([res '()] [xs xs] [i 1])
          (cond [(null? xs) (reverse res)]
                [else 
                 (let inner ([ks '()] [ys (map syntax->datum (car xs))])
                   (cond [(null? ys)
                          (let ([arm (cons (reverse ks) i)])
                            (loop (cons arm res) (cdr xs) (add1 i)))]
                         [else
                          (let ([y (car ys)])
                            (cond [(eq? (hash-ref hash y nothing) nothing)
                                   (hash-set! hash y i)
                                   (inner (cons y ks) (cdr ys))]
                                  [else
                                   (inner ks (cdr ys))]))]))])))

      (values arms hash))

    (define (sequential-case arms tmp-stx then-stx else-stx)
      (define or-stxes
        (let loop ([res   '()]
                   [kss   (map car arms)]
                   [thens (syntax->list then-stx)])
          (cond [(null? kss)       (reverse res)]
                [(null? (car kss)) (loop res (cdr kss) (cdr thens))]
                [else
                 (let ([stx #`[(or #,@(map (λ (k)
                                             (define eqv-stx (datum-eqv-stx k))
                                             #`(#,eqv-stx #,tmp-stx '#,k))
                                           (car kss)))
                               (let-values ()
                                 #,@(car thens))]])
                   (loop (cons stx res) (cdr kss) (cdr thens)))])))
      #`(cond #,@or-stxes
              [else (let-values () #,@else-stx)]))

    (define (datum-eqv-stx k)
      (if (or (and (number? k) 
                   (not (fixnum? k)))
              (char? k))
          #'eqv?
          #'eq?))
    
    (define (dispatch-case constant-hash tmp-stx then-stx else-stx)      
      #`(let ([index #,(mixed-dispatch tmp-stx constant-hash)])
          #,(index-binary-search #'index then-stx else-stx)))
    
    (define (index-binary-search index-stx arm-stx else-stx)
      (define legs    (list->vector (cons (syntax->list else-stx) 
                                          (syntax->list arm-stx))))
      (define len     (vector-length legs))
      
      (define (go min max)
        (cond [(= min max)
               #`(let-values () #,@(vector-ref legs min))]
              [(= max (add1 min))
               #`(if (unsafe-fx< #,index-stx #,max)
                     (let-values () #,@(vector-ref legs min))
                     (let-values () #,@(vector-ref legs max)))]
              [else
               (let ([mid (quotient (+ min max) 2)])
                 #`(if (unsafe-fx< #,index-stx #,mid)
                       #,(go min (sub1 mid))
                       #,(go mid max)))]))
      
      (go 0 (sub1 len)))
    
    (define (mixed-dispatch val-stx constant-hash)
      (define fixnum-hash (make-hasheq))
      (define symbol-hash (make-hasheq))
      (define char-hash   (make-hasheqv))
      (define other-hash  (make-hasheqv))
        
      (hash-for-each constant-hash
                     (λ (k v)
                       (cond [(fixnum? k)                   (hash-set! fixnum-hash k v)]
                             [(or (symbol? k) (keyword? k)) (hash-set! symbol-hash k v)]
                             [(char? k)                     (hash-set! char-hash   k v)]
                             [else                          (hash-set! other-hash  k v)])))

      #`(cond #,@(if (zero? (hash-count fixnum-hash))
                     #'()
                     #`([(fixnum? #,val-stx) 
                         #,(fixnum-dispatch val-stx fixnum-hash)]))
              #,@(if (zero? (hash-count symbol-hash))
                     #'()
                     #`([(symbol? #,val-stx) 
                         #,(symbol-dispatch val-stx symbol-hash)]))
              #,@(if (zero? (hash-count char-hash))
                     #'()
                     #`([(char? #,val-stx) 
                         #,(char-dispatch val-stx char-hash)]))
              #,@(if (zero? (hash-count other-hash))
                     #'()
                     (other-dispatch val-stx other-hash))
              [else 0]))
    
    (define (fixnum-dispatch val-stx constant-hash)
      
      (define (go intervals lo hi lo-bound hi-bound)
        (define len (length intervals))
        
        (cond [(or (>= lo-bound hi) (<= hi-bound lo))
               #'0]
              
              ;; TODO: Clinger's heuristic, based on code size. Should be tuned for Racket.
              ;; There is one modification: if we only have a single interval left, don't bother
              ;; using a table lookup.
              [(and (> len 1)
                    (< (- hi lo) (* len 5)))
               (fixnum-table-lookup  intervals lo hi lo-bound hi-bound)]
              [else
               (fixnum-binary-search intervals lo hi lo-bound hi-bound)]))
      
          
      (define (fixnum-table-lookup intervals lo hi lo-bound hi-bound)
        (define ref-stx 
          #`(let ([tbl 
                   #,(syntax-local-lift-expression 
                      #`(vector 
                         #,@(apply append
                                   (map (λ (int)
                                          (vector->list
                                           (make-vector (- (interval-hi int) 
                                                           (interval-lo int))
                                                        (interval-index int))))
                                        intervals))))])
              (unsafe-vector*-ref tbl (unsafe-fx- #,val-stx #,lo))))
      
        (cond [(and (<= hi-bound hi) 
                    (>= lo-bound lo))
               ref-stx]
              [(<= hi-bound hi)
               #`(if (unsafe-fx>= #,val-stx #,lo)
                     #,ref-stx
                     0)]
              [(>= lo-bound lo)
               #`(if (unsafe-fx< #,val-stx #,hi)
                     #,ref-stx
                     0)]
              [else
               #`(if (and (unsafe-fx>= #,val-stx #,lo) 
                          (unsafe-fx< #,val-stx #,hi))
                     #,ref-stx
                     0)]))
    
      (define (fixnum-binary-search intervals lo hi lo-bound hi-bound)
        (cond [(null? (cdr intervals)) 
               #`#,(interval-index (car intervals))]
              [else
               (define-values (lo-ints hi-ints) (split-intervals intervals))
               (define-values (lo-lo lo-hi) (lo+hi lo-ints))
               (define-values (hi-lo hi-hi) (lo+hi hi-ints))
             
               #`(if (unsafe-fx< #,val-stx #,hi-lo)
                     #,(go lo-ints lo-lo lo-hi lo-bound hi-lo)
                     #,(go hi-ints hi-lo hi-hi hi-lo hi-bound))]))
      
      (define (split-intervals intervals)
        (define n (quotient (length intervals) 2))
        (let loop ([n n] [lo '()] [hi intervals])
          (cond [(zero? n) (values (reverse lo) hi)]
                [else (loop (sub1 n) (cons (car hi) lo) (cdr hi))])))
      
      (define (lo+hi intervals)
        (define lo (interval-lo (car intervals)))
        (define hi (interval-hi (car (reverse intervals))))
        (values lo hi))
      
      (define intervals (constant-hash->intervals constant-hash))
      (define-values (lo hi) (lo+hi intervals))
      
      #`(if (and (unsafe-fx>= #,val-stx #,lo) 
                 (unsafe-fx< #,val-stx #,hi))
            #,(go intervals lo hi lo hi)
            0))
    
    (define (constant-hash->intervals constant-hash)
      (define xs 
        (sort (hash-map constant-hash cons) < car))
      
      (let loop ([xs xs] [start-key #f] [end-key #f] [cur-val #f] [res '()])
        (cond [(null? xs)
               (if start-key
                   (reverse (cons (interval start-key end-key cur-val) res))
                   '())]
              [else
               (let* ([x (car xs)]
                      [k (car x)]
                      [v (cdr x)])
                 (cond [(not start-key)
                        (loop (cdr xs) k (add1 k) v res)]
                       [(and (= end-key k) (= cur-val v))
                        (loop (cdr xs) start-key (add1 end-key) cur-val res)]
                       [(= end-key k)
                        (let ([interval (interval start-key end-key cur-val)])
                          (loop (cdr xs) k (add1 k) v (cons interval res)))]
                       [else
                        ;; insert an interval for the default
                        (let ([int1 (interval start-key end-key cur-val)]
                              [int2 (interval end-key k 0)])
                          (loop (cdr xs) k (add1 k) v (cons int2 (cons int1 res))))]))])))
      
    (define (symbol-dispatch val-stx constant-hash)
      (if (>= (hash-count constant-hash) *hash-threshold*)
          ;; hash table lookup
          #`(let ([tbl #,(syntax-local-lift-expression
                          #`(make-hasheq '#,(hash-map constant-hash (λ (k v) #`(#,k . #,v)))))])
              (hash-ref tbl #,val-stx 0))
          ;; sequential lookup
          #`(cond #,@(hash-map constant-hash (λ (k v) #`[(eq? #,val-stx (quote #,k)) #,v]))
                  [else 0])))
    
    (define (char-dispatch val-stx constant-hash)
      (define fixnum-hash (make-hasheq))

      (hash-for-each constant-hash
                     (λ (k v)
                       (hash-set! fixnum-hash (char->integer k) v)))

      #`(let ([codepoint (char->integer #,val-stx)])
          #,(fixnum-dispatch #'codepoint fixnum-hash)))
    
    (define (other-dispatch val-stx constant-hash)
      (hash-map constant-hash 
                (λ (k v)
                  (define eqv-stx (datum-eqv-stx k))
                  #`[(#,eqv-stx #,val-stx '#,k) #,v])))))
