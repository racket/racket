#lang racket/base
(require racket/contract
         racket/function
         racket/list
         racket/match
         racket/set

         "enumerator.rkt"
         "lang-struct.rkt"
         "match-a-pattern.rkt"
         "preprocess.rkt"
         "recursive-lang.rkt")

(provide 
 (contract-out
  [lang-enumerators (-> (listof nt?) lang-enum?)]
  [pat-enumerator (-> lang-enum?
                      any/c ;; pattern
                      enum?)]
  [enum-ith (-> enum? exact-nonnegative-integer? any/c)]
  [lang-enum? (-> any/c boolean?)]
  [enum? (-> any/c boolean?)]))

(struct lang-enum (enums unused-var/e))
(struct repeat (n terms) #:transparent)
(struct decomposition (ctx term) #:transparent)
(struct named (name val) #:transparent)
(struct named-t (val term) #:transparent)
(struct mismatch (name val) #:transparent)
(struct mismatch-t (vals term) #:transparent)
(struct named-rep (name) #:transparent)
(struct named-rep-t (n t) #:transparent)

(struct name-ref (name) #:transparent)
(struct mismatch-ref (name) #:transparent)

(struct named-pats (names map) #:transparent
        ) ;; listof symbol and hash symbol -o> (or named, mismatched, named-repeat, mismatch-repeat)

(define enum-ith decode)

(define (lang-enumerators lang)
  (define l-enums (make-hash))
  (define unused-var/e
    (except/e var/e
              (used-vars lang)))
  (define (enumerate-lang cur-lang enum-f)
    (for-each
     (λ (nt)
        (hash-set! l-enums
                   (nt-name nt)
                   (with-handlers ([exn:fail? fail/e])
                     (enum-f (nt-rhs nt)
                             l-enums))))
     cur-lang))
  (let-values ([(fin-lang rec-lang)
                (sep-lang lang)])
    (enumerate-lang fin-lang
                    (λ (rhs enums)
                       (enumerate-rhss rhs enums unused-var/e)))
    (enumerate-lang rec-lang
                    (λ (rhs enums)
                       (thunk/e +inf.f
                                (λ ()
                                   (enumerate-rhss rhs enums unused-var/e)))))

    (lang-enum l-enums unused-var/e)))

(define (pat-enumerator l-enum pat)
  (map/e
   to-term
   ;;identity
   (λ (_)
      (error 'pat-enum "Enumerator is not a  bijection"))
   (pat/e pat
          (lang-enum-enums l-enum)
          (lang-enum-unused-var/e l-enum))))

(define (enumerate-rhss rhss l-enums unused/e)
  (apply sum/e
         (map
          (λ (rhs)
             (pat/e (rhs-pattern rhs)
                    l-enums
                    unused/e))
          rhss)))

(define (pat/e pat l-enums unused/e)
  (define processed (preprocess pat))
  (enum-names processed
              (sep-names processed)
              l-enums
              unused/e))

(define (map-nt-rhs-pat f nonterminal)
  (nt (nt-name nonterminal)
      (map (compose rhs f rhs-pattern)
           (nt-rhs nonterminal))))

;; map-names : (symbol -> symbol), (symbol, symbol -> symbol, symbol), pattern -> pattern
(define (map-names namef repf pat)
  (let loop ([pat pat])
    (match-a-pattern
     pat
     [`any pat]
     [`number pat]
     [`string pat]
     [`natural pat]
     [`integer pat]
     [`real pat]
     [`boolean pat]
     [`variable pat]
     [`(variable-except ,s ...) pat]
     [`(variable-prefix ,s) pat]
     [`variable-not-otherwise-mentioned pat]
     [`hole pat]
     [`(nt ,id) pat]
     [`(name ,n ,pat)
      `(name ,n ,(namef pat))]
     [`(mismatch-name ,n ,pat)
      `(mismatch-name ,n ,(namef pat))]
     [`(in-hole ,p1 ,p2)
      `(in-hole ,(loop p1)
                ,(loop p2))]
     [`(hide-hole ,p)
      `(hide-hole ,(loop p))]
     [`(side-condition ,p ,g ,e) pat] ;; not supported
     [`(cross ,s) pat] ;; not supported
     [`(list ,sub-pats ...)
      `(list
        ,@(map (λ (sub-pat)
                  (match sub-pat
                    [`(repeat ,pat ,name ,mismatch)
                     (let-values ([(new-name new-mis)
                                   (repf name mismatch)])
                       `(repeat ,(loop pat)
                                ,new-name
                                ,new-mis))]
                    [else (loop sub-pat)]))
               sub-pats))]
     [(? (compose not pair?))
      pat])))

;; prepends '_' to all named repeats/mismatch repeats and names all
;; unnamed repeats
(define (name-all-repeats pat)
  (let ([i 0])
    (map-names identity
               (λ (rep mis)
                  (if (or rep mis)
                      (begin0
                          (values i #f)
                        (set! i (+ i 1)))
                      (values rep mis)))
               (prefix-names pat))))

(define (prefix-names pat)
  (let ([prefix
         (λ (s)
            (and s
                 (string->symbol
                  (string-append "_"
                                 (symbol->string s)))))])
    (map-names identity
               (λ (s1 s2)
                  (values (prefix s1)
                          (prefix s2)))
               pat)))

;; sep-names : single-pattern lang -> named-pats
(define (sep-names pat)
  (let loop ([pat pat]
             [named-pats empty-named-pats])
    (match-a-pattern
     pat
     [`any named-pats]
     [`number named-pats]
     [`string named-pats]
     [`natural named-pats]
     [`integer named-pats]
     [`real named-pats]
     [`boolean named-pats]
     [`variable named-pats]
     [`(variable-except ,s ...) named-pats]
     [`(variable-prefix ,s) named-pats]
     [`variable-not-otherwise-mentioned named-pats]
     [`hole named-pats]
     ;; names inside nts are separate
     [`(nt ,id) named-pats]
     [`(name ,n ,pat)
      (loop pat
            (add-named n pat named-pats))]
     [`(mismatch-name ,n ,pat)
      (loop pat
            (add-mismatch n pat named-pats))]
     [`(in-hole ,p1 ,p2)
      (loop p2
            (loop p1 named-pats))]
     [`(hide-hole ,p) (loop p named-pats)]
     [`(side-condition ,p ,g ,e) ;; not supported
      named-pats]
     [`(cross ,s)
      named-pats] ;; not supported
     [`(list ,sub-pats ...)
      (foldl (λ (sub-pat named-pats)
                (match sub-pat
                  [`(repeat ,pat #f #f)
                   (loop pat named-pats)]
                  [`(repeat ,pat ,name #f)
                   ;; Only works if there are no variables inside the repeat
                   (add-named-rep name named-pats)]
                  [`(repeat ,pat ,name ,mismatch)
                   (error 'unimplemented)]
                  [else (loop sub-pat named-pats)]))
             named-pats
             sub-pats)]
     [(? (compose not pair?))
      named-pats])))

;; named-pats combinators
(define empty-named-pats
  (named-pats '() (hash)))

(define (empty-named-pats? nps)
  (null? (named-pats-names nps)))

(define (next-named-pats nps)
  (hash-ref (named-pats-map nps)
            (car (named-pats-names nps))))

(define (rest-named-pats nps)
  (named-pats (cdr (named-pats-names nps))
              (named-pats-map nps)))

(define (member-named-pats name nps)
  (member name (named-pats-names nps)))

(define (add-named name pat nps)
  (cond [(member-named-pats name nps)
         nps]
        [else
         (add-named-pats name (named name pat) nps)]))

(define (add-mismatch n pat nps)
  (cond [(member-named-pats n nps)
         (named-pats-set n
                         (mismatch
                          n
                          (cons pat
                                (mismatch-val
                                 (hash-ref (named-pats-map nps)
                                           n))))
                         nps)]
        [else
         (add-named-pats n
                         (mismatch n (list pat))
                         nps)]))

(define (add-named-rep n nps)
  (cond [(member-named-pats n nps) nps]
        [else
         (add-named-pats n
                         (named-rep n)
                         nps)]))

(define (named-pats-set n val nps)
  (named-pats
   (named-pats-names nps)
   (hash-set (named-pats-map nps)
             n val)))

(define (add-named-pats n val nps)
  (named-pats (cons n (named-pats-names nps))
              (hash-set (named-pats-map nps) n val)))

(define (reverse-named-pats nps)
  (named-pats (named-pats-names nps)
              (foldl
               (λ (kv m)
                  (let ([key (car kv)]
                        [val (cdr kv)])
                    (hash-set m key
                              (cond [(named? val)
                                     val]
                                    [(mismatch? val)
                                     (mismatch (mismatch-name val)
                                               (reverse
                                                (mismatch-val val)))]))))
               (hash)
               (hash->list (named-pats-map nps)))))

(define (assoc-named n l)
  (cond [(null? l) #f]
        [else
         (or (let ([cur (car l)])
               (and (named? cur)
                    (equal? (named-name cur)
                            n)))
             (assoc-named n (cdr l)))]))

(define (enum-names pat nps nt-enums unused-var/e)
  (let rec ([nps nps]
            [env (hash)])
    (cond [(empty-named-pats? nps)
           (pat/e-with-names pat nt-enums env unused-var/e)]
          [else
           (let ([cur (next-named-pats nps)])
             (cond [(named? cur)
                    (let ([name (named-name cur)]
                          [pat (named-val cur)])
                      (map/e
                       (λ (ts)
                          (named name
                                 (named-t (car ts)
                                          (cdr ts))))
                       (λ (n)
                          (if (equal? (named-name n)
                                      name)
                              (let ([val (named-val n)])
                                (cons (named-t-val val)
                                      (named-t-term val)))
                              (error 'wrong-name
                                     "expected ~a, got ~a"
                                     name
                                     (named-name n))))
                       (dep/e
                        (pat/e-with-names pat nt-enums env unused-var/e)
                        (λ (term)
                           (rec (rest-named-pats nps)
                                (hash-set env
                                          name
                                          term))))))]
                   [(mismatch? cur)
                    (let ([name (mismatch-name cur)])
                      (map/e
                       (λ (ts)
                          (mismatch name
                                    (mismatch-t (car ts)
                                                (cdr ts))))
                       (λ (n)
                          (if (equal? (mismatch-name n)
                                      name)
                              (let ([val (mismatch-val n)])
                                (cons (mismatch-t-vals val)
                                      (mismatch-t-term val)))
                              (error 'wrong-name
                                     "expected ~a, got ~a"
                                     name
                                     (named-name n))))
                       (dep/e
                        (fold-enum
                         (λ (excepts pat)
                            (except/e
                             (pat/e-with-names pat
                                               nt-enums
                                               (hash-set env
                                                         (mismatch-name cur)
                                                         excepts)
                                               unused-var/e)
                             excepts))
                         (mismatch-val cur))
                        (λ (terms)
                           (rec (rest-named-pats nps)
                                (hash-set env
                                          name
                                          terms))))))]
                   [(named-rep? cur)
                    (let* ([name (named-rep-name cur)]
                           [f/e  (λ (n)
                                    (rec (rest-named-pats nps)
                                         (hash-set env name n)))])
                      (map/e
                       (λ (n-t)
                          (named-rep-t (car n-t)
                                       (cdr n-t)))
                       (λ (n-rep-t)
                          (cons (named-rep-t-n n-rep-t)
                                (named-rep-t-t n-rep-t)))
                       (sum/e (map/e
                               (λ (t)
                                  (cons 0 t))
                               cdr
                               (f/e 0))
                              (dep/e (nats+/e 1) f/e))))]
                   [else (error 'unexpected "expected name, mismatch or named-repeat, got: ~a in ~a" cur nps)]))])))

(define (pat/e-with-names pat nt-enums named-terms unused-var/e)
  (let loop ([pat pat])
    (match-a-pattern
     pat
     [`any 
      (sum/e
       any/e
       (many/e any/e))]
     [`number num/e]
     [`string string/e]
     [`natural natural/e]
     [`integer integer/e]
     [`real real/e]
     [`boolean bool/e]
     [`variable var/e]
     [`(variable-except ,s ...)
      (except/e var/e s)]
     [`(variable-prefix ,s)
      ;; todo
      (error 'unimplemented "var-prefix")]
     [`variable-not-otherwise-mentioned
      unused-var/e]
     [`hole
      (const/e the-hole)]
     [`(nt ,id)
      (hash-ref nt-enums id)]
     [`(name ,n ,pat)
      (const/e (name-ref n))]
     [`(mismatch-name ,n ,pat)
      (const/e (mismatch-ref n))]
     [`(in-hole ,p1 ,p2) ;; untested
      (map/e
       (λ (ts)
          (decomposition (car ts)
                         (cdr ts)))
       (λ (decomp)
          (cons (decomposition-ctx decomp)
                (decomposition-term decomp)))
       (prod/e
        (loop p1)
        (loop p2)))]
     [`(hide-hole ,p)
      (loop p)]
     [`(side-condition ,p ,g ,e)
      (unsupported pat)]
     [`(cross ,s)
      (unsupported pat)]
     [`(list ,sub-pats ...)
      ;; enum-list
      (list/e
       (map
        (λ (sub-pat)
           (match sub-pat
             [`(repeat ,pat #f #f)
              (map/e
               (λ (ts)
                  (repeat (length ts)
                          ts))
               (λ (rep)
                  (repeat-terms rep))
               (many/e (loop pat)))]
             [`(repeat ,pat ,name #f)
              (let ([n (hash-ref named-terms name)])
                (map/e
                 (λ (ts)
                    (repeat n ts))
                 (λ (rep)
                    (repeat-terms rep))
                 (many/e (loop pat)
                         n)))]
             [`(repeat ,pat ,name ,mismatch)
              (error 'unimplemented "mismatch-repeat")]
             [else (loop sub-pat)]))
        sub-pats))]
     [(? (compose not pair?)) 
      (const/e pat)])))

(define (flatten-1 xs)
  (append-map
   (λ (x)
      (if (or (pair? x)
              (null? x))
          x
          (list x)))
   xs))

;; lookup : lang symbol -> (listof rhs)
(define (lookup nts name)
  (let rec ([nts nts])
    (cond [(null? nts) (error 'unkown-nt)]
          [(eq? name (nt-name (car nts)))
           (nt-rhs (car nts))]
          [else (rec (cdr nts))])))

(define natural/e nats)

(define char/e
  (map/e
   integer->char
   char->integer
   (range/e #x61 #x7a)))

(define string/e
  (map/e
   list->string
   string->list
   (many/e char/e)))

(define integer/e
  #; ;; Simple "turn down the volume" list
  (from-list/e '(0 1 -1))
  (sum/e nats
         (map/e (λ (n) (- (+ n 1)))
                (λ (n) (- (- n) 1))
                nats)))

;; This is really annoying so I turned it off
(define real/e empty/e)
(define num/e
  (sum/e integer/e
         real/e))

(define bool/e
  (from-list/e '(#t #f)))

(define var/e
  (map/e
   (compose string->symbol list->string)
   (compose string->list symbol->string)
   (many1/e char/e)))

(define any/e
  (sum/e num/e
         string/e
         bool/e
         var/e))

;; to-term : augmented term -> redex term
(define (to-term aug)
  (cond [(named? aug)
         (rep-name aug)]
        [(mismatch? aug)
         (rep-mismatches aug)]
        [(decomposition? aug)
         (plug-hole aug)]
        [(named-rep-t? aug)
         (to-term (named-rep-t-t aug))]
        [(repeat? aug)
         (map-repeat to-term
                     aug)]
        [(list? aug)
         (expand-repeats
          (map to-term aug))]
        [else aug]))

(define (expand-repeats sub-terms)
  (append*
   (map
    (λ (t)
       (cond [(repeat? t)
              (repeat-terms t)]
             [else (list t)]))
    sub-terms)))

(define (rep-name s)
  (to-term
   (let* ([n (named-name s)]
          [v (named-val s)]
          [val (named-t-val v)]
          [term (named-t-term v)])
     (let loop ([term term])
       (cond [(and (name-ref? term)
                   (equal? (name-ref-name term) n))
              val]
             [(list? term)
              (map loop term)]
             [(named? term)
              (map-named loop
                         term)]
             [(decomposition? term)
              (map-decomp loop
                          term)]
             [(mismatch? term)
              (map-mismatch loop
                            term)]
             [(repeat? term)
              (map-repeat loop
                          term)]
             [else term])))))

(define (rep-mismatches m)
  (to-term
   (let* ([name (mismatch-name m)]
          [v (mismatch-val m)]
          [vals (mismatch-t-vals v)]
          [term (mismatch-t-term v)])
     (let ([vals vals])
       (let loop ([term term])
         (cond [(and (mismatch-ref? term)
                     (equal? (mismatch-ref-name term) name))
                (begin0
                    (car vals)
                  (set! vals (cdr vals)))]
               [(list? term)
                (map loop term)]
               [(named? term)
                (map-named loop
                           term)]
               [(decomposition? term)
                (map-decomp loop
                            term)]
               [(mismatch? term)
                (map-mismatch loop
                              term)]
               [(repeat? term)
                (map-repeat loop
                            term)]
               [else term]))))))

(define (plug-hole ctx term)
  (to-term
   (let loop ([ctx ctx])
     (cond [(hole? ctx) term]
           [(list? ctx) (map loop ctx)]
           [(named? )])
     (match
       ctx
       ['hole term]
       [`(,ts ...)
        (map loop ts)]
       [x x]))))

(define (map-decomp f dcmp)
  (let ([ctx (decomposition-ctx dcmp)]
        [term (decomposition-term dcmp)])
    (decomposition (f ctx)
                   (f term))))

(define (map-named f n)
  (let ([v (named-val n)])
    (named (named-name n)
           (named-t
            (named-t-val v)
            (f (named-t-term v))))))

(define (map-mismatch f m)
  (let ([v (mismatch-val m)])
    (mismatch (mismatch-name m)
              (mismatch-t
               (mismatch-t-vals v)
               (f (mismatch-t-term v))))))

(define (map-repeat f r)
  (repeat (repeat-n r)
          (map f (repeat-terms r))))

(define (unsupported pat)
  (error 'generate-term "#:i-th does not support ~s patterns" pat))

(define (unimplemented pat)
  (error 'generate-term "#:i-th does not yet support ~s patterns" pat))
