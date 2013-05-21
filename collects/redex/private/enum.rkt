#lang racket/base
(require racket/contract
         racket/list
         racket/match
         racket/function
         racket/set
         "lang-struct.rkt"
         "match-a-pattern.rkt"
         "enumerator.rkt")

(provide 
 (contract-out
  [lang-enumerators (-> (listof nt?) lang-enum?)]
  [pat-enumerator (-> lang-enum?
                      any/c ;; pattern
                      enum?)]
  [enum-ith (-> enum? exact-nonnegative-integer? any/c)]
  [lang-enum? (-> any/c boolean?)]
  [enum? (-> any/c boolean?)]))

(struct lang-enum (enums))
(struct repeat (n terms) #:transparent)
(struct decomposition (ctx term) #:transparent)
(struct named (name val) #:transparent)
(struct named-t (val term) #:transparent)
(struct mismatch (name val) #:transparent)
(struct mismatch-t (vals term) #:transparent)

(struct name-ref (name) #:transparent)
(struct mismatch-ref (name) #:transparent)

(struct unimplemented (msg) #:transparent)
(struct named-pats (names map) #:transparent
        ) ;; listof symbol and hash symbol -o> (or named, mismatched, named-repeat, mismatch-repeat)

(define enum-ith decode)

(define (lang-enumerators lang)
  (let ([l-enums (make-hash)])
    (let-values ([(fin-lang rec-lang)
                  (sep-lang lang)])
      (for-each
       (λ (nt)
          (hash-set! l-enums
                     (nt-name nt)
                     (enumerate-rhss (nt-rhs nt)
                                     l-enums)))
       fin-lang)
      (for-each
       (λ (nt)
          (hash-set! l-enums
                     (nt-name nt)
                     (thunk/enum +inf.f
                                 (λ ()
                                    (enumerate-rhss (nt-rhs nt)
                                                    l-enums)))))
       rec-lang))
    (lang-enum l-enums)))

(define (pat-enumerator l-enum pat)
  (map/enum
   to-term
   (λ (_)
      (error 'pat-enum "Enumerator is not a  bijection"))
   (pat/enum pat
             (lang-enum-enums l-enum))))

(define (enumerate-rhss rhss l-enums)
  (apply sum/enum
         (map
          (λ (rhs)
             (pat/enum (rhs-pattern rhs)
                       l-enums))
          rhss)))

;; find-edges : lang -> (hash symbol -o> (setof symbol))
(define (find-edges lang)
  (foldl
   (λ (nt m)
      (hash-set
       m (nt-name nt)
       (fold-map/set
        (λ (rhs)
           (let loop ([pat (rhs-pattern rhs)]
                      [s (set)])
             (match-a-pattern
              pat
              [`any s]
              [`number s]
              [`string s]
              [`natural s]
              [`integer s]
              [`real s]
              [`boolean s]
              [`variable s]
              [`(variable-except ,v ...) s]
              [`(variable-prefix ,v) s]
              [`variable-not-otherwise-mentioned s]
              [`hole s]
              [`(nt ,id)
               (set-add s id)]
              [`(name ,name ,pat)
               (loop pat s)]
              [`(mismatch-name ,name ,pat)
               (loop pat s)]
              [`(in-hole ,p1 ,p2)
               (set-union (loop p1 s)
                          (loop p2 s))]
              [`(hide-hole ,p) (loop p s)]
              [`(side-condition ,p ,g ,e) s]
              [`(cross ,s) s]
              [`(list ,sub-pats ...)
               (fold-map/set
                (λ (sub-pat)
                   (match sub-pat
                     [`(repeat ,pat ,name ,mismatch)
                      (loop pat s)]
                     [else (loop sub-pat s)]))
                sub-pats)]
              [(? (compose not pair?)) s])))
        (nt-rhs nt))))
   (hash)
   lang))

;; find-cycles : (hashsymbol -o> (setof symbol)) -> (setof symbol)
(define (find-cycles edges)
  (foldl
   (λ (v s)
      (if (let rec ([cur v]
                    [seen (set)])
            (cond [(set-member? seen cur) #t]
                  [else
                   (ormap
                    (λ (next)
                       (rec next
                            (set-add seen cur)))
                    (set->list (hash-ref edges
                                         cur)))]))
          (set-add s v)
          s))
   (set)
   (hash-keys edges)))

;; calls-rec? : pat (setof symbol) -> bool
(define (calls-rec? pat recs)
  (let rec ([pat pat])
    (match-a-pattern
     pat
     [`any #f]
     [`number #f]
     [`string #f]
     [`natural #f]
     [`integer #f]
     [`real #f]
     [`boolean #f]
     [`variable #f]
     [`(variable-except ,s ...) #f]
     [`(variable-prefix ,s) #f]
     [`variable-not-otherwise-mentioned #f]
     [`hole #f]
     [`(nt ,id)
      (set-member? recs id)]
     [`(name ,name ,pat)
      (rec pat)]
     [`(mismatch-name ,name ,pat)
      (rec pat)]
     [`(in-hole ,p1 ,p2)
      (or (rec p1)
          (rec p2))]
     [`(hide-hole ,p) (rec p)]
     [`(side-condition ,p ,g ,e) ;; error
      (unsupported/enum pat)]
     [`(cross ,s)
      (unsupported/enum pat)] ;; error
     [`(list ,sub-pats ...)
      (ormap (λ (sub-pat)
                (match sub-pat
                  [`(repeat ,pat ,name ,mismatch)
                   (rec pat)]
                  [else (rec sub-pat)]))
             sub-pats)]
     [(? (compose not pair?)) #f])))

;; fold-map : (a -> setof b) (listof a) -> (setof b)
(define (fold-map/set f l)
  (foldl
   (λ (x s)
      (set-union (f x) s))
   (set)
   l))

;; sep-lang : lang -> lang lang
;; topologically sorts non-terminals by dependency
;; sorts rhs's so that recursive ones go last
(define (sep-lang lang)
  (define (filter-edges edges lang)
    (foldl
     (λ (nt m)
        (let ([name (nt-name nt)])
          (hash-set m name
                    (hash-ref edges name))))
     (hash)
     lang))
  (let* ([edges (find-edges lang)]
         [cyclic-nts (find-cycles edges)])
    (let-values ([(cyclic non-cyclic)
                  (partition (λ (nt)
                                (set-member? cyclic-nts (nt-name nt)))
                             lang)])
      (let ([sorted-left (topo-sort non-cyclic
                                    (filter-edges edges non-cyclic))] ;; topological sort
            [sorted-right (sort-nt-terms cyclic
                                         cyclic-nts)] ;; rhs sort
            )
        (values sorted-left
                sorted-right)))))

;; recursive-rhss : lang (hash symbol -o> (setof symbol)) -> (hash symbol -o> (assoclist rhs bool))
(define (recursive-rhss lang recs)
  (foldl
   (λ (nt m)
      (let ([rhs (nt-rhs nt)])
        (hash-set m (nt-name nt)
                  (map (λ (rhs)
                          (cons rhs
                                (calls-rec? (rhs-pattern rhs)
                                            recs)))
                       rhs))))
   (hash)
   lang))

;; topo-sort : lang (hash symbol -o> (setof symbol)) -> lang
(define (topo-sort lang edges)
  (define (find-top rem edges)
    (let find ([rem rem])
      (let ([v (car rem)])
        (let check ([vs (hash-keys edges)])
          (cond [(empty? vs) v]
                [(set-member? (hash-ref edges (car vs))
                              v)
                 (find (cdr rem))]
                [else (check (cdr vs))])))))
  (let loop ([rem (hash-keys edges)]
             [edges edges]
             [out-lang '()])
    (cond [(empty? rem) out-lang]
          [else
           (let ([v (find-top rem edges)])
             (loop (remove v rem)
                   (hash-remove edges v)
                   (cons
                    (findf
                     (λ (nt)
                        (eq? v (nt-name nt)))
                     lang)
                    out-lang)))])))

;; sort-nt-terms : lang (setof symbol) -> lang
(define (sort-nt-terms lang nts)
  (let ([recs (recursive-rhss lang nts)])
    (map
     (λ (nt)
        (let ([rec-nts (hash-ref recs (nt-name nt))])
          (make-nt (nt-name nt)
                   (sort (nt-rhs nt)
                         (λ (r1 r2)
                            (and (not (cdr (assoc r1 rec-nts)))
                                 (cdr (assoc r2 rec-nts))))))))
     lang)))

(define (pat/enum pat l-enums)
  (enum-names pat
              (sep-names pat)
              l-enums))

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
                  [`(repeat ,pat ,name ,mismatch)
                   (loop pat
                         (add-unimplemented name "named/mismatched repeat" named-pats))]
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
(define (add-unimplemented name msg nps)
  (add-named-pats name
                  (unimplemented msg)
                  nps))

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
                                                (mismatch-val val)))]
                                    [(unimplemented? val)
                                     val]))))
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

(define (enum-names pat nps nt-enums)
  (let rec ([nps nps]
            [env (hash)])
    (cond [(empty-named-pats? nps)
           (pat/enum-with-names pat nt-enums env)]
          [else
           (let ([cur (next-named-pats nps)])
             (cond [(named? cur)
                    (let ([name (named-name cur)]
                          [pat (named-val cur)])
                      (map/enum
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
                       (dep/enum
                        (pat/enum-with-names pat nt-enums env)
                        (λ (term)
                           (rec (rest-named-pats nps)
                                (hash-set env
                                          name
                                          term))))))]
                   [(mismatch? cur)
                    (let ([name (mismatch-name cur)])
                      (map/enum
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
                       (dep/enum
                        (fold-enum
                         (λ (excepts pat)
                            (except/enum
                             (pat/enum-with-names pat
                                                  nt-enums
                                                  (hash-set env
                                                            (mismatch-name cur)
                                                            excepts))
                             excepts))
                         (mismatch-val cur))
                        (λ (terms)
                           (rec (rest-named-pats nps)
                                (hash-set env
                                          name
                                          terms))))))]
                   [(unimplemented? cur)
                    (error/enum 'unimplemented
                                (unimplemented-msg cur))]
                   [else (error 'unexpected "expected name, mismatch or unimplemented, got: ~a in ~a" cur nps)]))])))

(define (pat/enum-with-names pat nt-enums named-terms)
  (let loop ([pat pat])
    (match-a-pattern
     pat
     [`any 
      (sum/enum
       any/enum
       (listof/enum any/enum))]
     [`number num/enum]
     [`string string/enum]
     [`natural natural/enum]
     [`integer integer/enum]
     [`real real/enum]
     [`boolean bool/enum]
     [`variable var/enum]
     [`(variable-except ,s ...)
      (except/enum var/enum s)]
     [`(variable-prefix ,s)
      ;; todo
      (error/enum 'unimplemented "var-prefix")]
     [`variable-not-otherwise-mentioned
      (error/enum 'unimplemented "var-not-mentioned")] ;; error
     [`hole
      (const/enum the-hole)]
     [`(nt ,id)
      (hash-ref nt-enums id)]
     [`(name ,n ,pat)
      (const/enum (name-ref n))]
     [`(mismatch-name ,n ,pat)
      (const/enum (mismatch-ref n))]
     [`(in-hole ,p1 ,p2) ;; untested
      (map/enum
       (λ (ts)
          (decomposition (car ts)
                         (cdr ts)))
       (λ (decomp)
          (cons (decomposition-ctx decomp)
                (decomposition-term decomp)))
       (prod/enum
        (loop p1)
        (loop p2)))]
     [`(hide-hole ,p)
      (loop p)]
     [`(side-condition ,p ,g ,e)
      (unsupported/enum pat)]
     [`(cross ,s)
      (unsupported/enum pat)]
     [`(list ,sub-pats ...)
      ;; enum-list
      (list/enum
       (map
        (λ (sub-pat)
           (match sub-pat
             [`(repeat ,pat #f #f)
              (map/enum
               (λ (n-ts)
                  (repeat (car n-ts)
                          (cdr n-ts)))
               (λ (rep)
                  (cons (repeat-n rep)
                        (repeat-terms rep)))
               (dep/enum
                nats
                (λ (n)
                   (list/enum
                    (build-list n (const (loop pat)))))))]
             [`(repeat ,pat ,name #f)
              (error/enum 'unimplemented "named-repeat")]
             [`(repeat ,pat #f ,mismatch)
              (error/enum 'unimplemented "mismatch-repeat")]
             [else (loop sub-pat)]))
        sub-pats))]
     [(? (compose not pair?)) 
      (const/enum pat)])))

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

(define natural/enum nats)

(define char/enum
  (map/enum
   integer->char
   char->integer
   (range/enum #x61 #x7a)))

(define string/enum
  (map/enum
   list->string
   string->list
   (listof/enum char/enum)))

(define integer/enum
  (sum/enum nats
            (map/enum (λ (n) (- (+ n 1)))
                      (λ (n) (- (- n) 1))
                      nats)))

(define real/enum (from-list/enum '(0.5 1.5 123.112354)))
(define num/enum
  (sum/enum integer/enum
            real/enum))

(define bool/enum
  (from-list/enum '(#t #f)))

(define var/enum
  (map/enum
   (compose string->symbol list->string list)
   (compose car string->list symbol->string)
   char/enum))

(define any/enum
  (sum/enum num/enum
            string/enum
            bool/enum
            var/enum))

(define (to-term aug)
  (cond [(named? aug)
         (rep-name aug)]
        [(mismatch? aug)
         (rep-mismatches aug)]
        [(decomposition? aug)
         (plug-hole aug)]
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
