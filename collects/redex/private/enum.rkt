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
(struct decomposition (ctx term))
(struct hole ())
(struct named (name val))
(struct named-t (val term))
(struct name (name) #:transparent)
(struct unimplemented (msg))

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
              [`(side-condition ,p ,g ,e) ;; error
               (unsupported/enum pat)]
              [`(cross ,s)
               (unsupported/enum pat)] ;; error
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

;; sep-names : single-pattern lang -> (assoclist symbol pattern)
(define (sep-names pat)
  (let loop ([pat pat]
             [named-pats '()])
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
     [`(name ,name ,pat)
      (loop pat
            (add-if-new name pat named-pats))]
     [`(mismatch-name ,name ,pat)
      (loop pat (cons (unimplemented "mismatch") named-pats))]
     [`(in-hole ,p1 ,p2)
      (loop p2
            (loop p1 named-pats))]
     [`(hide-hole ,p) (loop p named-pats)]
     [`(side-condition ,p ,g ,e) ;; error
      (unsupported/enum pat)]
     [`(cross ,s)
      (unsupported/enum pat)] ;; error
     [`(list ,sub-pats ...)
      (foldl (λ (sub-pat named-pats)
                (match sub-pat
                  [`(repeat ,pat #f #f)
                   (loop pat named-pats)]
                  [`(repeat ,pat ,name #f)
                   (loop pat (cons (unimplemented "named repeat") named-pats))]
                  [`(repeat ,pat #f ,mismatch)
                   (loop pat (cons (unimplemented "mismatch repeat") named-pats))]
                  [else (loop sub-pat named-pats)]))
             named-pats
             sub-pats)]
     [(? (compose not pair?))
      named-pats])))

(define (add-if-new k v l)
  (cond [(assoc-named k l) l]
        [else (cons (named k v) l)]))

(define (assoc-named n l)
  (cond [(null? l) #f]
        [else
         (or (let ([cur (car l)])
               (and (named? cur)
                    (equal? (named-name cur)
                            n)))
             (assoc-named n (cdr l)))]))

(define (enum-names pat named-pats nt-enums)
  (let rec ([named-pats named-pats]
            [env (hash)])
    (cond [(null? named-pats)
           (pat/enum-with-names pat nt-enums env)]
          [else
           (let ([cur (car named-pats)])
             (cond ([named? cur]
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
                           (rec (cdr named-pats)
                                (hash-set env
                                          name
                                          term)))))))
                   [else (error/enum 'unimplemented
                                     (unimplemented-msg cur))]))])))

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
      (apply except/enum var/enum s)]
     [`(variable-prefix ,s)
      ;; todo
      (error/enum 'unimplemented "var-prefix")]
     [`variable-not-otherwise-mentioned
      (error/enum 'unimplemented "var-not-mentioned")] ;; error
     [`hole
      (const/enum 'hole)]
     [`(nt ,id)
      (hash-ref nt-enums id)]
     [`(name ,n ,pat)
      (const/enum (name n))]
     [`(mismatch-name ,name ,pat)
      (error/enum 'unimplemented "mismatch-name")]
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
      (map/enum
       flatten-1
       identity
       (list/enum
        (map
         (λ (sub-pat)
            (match sub-pat
              [`(repeat ,pat #f #f)
               (map/enum
                cdr
                (λ (ts)
                   (cons (length ts)
                         ts))
                (dep/enum
                 nats
                 (λ (n)
                    (list/enum
                     (build-list n (const (loop pat)))))))]
              [`(repeat ,pat ,name #f)
               (error/enum 'unimplemented "named-repeat")]
              [`(repeat ,pat #f ,mismatch)
               (error/enum 'unimplemented "mismatch-repeat")]
              [else (map/enum
                     list
                     car
                     (loop sub-pat))]))
         sub-pats)))]
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

(define real/enum (from-list/enum '(0.0 1.5 123.112354)))
(define num/enum
  (sum/enum natural/enum
            integer/enum
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
        [else aug]))

(define (rep-name s)
  (to-term
   (let* ([n (named-name s)]
          [v (named-val s)]
          [val (named-t-val v)]
          [term (named-t-term v)])
     (let loop ([term term])
       (cond [(and (name? term)
                   (equal? (name-name term) n))
              val]
             [(named? term)
              (map-named loop
                         term)]
             [else term])))))

(define (map-named f n)
  (let ([v (named-val n)])
   (named (named-name n)
          (named-t
           (named-t-val v)
           (f (named-t-term v))))))

#;
(define (plug-hole ctx term)
  (to-term
   (let loop ([ctx ctx])
     (cond [(hole? ctx) term]
           [(cons? ctx) (map loop ctx)]
           [(named? )])
     (match
       ctx
       ['hole term]
       [`(,ts ...)
        (map loop ts)]
       [x x]))))
