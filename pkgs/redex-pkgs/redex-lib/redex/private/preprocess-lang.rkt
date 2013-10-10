#lang racket/base
(require racket/contract
         racket/list
         racket/match
         racket/set
         "lang-struct.rkt"
         "match-a-pattern.rkt")

(provide
 (contract-out
  [sep-lang (-> (listof nt?)
                (values (listof nt?)
                        (listof nt?)))]
  [used-vars (-> (listof nt?)
                 (listof symbol?))]))

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
  (define edges (find-edges lang))
  (define cyclic-nts (find-cycles edges))
  (define-values (cyclic non-cyclic)
    (partition (λ (nt)
                  (set-member? cyclic-nts (nt-name nt)))
               lang))
  ;; topological sort
  (define sorted-left
    (topo-sort non-cyclic
               (filter-edges edges non-cyclic)))
  ;; rhs sort
  (define sorted-right
    (sort-nt-terms cyclic
                   cyclic-nts))
  (values sorted-left
          sorted-right))

;; find-edges : lang -> (hash[symbol] -o> (setof (listof symbol)))
(define (find-edges lang)
  (foldl
   (λ (nt m)
      (hash-set
       m (nt-name nt)
       (fold-map/set
        (λ (rhs)
           (let loop ([pat (rhs-pattern rhs)]
                      [s (set)])
             (match
               pat
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
               [`(list ,sub-pats ...)
                (fold-map/set
                 (λ (sub-pat)
                    (match sub-pat
                      [`(repeat ,pat ,name ,mismatch)
                       (loop pat s)]
                      [else (loop sub-pat s)]))
                 sub-pats)]
               [else s])))
        (nt-rhs nt))))
   (hash)
   lang))

;; find-cycles : (hash[symbol] -o> (setof symbol)) -> (setof symbol)
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
     ;; not sure about these 2, but they are unsupported by enum anyway
     [`(side-condition ,p ,g ,e) #f] 
     [`(cross ,s) #f]
     [`(list ,sub-pats ...)
      (ormap (λ (sub-pat)
                (match sub-pat
                  [`(repeat ,pat ,name ,mismatch)
                   (rec pat)]
                  [else (rec sub-pat)]))
             sub-pats)]
     [(? (compose not pair?)) #f])))
      
;; recursive-rhss : lang (hash[symbol] -o> symbol)
;;                  -> (hash[symbol] -o> (assoclist rhs bool))
(define (recursive-rhss cyclic recs)
  (for/hash ([cur-nt (in-list cyclic)])
    (match-define (nt cur-name cur-rhss) cur-nt)
    (define rhss
      (for/list ([cur-rhs (in-list cur-rhss)])
        (cons cur-rhs
              (calls-rec? (rhs-pattern cur-rhs)
                          recs))))
    (values cur-name rhss)))

;; topo-sort : lang (hash[symbol] -o> (setof symbol)) -> lang
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
                    (find-nt lang v)
                    out-lang)))])))

;; find-nt : lang, symbol -> nt
(define (find-nt lang name)
  (findf (λ (nt)
            (eq? name (nt-name nt)))
         lang))


;; sort-nt-terms : lang, (hash[symbol] -o> (setof symbol)) -> lang
(define (sort-nt-terms cyclic nts)
  (define recs (recursive-rhss cyclic nts))
  (for/list ([cur-nt (in-list cyclic)])
    (match cur-nt
      [(nt name rhs)
       (define rec-nts (hash-ref recs name))
       (nt name
           (sort rhs
                 (λ (r1 r2)
                    (and (not (cdr (assoc r1 rec-nts)))
                         (cdr (assoc r2 rec-nts))))))])))

;; directly-used-nts : pat -> (setof symbol)
(define (directly-used-nts pat)
  (match pat
    [`(nt id) (set nt)]
    [(or `(name ,n ,p)
         `(mismatch-name ,n ,p))
     (directly-used-nts p)]
    [`(in-hole ,p1 ,p2)
     (set-union (directly-used-nts p1)
                (directly-used-nts p2))]
    [`(hide-hole ,p)
     (directly-used-nts p)]
    [`(list ,sub-pats ...)
     (fold-map/set
      (λ (sub-pat)
         (match sub-pat
           ;; Not a direct reference since an empty list can always be
           ;; enumerated
           [`(repeat ,p ,n ,m) (set)]
           [else (directly-used-nts sub-pat)]))
      sub-pats)]
    [else (set)]))

;; used-vars : lang -> (listof symbol)
(define (used-vars lang)
  (set->list
   (fold-map/set
    (λ (the-nt)
       (fold-map/set
        (λ (the-rhs)
           (let loop ([pat (rhs-pattern the-rhs)])
             (match-a-pattern
              pat
              [`any (set)]
              [`number (set)]
              [`string (set)]
              [`natural (set)]
              [`integer (set)]
              [`real (set)]
              [`boolean (set)]
              [`variable (set)]
              [`(variable-except ,s ...) (set)]
              [`(variable-prefix ,s) (set)]
              [`variable-not-otherwise-mentioned (set)]
              [`hole (set)]
              [`(nt ,id) (set)]
              [`(name ,name ,pat) (set)]
              [`(mismatch-name ,name ,pat) (set)]
              [`(in-hole ,p1 ,p2)
               (set-union (loop p1)
                          (loop p2))]
              [`(hide-hole ,p) (loop p)]
              ;; not sure about these 2, but they are unsupported by enum anyway
              [`(side-condition ,p ,g ,e) (set)] 
              [`(cross ,s) (set)]
              [`(list ,sub-pats ...)
               (fold-map/set
                (λ (sub-pat)
                   (match sub-pat
                     [`(repeat ,pat ,name ,mismatch)
                      (loop pat)]
                     [else (loop sub-pat)]))
                sub-pats)]
              [(? (compose not pair?))
               (if (symbol? pat)
                   (set pat)
                   (set))])))
        (nt-rhs the-nt)))
    lang)))

;; fold-map/set : (a -> setof b) (listof a) -> (setof b)
(define (fold-map/set f l)
  (foldl
   (λ (x s)
      (set-union (f x) s))
   (set)
   l))
