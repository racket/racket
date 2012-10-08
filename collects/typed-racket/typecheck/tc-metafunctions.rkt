#lang racket/base

(require "../utils/utils.rkt"
         (rename-in (types subtype abbrev union utils filter-ops)
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (rep type-rep filter-rep object-rep rep-utils) racket/list
         racket/contract racket/match unstable/match
         (for-syntax racket/base))

(provide abstract-results)


(define/cond-contract (abstract-results results arg-names)
     (tc-results? (listof identifier?) . -> . (or/c Values? ValuesDots?))
     (define keys (for/list ([(nm k) (in-indexed arg-names)]) k))
     (match results
       [(tc-results: ts fs os dty dbound)
        (make-ValuesDots
         (for/list ([t ts] [f fs] [o os])
           (make-Result t (abstract-filter arg-names keys f) (abstract-object arg-names keys o)))
         dty dbound)]
       [(tc-results: ts fs os)
        (make-Values
         (for/list ([t ts] [f fs] [o os])
           (make-Result t (abstract-filter arg-names keys f) (abstract-object arg-names keys o))))]))


(define/cond-contract (abstract-object ids keys o)
  (-> (listof identifier?) (listof name-ref/c) Object? Object?)
  (define (lookup y)
    (for/first ([x ids] [i keys] #:when (free-identifier=? x y)) i))
  (define-match-expander lookup:
    (syntax-rules ()
      [(_ i) (app lookup (? values i))]))
  (match o
    [(Path: p (lookup: idx)) (make-Path p idx)]
    [_ (make-Empty)]))


(define/cond-contract (abstract-filter ids keys fs)
  (-> (listof identifier?) (listof name-ref/c) FilterSet/c FilterSet/c)
  (match fs
    [(FilterSet: f+ f-)
     (-FS (abo ids keys f+) (abo ids keys f-))]
    [(NoFilter:) (-FS -top -top)]))

(define/cond-contract (abo xs idxs f)
  ((listof identifier?) (listof name-ref/c) Filter/c . -> . Filter/c)
  (define/cond-contract (lookup y)
       (identifier? . -> . (or/c #f integer?))
       (for/first ([x xs] [i idxs] #:when (free-identifier=? x y)) i))
  (define-match-expander lookup:
    (syntax-rules ()
      [(_ i) (or (? identifier? (app lookup (? values i)))
                 i)]))
  (define (rec f) (abo xs idxs f))
  (define (sb-t t) t)
  (filter-case (#:Type sb-t #:Filter rec) f
               [#:TypeFilter
                t p (lookup: idx)
                (-filter t idx p)]
               [#:NotTypeFilter
                t p (lookup: idx)
                (-not-filter t idx p)]))

(define (merge-filter-sets fs)
  (match fs
    [(list (FilterSet: f+ f-) ...)
     (-FS (make-AndFilter f+) (make-AndFilter f-))]))

(define (tc-results->values tc)
  (match tc
    [(tc-results: ts) (-values ts)]))

(provide combine-props tc-results->values)


(define/cond-contract (resolve atoms prop)
  ((listof Filter/c)
   Filter/c
   . -> .
   Filter/c)
  (for/fold ([prop prop])
    ([a (in-list atoms)])
    (match prop
      [(AndFilter: ps)
       (let loop ([ps ps] [result null])
         (if (null? ps)
             (apply -and result)
             (let ([p (car ps)])
               (cond [(opposite? a p) -bot]
                     [(implied-atomic? p a) (loop (cdr ps) result)]
                     [else (loop (cdr ps) (cons p result))]))))]
      [_ prop])))

(define (flatten-props ps)
  (let loop ([ps ps])
    (match ps
      [(list) null]
      [(cons (AndFilter: ps*) ps) (loop (append ps* ps))]
      [(cons p ps) (cons p (loop ps))])))

(define/cond-contract (combine-props new-props old-props flag)
  ((listof Filter/c) (listof Filter/c) (box/c boolean?)
   . -> .
   (values (listof (or/c ImpFilter? OrFilter? AndFilter?)) (listof (or/c TypeFilter? NotTypeFilter?))))
  (define (atomic-prop? p) (or (TypeFilter? p) (NotTypeFilter? p)))
  (define-values (new-atoms new-formulas) (partition atomic-prop? (flatten-props new-props)))
  (let loop ([derived-props null]
             [derived-atoms new-atoms]
             [worklist (append old-props new-formulas)])
    (if (null? worklist)
        (values derived-props derived-atoms)
        (let* ([p (car worklist)]
               [p (resolve derived-atoms p)])
          (match p
            [(AndFilter: ps) (loop derived-props derived-atoms (append ps (cdr worklist)))]
            [(ImpFilter: a c)
             ;(printf "combining ~a with ~a\n" p (append derived-props derived-atoms))
             (if (for/or ([p (append derived-props derived-atoms)])
                   (implied-atomic? a p))
                 (loop derived-props derived-atoms (cons c (cdr worklist)))
                 (loop (cons p derived-props) derived-atoms (cdr worklist)))]
            [(OrFilter: ps)
             (let ([new-or
                    (let or-loop ([ps ps] [result null])
                      (cond
                        [(null? ps) (apply -or result)]
                        [(for/or ([other-p (in-list (append derived-props derived-atoms))])
                             (opposite? (car ps) other-p))
                         (or-loop (cdr ps) result)]
                        [(for/or ([other-p (in-list derived-atoms)])
                             (implied-atomic? (car ps) other-p))
                         -top]
                        [else (or-loop (cdr ps) (cons (car ps) result))]))])
               (if (OrFilter? new-or)
                   (loop (cons new-or derived-props) derived-atoms (cdr worklist))
                   (loop derived-props derived-atoms (cons new-or (cdr worklist)))))]
            [(TypeFilter: (== (Un) type-equal?) _ _) (set-box! flag #f) (values derived-props derived-atoms)]
            [(TypeFilter: _ _ _) (loop derived-props (cons p derived-atoms) (cdr worklist))]
            [(NotTypeFilter: (== Univ type-equal?) _ _) (set-box! flag #f) (values derived-props derived-atoms)]
            [(NotTypeFilter: _ _ _) (loop derived-props (cons p derived-atoms) (cdr worklist))]
            [(Top:) (loop derived-props derived-atoms (cdr worklist))]
            [(Bot:) (set-box! flag #f) (values derived-props derived-atoms)]
            [_ (loop (cons p derived-props) derived-atoms (cdr worklist))])))))


