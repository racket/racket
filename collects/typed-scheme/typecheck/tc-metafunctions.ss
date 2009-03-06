#lang scheme/base

(require "../utils/utils.ss")
(require (rename-in (types subtype convenience remove-intersect union)                   
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (rep type-rep)
         scheme/contract scheme/match
         stxclass/util
         (for-syntax scheme/base))

(provide combine-filter apply-filter abstract-filter)

;; this implements the sequence invariant described on the first page relating to Bot
(define (lcombine l1 l2)
  (cond [(memq (make-LBot) l1)
         (make-LFilterSet (list (make-LBot)) null)]
        [(memq (make-LBot) l2)
         (make-LFilterSet null (list (make-LBot)))]
        [else (make-LFilterSet l1 l2)]))

(define (combine l1 l2)
  (cond [(memq (make-Bot) l1)
         (make-FilterSet (list (make-Bot)) null)]
        [(memq (make-Bot) l2)
         (make-FilterSet null (list (make-Bot)))]
        [else (make-FilterSet l1 l2)]))

(define/contract (abstract-filter x idx fs)
  (-> identifier? index/c FilterSet? LFilterSet?)
  (match fs
    [(FilterSet: f+ f-)
     (lcombine
      (apply append (for/list ([f f+]) (abo x idx f)))
      (apply append (for/list ([f f-]) (abo x idx f))))]))

(define/contract (abo x idx f)
  (-> identifier? index/c Filter/c (or/c '() (list/c LatentFilter/c)))
  (define-match-expander =x
    (lambda (stx) #'(? (lambda (id) (free-identifier=? id x)))))
  (match f
    [(Bot:) (list (make-LBot))]
    [(TypeFilter: t p (=x)) (list (make-LTypeFilter t p idx))]
    [(NotTypeFilter: t p (=x)) (list (make-LNotTypeFilter t p idx))]
    [_ null]))

(define/contract (apply-filter lfs t o)
  (-> LFilterSet? Type/c Object? FilterSet?)
  (match lfs
    [(LFilterSet: lf+ lf-)
     (combine
      (apply append (for/list ([lf lf+]) (apo lf t o)))
      (apply append (for/list ([lf lf-]) (apo lf t o))))]))

(define/contract (apo lf s o)
  (-> LatentFilter/c Type/c Object? (or/c '() (list/c Filter/c)))
  (match* (lf s o)
    [((LBot:) _ _) (list (make-Bot))]
    [((LNotTypeFilter: (? (lambda (t) (subtype s t))) (list) _) _ _) (list (make-Bot))]
    [((LTypeFilter: (? (lambda (t) (not (overlap s t)))) (list) _) _ _) (list (make-Bot))]
    [(_ _ (Empty:)) null]
    [((LTypeFilter: t pi* _) _ (Path: pi x)) (list (make-TypeFilter t (append pi* pi) x))]
    [((LNotTypeFilter: t pi* _) _ (Path: pi x)) (list (make-NotTypeFilter t (append pi* pi) x))]))

(define-match-expander T-FS:
  (lambda (stx) #'(FilterSet: _ (list (Bot:)))))
(define-match-expander F-FS:
  (lambda (stx) #'(FilterSet: (list (Bot:)) _)))

(define/contract (combine-filter f1 f2 f3)
  (FilterSet? FilterSet? FilterSet? . -> . FilterSet?)
  (match* (f1 f2 f3)
    [(f (T-FS:) (F-FS:)) f] ;; the student expansion
    [((T-FS:) f _) f]
    [((F-FS:) _ f) f]
    ;; skipping the general or/predicate rule because it's really complicated
    ;; or/predicate special case for one elem lists
    ;; note that we are relying on equal? on identifiers here
    [((FilterSet: (list (TypeFilter: t pi x)) (list (NotTypeFilter: t pi x)))
      (T-FS:)
      (FilterSet: (list (TypeFilter: s pi x)) (list (NotTypeFilter: s pi x))))
     (make-FilterSet (list (make-TypeFilter (Un t s) pi x)) (list (make-NotTypeFilter (Un t s) pi x)))]
    ;; or
    [((FilterSet: f1+ f1-) (T-FS:) (FilterSet: f3+ f3-)) (combine null (append f1- f3-))]
    ;; and
    [((FilterSet: f1+ f1-) (FilterSet: f2+ f2-) (F-FS:)) (combine (append f1+ f2+) null)]
    [(f f* f*) f*]
    [(_ _ _)
     ;; could intersect f2 and f3 here
     (make-FilterSet null null)]))