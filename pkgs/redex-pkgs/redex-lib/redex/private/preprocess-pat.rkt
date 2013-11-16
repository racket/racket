#lang racket/base

(require racket/match
         racket/set

         "2set.rkt"
         "env.rkt"
         "error.rkt"
         "match-a-pattern.rkt")

(provide preprocess
         (struct-out ann-pat))

;; preprocess : pat -> (ann-pat env pat)
(define (preprocess pat)
  (build-env (remove-names pat)))

;; This function returns an env containing all top-level name references, i.e., the ones that need to be enumerated doing anything
(define (build-env pat)
  (define counter 0)
  (define (get-and-inc!)
    (begin0 counter
      (set! counter (add1 counter))))
  (define (walk pat)
    (match-a-pattern/single-base-case pat
      [`(name ,n ,subpat)
       (match-define (ann-pat subenv new-subpat) (walk subpat))
       (ann-pat (add-name subenv n subpat)
                `(name ,n ,new-subpat))]
      [`(mismatch-name ,n ,subpat)
       (unimplemented "mismatch")
       (match-define (ann-pat subenv new-subpat) (walk subpat))
       (define tag (get-and-inc!))
       (ann-pat (add-mismatch subenv n subpat tag)
                `(mismatch-name ,n ,tag))]
      [`(in-hole ,p1 ,p2)
       (match-define (ann-pat subenv1 newsub1)
                     (walk p1))
       (match-define (ann-pat subenv2 newsub2)
                     (walk p2))
       (ann-pat (env-union subenv1 subenv2)
                `(in-hole ,newsub1 ,newsub2))]
      [`(hide-hole ,p)
       (match-define (ann-pat subenv newsub)
                     (walk p))
       (ann-pat subenv `(hide-hole ,newsub))]
      [`(side-condition ,p ,c ,s)
       (unsupported "side-condition")]
      [`(list ,sub-pats ...)
       (define ann-sub-pats
         (for/list ([sub-pat (in-list sub-pats)])
           (match sub-pat
             [`(repeat ,p #f #f)
              (ann-pat empty-env sub-pat)]
             [`(repeat ,p ,n #f)
              (match-define (ann-pat subenv subp)
                            (walk p))
              (define tag (get-and-inc!))
              (ann-pat (pure-nrep n subenv tag subp)
                       `(repeat ,tag ,n #f))]
             [`(repeat ,p ,n ,m)
              (unimplemented "mismatch repeat")]
             [_ (walk sub-pat)])))
       (define list-env
         (for/fold ([accenv empty-env])
                   ([sub-apat (in-list ann-sub-pats)])
           (match sub-apat
             [(ann-pat subenv _)
              (env-union subenv accenv)])))
       (ann-pat list-env (cons 'list (map ann-pat-pat ann-sub-pats)))]
      [_ (pure-ann-pat pat)]))
  (define res
    (walk pat))
  res)

(define (remove-names pat)
  (define names-2set (find-names pat))
  (define names (2set-ones names-2set))
  (define 2names (2set-manys names-2set))
  (define badnames (set-subtract names 2names))
  (define (strip-named name subpat con)
    (define sub-stripped (strip subpat))
    (if (set-member? badnames name)
        sub-stripped
        (con name sub-stripped)))
  (define (keep-if-good name)
    (and (not (set-member? badnames name))
         name))
  (define (strip pat)
    (match-a-pattern/single-base-case pat
      [`(name ,n ,subpat)
       (strip-named n subpat (λ (n s) `(name ,n ,s)))]
      [`(mismatch-name ,n ,subpat)
       (strip-named n subpat (λ (n s) `(mismatch-name ,n ,s)))]
      [`(in-hole ,p1 ,p2)
       `(in-hole ,(strip p1)
                 ,(strip p2))]
      [`(hide-hole ,p)
       `(hide-hole ,(strip p))]
      [`(side-condition ,p ,c ,s)
       `(side-condition ,(strip p) ,c ,s)]
      [`(list ,sub-pats ...)
       (cons 'list
             (map (match-lambda
                   [`(repeat ,p ,n ,m)
                    (define sub (strip p))
                    (define s-n (keep-if-good n))
                    (define s-m (keep-if-good m))
                    `(repeat ,sub ,s-n ,s-m)]
                   [sub-pat (strip sub-pat)])
                  sub-pats))]
      [_ pat]))
  (strip pat))

(define (find-names pat)
  (match-a-pattern/single-base-case pat
    [`(name ,n ,subpat)
     (2set-add (find-names subpat)
               n)]
    [`(mismatch-name ,n ,subpat)
     (2set-add (find-names subpat)
               n)]
    [`(in-hole ,p1 ,p2)
     (2set-union (find-names p1)
                 (find-names p2))]
    [`(hide-hole ,p)
     (find-names p)]
    [`(side-condition ,p ,c ,s)
     (find-names p)]
    [`(list ,sub-pats ...)
     (foldr 2set-union
            (2set)
            (map (match-lambda
                  [`(repeat ,p ,n ,m)
                   (2set-add (find-names p) n m)]
                  [sub-pat (find-names sub-pat)])
                 sub-pats))]
    [_ (2set)]))

;; Patterns annotated with variable/name/repeat information
(struct ann-pat (ann pat)
        #:transparent)

(define (pure-ann-pat pat)
  (ann-pat empty-env pat))
