#lang racket/base

;; Functionality to optimize a static contract to provide faster checking.
;; Also supports droping checks on either side.

(require
  "combinators.rkt"
  "structures.rkt"
  racket/set
  racket/syntax
  racket/dict
  syntax/id-table
  racket/list
  racket/contract
  racket/match)



(provide
  (contract-out
    [optimize ((static-contract?) (#:trusted-positive boolean? #:trusted-negative boolean?)
               . ->* . static-contract?)]))

;; Reduce a static contract to a smaller simpler one that protects in the same way
(define (reduce sc)
  (match sc
    ;; none/sc cases
    [(listof/sc: (none/sc:)) empty-list/sc]
    [(list/sc: sc1 ... (none/sc:) sc2 ...) none/sc]
    [(set/sc: (none/sc:)) empty-set/sc]
    [(syntax/sc: (none/sc:)) none/sc]
    ;; The following are unsound because chaperones allow operations on these data structures to
    ;; can call continuations and thus be useful even if they cannot return values.
    ;[(vectorof/sc: (none/sc:)) empty-vector/sc]
    ;[(vector/sc: sc1 ... (none/sc:) sc2 ...) none/sc]
    ;[(box/sc: (none/sc:)) none/sc]
    ;[(promise/sc: (none/sc:)) none/sc]
    ;[(hash/sc: (none/sc:) value/sc) empty-hash/sc]
    ;[(hash/sc: key/sc (none/sc:)) empty-hash/sc]

    ;; any/sc cases
    [(cons/sc: (any/sc:) (any/sc:)) cons?/sc]
    [(listof/sc: (any/sc:)) list?/sc]
    [(list/sc: (and scs (any/sc:)) ...) (list-length/sc (length scs))]
    [(vectorof/sc: (any/sc:)) vector?/sc]
    [(vector/sc: (and scs (any/sc:)) ...) (vector-length/sc (length scs))]
    [(set/sc: (any/sc:)) set?/sc]
    [(box/sc: (any/sc:)) box?/sc]
    [(syntax/sc: (any/sc:)) syntax?/sc]
    [(promise/sc: (any/sc:)) promise?/sc]
    [(hash/sc: (any/sc:) (any/sc:)) hash?/sc]

    ;; or/sc cases
    [(or/sc: scs ...)
     (match scs
      [(list) none/sc]
      [(list sc) sc]
      [(? (λ (l) (member any/sc l))) any/sc]
      [(? (λ (l) (member none/sc l)))
       (apply or/sc (remove* (list none/sc) scs))]
      [else sc])]

    ;; and/sc cases
    [(and/sc: scs ...)
     (match scs
      [(list) any/sc]
      [(list sc) sc]
      [(? (λ (l) (member none/sc l))) none/sc]
      [(? (λ (l) (member any/sc l)))
       (apply and/sc (remove* (list any/sc) scs))]
      [else sc])]


    ;; case->/sc cases
    [(case->/sc: arrs ...)
     (match arrs
       ;; We can turn case->/sc contracts int ->* contracts in some cases.
       [(list (arr/sc: args #f ranges) ...) (=> fail)
        ;; All results must have the same range
        (unless (equal? (set-count (apply set ranges)) 1)
          (fail))
        (define sorted-args (sort args (λ (l1 l2) (< (length l1) (length l2)))))
        (define shortest-args (first sorted-args))
        (define longest-args (last sorted-args))
        ;; The number of arguments must increase by 1 with no gaps
        (unless (equal? (map length sorted-args)
                        (range (length shortest-args)
                               (add1 (length longest-args))))
          (fail))
        ;; All arities must be prefixes of the longest arity
        (unless (for/and ([args (in-list sorted-args)])
                  (equal? args (take longest-args (length args))))
          (fail))
        ;; All the checks passed
        (function/sc
          (take longest-args (length shortest-args))
          (drop longest-args (length shortest-args))
          empty
          empty
          #f
          (first ranges))]
       [else sc])]



    [else sc]))


;; Reduce a static contract assuming that we trusted the current positive side
(define (trusted-side-reduce sc)
  (match sc
    [(->/sc: mand-args opt-args mand-kw-args opt-kw-args rest-arg (list (any/sc:) ...))
     (function/sc mand-args opt-args mand-kw-args opt-kw-args rest-arg #f)]
    [(arr/sc: args rest (list (any/sc:) ...))
     (arr/sc args rest #f)]
    [(none/sc:) any/sc]
    [(app sc-terminal-kind 'flat) any/sc]
    [else sc]))



(define (invert-side v)
  (case v
    [(positive) 'negative]
    [(negative) 'positive]
    [(both) 'both]))

(define (combine-variance side var)
  (case var
    [(covariant) side]
    [(contravariant) (invert-side side)]
    [(invariant) 'both]))

(define (remove-unused-recursive-contracts sc)
  (define root (generate-temporary))
  (define main-table (make-free-id-table))
  (define (search)
    (define table (make-free-id-table))
    (define (recur sc variance)
      (match sc
        [(recursive-sc-use id)
         (dict-set! table id #t)]
        [(recursive-sc names values body)
         (recur body 'covariant)
         (for ([name (in-list names)]
               [value (in-list values)])
          (dict-set! main-table name ((search) value)))]
        [else
          (sc-traverse sc recur)]))
    (lambda (sc)
      (recur sc 'covariant)
      table))
  (define reachable ((search) sc))
  (define seen (make-free-id-table reachable))
  (let loop ((to-look-at reachable))
    (unless (zero? (dict-count to-look-at))
      (define new-table (make-free-id-table))
      (for ([(id _) (in-dict to-look-at)])
        (for ([(id _) (in-dict (dict-ref main-table id))])
          (unless (dict-has-key? seen id)
            (dict-set! seen id #t)
            (dict-set! new-table id #t))))
      (loop new-table)))

  ;; Determine if the recursive name is referenced in the static contract
  (define (unused? new-name sc)
    (let/ec exit
      (define (recur sc variance)
        (match sc
          [(recursive-sc-use (== new-name free-identifier=?))
           (exit #f)]
          [else
            (sc-traverse sc recur)]))
      (recur sc 'covariant)
      #t))

  (define (trim sc variance)
    (match sc
      [(recursive-sc names values body)
       (define new-body (trim body 'covariant))

       (define new-name-values
         (for/list ([name (in-list names)]
                    [value (in-list values)]
                    #:when (dict-ref seen name #f))
            (list name value)))
       (define new-names (map first new-name-values))
       (define new-values (map (λ (v) (trim v 'covariant))
                               (map second new-name-values)))
       (cond
         [(empty? new-names) new-body]
         [(and
            (equal? (length new-names) 1)
            (recursive-sc-use? new-body)
            (free-identifier=? (first new-names) (recursive-sc-use-name new-body))
            (unused? (first new-names) (first new-values)))
          (first new-values)]
         [else
          (recursive-sc new-names new-values new-body)])]
      [else
        (sc-map sc trim)]))
  (trim sc 'covariant))


;; If we trust a specific side then we drop all contracts protecting that side.
(define (optimize sc #:trusted-positive [trusted-positive #f] #:trusted-negative [trusted-negative #f])
  ;; single-step: reduce and trusted-side-reduce if appropriate
  (define (single-step sc side)
    (define trusted
      (case side
        [(positive) trusted-positive]
        [(negative) trusted-negative]
        [(both) (and trusted-positive trusted-negative)]))

    (reduce
      (if trusted
          (trusted-side-reduce sc)
          sc)))

  ;; full-pass: single-step at every static contract subpart
  (define (full-pass sc)
    (define ((recur side) sc variance)
      (define new-side (combine-variance side variance))
      (single-step (sc-map sc (recur new-side)) new-side))
    ((recur 'positive) sc 'covariant))

  ;; Do full passes until we reach a fix point, and then remove all unneccessary recursive parts
  (let loop ((sc sc))
    (define new-sc (full-pass sc))
    (if (equal? sc new-sc)
        (remove-unused-recursive-contracts new-sc)
        (loop new-sc))))
