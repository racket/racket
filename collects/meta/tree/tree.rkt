#lang scheme/base

(provide (struct-out tree) leaf? tree-foldl tree-foldr tree-for-each tree->list
         and: or: not: tree-filter)

(require scheme/list)

;; ----------------------------------------------------------------------------
;; Type definitions

;; This is a generic tree representation, subs is a list of subtrees, or #f for
;; a leaf.
;; - `name' is a name for this tree as a byte string, with a "/" suffix for
;;   non-leaf nodes (the filtering code relies on this assumption)
;; - `subs' is a list of subtrees, or #f to mark a leaf
;; - `path' is the full path for to this tree (eg, FS path or a subvesion url),
;;   this code has no assumptions on what's in there
;; - `data' is a placeholder for additional data

(define-struct tree (name subs path [data #:auto #:mutable]))
(define-syntax-rule (leaf? tree) (not (tree-subs tree)))

;; ----------------------------------------------------------------------------
;; Tree utilities

(define (tree-foldl f init tree)
  (let loop ([tree tree] [acc init])
    (let ([subs (tree-subs tree)])
      (if subs
        (let dloop ([subs subs] [acc (f tree acc)])
          (if (null? subs)
            acc
            (dloop (cdr subs) (loop (car subs) acc))))
        (f tree acc)))))

(define (tree-foldr f init tree)
  (let loop ([tree tree] [acc init])
    (let ([subs (tree-subs tree)])
      (f tree (if subs
                (let dloop ([subs subs])
                  (if (null? subs)
                    acc
                    (loop (car subs) (dloop (cdr subs)))))
                acc)))))

(define (tree-for-each f tree)
  (let loop ([tree tree])
    (f tree)
    (let ([subs (tree-subs tree)])
      (when subs (for-each loop subs)))))

(define (tree->list tree) (tree-foldr cons '() tree))

;; ----------------------------------------------------------------------------
;; Tree filtering

;; A tree-filtering predicate is a function that receives a tree, and returns
;; either #t/#f to include or exclude it, or it can return a function to be
;; applied on its sub-trees.  This setup makes it possible to minimize the
;; filtering work that is needed (compared to the old code that would compare
;; full paths).  `tree-filter' takes such a predicate and returns a tree with
;; filtered subtrees, so the smallest result is the empty root.

;; Turns a byte string with globbing into a regexp string.  "*" turns to ".*",
;; "?" turns to ".", "[...]" ranges are used as is, "{...|...}" turns to
;; "(?:...|...)", backslash escapes as usual.  If the glob is "*", "*/", "**",
;; or "**/", a corresponding symbol is returned; and if the glob is all
;; literal, a byte string is returned.  No special treatment of "/"s, since
;; these are used against path elements.  Note that this is applied on each
;; part of a glob string, so "{...|...}" should not have "/"s in them.
(define glob->regexp-or-literal
  (let ([glob-item
         ((compose byte-regexp bytes-append)
          #"(?:"
          #"\\\\."   ; escaped item
          #"|"
          #"[*?{|}]" ; wildcards and options -- the only 1-character matches
          #"|\\[(?:\\^?\\]|\\^?[^]^])[^]]*\\]" ; [...] ranges
          #")"
          )]
        [substs (map cons
                     (bytes->list #"*?{|}")
                     (regexp-split #rx" " #".* . (?: | )"))])
    (define (subq bstr . xs) (regexp-quote (apply subbytes bstr xs)))
    (lambda (glob)
      (define (loop i ps r)
        (if (null? ps)
          (let ([r (apply bytes-append (reverse (cons (subq glob i) r)))])
            (byte-regexp (bytes-append #"^" r #"$")))
          (loop
           (cdar ps) (cdr ps)
           ;; length=1 is only for `*' or `?'
           (cons (if (= 1 (- (cdar ps) (caar ps)))
                   (cdr (or (assq (bytes-ref glob (caar ps)) substs)
                            (error "internal error")))
                   ;; everything else passes through as is, including all
                   ;; backslashed escapes (not always needed, but harmless)
                   (subbytes glob (caar ps) (cdar ps)))
                 ;; and stuff between these things is getting quoted
                 (if (= i (caar ps))
                   r (cons (subq glob i (caar ps)) r))))))
      (cond [(equal? #"*"   glob) '*]
            [(equal? #"*/"  glob) '*/]
            [(equal? #"**"  glob) '**]
            [(equal? #"**/" glob) '**/]
            [(regexp-match #rx#"^[*]+/?$" glob)
             (error 'glob->regexp-or-literal "bad glob: ~e" glob)]
            [else (let ([ps (regexp-match-positions* glob-item glob)])
                    (if (null? ps) glob (loop 0 ps '())))]))))

(define (glob->pred glob)
  (let loop (;; split the glob to its parts, ignoring "//"s and a prefix "/"
             ;; (filter never uses the root path)
             [xs (let ([xs (regexp-match* #rx#"[^/]+(?:/|$)" glob)])
                   ;; it's not clear what should the meaning of an empty glob
                   ;; be: return everything? just the root? nothing? -- throw
                   ;; an error for now, maybe change it later
                   (if (null? xs) (error 'glob->pred "bad glob: ~e" glob)
                       xs))])
    ;; - an element without a trailing slash must be the last one
    ;; - an element with a trailing slash matches non-leaf nodes only, so need
    ;;   to test subs for `*/' and `**/'
    ;; - things usually work out fine, but if it's the last element, then we
    ;;   better return #t or #f rather than a continuation predicate, since a
    ;;   predicate result will never be used and it will mess up (eg, a
    ;;   predicate result for a leaf is considered true, but (not: (lambda (t)
    ;;   #t)) is also a predicate) => use #t for `r' in this case
    (or
     (null? xs)
     (let* ([x  (car xs)]
            [x* (glob->regexp-or-literal x)]
            [xs (cdr xs)]
            [r  (loop xs)])
       (cond
         [(eq? '*   x*) (lambda (t) #t)] ; it's the last one
         [(eq? '*/  x*) (lambda (t) (and (tree-subs t) r))]
         [(eq? '**  x*) (lambda (t) #t)]
         [(eq? '**/ x*)
          (letrec ([R (or: r (lambda (t) (and (tree-subs t) R)))]) R)]
         ;; if it's the last one and it has no "/" suffix then it will match
         ;; only leaves => in this case, allow matches on non-leaf nodes by
         ;; adding the "/" (if this is not done then it's very easy to make
         ;; mistakes)
         [else
          (let ([x*/ (cond [(or (pair? xs) (regexp-match? #rx#"/$" x)) #f]
                           [(bytes? x*) (bytes-append x* #"/")]
                           [(byte-regexp? x*)
                            (glob->regexp-or-literal (bytes-append x #"/"))]
                           [else (error 'glob->pred "bad glob part: ~e" x)])])
            (cond
              [(bytes? x*/)
               (lambda (t)
                 (let ([x (if (tree-subs t) x*/ x*)])
                   (and (equal? x (tree-name t)) r)))]
              [(byte-regexp? x*/)
               (lambda (t)
                 (let ([x (if (tree-subs t) x*/ x*)])
                   (and (regexp-match? x (tree-name t)) r)))]
              [(bytes? x*)
               (lambda (t) (and (tree-subs t) (equal? x* (tree-name t)) r))]
              [(byte-regexp? x*)
               (lambda (t) (and (regexp-match? x* (tree-name t)) r))]))])))))

(define (pred/glob->pred pred/glob)
  (cond [(string? pred/glob) (glob->pred (string->bytes/utf-8 pred/glob))]
        [(bytes? pred/glob) (glob->pred pred/glob)]
        [(procedure? pred/glob) pred/glob]
        [else (error 'pred/glob->pred "bad predicate or glob: ~e" pred/glob)]))

;; Combine tree-filter predicates efficiently: stop when the result is #f or #t
;; for `and:' or `or:' resp., drop predicates that returned #t or #f for them.
(define-syntax-rule (define-combiner name: raw-name: pos neg)
  (begin
    (define raw-name:
      (case-lambda [() (lambda (tree) pos)]
                   [(p1) p1]
                   [(p1 p2) (lambda (tree)
                              (let ([r1 (p1 tree)] [r2 (p2 tree)])
                                (cond [(eq? neg r1) neg]
                                      [(eq? neg r2) neg]
                                      [(eq? pos r1) r2]
                                      [(eq? pos r2) r1]
                                      [else (raw-name: r1 r2)])))]
                   [ps (lambda (tree)
                         (let loop ([ps ps] [rs '()])
                           (if (null? ps)
                             (apply raw-name: (reverse rs))
                             (let ([r ((car ps) tree)] [ps (cdr ps)])
                               (cond [(eq? neg r) neg]
                                     [(eq? pos r) (loop ps rs)]
                                     [else (loop ps (cons r rs))])))))]))
    (define (name: . preds/globs)
      (apply raw-name: (map pred/glob->pred preds/globs)))))
(define-combiner and: raw-and: #t #f)
(define-combiner or:  raw-or:  #f #t)

;; Negating predicates is a little tricky, for example (not: "*/*") would
;; filter out everything in all subtrees, and since empty non-leaf nodes are
;; usually dropped by `tree-filter', this means that the containing trees will
;; be dropped too, leaving only immediate leaves.  The way to make this behave
;; more intuitively is to mark negated predicates, and when filtering with a
;; negated predicate the default is to keep empty non-leaf nodes rather than
;; drop them.  (As an aside, this can also be used to make (not: (not: f))
;; return `f'.)
(define-struct negated (pred orig) #:property prop:procedure 0)
(define (raw-not: p)
  (if (negated? p)
    (negated-orig p)
    (make-negated (lambda (tree)
                    (let ([r (p tree)])
                      (cond [(eq? #t r) #f]
                            [(eq? #f r) #t]
                            [else (raw-not: r)])))
                  p)))
(define (not: pred/glob)
  (raw-not: (pred/glob->pred pred/glob)))

;; filter a whole tree
(define (tree-filter pred/glob tree)
  (define pred (pred/glob->pred pred/glob))
  (define (subs-filter pred tree)
    (let* ([same? #t]
           [subs (tree-subs tree)]
           [new-subs (filter-map (lambda (sub)
                                   (let ([r (loop sub pred)])
                                     (unless (eq? r sub) (set! same? #f))
                                     r))
                                 subs)])
      (cond [(and (null? new-subs) (not (negated? pred))) #f]
            [same? tree]
            [else (make-tree (tree-name tree) new-subs (tree-path tree))])))
  (define (loop tree pred)
    (let ([r (pred tree)])
      (cond [(eq? #t r) tree]
            [(eq? #f r) #f]
            [(procedure? r) (and (tree-subs tree) (subs-filter r tree))]
            [else (error 'tree-filter "bad result from predicate: ~e" r)])))
  (if (leaf? tree)
    (error 'tree-filter "expecting a non-leaf, got ~e" tree)
    (or (subs-filter pred tree)
        (make-tree (tree-name tree) '() (tree-path tree)))))
