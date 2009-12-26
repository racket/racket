#lang scheme/base

(provide tree-foldl tree-foldr tree-for-each print-tree
         tree->list tree->path-list and: or: not: tree-filter get-tree)

(require scheme/list)

;; ----------------------------------------------------------------------------

(define-struct tree (name))
(define-struct (file tree) ())
(define-struct (dir tree) (subs))

(define (tree-foldl f init tree)
  (let loop ([tree tree] [base #""] [acc init])
    (if (file? tree)
      (f tree base acc)
      (let ([base* (bytes-append base (tree-name tree) #"/")])
        (let dloop ([trees (dir-subs tree)] [acc (f tree base acc)])
          (if (null? trees)
            acc
            (dloop (cdr trees) (loop (car trees) base* acc))))))))

(define (tree-foldr f init tree)
  (let loop ([tree tree] [base #""] [acc init])
    (f tree base
       (if (file? tree)
         acc
         (let ([base* (bytes-append base (tree-name tree) #"/")])
           (let dloop ([trees (dir-subs tree)])
             (if (null? trees)
               acc
               (loop (car trees) base* (dloop (cdr trees))))))))))

(define (tree-for-each f tree)
  (let loop ([tree tree] [base #""])
    (f tree base)
    (when (dir? tree)
      (let ([base* (bytes-append base (tree-name tree) #"/")])
        (for/list ([tree (in-list (dir-subs tree))]) (loop tree base*)))))
  (void))

(define (print-tree tree)
  (tree-for-each
   (lambda (tree base)
     (write-bytes base) (write-bytes (tree-name tree)) (newline))
   tree))

(define (tree->list tree)
  (tree-foldr (lambda (tree base acc) (cons tree acc)) '() tree))

(define (tree->path-list tree)
  (tree-foldr (lambda (tree base acc)
                         (cons (bytes-append base (tree-name tree)) acc))
                       '() tree))

;; ----------------------------------------------------------------------------

;; a tree-filtering predicate is a function that receives a tree, and returns
;; either #t/#f to include or exclude it, or it can return a function to be
;; applied on the sub-trees of a directory.  This setup makes it possible to
;; minimize the filtering work that is needed (compared to the old code that
;; would compare full paths).  `tree-filter' takes such a predicate and returns
;; a tree with filtered subtrees, so the smallest result is the empty root.

;; Turns a byte string with globbing into a regexp string.  "*" turns to ".*",
;; "?" turns to ".", "[...]" ranges are used as is, "{...|...}" turns to
;; "(?:...|...)", backslash escapes as usual.  If the glob is "*" or "**", a
;; corresponding symbol is returned; and if the glob is all literal, a byte
;; string is returned.  No special treatment of "/"s, since these are used
;; against path elements.  Note that this is applied on each part of a glob
;; string, so "{...|...}" should not have "/"s in them.
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
      (cond [(equal? #"*" glob) '*]
            [(equal? #"**" glob) '**]
            [(regexp-match #rx#"^[*]+$" glob)
             (error 'glob->regexp-or-literal "bad glob: ~e" glob)]
            [else (let ([ps (regexp-match-positions* glob-item glob)])
                    (if (null? ps) glob (loop 0 ps '())))]))))

(define (glob->pred glob)
  (let loop ([xs (map glob->regexp-or-literal (regexp-split #rx#"/" glob))])
    ;; xs is never null
    (let* ([x (car xs)]
           [xs (cdr xs)]
           [r (and (pair? xs) (loop xs))])
      (if r
        ;; there's more to match => can only be true for dirs
        (cond
          [(eq? '*  x) (lambda (t) (and (dir? t) r))]
          [(eq? '** x) (letrec ([R (or: r (lambda (t) (and (dir? t) R)))]) R)]
          [(bytes?  x) (lambda (t) (and (dir? t) (equal? x (tree-name t)) r))]
          [(byte-regexp? x)
           (lambda (t) (and (dir? t) (regexp-match? x (tree-name t)) r))]
          [else (error 'glob->pred "bad glob element: ~e" x)])
        ;; the last element => matches files and dirs, returns a proper boolean
        (cond
          [(or (eq? '* x) (eq? '** x) (equal? #"" x)) (lambda (tree) #t)]
          [(bytes? x) (lambda (t) (equal? x (tree-name t)))]
          [(byte-regexp? x) (lambda (t) (regexp-match? x (tree-name t)))]
          [else (error 'glob->pred "bad glob element: ~e" x)])))))

(define (pred/glob->pred pred/glob)
  (cond [(string? pred/glob) (glob->pred (string->bytes/utf-8 pred/glob))]
        [(bytes? pred/glob) (glob->pred pred/glob)]
        [(procedure? pred/glob) pred/glob]
        [else (error 'pred/glob->pred "bad predicate or glob: ~e" pred/glob)]))

;; combine tree-filter predicates efficiently: stop when the result is #f or #t
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
(define (raw-not: p)
  (lambda (tree)
    (let ([r (p tree)])
      (cond [(eq? #t r) #f]
            [(eq? #f r) #t]
            [else (raw-not: r)]))))
(define (not: pred/glob)
  (raw-not: (pred/glob->pred pred/glob)))

(define (tree-filter pred/glob tree)
  (define pred (pred/glob->pred pred/glob))
  (define-syntax-rule (dir-filter pred dir)
    (let* ([same? #t]
           [subs (dir-subs dir)]
           [new-subs (filter-map (lambda (sub)
                                   (let ([r (loop sub pred)])
                                     (unless (eq? r sub) (set! same? #f))
                                     r))
                                 subs)])
      (and (pair? new-subs)
           (if same? dir (make-dir (tree-name dir) new-subs)))))
  (define (loop tree pred)
    (let ([r (pred tree)])
      (cond [(eq? #t r) tree]
            [(eq? #f r) #f]
            [(procedure? r) (and (dir? tree) (dir-filter r tree))]
            [else (error 'tree-filter "bad result from predicate: ~e" r)])))
  (if (file? tree)
    (error 'tree-filter "expecting a `dir', got ~e" tree)
    (or (dir-filter pred tree) (make-dir (tree-name tree) '()))))

;; ----------------------------------------------------------------------------

(define (get-tree dir)
  (define (subs dir)
    (parameterize ([current-directory dir])
      (sort
       (for/list ([path (directory-list)])
         (let ([name (path-element->bytes path)])
           (cond
             [(directory-exists? path)
              (make-dir name (subs path))]
             [(file-exists? path) (make-file name)]
             [else (error 'get-tree "bad path encountered: ~a/~a"
                          (current-directory) path)])))
       bytes<?
       #:key tree-name)))
  (make-dir (regexp-replace #rx#"/$" (path->bytes (simplify-path dir)) #"")
            (subs dir)))
