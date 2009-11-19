(module sort '#%kernel

(#%require "small-scheme.ss" "define.ss" (for-syntax "stxcase-scheme.ss"))

(#%provide sort)

;; This is a destructive stable merge-sort, adapted from slib and improved by
;; Eli Barzilay.
;; The original source said:
;;   It uses a version of merge-sort invented, to the best of my knowledge, by
;;   David H. D. Warren, and first used in the DEC-10 Prolog system.
;;   R. A. O'Keefe adapted it to work destructively in Scheme.
;; but it's a plain destructive merge sort, which I optimized further.

;; The source uses macros to optimize some common cases (eg, no `getkey'
;; function, or precompiled versions with inlinable common comparison
;; predicates) -- they are local macros so they're not left in the compiled
;; code.

;; Note that there is no error checking on the arguments -- the `sort' function
;; that this module provide is then wrapped up by a keyworded version in
;; "scheme/private/list.ss", and that's what everybody sees.  The wrapper is
;; doing these checks.

(define sort (let ()

(define-syntax define-syntax-rule
  (syntax-rules ()
    [(dr (foo . pattern) template)
     (define-syntax foo (syntax-rules () [(_ . pattern) template]))]))

(define-syntax-rule (sort-internal-body lst *less? n has-getkey? getkey)
  (begin
    (define-syntax-rule (less? x y)
      (if has-getkey? (*less? (getkey x) (getkey y)) (*less? x y)))
    (define (merge-sorted! a b)
      ;; r-a? for optimization -- is r connected to a?
      (define (loop r a b r-a?)
        (if (less? (mcar b) (mcar a))
          (begin
            (when r-a? (set-mcdr! r b))
            (if (null? (mcdr b)) (set-mcdr! b a) (loop b a (mcdr b) #f)))
          ;; (car a) <= (car b)
          (begin
            (unless r-a? (set-mcdr! r a))
            (if (null? (mcdr a)) (set-mcdr! a b) (loop a (mcdr a) b #t)))))
      (cond [(null? a) b]
            [(null? b) a]
            [(less? (mcar b) (mcar a))
             (if (null? (mcdr b)) (set-mcdr! b a) (loop b a (mcdr b) #f))
             b]
            [else ; (car a) <= (car b)
             (if (null? (mcdr a)) (set-mcdr! a b) (loop a (mcdr a) b #t))
             a]))
    (let step ([n n])
      (cond [(> n 3)
             (let* (; let* not really needed with mzscheme's l->r eval
                    [j (quotient n 2)] [a (step j)] [b (step (- n j))])
               (merge-sorted! a b))]
            ;; the following two cases are just explicit treatment of sublists
            ;; of length 2 and 3, could remove both (and use the above case for
            ;; n>1) and it would still work, except a little slower
            [(= n 3) (let ([p lst] [p1 (mcdr lst)] [p2 (mcdr (mcdr lst))])
                       (let ([x (mcar p)] [y (mcar p1)] [z (mcar p2)])
                         (set! lst (mcdr p2))
                         (cond [(less? y x) ; y x
                                (cond [(less? z y) ; z y x
                                       (set-mcar! p  z)
                                       (set-mcar! p1 y)
                                       (set-mcar! p2 x)]
                                      [(less? z x) ; y z x
                                       (set-mcar! p  y)
                                       (set-mcar! p1 z)
                                       (set-mcar! p2 x)]
                                      [else ; y x z
                                       (set-mcar! p  y)
                                       (set-mcar! p1 x)])]
                               [(less? z x) ; z x y
                                (set-mcar! p  z)
                                (set-mcar! p1 x)
                                (set-mcar! p2 y)]
                               [(less? z y) ; x z y
                                (set-mcar! p1 z)
                                (set-mcar! p2 y)])
                         (set-mcdr! p2 '())
                         p))]
            [(= n 2) (let ([x (mcar lst)] [y (mcar (mcdr lst))] [p lst])
                       (set! lst (mcdr (mcdr lst)))
                       (when (less? y x)
                         (set-mcar! p y)
                         (set-mcar! (mcdr p) x))
                       (set-mcdr! (mcdr p) '())
                       p)]
            [(= n 1) (let ([p lst])
                       (set! lst (mcdr lst))
                       (set-mcdr! p '())
                       p)]
            [else '()]))))

(define sort-internals (make-hasheq))
(define _
  (let ()
    (define-syntax-rule (precomp less? more ...)
      (let ([proc (lambda (lst n) (sort-internal-body lst less? n #f #f))])
        (hash-set! sort-internals less? proc)
        (hash-set! sort-internals more proc) ...))
    (precomp < <=)
    (precomp > >=)
    (precomp string<? string<=?)
    (precomp string-ci<? string-ci<=?)
    (precomp keyword<?)))

(define sort-internal
  (case-lambda
    [(less? lst n)
     (let ([si (hash-ref sort-internals less? #f)])
       (if si
         ;; use a precompiled function if found
         (si lst n)
         ;; otherwise, use the generic code
         (let () (sort-internal-body lst less? n #f #f))))]
    [(less? lst n getkey)
     (sort-internal-body lst less? n #t getkey)]))

(define-syntax-rule (sort-body lst *less? has-getkey? getkey cache-keys?)
  (let ([n (length lst)])
    (define-syntax-rule (less? x y)
      (if has-getkey? (*less? (getkey x) (getkey y)) (*less? x y)))
    (cond
      ;; trivial case
      [(= n 0) lst]
      ;; below we can assume a non-empty input list
      [cache-keys?
       ;; decorate while converting to an mlist, and undecorate when going
       ;; back, always do this for consistency
       (let (;; list -> decorated-mlist
             [mlst (let ([x (car lst)]) (mcons (cons (getkey x) x) null))])
         (let loop ([last mlst] [lst (cdr lst)])
           (when (pair? lst)
             (let ([new (let ([x (car lst)]) (mcons (cons (getkey x) x) null))])
               (set-mcdr! last new)
               (loop new (cdr lst)))))
         ;; decorated-mlist -> list
         (let loop ([r (sort-internal *less? mlst n car)])
           (if (null? r) r (cons (cdr (mcar r)) (loop (mcdr r))))))]
      ;; trivial cases
      [(< n 2) lst]
      ;; check if the list is already sorted (which can be common, eg,
      ;; directory lists)
      [(let loop ([last (car lst)] [next (cdr lst)])
         (or (null? next)
             (and (not (less? (car next) last))
                  (loop (car next) (cdr next)))))
       lst]
      ;; below we can assume an unsorted list
      ;; inlined case, for optimization of short lists
      [(<= n 3)
       (if (= n 2)
         ;; (because of the above test, we can assume that the input is
         ;; unsorted)
         (list (cadr lst) (car lst))
         (let ([a (car lst)] [b (cadr lst)] [c (caddr lst)])
           ;; General note: we need a stable sort, so we should always compare
           ;; (less? later-item earlier-item) since it gives more information.
           ;; A good way to see that we have good code is to check that each
           ;; permutation appears exactly once.  This means that n=4 will have
           ;; 23 cases, so don't bother.  (Homework: write a macro to generate
           ;; code for a specific N.  Bonus: prove correctness.  Extra bonus:
           ;; prove optimal solution.  Extra extra bonus: prove optimal
           ;; solution exists, extract macro from proof.)
           (let ([a (car lst)] [b (cadr lst)] [c (caddr lst)])
             (if (less? b a)
               ;; b<a
               (if (less? c b)
                 (list c b a)
                 ;; b<a, b<=c
                 (if (less? c a) (list b c a) (list b a c)))
               ;; a<=b, so c<b (b<=c is impossible due to above test)
               (if (less? c a) (list c a b) (list a c b))))))]
      [else (let (;; list -> mlist
                  [mlst (mcons (car lst) null)])
              (let loop ([last mlst] [lst (cdr lst)])
                (when (pair? lst)
                  (let ([new (mcons (car lst) null)])
                    (set-mcdr! last new)
                    (loop new (cdr lst)))))
              ;; mlist -> list
              (let loop ([r (if getkey
                              (sort-internal *less? mlst n getkey)
                              (sort-internal *less? mlst n))])
                (if (null? r) r (cons (mcar r) (loop (mcdr r))))))])))

;; Finally, this is the provided `sort' value
(case-lambda
  [(lst less?) (sort-body lst less? #f #f #f)]
  [(lst less? getkey)
   (if (and getkey (not (eq? values getkey)))
     (sort lst less? getkey #f) (sort lst less?))]
  [(lst less? getkey cache-keys?)
   (if (and getkey (not (eq? values getkey)))
     (sort-body lst less? #t getkey cache-keys?) (sort lst less?))])

)))
