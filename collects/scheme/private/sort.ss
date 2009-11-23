(module sort '#%kernel

(#%require "small-scheme.ss" "define.ss" (for-syntax "stxcase-scheme.ss"))

(#%provide sort)

#|

Based on "Fast mergesort implementation based on half-copying merge algorithm",
Cezary Juszczak, http://kicia.ift.uni.wroc.pl/algorytmy/mergesortpaper.pdf
Written in Scheme by Eli Barzilay.  (Note: the reason for the seemingly
redundant pointer arithmetic in that paper is dealing with cases of uneven
number of elements.)

The source uses macros to optimize some common cases (eg, no `getkey'
function, or precompiled versions with inlinable common comparison
predicates) -- they are local macros so they're not left in the compiled
code.

Note that there is no error checking on the arguments -- the `sort' function
that this module provide is then wrapped up by a keyworded version in
"scheme/private/list.ss", and that's what everybody sees.  The wrapper is
doing these checks.

|#


(define sort (let ()

(define-syntax define-syntax-rule
  (syntax-rules ()
    [(dr (foo . pattern) template)
     (define-syntax foo (syntax-rules () [(_ . pattern) template]))]))

(define-syntax-rule (sort-internal-body v *<? n has-getkey? getkey)
  (begin
    (define-syntax-rule (<? x y)
      (if has-getkey? (*<? (getkey x) (getkey y)) (*<? x y)))
    (define-syntax-rule (ref n) (vector-ref v n))
    (define-syntax-rule (set! n x) (vector-set! v n x))

    (define (merge1 A1 A2 B1 B2 C1 C2)
      (when (< C1 B1)
        (if (< B1 B2)
          (if (<? (ref B1) (ref A1))
            (begin (set! C1 (ref B1))
                   (merge1 A1 A2 (add1 B1) B2 (add1 C1) C2))
            (begin (set! C1 (ref A1))
                   (merge1 (add1 A1) A2 B1 B2 (add1 C1) C2)))
          (begin (set! C1 (ref A1))
                 (merge1 (add1 A1) A2 B1 B2 (add1 C1) C2)))))
    (define (merge2 A1 A2 B1 B2 C1 C2)
      (when (< C1 B1)
        (if (< B1 B2)
          (if (<? (ref A1) (ref B1))
            (begin (set! C1 (ref A1))
                   (merge2 (add1 A1) A2 B1 B2 (add1 C1) C2))
            (begin (set! C1 (ref B1))
                   (merge2 A1 A2 (add1 B1) B2 (add1 C1) C2)))
          (begin (set! C1 (ref A1))
                 (merge2 (add1 A1) A2 B1 B2 (add1 C1) C2)))))

    (define (copying-mergesort Alo Ahi Blo Bhi)
      (cond [(< Alo (sub1 Ahi))
             (let ([Amid (/ (+ Alo Ahi) 2)] [Bmid (/ (+ Blo Bhi) 2)])
               (copying-mergesort Amid Ahi Bmid Bhi)
               (copying-mergesort Alo Amid Amid Ahi)
               (merge1 Amid Ahi Bmid Bhi Blo Bhi))]
            [(= Alo (sub1 Ahi))
             (set! Blo (ref Alo))]))

    (define (mergesort Alo Ahi B1lo B1hi)
      (let ([Amid (/ (+ Alo Ahi) 2)])
        (copying-mergesort Amid Ahi B1lo B1hi)
        (copying-mergesort Alo Amid Amid Ahi)
        (merge2 B1lo B1hi Amid Ahi Alo Ahi)))

    (mergesort 0 n n (+ n (/ n 2)))))

(define sort-internals (make-hasheq))
(define _
  (let ()
    (define-syntax-rule (precomp <? more ...)
      (let ([proc (lambda (vec n) (sort-internal-body vec <? n #f #f))])
        (hash-set! sort-internals <? proc)
        (hash-set! sort-internals more proc) ...))
    (precomp < <=)
    (precomp > >=)
    (precomp string<? string<=?)
    (precomp string-ci<? string-ci<=?)
    (precomp keyword<?)))

(define sort-internal
  (case-lambda
    [(<? vec n)
     (let ([si (hash-ref sort-internals <? #f)])
       (if si
         ;; use a precompiled function if found
         (si vec n)
         ;; otherwise, use the generic code
         (let () (sort-internal-body vec <? n #f #f))))]
    [(<? vec n getkey)
     (let () (sort-internal-body vec <? n #t getkey))]))

(define-syntax-rule (sort-body lst *<? has-getkey? getkey cache-keys?)
  (let ([n (length lst)])
    (define-syntax-rule (<? x y)
      (if has-getkey? (*<? (getkey x) (getkey y)) (*<? x y)))
    (cond
      ;; trivial case
      [(= n 0) lst]
      ;; below we can assume a non-empty input list
      [cache-keys?
       ;; decorate while converting to a vector, and undecorate when going
       ;; back, always do this for consistency
       (let ([vec (make-vector (+ n (/ n 2)))])
         ;; list -> decorated-vector
         (let loop ([i 0] [lst lst])
           (when (pair? lst)
             (let ([x (car lst)])
               (vector-set! vec i (cons (getkey x) x))
               (loop (add1 i) (cdr lst)))))
         ;; sort
         (sort-internal *<? vec n car)
         ;; decorated-vector -> list
         (let loop ([i n] [r '()])
           (let ([i (sub1 i)])
             (if (< i 0) r (loop i (cons (cdr (vector-ref vec i)) r))))))]
      ;; trivial cases
      [(< n 2) lst]
      ;; check if the list is already sorted (which can be common, eg,
      ;; directory lists)
      [(let loop ([last (car lst)] [next (cdr lst)])
         (or (null? next)
             (and (not (<? (car next) last))
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
           ;; (<? later-item earlier-item) since it gives more information.  A
           ;; good way to see that we have good code is to check that each
           ;; permutation appears exactly once.  This means that n=4 will have
           ;; 23 cases, so don't bother.  (Homework: write a macro to generate
           ;; code for a specific N.  Bonus: prove correctness.  Extra bonus:
           ;; prove optimal solution.  Extra extra bonus: prove optimal
           ;; solution exists, extract macro from proof.)
           (let ([a (car lst)] [b (cadr lst)] [c (caddr lst)])
             (if (<? b a)
               ;; b<a
               (if (<? c b)
                 (list c b a)
                 ;; b<a, b<=c
                 (if (<? c a) (list b c a) (list b a c)))
               ;; a<=b, so c<b (b<=c is impossible due to above test)
               (if (<? c a) (list c a b) (list a c b))))))]
      [else (let ([vec (make-vector (+ n (/ n 2)))])
              ;; list -> vector
              (let loop ([i 0] [lst lst])
                (when (pair? lst)
                  (vector-set! vec i (car lst))
                  (loop (add1 i) (cdr lst))))
              ;; sort
              (if getkey
                (sort-internal *<? vec n getkey)
                (sort-internal *<? vec n))
              ;; vector -> list
              (let loop ([i n] [r '()])
                (let ([i (sub1 i)])
                  (if (< i 0) r (loop i (cons (vector-ref vec i) r))))))])))

;; Finally, this is the provided `sort' value
(case-lambda
  [(lst <?) (sort-body lst <? #f #f #f)]
  [(lst <? getkey)
   (if (and getkey (not (eq? values getkey)))
     (sort lst <? getkey #f) (sort lst <?))]
  [(lst <? getkey cache-keys?)
   (if (and getkey (not (eq? values getkey)))
     (sort-body lst <? #t getkey cache-keys?) (sort lst <?))])

)))
