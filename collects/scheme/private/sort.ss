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
  (let* ([n/2- (arithmetic-shift n -1)] [n/2+ (- n n/2-)])
    (define-syntax-rule (<? x y)
      (if has-getkey? (*<? (getkey x) (getkey y)) (*<? x y)))
    (define-syntax-rule (ref n) (vector-ref v n))
    (define-syntax-rule (set! n x) (vector-set! v n x))

    (define-syntax-rule (merge lo? A1 A2 B1 B2 C1)
      (let ([b2 B2])
        (let loop ([a1 A1] [b1 B1] [c1 C1])
          (let ([x (ref a1)] [y (ref b1)])
            (if (if lo? (not (<? y x)) (<? x y))
              (begin (set! c1 x)
                     (let ([a1 (add1 a1)] [c1 (add1 c1)])
                       (when (< c1 b1) (loop a1 b1 c1))))
              (begin (set! c1 y)
                     (let ([b1 (add1 b1)] [c1 (add1 c1)])
                       (if (<= b2 b1)
                         (let loop ([a1 a1] [c1 c1])
                           (when (< c1 b1)
                             (set! c1 (ref a1))
                             (loop (add1 a1) (add1 c1))))
                         (loop a1 b1 c1)))))))))

    (define-syntax-rule (copying-insertionsort Alo Blo n)
      (let iloop ([i 0] [A Alo])
        (when (< i n)
          (let ([ref-i (ref A)])
            (let jloop ([j (+ Blo i)])
              (let ([ref-j-1 (ref (sub1 j))])
                (if (and (> j Blo) (<? ref-i ref-j-1))
                  (begin (set! j ref-j-1) (jloop (sub1 j)))
                  (begin (set! j ref-i) (iloop (add1 i) (add1 A))))))))))

    (define (copying-mergesort Alo Blo n)
      ;; n is never 0, smaller values are more frequent
      (cond
        [(= n 1) (set! Blo (ref Alo))]
        [(= n 2) (let ([x (ref Alo)] [y (ref (add1 Alo))])
                   (if (<? y x)
                     (begin (set! Blo y) (set! (add1 Blo) x))
                     (begin (set! Blo x) (set! (add1 Blo) y))))]
        [(n . < . 16) ; not much difference up to ~30
         (copying-insertionsort Alo Blo n)]
        [else (let* ([n/2- (arithmetic-shift n -1)] [n/2+ (- n n/2-)])
                (let ([Amid1 (+ Alo n/2-)]
                      [Amid2 (+ Alo n/2+)]
                      [Bmid1 (+ Blo n/2-)])
                  (copying-mergesort Amid1 Bmid1 n/2+)
                  (copying-mergesort Alo Amid2 n/2-)
                  (merge #t Amid2 (+ Alo n) Bmid1 (+ Blo n) Blo)))]))

    (let ([Alo 0] [Amid1 n/2-] [Amid2 n/2+] [Ahi n] [B1lo n])
      (copying-mergesort Amid1 B1lo n/2+)
      (unless (zero? n/2-) (copying-mergesort Alo Amid2 n/2-))
      (merge #f B1lo (+ B1lo n/2+) Amid2 Ahi Alo))))

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
       (let ([vec (make-vector (+ n (ceiling (/ n 2))))])
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
      [else (let ([vec (make-vector (+ n (ceiling (/ n 2))))])
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
