(module sort '#%kernel

(#%require "small-scheme.rkt" "define.rkt" (for-syntax "stxcase-scheme.rkt"))

;; note, these are the raw interfaces --- user-facing definitions
;; are exported from private/list.rkt and vector.rkt
(#%provide sort
           vector-sort
           vector-sort!)

#|

Based on "Fast mergesort implementation based on half-copying merge algorithm",
Cezary Juszczak, http://kicia.ift.uni.wroc.pl/algorytmy/mergesortpaper.pdf
Written in Racket by Eli Barzilay.  (Note: the reason for the seemingly
redundant pointer arithmetic in that paper is dealing with cases of uneven
number of elements.)

The source uses macros to optimize some common cases (eg, no `getkey'
function) -- they are local macros so they're not left in the compiled
code.

|#

;; This code works with unsafe operations, if there are problems, the commented
;; chunk of code below can be used to run it in safe mode.
(#%require (rename '#%unsafe i+ unsafe-fx+)
           (rename '#%unsafe i- unsafe-fx-)
           (rename '#%unsafe i= unsafe-fx=)
           (rename '#%unsafe i< unsafe-fx<)
           (rename '#%unsafe i<= unsafe-fx<=)
           (rename '#%unsafe i> unsafe-fx>)
           (rename '#%unsafe i>= unsafe-fx>=)
           (rename '#%unsafe i>> unsafe-fxrshift)
           (rename '#%unsafe i<< unsafe-fxlshift)
           (rename '#%unsafe vref unsafe-vector-ref)
           (rename '#%unsafe vset! unsafe-vector-set!)
           (rename '#%unsafe ucar unsafe-car)
           (rename '#%unsafe ucdr unsafe-cdr)
           (rename '#%unsafe unsafe-fl< unsafe-fl<)
           (rename '#%unsafe unsafe-fl<= unsafe-fl<=)
           (rename '#%unsafe unsafe-fl> unsafe-fl>)
           (rename '#%unsafe unsafe-fl>= unsafe-fl>=))

(define-values (sort
                vector-sort
                vector-sort!)
(let ()

  (define-syntax define-syntax-rule
    (syntax-rules ()
      [(dr (foo . pattern) template)
       (define-syntax foo (syntax-rules () [(_ . pattern) template]))]))
  
  ;; Use this to make it safe:
  ;;(define-syntax-rule (i+ x y) (+ x y))
  ;;(define-syntax-rule (i- x y) (- x y))
  ;;(define-syntax-rule (i= x y) (= x y))
  ;;(define-syntax-rule (i< x y) (< x y))
  ;;(define-syntax-rule (i<= x y) (<= x y))
  ;;(define-syntax-rule (i>> x y) (arithmetic-shift x (- y)))
  ;;(define-syntax-rule (i<< x y) (arithmetic-shift x y))
  ;;(define-syntax-rule (vref v i) (vector-ref v i))
  ;;(define-syntax-rule (vset! v i x) (vector-set! v i x))
  ;;(define ucar car)
  ;;(define ucdr cdr)

  (define-syntax-rule (i/2 x) (i>> x 1))
  (define-syntax-rule (i*2 x) (i<< x 1))
  
  (define-syntax-rule (sort-internal-body A less-than? n key)
    (let ()
      ;; comparison & vector access macros
      (define-syntax-rule (<? x y)
        (if key
            (less-than? (key x) (key y))
            (less-than? x y)))
      (define-syntax-rule (ref index) (vref A index))
      (define-syntax-rule (set! index val) (vset! A index val))
      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ;; Stable Sort (Mergesort)
      ;; (used by `sort', `vector-sort', and `vector-sort!')
      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ;; Based on "Fast mergesort implementation based on half-copying merge algorithm",
      ;; Cezary Juszczak, http://kicia.ift.uni.wroc.pl/algorytmy/mergesortpaper.pdf
      ;; Written in Racket by Eli Barzilay.  (Note: the reason for the seemingly
      ;; redundant pointer arithmetic in that paper is dealing with cases of uneven
      ;; number of elements.)
      (let* ([n/2- (i/2 n)]
             [n/2+ (i- n n/2-)])
        ;; - - - - - - - - - - - - - - - - - - -
        ;; Merge
        ;; - - - - - - - - - - - - - - - - - - -
        (define-syntax-rule (merge lo? A1 A2 B1 B2 C1)
          (let ([b2 B2])
            (let loop ([a1 A1] [b1 B1] [c1 C1])
              (let ([x (ref a1)] [y (ref b1)])
                (if (if lo? (not (<? y x)) (<? x y))
                    (begin (set! c1 x)
                           (let ([a1 (i+ a1 1)] [c1 (i+ c1 1)])
                             (when (i< c1 b1) (loop a1 b1 c1))))
                    (begin (set! c1 y)
                           (let ([b1 (i+ b1 1)] [c1 (i+ c1 1)])
                             (if (i<= b2 b1)
                                 (let loop ([a1 a1] [c1 c1])
                                   (when (i< c1 b1)
                                     (set! c1 (ref a1))
                                     (loop (i+ a1 1) (i+ c1 1))))
                                 (loop a1 b1 c1)))))))))
        
        ;; - - - - - - - - - - - - - - - - - - -
        ;; copying-insertionsort
        ;; - - - - - - - - - - - - - - - - - - -
        (define-syntax-rule (copying-insertionsort Alo Blo n)
          ;; n is never 0
          (begin (set! Blo (ref Alo))
                 (let iloop ([i 1])
                   (when (i< i n)
                     (let ([ref-i (ref (i+ Alo i))])
                       (let jloop ([j (i+ Blo i)])
                         (let ([ref-j-1 (ref (i- j 1))])
                           (if (and (i< Blo j) (<? ref-i ref-j-1))
                               (begin (set! j ref-j-1) (jloop (i- j 1)))
                               (begin (set! j ref-i) (iloop (i+ i 1)))))))))))
        
        ;; - - - - - - - - - - - - - - - - - - -
        ;; Mergesort
        ;; - - - - - - - - - - - - - - - - - - -
        (define (copying-mergesort Alo Blo n)
          (cond
            ;; n is never 0, smaller values are more frequent
            [(i= n 1) (set! Blo (ref Alo))]
            [(i= n 2) (let ([x (ref Alo)] [y (ref (i+ Alo 1))])
                        (if (<? y x)
                            (begin (set! Blo y) (set! (i+ Blo 1) x))
                            (begin (set! Blo x) (set! (i+ Blo 1) y))))]
            ;; insertion sort for small chunks (not much difference up to ~30)
            [(i< n 16) (copying-insertionsort Alo Blo n)]
            [else (let* ([n/2- (i/2 n)]
                         [n/2+ (i- n n/2-)])
                    (let ([Amid1 (i+ Alo n/2-)]
                          [Amid2 (i+ Alo n/2+)]
                          [Bmid1 (i+ Blo n/2-)])
                      (copying-mergesort Amid1 Bmid1 n/2+)
                      (copying-mergesort Alo Amid2 n/2-)
                      (merge #t Amid2 (i+ Alo n) Bmid1 (i+ Blo n) Blo)))]))
        ;; start the sorting!
        (let ([Alo 0] [Amid1 n/2-] [Amid2 n/2+] [Ahi n] [B1lo n])
          (copying-mergesort Amid1 B1lo n/2+)
          (unless (zero? n/2-) (copying-mergesort Alo Amid2 n/2-))
          (merge #f B1lo (i+ B1lo n/2+) Amid2 Ahi Alo)))))
  
  (define (generic-sort A less-than? n)
    (sort-internal-body A less-than? n #f))
  
  (define (generic-sort/key A less-than? n key)
    (sort-internal-body A less-than? n key))
  
  (define-syntax (sort-internal stx)
    (syntax-case stx ()
      [(_ vec less-than? n #:key #f)
       #'(generic-sort vec less-than? n)]
      [(_ vec less-than? n #:key key)
       #'(generic-sort/key vec less-than? n key)]))
  
  ;; - - - - - - - - - - - - - - - - - - - - - - - -
  ;; List Sorting Definition Body
  ;; - - - - - - - - - - - - - - - - - - - - - - - -
  
  (define-syntax (list-sort-body stx)
    (syntax-case stx ()
      [(_ lst less-than? #:key key #:cache-keys? cache-keys?)
       #'(let ([n (length lst)])
           (define-syntax-rule (<? x y)
             (if key
                 (less-than? (key x) (key y))
                 (less-than? x y)))
           (cond
             ;; trivial cases (where we know there is no caching to be done)
             [(i= n 0) lst]
             ;; below we can assume a non-empty input list
             ;; if we know statically this is not a cache-keys? use
             ;; case, don't include this case in the cond
             [cache-keys?
              ;; decorate while converting to a vector, and undecorate when going
              ;; back, always do this for consistency
              (let ([vec (make-vector (+ n (ceiling (/ n 2))))])
                ;; list -> decorated-vector
                (let loop ([i 0] [lst lst])
                  (when (pair? lst)
                    (let ([x (car lst)])
                      (vset! vec i (cons (key x) x))
                      (loop (i+ i 1) (cdr lst)))))
                ;; sort
                (sort-internal vec less-than? n #:key ucar)
                ;; decorated-vector -> list
                (let loop ([i n] [r '()])
                  (let ([i (i- i 1)])
                    (if (i< i 0)
                        r
                        (loop i (cons (ucdr (vref vec i)) r))))))]
             ;; check if the list is already sorted (which can be common, eg,
             ;; directory lists)
             [(let loop ([last (car lst)] [next (cdr lst)])
                (or (null? next)
                    (and (not (<? (ucar next) last))
                         (loop (ucar next) (ucdr next)))))
              lst]
             ;; below we can assume an unsorted list
             ;; inlined case, for optimization of short lists
             [(i<= n 3)
              (cond
                [(i= n 1) lst]
                [(i= n 2)
                 ;; (because of the above test, we know lst is unsorted)
                 (list (cadr lst) (car lst))]
                [else
                 (let ([a (car lst)] [b (cadr lst)] [c (caddr lst)])
                   ;; General note: we need a stable sort, so we should always compare
                   ;; (<? later-item earlier-item) since it gives more information.
                   ;; Good code should have each permutation appears exactly once.
                   ;; This means that n=4 will have 23 cases, so don't bother.                 
                   (if (<? b a)
                       ;; b<a
                       (if (<? c b)
                           (list c b a)
                           ;; b<a, b<=c
                           (if (<? c a) (list b c a) (list b a c)))
                       ;; a<=b, so c<b (b<=c is impossible due to above test)
                       (if (<? c a) (list c a b) (list a c b))))])]
             [else (let ([vec (make-vector (+ n (ceiling (/ n 2))))])
                     ;; list -> vector
                     (let loop ([i 0] [lst lst])
                       (when (pair? lst)
                         (vector-set! vec i (car lst))
                         (loop (add1 i) (cdr lst))))
                     ;; sort
                     (sort-internal vec less-than? n #:key key)
                     ;; vector -> list
                     (let loop ([i n] [r '()])
                       (let ([i (sub1 i)])
                         (if (< i 0) r (loop i (cons (vector-ref vec i) r))))))]))]))
  
  
  ;; - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Vector Sorting Definition Body
  ;; - - - - - - - - - - - - - - - - - - - - - - - -
  
  (define-syntax (vector-sort-body stx)
    (syntax-case stx ()
      [(_ src-vec less-than? start end
          #:key key
          #:cache-keys? cache-keys?
          #:constructive? constructive?)
       #'(let ([n (- end start)])
           (define-syntax-rule (<? x y)
             (if key
                 (less-than? (key x) (key y))
                 (less-than? x y)))
           (define-syntax-rule (swap! A i j)
             (let ([tmp (vref A  i)])
               (vset! A i (vref A  j))
               (vset! A j tmp)))
           (define dst-vec (if constructive? (make-vector n) src-vec))
           (define dst-start (if constructive? 0 start))
           (cond
             ;; trivial case (where we know we don't even need to cache a key)
             [(i= n 0) (void)]
             ;; below we can assume a non-empty input vector
             ;; if we statically know we're not caching keys, don't
             ;; include this case in the cond
             [cache-keys?
              ;; decorate while converting to a vector for sorting,
              ;; and undecorate when going back, always do this for
              ;; consistency
              (define work-vec (make-vector (+ n (ceiling (/ n 2))) #t))
              ;; vector -> decorated-vector
              (let loop ([i 0])
                (when (i< i n)
                  (let ([x (vref src-vec (i+ i start))])
                    (vset! work-vec i (cons (key x) x)))
                  (loop (i+ i 1))))
              ;; sort
              (sort-internal work-vec less-than? n #:key ucar)
              ;; decorated-vector -> vector
              (let loop ([i 0])
                (when (i< i n)
                  (vset! dst-vec (i+ i dst-start) (ucdr (vref work-vec i)))
                  (loop (i+ i 1))))]
             ;; check if the vector is already sorted
             [(let loop ([prev-val (vref src-vec start)]
                         [next-index (i+ start 1)])
                (or (i= next-index end)
                    (let ([next-val (vref src-vec next-index)])
                      (and (not (<? next-val prev-val))
                           (loop next-val (i+ next-index 1))))))
              (when constructive?
                (vector-copy! dst-vec dst-start src-vec start end))]
             ;; other easy/small cases
             ;; below we can assume an unsorted list
             ;; inlined case, for optimization of short lists
             [(i<= n 3)
              (when constructive?
                (vector-copy! dst-vec dst-start src-vec start end))
              (cond
                [(i= n 1) (void)]
                [(i= n 2)
                 ;; (because of the above test, we know lst is unsorted)
                 (swap! dst-vec
                        (i+ dst-start 0)
                        (i+ dst-start 1))]
                [else
                 (let ([a (vref dst-vec (i+ dst-start 0))]
                       [b (vref dst-vec (i+ dst-start 1))]
                       [c (vref dst-vec (i+ dst-start 2))])
                   (cond
                     [(<? b a)
                      (cond
                        [(<? c b)
                         (vset! dst-vec (i+ dst-start 0) c)
                         (vset! dst-vec (i+ dst-start 2) a)]
                        [(<? c a)
                         (vset! dst-vec (i+ dst-start 0) b)
                         (vset! dst-vec (i+ dst-start 1) c)
                         (vset! dst-vec (i+ dst-start 2) a)]
                        [else (vset! dst-vec (i+ dst-start 0) b)
                              (vset! dst-vec (i+ dst-start 1) a)])]
                     [(<? c a) (vset! dst-vec (i+ dst-start 0) c)
                               (vset! dst-vec (i+ dst-start 1) a)
                               (vset! dst-vec (i+ dst-start 2) b)]
                     [else  (vset! dst-vec (i+ dst-start 1) c)
                            (vset! dst-vec (i+ dst-start 2) b)]))])]
             [else
              (let ([work-vec (make-vector (+ n (ceiling (/ n 2))) #f)])
                ;; src vector -> work-vector (to do merge sort in)
                (vector-copy! work-vec 0 src-vec start end)
                ;; sort!
                (sort-internal work-vec less-than? n #:key key)
                ;; work-vector -> dst vector
                (vector-copy! dst-vec dst-start work-vec 0 n))])
           (if constructive?
               dst-vec
               (void)))]))
  
  
  ;; macro for defining the various vector-sort case-lambdas
  (define-syntax-rule (vector-sort-case-lambda sort-name #:constructive? constructive?)
    (case-lambda
      [(vec less-than? start end)
       (vector-sort-body vec less-than? start end
                         #:key #f
                         #:cache-keys? #f
                         #:constructive? constructive?)]
      [(vec less-than? start end getkey cache-keys?)
       (if (and getkey (not (eq? values getkey)))
           (vector-sort-body vec less-than? start end
                             #:key getkey
                             #:cache-keys? cache-keys?
                             #:constructive? constructive?)
           (sort-name vec less-than? start end))]))
  
  ;; - - - - - - - - - - - - - - - - - - - - - - - -
  ;; Actual Sorting Function Definitions
  ;; - - - - - - - - - - - - - - - - - - - - - - - -
  
  ;; Finally, these are the provided `sort' values
  (values
   ;; sort
   (case-lambda
     [(lst less-than?)
      (list-sort-body lst less-than? #:key #f #:cache-keys? #f)]
     [(lst less-than? getkey)
      (if (and getkey (not (eq? values getkey)))
          (sort lst less-than? getkey #f)
          (sort lst less-than?))]
     [(lst less-than? getkey cache-keys?)
      (if (and getkey (not (eq? values getkey)))
          (list-sort-body lst less-than? #:key getkey #:cache-keys? cache-keys?)
          (sort lst less-than?))])
   ;; vector-sort
   (vector-sort-case-lambda vector-sort #:constructive? #t)
   ;; vector-sort!
   (vector-sort-case-lambda vector-sort! #:constructive? #f))

))) ;; end of module
