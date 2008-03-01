;;; greedy-set-cover.scm  --  Jens Axel Søgaard 

(require (planet "set.scm" ("soegaard" "galore.plt" 2 1)))

; An instance (X,F) of the "set-covering problem" consists
; of a finite set X and a family F of subsets of X covering X,
; that is, every element of X belongs to at least one subset in F.

; Example:

(define X (insert* '(a b c
                     d e f
                     g h i
                     j k l)
                   (empty)))

(define S1 (insert* '(a b c d e f)  (empty)))
(define S2 (insert* '(e f h i)      (empty)))
(define S3 (insert* '(a d g j)      (empty)))
(define S4 (insert* '(b e h k j)    (empty)))
(define S5 (insert* '(c f i l)      (empty)))
(define S6 (insert* '(j k)          (empty)))

(define F  (list S1 S2 S3 S4 S5 S6))

; A minimum set cover is C={S3,S4,S5}.


; Let's try a greedy approach:

; greedy-set-cover : (set alpha) (list (set alpha))  ->  (list (set alpha))
;   given a set X and a list F of subsets of X covering X,
;   returns an approximation of the minimal set covering.
;   The covering is returned as a list of subsets from F.
(define (greedy-set-cover X F)
  (let loop ([U X]     ; elements not covered by the subsets in C
             [C '()])  ; list of subset in the covering chosen so far
    (if (empty? U)   
        C
        (let ([S (select-S-in-F-such-that-S-intersect-U-maximal F U)])
          (loop (difference U S)
                (cons S C))))))

(define (select-S-in-F-such-that-S-intersect-U-maximal F U)
  (foldl (lambda (S S-best)
           (if (> (size (intersection S U))
                  (size (intersection S-best U)))
               S
               S-best))
         (car F)
         (cdr F)))

; Let's try it on the exmaple:

(map elements
     (greedy-set-cover X F))

; => ((a d g j) (c f i l) (b e h j k) (a b c d e f))

; The greedy approach produces C={S1,S4,S5,S3} and
; thus doesn't find the minimal covering.

