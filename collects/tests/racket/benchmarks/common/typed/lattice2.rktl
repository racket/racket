;; Like "lattice.sch", but uses `reverse' instead of
;; defining `reverse!' (to avoid `set-cdr!')

;;; LATTICE -- Obtained from Andrew Wright.

(define-type Verdict (U 'less 'more 'equal 'uncomparable))

;; Given a comparison routine that returns one of
;;       less
;;       more
;;       equal
;;       uncomparable
;; return a new comparison routine that applies to sequences.
(: lexico (All (X) ((X X -> Verdict) -> ((Listof X) (Listof X) -> Verdict))))
(define lexico
  (lambda (base)
    (: lex-fixed (Verdict (Listof X) (Listof X) -> Verdict))
    (define lex-fixed
      (lambda (fixed lhs rhs)
        (: check ((Listof X) (Listof X) -> Verdict))
        (define check
          (lambda (lhs rhs)
            (if (null? lhs)
                fixed
                (let ((probe
                       (base (car lhs)
                             (car rhs))))
                  (if (or (eq? probe 'equal)
                          (eq? probe fixed))
                      (check (cdr lhs)
                             (cdr rhs))
                      'uncomparable)))))
        (check lhs rhs)))
    (: lex-first ((Listof X) (Listof X) -> Verdict))
    (define lex-first
      (lambda (lhs rhs)
        (if (null? lhs)
            'equal
            (let: ((probe : Verdict
                          (base (car lhs)
                                (car rhs))))
                  (case probe
                    ((less more)
                     (lex-fixed probe
                                (cdr lhs)
                                (cdr rhs)))
                    ((equal)
                     (lex-first (cdr lhs)
                                (cdr rhs)))
                    (else
                     'uncomparable))))))
    lex-first))

(define-type (Lattice X) (Pair (Listof X) (X X -> Verdict)))

(: make-lattice (All (X) ((Listof X) (X X -> Verdict) -> (Lattice X))))
(define (make-lattice elem-list cmp-func)
  (cons elem-list cmp-func))

(: lattice->elements (All (X) ((Lattice X) -> (Listof X))))
(define (lattice->elements l) (car l))

(: lattice->cmp (All (X) ((Lattice X) -> (X X -> Verdict))))
(define (lattice->cmp l) (cdr l))

;; Select elements of a list which pass some test.
(: zulu-select (All (X) ((X -> Any) (Listof X) -> (Listof X))))
(define zulu-select
  (lambda (test lst)
    (: select-a ((Listof X) (Listof X) -> (Listof X)))
    (define select-a
      (lambda (ac lst)
        (if (null? lst)
            (reverse ac)
            (select-a
             (let ((head (car lst)))
               (if (test head)
                   (cons head ac)
                   ac))
             (cdr lst)))))
    (select-a '() lst)))

;; Select elements of a list which pass some test and map a function
;; over the result.  Note, only efficiency prevents this from being the
;; composition of select and map.
(: select-map (All (X Y) ((X -> Any) (X -> Y) (Listof X) -> (Listof Y))))
(define select-map
  (lambda (test func lst)
    (: select-a ((Listof Y) (Listof X) -> (Listof Y)))
    (define select-a
      (lambda (ac lst)
        (if (null? lst)
            (reverse ac)
            (select-a
             (let ((head (car lst)))
               (if (test head)
                   (cons (func head)
                         ac)
                   ac))
             (cdr lst)))))
    (select-a '() lst)))



;; This version of map-and tail-recurses on the last test.
(: map-and (All (X) ((X -> Any) (Listof X) -> Any)))
(define map-and
  (lambda (proc lst)
    (if (null? lst)
        #t
        (letrec: ((drudge : ((Listof X) -> Any)
                          (lambda (lst)
                            (let ((rest (cdr lst)))
                              (if (null? rest)
                                  (proc (car lst))
                                  (and (proc (car lst))
                                       (drudge rest)))))))
                 (drudge lst)))))

(: maps-1 (All (X Y) ((Lattice X) (Lattice Y) (Listof (Pair X Y)) X
                      -> (Listof Y))))
(define (maps-1 source target pas new)
  (let ((scmp (lattice->cmp source))
        (tcmp (lattice->cmp target)))
    (let ((less
           ((inst select-map (Pair X Y) Y)
            (lambda: ((p : (Pair X Y)))
                     (eq? 'less
                          (scmp (car p) new)))
            cdr
            pas))
          (more
           ((inst select-map (Pair X Y) Y)
            (lambda: ((p : (Pair X Y)))
                     (eq? 'more
                          (scmp (car p) new)))
            cdr
            pas)))
      (zulu-select
       (lambda: ((t : Y))
                (and
                 ((inst map-and Y)
                  (lambda: ((t2 : Y))
                           ((inst memq Verdict) (tcmp t2 t) '(less equal)))
                  less)
                 ((inst map-and Y)
                  (lambda: ((t2 : Y))
                           ((inst memq Verdict) (tcmp t2 t) '(more equal)))
                  more)))
       (lattice->elements target)))))

(: maps-rest (All (X Y Z) ((Lattice X) (Lattice Y) (Listof (Pair X Y))
                           (Listof X) ((Listof (Pair X Y)) -> Z)
                           ((Listof Z) -> Z)
                           -> Z)))
(define (maps-rest source target pas rest to-1 to-collect)
  (if (null? rest)
      (to-1 pas)
      (let ((next (car rest))
            (rest (cdr rest)))
        (to-collect
         (map
          (lambda: ((x : Y))
                   (maps-rest source target
                              (cons
                               (cons next x)
                               pas)
                              rest
                              to-1
                              to-collect))
          (maps-1 source target pas next))))))

(: maps (All (X Y) ((Lattice X) (Lattice Y) -> (Lattice (Listof Y)))))
(define (maps source target)
  (make-lattice
   (maps-rest source
              target
              '()
              (lattice->elements source)
              (lambda: ((x : (Listof (Pair X Y))))
                       (list ((inst map Y (Pair X Y)) cdr x)))
              (lambda: ((x : (Listof (Listof (Listof Y)))))
                       (apply append x)))

   (lexico (lattice->cmp target))))

(: count-maps (All (X Y) ((Lattice X) (Lattice Y) -> Integer)))
(define (count-maps source target)
  ((inst maps-rest X Y Integer) source
   target
   '()
   (lattice->elements source)
   (lambda (x) 1)
   sum))

(: sum ((Listof Integer) -> Integer))
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

(: run ( -> Integer))
(define (run)
  (let* ((l2
          (make-lattice '(low high)
                        (lambda (lhs rhs)
                          (case lhs
                            ((low)
                             (case rhs
                               ((low)
                                'equal)
                               ((high)
                                'less)
                               (else
                                (error 'make-lattice "base" rhs))))
                            ((high)
                             (case rhs
                               ((low)
                                'more)
                               ((high)
                                'equal)
                               (else
                                (error 'make-lattice "base" rhs))))
                            (else
                             (error 'make-lattice "base" lhs))))))
         (l3 (maps l2 l2))
         (l4 (maps l3 l3)))
    (count-maps l2 l2)
    (count-maps l3 l3)
    (count-maps l2 l3)
    (count-maps l3 l2)
    (count-maps l4 l4)))

(time (let: loop : Integer ((n : Integer 3) (v : Integer 0))
        (if (zero? n)
            v
            (loop (- n 1)
                  (run)))))
