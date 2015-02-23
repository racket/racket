;; Examples from some papers, and from Oleg Kiselyov's:
;;  Generic implementation of all four delimited control operators
;;     shift/reset, prompt/control, shift0/reset0 and prompt0/control0
;;                    aka. -F- through +F+   
;;  $Id: delim-control-n.scm 815 2005-09-05 23:02:12Z oleg $

(load-relative "loadtest.rktl")

(Section 'control)

(require racket/control)

;;-----------------------------------------------------------------------

(define-syntax ctest
  (syntax-rules ()
    [(_ expr expect)
     (test expect 'expr expr)]))

;;-----------------------------------------------------------------------
;;                       Shift tests

(ctest (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3)))))))
       117)

(ctest (* 10 (reset (* 2 (shift g (reset 
                                   (* 5 (shift f (+ (f 1) 1))))))))
       60)

(ctest (let ((f (lambda (x) (shift k (k (k x))))))
         (+ 1 (reset (+ 10 (f 100)))))
       121)

(ctest (reset
        (let ((x (shift f (cons 'a (f '())))))
          (shift g x)))
       '(a))

(define (shift* p) (shift f (p f)))
(ctest (reset (let ((x 'abcde)) (eq? x ((shift* shift*) x))))
       #t)

(define traverse
  (lambda (xs)
    (letrec ((visit
              (lambda (xs)
                (if (null? xs)
                    '()
                    (visit (shift k
                                  (cons (car xs)
                                        (k (cdr xs)))))))))
      (reset
       (visit xs)))))

(ctest (traverse '(1 2 3 4 5))
       '(1 2 3 4 5))

;;-----------------------------------------------------------------------
;;                       Control tests
;; Example from Sitaram, Felleisen

(define (abort v) (control k v))

(ctest (let ((g (prompt (* 2 (control k k)))))
         (* 3 (prompt (* 5 (abort (g 7))))))
       42)

;; Olivier Danvy's puzzle

(define traverse
  (lambda (xs)
    (letrec ((visit
              (lambda (xs)
                (if (null? xs)
                    '()
                    (visit (control k
                                    (cons (car xs) 
                                          (k (cdr xs)))))))))
      (prompt
       (visit xs)))))

(ctest (traverse '(1 2 3 4 5))
       '(5 4 3 2 1))

(ctest (+ 10 (prompt (+ 2 (control k (+ 100 (k (k 3)))))))
       117)

(ctest (prompt (let ((x (control f (cons 'a (f '()))))) (control g x)))
       '())

(ctest (prompt ((lambda (x) (control l 2))
                (control l (+ 1 (l 0)))))
       2)
(ctest (prompt (control f (cons 'a (f '()))))
       '(a))
(ctest (prompt (let ((x (control f (cons 'a (f '()))))) 
                 (control g (g x))))
       '(a))

(define (control* f) (control k (f k)))
(ctest (prompt (let ((x 'abcde)) (eq? x ((control* control*) x))))
       #t)

;;------------------------------------------------------------------------
;;                       shift0/control0 tests

(ctest (+ 10 (prompt0 (+ 2 (control k (+ 100 (k (k 3)))))))
       117)

(ctest (prompt0 (prompt0 
                 (let ((x (control f (cons 'a (f '()))))) 
                   (control g x))))
       '())

(ctest (+ 10 (prompt0 (+ 2 (shift0 k (+ 100 (k (k 3)))))))
       117)

(ctest (prompt0 (cons 'a (prompt0 (shift0 f (shift0 g '())))))
       '())

(ctest (prompt (cons 'a (prompt (shift0 f (shift0 g '())))))
       '(a))


;; ----------------------------------------
;; Examples from Dorai Sitaram's dissertation

(define make-fringe
  (lambda (tree)
    (lambda (any)
      (let loop ([tree tree])
        (cond
         [(pair? tree) 
          (loop (car tree))
          (loop (cdr tree))]
         [(null? tree) '*]
         [else (fcontrol tree)]))
      (fcontrol '()))))

(define same-fringe?
  (lambda (tree1 tree2)
    (let loop ([fringe1 (make-fringe tree1)]
               [fringe2 (make-fringe tree2)])
      (% (fringe1 '*)
         (lambda (leaf1 rest-of-fringe1)
           (% (fringe2 '*)
              (lambda (leaf2 rest-of-fringe2)
                (cond
                 [(and (null? leaf1) (null? leaf2)) #t]
                 [(or (null? leaf1) (null? leaf2)) #f]
                 [(eqv? leaf1 leaf2) (loop rest-of-fringe1
                                           rest-of-fringe2)]
                 [else #f]))))))))

(ctest (same-fringe? '(1 . (2 . (3 . 4)))
                     '(1 . ((2 . 3) . 4)))
       #t)
(ctest (same-fringe? '(1 . (2 . (3 . 4)))
                     '(1 . ((2 . 5) . 4)))
       #f)

(define all-prefixes
  (lambda (l)
    (letrec ([loop (lambda (l)
                     (if (null? l)
                         (fcontrol 'done)
                         (cons (car l)
                               (fcontrol (cdr l)))))])
      (% (loop l)
         (letrec ([h (lambda (r k)
                       (if (eq? r 'done)
                           '()
                           (cons (k '())
                                 (% (k (loop r)) h))))])
           h)))))

(ctest (all-prefixes '(1 2 3 4))
      '((1) (1 2) (1 2 3) (1 2 3 4)))

;; ----------------------------------------
;; fcontrol/% with prompt tags

(ctest (let ([pt (make-continuation-prompt-tag)])
         (* 2 (% (% (fcontrol 5 #:tag pt)
                    (lambda (v k) (k v)))
                 (lambda (v k) (k (add1 v)))
                 #:tag pt)))
       12)

;; ------------------------------------------------------------
;; spawn
;;  example from Queinnec & Serpete, POPL'91

(ctest (spawn (lambda (f) 
               (let ([v (f (lambda (c2) 
                             (cons 2 (c2 3))))])
                 (cons (f (lambda (cl) 
                            (cons 1 (cl 4)))) 
                       v))))
      '(2 1 4 . 3))

(ctest (spawn (lambda (f) 
               (cons (f (lambda (cl) 
                          (cons 1 (cl 4)))) 
                     (f (lambda (c2) 
                          (cons 2 (c2 3)))))))
      '(1 2 4 . 3))

;; ------------------------------------------------------------
;; splitter
;;  example from Queinnec & Serpete, POPL'91

(define (visit tree fn) 
  (if (pair? tree) 
      (begin (visit (car tree) fn) 
             (visit (cdr tree) fn)) 
      (fn tree)))

(define (make-tree-walker visit) 
  (lambda (tree) 
    (splitter 
     (lambda (exit grab) 
       (visit 
        tree 
        (lambda (leaf) 
          (grab 
           (lambda (c) 
             (exit (lambda () 
                     (cons 
                      leaf 
                      (lambda (v) 
                        (splitter 
                         (lambda (k j ) 
                           (set! grab j) 
                           (set! exit k) 
                           (c v)))))))))))))))

(define (compare-fringes walk trees) 
  (let ((end (list 'end)))
    (define (end? leaf) (eq? leaf end)) 
    (define (loop leafs) 
      (define (same-leaf? leaf) 
        (eq? (car leaf) (caar leafs)))
      (or ; all trees are finished ? 
       (andmap end? leafs) 
       ;; some trees are jinished ? 
       (if (ormap end? leafs)
           #f
           (and (andmap same-leaf? 
                        (cdr leafs)) 
                ;; all leaves are equal ! 
                (loop (map
                       (lambda (leaf) 
                         ((cdr leaf) end) ) 
                       leafs )) ) ) ) ) 
    (loop (map walk trees)) ) )

(define (same-fringe trees) 
  (compare-fringes (make-tree-walker visit) 
                   trees))

(ctest (same-fringe (list '(1 . (2 . (3 . 4)))
                         '(1 . ((2 . 3) . 4))))
      #t)
(ctest (same-fringe (list '(1 . (2 . (3 . 4)))
                         '(1 . ((2 . 5) . 4))))
      #f)

;; ----------------------------------------
;; cupto

(ctest (let ([p (new-prompt)])
         (set p
              (+ 1
                 (cupto p k (+ 3 (k 2) (k 5))))))
       12)

;; ----------------------------------------

(report-errs)
