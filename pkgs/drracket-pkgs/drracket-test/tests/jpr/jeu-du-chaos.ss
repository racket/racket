#lang scheme
(require graphics/graphics)

(open-graphics)
(define VIEW (open-viewport "Essai Graphics" 300 100))
(define tr-segment (draw-line VIEW))  ; un traceur de segment
(define tr-pixel (draw-pixel VIEW))   ; un traceur de pixel

(define A (make-posn 150 10))
(define B (make-posn 10 90))
(define C (make-posn 290 90))

(tr-segment A B "red")
(tr-segment B C "red")
(tr-segment C A "red")

(define M-INIT (make-posn (random 300) (random 100)))

(define (jeu-du-chaos)
  (define (moyenne x y)
    (/ (+ x y) 2))
  (define (milieu A B)
    (make-posn (moyenne (posn-x A) (posn-x B))
               (moyenne (posn-y A) (posn-y B))))
  (define (iter nb-fois M)   ; M est le dernier point courant affiche
    (if (= nb-fois 0)
      (void)
      (let* ((S (case (random 3) ((0) A) ((1) B) ((2) C)))
             (Msuiv (milieu M S)))
        (tr-pixel Msuiv "blue")
        (iter (- nb-fois 1) Msuiv))))
  (tr-pixel M-INIT "blue")
  (iter 4000 M-INIT))

(jeu-du-chaos)

;; For automatic testing, check that we can at least start,
;; but exit soon after:
(module+ test
  (require racket/gui/base)
  (void
   (thread
    (lambda ()
      (sleep 1)
      (queue-callback (lambda () (exit)) #f)))))

