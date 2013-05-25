;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname balle-grav-frot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write mixed-fraction #t #t none #f ())))
;;; Language : advanced student

(require "valrose.ss")

(define (balle-avec-gravitation-et-frottement x0 y0 dx0 dy0)
  (local [(define BALLE (bitmap "ballon.png"))
          (define R (/ (image-width BALLE) 2))
          (define SIZE 400)
          (define FOND (place-image (text "Mouse or Space !" 18 "Blue") 200 80 (rectangle SIZE SIZE 'solid "yellow")))
          (define-struct monde (x y dx dy))
          (define INIT (make-monde x0 y0 dx0 dy0))
          (define G #i1)
          (define F #i0.95)
          (define (suivant m)
            (local [(define x (monde-x m))
                    (define y (monde-y m))
                    (define dx (monde-dx m))
                    (define dy (monde-dy m))
                    (define xs (+ x dx))
                    (define ys (+ y dy))]
              (cond ((> ys (- SIZE R)) (make-monde xs (- SIZE R) (* F dx) (+ (* F (- dy)) G)))
                    ((< xs R) (make-monde R ys (* F (- dx)) (* F (+ dy G))))
                    ((> (+ xs R) SIZE) (make-monde (- SIZE R) ys (* F (- dx)) (* F (+ dy G))))
                    ((< ys R) (make-monde xs R dx (+ (* F (- dy)) G)))
                    (else (make-monde xs ys dx (+ dy G))))))
          (define (souris m x y evt)
            (if (mouse=? evt "button-down")
                (make-monde x y (monde-dx m) (monde-dy m))
                m))
          (define (clavier m key)
            (if (key=? key " ")
                (make-monde (+ R (random (- SIZE (* 2 R)))) (+ R (random (- SIZE (* 2 R)))) (monde-dx m) (monde-dy m))
                m))
          (define (dessiner m)
            (place-image BALLE (monde-x m) (monde-y m) FOND))
          (define (final? m)
            (and (< (abs (- SIZE (monde-y m) R)) 1) (< (abs (monde-dx m)) 1) (< (abs (monde-dy m)) 1)))]
    (big-bang INIT
              (on-tick suivant)
              (on-draw dessiner SIZE SIZE)
              (on-mouse souris)
              (on-key clavier)
              (stop-when final?))))

(balle-avec-gravitation-et-frottement 200 200 5 15)
