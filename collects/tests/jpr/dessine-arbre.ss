;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname dessine-arbre) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write mixed-fraction #t #t none #f ())))
;;; dessine-arbre.ss

(require "valrose.ss")

(define (objet->image x)    ; x est un operateur ou une feuille
  (text (if (number? x) (number->string x) (symbol->string x))
        18 "black"))

(define (vert h) (rectangle 1 h 'solid "white"))
(define (horiz w) (rectangle w 1 'solid "white"))

(define (arbre->image A)    ; Arbre --> Image au niveau n
  (if (feuille? A)
      (objet->image A)
      (local [(define ig (arbre->image (fg A)))
              (define wg/2 (/ (image-width ig) 2))
              (define id (arbre->image (fd A)))
              (define wd/2 (/ (image-width id) 2))
              (define igd (beside/align 'top ig (horiz 20) id))
              (define wgd/2 (/ (image-width igd) 2))]
        (above (objet->image (racine A))
               (beside (horiz wg/2) 
                       (line (- wg/2 wgd/2) 20 "black") 
                       (line (- wgd/2 wd/2) 20 "black")
                       (horiz wd/2))
               (vert 5)
               igd))))

(arbre->image '(+ (* (+ (* x (- x y)) 2) (* (- a b) longueur)) (/ (* x 2) y)))


