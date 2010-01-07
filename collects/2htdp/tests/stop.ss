;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname stop) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require htdp/image)

;; on RETURN stop

(define (main debug?)
  (big-bang ""
            (on-key (lambda (w ke)
                      (cond
                        [(key=? ke "\r") (stop-with w)]
                        [(= (string-length ke) 1)
                         (string-append w ke)]
                        [else w])))
            (state debug?)
            (on-draw (lambda (w)
                       (place-image
                        (text w 22 'black)
                        3 3
                        (empty-scene 100 100))))))
