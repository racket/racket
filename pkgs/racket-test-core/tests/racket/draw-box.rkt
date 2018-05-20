#lang racket/base

;; This test is intended for checking Unicode output to a Windows
;; console --- because that requires yet another decoding/encoding
;; layer within rktio to use WriteConsoleW --- so its output is
;; intended to be inspected directly.

(define H 100)
(define W 40)

(define MAX-FLUSH-SPAN 10)

(define (go content-char)

  (define tl #\u2554) (define h #\u2550) (define tr #\u2557)
  (define v #\u2551)
  (define bl #\u255A)                    (define br #\u255D)

  (define (draw)
    (display (string tl))
    (display (make-string (- W 2) h))
    (display (string tr))
    (newline)

    (for ([i (- H 2)])
      (display v)
      (display (make-string (- W 2) content-char))
      (display v)
      (newline))

    (display (string bl))
    (display (make-string (- W 2) h))
    (display (string br))
    (newline))

  ;; ----------------------------------------
  ;; Direct to console:
  (draw)

  ;; ----------------------------------------
  ;; Write UTF-8 byte string piecewise (to check that
  ;; incomplete encodings are buffered)

  (define o (open-output-bytes))
  (parameterize ([current-output-port o])
    (draw))

  (define bstr (get-output-bytes o))
  (let loop ([i 0])
    (unless (= i (bytes-length bstr))
      (define span (min (add1 (random MAX-FLUSH-SPAN))
                        (- (bytes-length bstr) i)))
      (write-bytes bstr (current-output-port) i (+ i span))
      (flush-output)
      (loop (+ i span)))))

(go #\uA9)    ; copyright (2 bytes in UTF-8)
(go #\u3BB)   ; lambda (3 bytes in UTF-8)
(go #\U24b6)  ; white-circle "A"
(go #\U1F150) ; black-circle "A" (2 bytes in UTF-8; surrogate pair in UTF-16)
