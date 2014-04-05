#lang racket

(require plot rackunit)

(check-exn #rx"'\\(1\\)" (λ () (lines '((1)))))
(check-exn #rx"\\(list \\(ivl 1 2\\)\\)" (λ () (rectangles (list (list (ivl 1 2))))))
(check-exn #rx"'\\(1\\)" (λ () (points '((1)))))
(check-exn #rx"'\\(1\\)" (λ () (error-bars '((1)))))
(check-exn #rx"'\\(1 2\\)" (λ () (error-bars '((1 2)))))
(check-exn #rx"'\\(1\\)" (λ () (lines-interval '((1)) '((1 2)))))
(check-exn #rx"'\\(1\\)" (λ () (lines-interval '((1 2)) '((1)))))

(check-exn #rx"'\\(1\\)" (λ () (lines3d '((1)))))
(check-exn #rx"'\\(1 2\\)" (λ () (lines3d '((1 2)))))
(check-exn #rx"\\(list \\(ivl 1 2\\)\\)" (λ () (rectangles3d (list (list (ivl 1 2))))))
(check-exn #rx"\\(list \\(ivl 1 2\\) \\(ivl 2 3\\)\\)" (λ () (rectangles3d
                                                              (list (list (ivl 1 2) (ivl 2 3))))))
(check-exn #rx"'\\(1\\)" (λ () (points3d '((1)))))
(check-exn #rx"'\\(1 2\\)" (λ () (points3d '((1 2)))))
