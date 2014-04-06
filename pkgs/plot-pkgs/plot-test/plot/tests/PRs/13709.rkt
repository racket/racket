#lang racket

(require plot rackunit)

(check-exn exn:fail:contract? (λ () (lines '((1)))))
(check-exn exn:fail:contract? (λ () (rectangles (list (list (ivl 1 2))))))
(check-exn exn:fail:contract? (λ () (points '((1)))))
(check-exn exn:fail:contract? (λ () (error-bars '((1)))))
(check-exn exn:fail:contract? (λ () (error-bars '((1 2)))))
(check-exn exn:fail:contract? (λ () (lines-interval '((1)) '((1 2)))))
(check-exn exn:fail:contract? (λ () (lines-interval '((1 2)) '((1)))))

(check-exn exn:fail:contract? (λ () (lines3d '((1)))))
(check-exn exn:fail:contract? (λ () (lines3d '((1 2)))))
(check-exn exn:fail:contract? (λ () (rectangles3d (list (list (ivl 1 2))))))
(check-exn exn:fail:contract? (λ () (rectangles3d (list (list (ivl 1 2) (ivl 2 3))))))
(check-exn exn:fail:contract? (λ () (points3d '((1)))))
(check-exn exn:fail:contract? (λ () (points3d '((1 2)))))
