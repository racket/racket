#lang racket/base
(require "test-suite-utils.rkt")

(module test racket/base)

(test
 'dragable-min-size1
 (λ (min-w/min-h) (equal? min-w/min-h '(10 20)))
 `(call-with-values (λ () (panel:dragable-container-size '((10 20 #f #f)) 0 #t))
                    list))

(test
 'dragable-min-size2
 (λ (min-w/min-h) (equal? min-w/min-h '(10 20)))
 `(call-with-values (λ () (panel:dragable-container-size '((10 20 #f #f)) 0 #f))
                    list))

(test
 'dragable-min-size3
 (λ (min-w/min-h) (equal? min-w/min-h '(30 60)))
 `(call-with-values (λ () (panel:dragable-container-size '((10 20 #f #f) (30 40 #f #f)) 0 #t))
                    list))

(test
 'dragable-min-size4
 (λ (min-w/min-h) (equal? min-w/min-h '(40 40)))
 `(call-with-values (λ () (panel:dragable-container-size '((10 20 #f #f) (30 40 #f #f)) 0 #f))
                    list))

(test
 'dragable-min-size5
 (λ (min-w/min-h) (equal? min-w/min-h '(30 65)))
 `(call-with-values (λ () (panel:dragable-container-size '((10 20 #f #f) (30 40 #f #f)) 5 #t))
                    list))

(test
 'dragable-min-size6
 (λ (min-w/min-h) (equal? min-w/min-h '(45 40)))
 `(call-with-values (λ () (panel:dragable-container-size '((10 20 #f #f) (30 40 #f #f)) 5 #f))
                    list))

(test
 'dragable-place-children1
 (λ (l) (equal? l '(() ())))
 `(call-with-values (λ () (panel:dragable-place-children '() 100 200 '() 0 #t))
                    list))

(test
 'dragable-place-children2
 (λ (l) (equal? l '(((0 0 100 200)) ())))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f)) 100 200 '(1) 0 #t))
                    list))

(test
 'dragable-place-children3
 (λ (l) (equal? l '(((0 0 100 200)) ())))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f)) 100 200 '(1) 0 #f))
                    list))

(test
 'dragable-place-children4
 (λ (l) (equal? l '(((0 0 100 150) (0 150 100 150)) ((150 150)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (10 10 #t #f)) 100 300 '(1/2 1/2) 0 #t))
                    list))

(test
 'dragable-place-children5
 (λ (l) (equal? l '(((0 0 50 300) (50 0 50 300)) ((50 50)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (10 10 #t #f)) 100 300 '(1/2 1/2) 0 #f))
                    list))

(test
 'dragable-place-children5
 (λ (l) (equal? l '(((0 0 100 100) (0 100 100 200)) ((100 100)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (10 10 #t #f)) 100 300 '(1/3 2/3) 0 #t))
                    list))

(test
 'dragable-place-children6
 (λ (l) (equal? l '(((0 0 10 300) (10 0 90 300)) ((10 10)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (10 10 #t #f)) 100 300 '(1/10 9/10) 0 #f))
                    list))

(test
 'dragable-place-children7
 (λ (l) (equal? l '(((0 0 10 300) (20 0 90 300)) ((10 20)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (10 10 #t #f)) 110 300 '(1/10 9/10) 10 #f))
                    list))

(test
 'dragable-place-children8
 (λ (l) (equal? l '(((0 0 10 300) (20 0 20 300) (50 0 70 300)) ((10 20) (40 50)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (10 10 #t #f) (10 10 #t #f)) 120 300 '(1/10 2/10 7/10) 10 #f))
                    list))

(test
 'dragable-place-children9
 (λ (l) (equal? l '(((0 0 30 300) (30 0 70 300)) ((30 30)))))
 `(call-with-values (λ () (panel:dragable-place-children '((10 10 #t #f) (70 10 #t #f)) 100 300 '(1/2 1/2) 0 #f))
                    list))

(test
 'dragable-place-children10
 (λ (l) (equal? l '(((0 0 70 300) (70 0 30 300)) ((70 70)))))
 `(call-with-values (λ () (panel:dragable-place-children '((70 10 #t #f) (10 10 #t #f)) 100 300 '(1/2 1/2) 0 #f))
                    list))

(test
 'dragable-place-children11
 (λ (l) (equal? l '(((0 0 70 300) (70 0 10 300) (80 0 20 300)) ((70 70) (80 80)))))
 `(call-with-values (λ () (panel:dragable-place-children '((70 10 #t #f) (10 10 #t #f) (20 10 #t #f)) 100 300 '(1/2 1/4 1/4) 0 #f))
                    list))

(test
 'dragable-place-children12
 (λ (l) (equal? l '(((0 0 242 629) (247 0 243 629)) ((242 247)))))
 `(call-with-values (λ () (panel:dragable-place-children '((30 30 #t #t) (30 30 #t #t)) 490 629 '(1/2 1/2) 5 #f))
                    list))
