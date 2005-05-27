(require
 (lib "etc.ss")
 (lib "list.ss")
 (lib "match.ss")
 "../alignment.ss"
 "test-macro.ss")

;;los-equal? ((listof rect?) (listof rect?) . -> . boolean?)
;;tests the equality of the list of structures
(define (los-equal? a b)
  (equal?
   (map rect->list a)
   (map rect->list b)))

;;rect->list (rect? . -> . vector?)
;;a vector of the fields in the rect
(define rect->list
  (match-lambda
    [($ rect ($ dim x width stretchable-width?) ($ dim y height stretchable-height?))
     (list x width stretchable-width? y height stretchable-height?)]))

;;empty pasteboard
(test
 los-equal?
 (align 'vertical 100 100 empty)
 empty)

;;empty pasteboard
(test
 los-equal?
 (align 'horizontal 100 100 empty)
 empty)

;;one unstretchable snip
(test
 los-equal?
 (align 'vertical
        100 100
        (list (make-rect (make-dim 0 10 false)
                         (make-dim 0 10 false))))
 (list (make-rect (make-dim 0 10 false)
                  (make-dim 0 10 false))))

(test
 los-equal?
 (align 'horizontal
        100 100
        (list (make-rect (make-dim 0 10 false)
                         (make-dim 0 10 false))))
 (list (make-rect (make-dim 0 10 false)
                  (make-dim 0 10 false))))

;;one stretchable snip
(test
 los-equal?
 (align 'vertical
        100 100
        (list (make-rect (make-dim 0 10 true)
                         (make-dim 0 10 true))))
 (list (make-rect (make-dim 0 100 true)
                  (make-dim 0 100 true))))

;;two stretchable snips
(test
 los-equal?
 (align 'vertical
        10
        10
        (list
         (make-rect (make-dim 0 0 true)
                    (make-dim 0 0 true))
         (make-rect (make-dim 0 0 true)
                    (make-dim 0 0 true))))
 (list
  (make-rect (make-dim 0 10 true)
             (make-dim 0 5 true))
  (make-rect (make-dim 0 10 true)
             (make-dim 5 5 true))))

;;three stretchable, one too big
(test
 los-equal?
 (align 'vertical
        50 100
        (list (make-rect (make-dim 0 0 true)
                         (make-dim 0 50 true))
              (make-rect (make-dim 0 0 true)
                         (make-dim 0 0 true))
              (make-rect (make-dim 0 0 true)
                         (make-dim 0 0 true))))
 (list (make-rect (make-dim 0 50 true)
                  (make-dim 0 50 true))
       (make-rect (make-dim 0 50 true)
                  (make-dim 50 25 true))
       (make-rect (make-dim 0 50 true)
                  (make-dim 75 25 true))))

;;three stetchable, one too big, and an unstetchable
(test
 los-equal?
 (align 'vertical
        50 100
        (list (make-rect (make-dim 0 0 true)
                         (make-dim 0 50 true))
              (make-rect (make-dim 0 0 true)
                         (make-dim 0 0 true))
              (make-rect (make-dim 0 0 true)
                         (make-dim 0 0 true))
              (make-rect (make-dim 0 50 false)
                         (make-dim 0 10 false))))
 (list (make-rect (make-dim 0 50 true)
                  (make-dim 0 50 true))
       (make-rect (make-dim 0 50 true)
                  (make-dim 50 20 true))
       (make-rect (make-dim 0 50 true)
                  (make-dim 70 20 true))
       (make-rect (make-dim 0 50 false)
                  (make-dim 90 10 false))))

;;failure from test-suite frame
;;wrong answer given was (list (make-rect 0 0 335.0 10 #t))
(test
 los-equal?
 (align 'vertical
        335.0
        563.0
        (list
         (make-rect (make-dim 0 10.0 #t)
                    (make-dim 0 10.0 #t))))
 (list (make-rect (make-dim 0 335.0 true)
                  (make-dim 0 563.0 true))))

;;sort of like the previous failed test but with a nonsizable snip
(test
 los-equal?
 (align 'vertical
        563.0
        335.0
        (list
         (make-rect (make-dim 0 10.0 #t)
                    (make-dim 0 10.0 #t))
         (make-rect (make-dim 0 10.0 false)
                    (make-dim 0 10.0 false))))
 (list (make-rect (make-dim 0 563.0 true)
                  (make-dim 0 325.0 true))
       (make-rect (make-dim 0 10.0 false)
                  (make-dim 325.0 10.0 false))))

;;something that requires a little modulo in division
(test
 los-equal?
 (align 'vertical
        10
        10
        (list
         (make-rect (make-dim 0 0 true)
                    (make-dim 0 0 true))
         (make-rect (make-dim 0 0 true)
                    (make-dim 0 0 true))
         (make-rect (make-dim 0 0 true)
                    (make-dim 0 0 true))))
 (list (make-rect (make-dim 0 10 true)
                  (make-dim 0 4 true))
       (make-rect (make-dim 0 10 true)
                  (make-dim 4 3 true))
       (make-rect (make-dim 0 10 true)
                  (make-dim 7 3 true))))

;; 1 snip only stretches in off dimention
(test
 los-equal?
 (align 'vertical
        100
        400
        (list
         (make-rect (make-dim 0 10 true)
                    (make-dim 0 30 false))))
 (list (make-rect (make-dim 0 100 true)
                  (make-dim 0 30 false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following examples of usage were taken from the test-suite tool and turned into test cases ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test
 los-equal?
 (align 'vertical 563.0 335.0 (list))
 empty)

(test
 los-equal?
 (align 'vertical 563.0 335.0
        (list (make-rect (make-dim 0 241 #t) (make-dim 0 114 #f))))
 (list (make-rect (make-dim 0 563.0 #t) (make-dim 0 114 #f))))

(test
 los-equal?
 (align 'vertical 551.0 102.0
        (list (make-rect (make-dim 0 34 #t) (make-dim 0 47 #t))
              (make-rect (make-dim 0 231 #t) (make-dim 0 57 #t))))
 (list (make-rect (make-dim 0 551.0 #t) (make-dim 0 47 #t))
       (make-rect (make-dim 0 551.0 #t) (make-dim 47 57 #t))))

(test
 los-equal?
 (align 'vertical 539.0 35.0
        (list (make-rect (make-dim 0 24 #f) (make-dim 0 13 #f))
              (make-rect (make-dim 0 11 #f) (make-dim 0 24 #f))))
 (list (make-rect (make-dim 0 24 #f) (make-dim 0 13 #f))
       (make-rect (make-dim 0 11 #f) (make-dim 13 24 #f))))

(test
 los-equal?
 (align 'horizontal 539.0 45.0
        (list (make-rect (make-dim 0 65 #t) (make-dim 0 47 #t))
              (make-rect (make-dim 0 48 #t) (make-dim 0 47 #t))
              (make-rect (make-dim 0 63 #t) (make-dim 0 47 #t))
              (make-rect (make-dim 0 45 #f) (make-dim 0 44 #f))))
 (list
  (make-rect (make-dim 0 165.0 true) (make-dim 0 45.0 true))
  (make-rect (make-dim 165.0 165.0 true) (make-dim 0 45.0 true))
  (make-rect (make-dim 330.0 164.0 true) (make-dim 0 45.0 true))
  (make-rect (make-dim 494.0 45 false) (make-dim 0 44 false))))

(test
 los-equal?
 (align 'vertical 153.0 33.0
        (list (make-rect (make-dim 0 55 #f) (make-dim 0 13 #f))
              (make-rect (make-dim 0 11 #f) (make-dim 0 24 #f))))
 (list
  (make-rect (make-dim 0 55 false) (make-dim 0 13 false))
  (make-rect (make-dim 0 11 false) (make-dim 13 24 false))))

(test
 los-equal?
 (align 'vertical 153.0 33.0
        (list (make-rect (make-dim 0 38 #f) (make-dim 0 13 #f))
              (make-rect (make-dim 0 11 #f) (make-dim 0 24 #f))))
 (list
  (make-rect (make-dim 0 38 false) (make-dim 0 13 false))
  (make-rect (make-dim 0 11 false) (make-dim 13 24 false))))

(test
 los-equal?
 (align 'vertical 152.0 33.0
        (list (make-rect (make-dim 0 26 #f) (make-dim 0 13 #f))
              (make-rect (make-dim 0 53 #f) (make-dim 0 24 #f))))
 (list
  (make-rect (make-dim 0 26 false) (make-dim 0 13 false))
  (make-rect (make-dim 0 53 false) (make-dim 13 24 false))))