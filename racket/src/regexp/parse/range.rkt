#lang racket/base
(require "chyte.rkt"
         "chyte-case.rkt"
         "ast.rkt"
         "config.rkt"
         "error.rkt"
         "../common/range.rkt"
         "class.rkt"
         "case.rkt")

(provide parse-range
         parse-range/not)

;; Returns (value range position)
(define (parse-range/not s pos config)
  (chyte-case/eos
   s pos
   [(eos)
    (missing-square-closing-error s pos config)]
   [(#\^)
    (define-values (range pos2) (parse-range s (add1 pos) config))
    (values (range-invert range (chytes-limit s)) pos2)]
   [else
    (parse-range s pos config)]))

;; Returns (value range position)
(define (parse-range s pos config)
  (chyte-case/eos
   s pos
   [(eos)
    (missing-square-closing-error s pos config)]
   [(#\])
    (parse-range-rest (range-add empty-range (chyte #\])) s (add1 pos) config)]
   [(#\-)
    (parse-range-rest (range-add empty-range (chyte #\-)) s (add1 pos) config)]
   [else
    (parse-range-rest empty-range s pos config)]))

;; Returns (value range position)
(define (parse-range-rest range s pos config
                          #:span-from [span-from #f]
                          #:must-span-from [must-span-from #f])
  (chyte-case/eos
   s pos
   [(eos)
    (missing-square-closing-error s pos config)]
   [(#\])
    (values (range-add* range span-from config) (add1 pos))]
   [(#\-)
    (define pos2 (add1 pos))
    (chyte-case/eos
     s pos2
     [(eos)
      (missing-square-closing-error s (add1 pos2) config)]
     [(#\])
      (cond
       [must-span-from (misplaced-hyphen-error s pos config)]
       [else (values (range-add (range-add* range span-from config) (chyte #\-))
                     (add1 pos2))])]
     [else
      (cond
       [span-from (parse-range-rest range s pos2 config #:must-span-from span-from)]
       [else (misplaced-hyphen-error s pos config)])])]
   [(#\\)
    (cond
     [(parse-config-px? config)
      (define pos2 (add1 pos))
      (cond
       [(= pos2 (chytes-length s))
        (parse-error s pos config "escaping backslash at end pattern (within square brackets)")]
       [else
        (define c (chytes-ref s pos2))
        (cond
         [(or (and (c . >= . (chyte #\a)) (c . <= . (chyte #\z)))
              (and (c . >= . (chyte #\A)) (c . <= . (chyte #\Z))))
          (cond
           [must-span-from
            (parse-error s pos config "misplaced hyphen within square brackets in pattern")]
           [else
            (define-values (success? range1 pos3) (parse-class s pos2 config))
            (unless success?
              (parse-error s pos3 config "illegal alphabetic escape"))
            (define range2 (range-union range1 (range-add* range span-from config)))
            (parse-range-rest range2 s (add1 pos2) config)])]
         [else
          (parse-range-rest/span c range s (add1 pos2) config
                                 #:span-from span-from
                                 #:must-span-from must-span-from)])])]
     [else
      (parse-range-rest/span (chyte #\\) range s (add1 pos) config
                             #:span-from span-from
                             #:must-span-from must-span-from)])]
   [(#\[)
    (define-values (success? range1 pos2)
      (cond
       [(and (parse-config-px? config)
             (not must-span-from))
        (parse-posix-char-class s (add1 pos))]
       [else
        (values #f #f #f)]))
    (cond
     [success?
      (define range2 (range-union range1 (range-add* range span-from config)))
      (parse-range-rest range2 s pos2 config)]
     [else
      (parse-range-rest/span (chyte #\[) range s (add1 pos) config
                             #:span-from span-from
                             #:must-span-from must-span-from)])]
   [else
    (parse-range-rest/span (chytes-ref s pos) range s (add1 pos) config
                           #:span-from span-from
                           #:must-span-from must-span-from)]))
  
(define (parse-range-rest/span c range s pos config
                               #:span-from span-from
                               #:must-span-from must-span-from)
  (cond
   [must-span-from
    (cond
     [(must-span-from . > . c)
      (parse-error s pos config "invalid range within square brackets in pattern")]
     [else
      (parse-range-rest (range-add-span* range must-span-from c config) s pos config)])]
   [else
    (parse-range-rest (range-add* range span-from config) s pos config
                      #:span-from c)]))

(define (missing-square-closing-error s pos config)
  (parse-error s pos config "missing closing square bracket in pattern"))

(define (misplaced-hyphen-error s pos config)
  (parse-error s pos config "misplaced hyphen within square brackets in pattern"))
