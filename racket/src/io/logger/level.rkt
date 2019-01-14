#lang racket/base
(require "../common/check.rkt")

(provide check-level
         level>=?
         level-max
         level-min
         parse-filters
         filters-level-for-topic
         filters-max-level
         level->user-representation)

;; A filter set is represented as an improper list of pairs ending
;; with a (non-pair) level symbol. The ending symbol is the level that
;; applies if a name match is not found for any of the preceding
;; elements of the list.

(define (level->value lvl)
  (case lvl
    [(none) 0]
    [(fatal) 1]
    [(error) 2]
    [(warning) 3]
    [(info) 4]
    [(debug) 5]
    [else #f]))

(define (level>=? a b)
  ((level->value a) . >= . (level->value b)))

(define (level-max a b)
  (if ((level->value a) . < . (level->value b))
      b
      a))

(define (level-min a b)
  (if ((level->value a) . < . (level->value b))
      a
      b))

(define (check-level who v)
  (unless (level->value v)
    (raise-argument-error who
                          "(or/c 'none 'fatal 'error 'warning 'info 'debug)"
                          v)))

;; ----------------------------------------

(define (parse-filters who l #:default-level default-level)
  (let loop ([l l] [accum null] [default-level default-level])
  (cond
    [(null? l)
     (append accum default-level)]
    [else
     (define level (car l))
     (check-level who level)
     (cond
       [(null? (cdr l))
        (append accum level)]
       [else
        (define topic (cadr l))
        (unless (or (not topic) (symbol? topic))
          (raise-argument-error who "(or/c #f symbol?)" topic))
        (if (not topic)
            (loop (cddr l) accum level)
            (loop (cddr l)
                  (cons (cons topic level) accum)
                  default-level))])])))

(define (filters-level-for-topic filters topic)
  (let loop ([filters filters])
    (cond
      [(pair? filters)
       (cond
         [(eq? (caar filters) topic)
          (cdar filters)]
         [else
          (loop (cdr filters))])]
      [else
       ;; default:
       filters])))

(define (filters-max-level filters)
  (let loop ([filters filters] [best-level 'none])
    (cond
      [(pair? filters)
       (loop (cdr filters)
             (level-max best-level (cdar filters)))]
      [else
       (level-max best-level filters)])))

;; ----------------------------------------

(define (level->user-representation lvl)
  (if (eq? lvl 'none)
      #f
      lvl))
