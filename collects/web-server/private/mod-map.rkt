#lang racket/base
(require racket/contract
         racket/list
         racket/match)
(provide/contract
 [compress-serial (list? . -> . list?)]
 [decompress-serial (list? . -> . list?)])

;; compress-mod-map : (listof (cons mod-spec symbol)) -> (listof (cons (or mod-spec number) symbol))
(define (compress-mod-map mm)
  (compress-mod-map/seen empty mm))

(define (lookup-seen ms seen)
  (match seen
    [(list)
     (values #f (list ms))]
    [(list-rest ms+ seen+)
     (if (equal? ms ms+)
         (values 0 (list* ms+ seen+))
         (let-values ([(i seen++) (lookup-seen ms seen+)])
           (values (if i (add1 i) #f) (list* ms+ seen++))))]))

(define (compress-mod-map/seen seen mm)
  (match mm
    [(list) 
     (list)]
    [(list-rest (list-rest mod-spec sym) mm)
     (define-values (i seen+) (lookup-seen mod-spec seen))
     (if i
         (list* (cons i sym) (compress-mod-map/seen seen+ mm))
         (list* (cons mod-spec sym) (compress-mod-map/seen seen+ mm)))]))

;; decompress-mod-map : (listof (cons (or mod-spec number) symbol)) -> (listof (cons mod-spec symbol))
(define (decompress-mod-map cmm)
  (decompress-mod-map/seen empty cmm))

(define (decompress-mod-map/seen seen cmm)
  (match cmm
    [(list)
     (list)]
    [(list-rest (list-rest mod-spec-or-n sym) cmm)
     (if (number? mod-spec-or-n)
         (list* (cons (list-ref seen mod-spec-or-n) sym)
                (decompress-mod-map/seen seen cmm))
         (list* (cons mod-spec-or-n sym)
                (decompress-mod-map/seen (append seen (list mod-spec-or-n)) cmm)))]))

; compress-serial : serial -> serial (with compressed mod-map)
(define compress-serial
  (match-lambda
    [(list vs e0 mm e2 e3 e4 e5)
     (list vs e0 (compress-mod-map mm) e2 e3 e4 e5)]))

; decompress-serial : serial (with compressed mod-map) -> serial
(define decompress-serial
  (match-lambda
    [(list vs e0 cmm e2 e3 e4 e5)
     (list vs e0 (decompress-mod-map cmm) e2 e3 e4 e5)]))
