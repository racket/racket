#lang racket/base

(require racket/bool
         racket/match
         racket/contract
         "dv.rkt"
         "contract.rkt")

(define-struct t (sorter equality data))

;; sorter: elements which have the most trueness according to 
;; the sorter pop out first
(define (make-heap sorter equality)
  (define data (dv:make 5))
  (dv:append data 0)
  (make-t sorter equality data))

(define (heap-size heap) (- (dv:length (t-data heap)) 1))
(define (heap-empty? heap) (= (heap-size heap) 0))
(define (heap-last heap) (- (dv:length (t-data heap)) 1))
(define (heap-parent i) (floor (/ i 2)))
(define (heap-left i) (* i 2))
(define (heap-right i) (+ 1 (* i 2)))
(define (heap-has-right heap i) (<= (heap-right i) (heap-last heap)))
(define (heap-has-left heap i) (<= (heap-left i) (heap-last heap)))

(define (heap-insert heap item)
  (match heap
    [(struct t (sorter _ data))
     (dv:append data item)
     (let ([d (let loop ([prev (heap-last heap)]
                         [current (heap-parent (heap-last heap))])
                
                (cond [(= current 0) prev]
                      [(sorter item (dv:ref data current)) 
                       (dv:set! data prev (dv:ref data current))
                       (loop current (heap-parent current))]
                      [else prev]))])
       (dv:set! data d item))]))

(define (heap-peak heap)
  (dv:ref (t-data heap) 1))

(define (heap-pop heap)
  (begin0 (dv:ref (t-data heap) 1)
          (heap-remove-pos heap 1)))

(define (heap-remove-pos heap pos)
  (match heap
    [(struct t (sorter _ data))
     (cond [(= pos (heap-last heap)) (dv:remove-last data)]
           [else
            (let ((item (dv:ref data (heap-last heap))))
              (dv:remove-last data)
              (let loop ([current pos])
                
                (dv:set! data current item)
                (let* ([left (heap-left current)]
                       [right (heap-right current)]
                       [best-1 (if (and (heap-has-left heap current)
                                        (sorter (dv:ref data left) item)) 
                                   left current)]
                       [best-2 (if (and (heap-has-right heap current)
                                        (sorter (dv:ref data right)
                                                (dv:ref data best-1)))
                                   right best-1)])                  
                  (unless (= best-2 current)
                    (dv:set! data current (dv:ref data best-2))
                    (loop best-2)))))])]))

;; return false if the object is not found
(define (heap-remove heap item)
  (let ([pos (heap-find heap item)])
    (if (not pos) false
        ; There is something present, so it must not be empty
        (begin (heap-remove-pos heap pos) true))))

(define (heap-contains heap item)
  (if (heap-find heap item) true false))

(define (heap-find heap item)
  (match heap
    [(struct t (sorter equality data))
     (let loop ([current 1])
       (let ([current-item (dv:ref data current)])
         (cond [(equality item current-item) current]
               [(sorter item current-item) #f]
               [else (or (and (heap-has-left heap current)
                              (not (sorter item (dv:ref data (heap-left current))))
                              (loop (heap-left current)))
                         (and (heap-has-right heap current)
                              (not (sorter item (dv:ref data (heap-right current))))
                              (loop (heap-right current))))])))]))

(define (heap-resort heap item)
  (heap-remove heap item)
  (heap-insert heap item))

(define sorter/c
  (-> any/c any/c boolean?))
(define equality/c
  (-> any/c any/c boolean?))
(define heap? t?)
(define (non-empty-heap? heap)
  (and (heap? heap)
       (not (= (heap-size heap) 0))))

(provide/contract*
 [heap? (any/c . -> . boolean?)]
 [non-empty-heap? (any/c . -> . boolean?)]
 [make-heap (sorter/c equality/c . -> . heap?)]
 [heap-empty? (heap? . -> . boolean?)]
 [heap-insert (heap? any/c . -> . void)]
 [heap-pop (non-empty-heap? . -> . any/c)]
 [heap-peak (non-empty-heap? . -> . any/c)]
 [heap-remove-pos (non-empty-heap? exact-nonnegative-integer? . -> . void)]
 [heap-remove (heap? any/c . -> . boolean?)]
 [heap-contains (heap? any/c . -> . boolean?)])
