(module ss-funcs mzscheme

(require "data-synthesis.ss"
         "distributions.ss"
         (rename (lib "frtime.ss" "frtime") undefined? undefined?)
         )

;;filter: ('a -> bool) * 'a list -> 'a list
(define (filter pred lst)
  (cond [(null? lst) '()]
        [(pred (car lst))
         (cons (car lst)
               (filter pred (cdr lst)))]
        [else (filter pred (cdr lst))]))

;; count : ('a -> bool) * 'a list -> num
(define (count pred lst)
  (cond [(null? lst) 0]
        [(pred (car lst))
         (+ 1 (count pred (cdr lst)))]
        [else (count pred (cdr lst))])) 

;;(num U undefined) list -> num list
(define (filter-out-undefined lst)
  (filter (lambda (o)
            (not (undefined? o)))
          lst))

; list -> num
(define (count-undefineds lst)
  (count undefined? lst))

(define (name->dist-fit-func name)
  (cond [(equal? name 'uniform)
         fit-uniform-distribution]
        [(equal? name 'normal)
         fit-normal-distribution]
        [(equal? name 'exponential)
         fit-exponential-distribution]
;        [(equal? name 'gamma)
;         fit-gamma-distribution]  Gamme distribution is messed up.  See distributions.ss
        [else (raise (format "~a not an allowable distribution name"))]))

;;('a -> bool) * 'a list * 'a list -> 'a list
(define (replace pred lst filler)
  (cond [(null? lst)
         '()]
        [(not (pred (car lst)))
         (cons (car lst)
               (replace pred
                        (cdr lst)
                        filler))]
        [(null? filler)
         (raise "not enough filler to complete replace function")]
        [(pred (car lst))
         (cons (car filler)
               (replace pred
                        (cdr lst)
                        (cdr filler)))]))

;;('a -> bool) * 'a list * (-> 'a) -> 'a list
(define (replace-from-generator pred lst gen)
  (cond [(null? lst)
         '()]
        [(not (pred (car lst)))
         (cons (car lst)
               (replace-from-generator pred
                                       (cdr lst)
                                       gen))]
        [(pred (car lst))
         (cons (gen)
               (replace-from-generator pred
                                       (cdr lst)
                                       gen))]))

;; symbol * (num U undefined) list -> num list
(define (inflate-data dist-name data)
  (replace-from-generator undefined?
                          data
                          (distribution-rand ((name->dist-fit-func dist-name) 
                                              (filter-out-undefined data)))))
;
;(define ndata (synthesize-random-data (make-uniform-distribution 0 100)
;                                      10000))
;(define holy-data 
;  (let loop ([c 2] [lst ndata])
;    (cond [(null? lst) '()]
;          [(zero? c) (cons undefined
;                           (loop 3
;                                 (cdr lst)))]
;          [else (cons (car lst)
;                      (loop (sub1 c)
;                            (cdr lst)))])))
;(define unholy-data (inflate-data 'uniform holy-data))
;
;;"Source Mean: 0"
;"Data Mean:" (sample-mean ndata)
;"Holy-Data Mean:" (sample-mean (filter-out-undefined holy-data))
;"Unholy-Data Mean:" (sample-mean unholy-data)
;;"Source Variance: 1"
;"Data Variance:" (sample-variance ndata)
;"Holy-Data Variance:" (sample-variance (filter-out-undefined holy-data))
;"Unholy-Data Variance:" (sample-variance unholy-data)

(provide inflate-data)
)