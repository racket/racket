#lang racket/base
(provide (struct-out arguments)
         (struct-out arity)
         no-arguments
         no-arity
         to-procedure-arity
         arguments->arity
         check-arity
         check-arity/neg
         check-curry
         join-sep
         kw->string
         diff/sorted/eq)

#|
An Arguments is
  #s(arguments (listof stx) (listof keyword) (listof stx))
|#
(define-struct arguments (pargs kws kwargs) #:prefab)

(define no-arguments (arguments null null null))

#|
An Arity is
  #s(arity nat nat/+inf.0 (listof keyword) (listof keyword))
|#
(define-struct arity (minpos maxpos minkws maxkws)
  #:prefab)

(define no-arity (arity 0 0 null null))

;; ----

(define (to-procedure-arity minpos maxpos)
  (cond [(= minpos maxpos) minpos]
        [(= maxpos +inf.0) (arity-at-least minpos)]
        [else (for/list ([i (in-range minpos (add1 maxpos))]) i)]))

(define (arguments->arity argu)
  (let ([pos (length (arguments-pargs argu))]
        [kws (arguments-kws argu)])
    (arity pos pos kws kws)))

(define (check-arity arity pos-count keywords proc)
  (let ([msg (gen-arity-msg (arity-minpos arity)
                            (arity-maxpos arity)
                            (arity-minkws arity)
                            (arity-maxkws arity)
                            pos-count (sort keywords keyword<?))])
    (when msg
      (proc msg))))

(define (check-arity/neg arity pos-count keywords proc)
  (let ([msg (gen-arity-msg/neg (arity-minpos arity)
                                (arity-maxpos arity)
                                (arity-minkws arity)
                                (arity-maxkws arity)
                                pos-count (sort keywords keyword<?))])
    (when msg
      (proc msg))))

(define (arity-sat? minpos maxpos minkws maxkws pos-count keywords)
  (and (<= minpos pos-count maxpos)
       (null? (diff/sorted/eq minkws keywords))
       (null? (diff/sorted/eq keywords maxkws))))

(define (gen-arity-msg minpos maxpos minkws maxkws pos-count keywords)
  (if (arity-sat? minpos maxpos minkws maxkws pos-count keywords)
      #f
      (let ([pos-exp (gen-pos-exp-msg minpos maxpos)]
            [minkws-exp (gen-minkws-exp-msg minkws)]
            [optkws-exp (gen-optkws-exp-msg minkws maxkws)]
            [pos-got (gen-pos-got-msg pos-count)]
            [kws-got (gen-kws-got-msg keywords maxkws)])
        (string-append
         "expected "
         (join-sep (filter string? (list pos-exp minkws-exp optkws-exp))
                   "," "and")
         "; got "
         (join-sep (filter string? (list pos-got kws-got))
                   "," "and")))))

(define (gen-arity-msg/neg minpos maxpos minkws maxkws pos-count keywords)
  (if (arity-sat? minpos maxpos minkws maxkws pos-count keywords)
      #f
      (let ([pos-exp (gen-pos-exp-msg minpos maxpos)]
            [minkws-exp (gen-minkws-exp-msg minkws)]
            [optkws-exp (gen-optkws-exp-msg minkws maxkws)]
            [pos-got (gen-pos-got-msg pos-count)]
            [kws-got (gen-kws-got-msg keywords maxkws)])
        (string-append
         "expected a syntax class that accepts "
         (join-sep (filter string? (list pos-got kws-got))
                   "," "and")
         "; got one that accepts "
         (join-sep (filter string? (list pos-exp minkws-exp optkws-exp))
                   "," "and")))))

(define (check-curry arity pos-count keywords proc)
  (let ([maxpos (arity-maxpos arity)]
        [maxkws (arity-maxkws arity)])
    (when (> pos-count maxpos)
      (proc (format "too many arguments: expected at most ~s, got ~s"
                    maxpos pos-count)))
    (let ([extrakws (diff/sorted/eq keywords maxkws)])
      (when (pair? extrakws)
        (proc (format "syntax class does not accept keyword arguments for ~a"
                      (join-sep (map kw->string extrakws) "," "and")))))))

;; ----

(define (gen-pos-exp-msg minpos maxpos)
  (format "~a positional argument~a"
          (cond [(= maxpos minpos) minpos]
                [(= maxpos +inf.0) (format "at least ~a" minpos)]
                [else
                 (format "between ~a and ~a" minpos maxpos)])
          (if (= minpos maxpos 1) "" "s")))

(define (gen-minkws-exp-msg minkws)
  (and (pair? minkws)
       (format "~amandatory keyword argument~a for ~a"
               (if (= (length minkws) 1) "a " "")
               (if (= (length minkws) 1) "" "s")
               (join-sep (map kw->string minkws) "," "and"))))

(define (gen-optkws-exp-msg minkws maxkws)
  (let ([optkws (diff/sorted/eq maxkws minkws)])
    (and (pair? optkws)
         (format "~aoptional keyword argument~a for ~a"
                 (if (= (length optkws) 1) "an " "")
                 (if (= (length optkws) 1) "" "s")
                 (join-sep (map kw->string optkws) "," "and")))))

(define (gen-pos-got-msg pos-count)
  (format "~a positional argument~a"
          pos-count (if (= pos-count 1) "" "s")))

(define (gen-kws-got-msg keywords maxkws)
  (cond [(pair? keywords)
         (format "~akeyword argument~a for ~a"
                 (if (= (length keywords) 1) "a " "")
                 (if (= (length keywords) 1) "" "s")
                 (join-sep (map kw->string keywords) "," "and"))]
        [(pair? maxkws) "no keyword arguments"]
        [else #f]))

;; ----

(define (kw->string kw) (format "~a" kw))

(define (diff/sorted/eq xs ys)
  (if (pair? xs)
      (let ([ys* (memq (car xs) ys)])
        (if ys*
            (diff/sorted/eq (cdr xs) (cdr ys*))
            (cons (car xs) (diff/sorted/eq (cdr xs) ys))))
      null))

(define (join-sep items sep0 ult0 [prefix ""])
  (define sep (string-append sep0 " "))
  (define ult (string-append ult0 " "))
  (define (loop items)
    (cond [(null? items)
           null]
          [(null? (cdr items))
           (list sep ult (car items))]
          [else
           (list* sep (car items) (loop (cdr items)))]))
  (case (length items)
    [(0) #f]
    [(1) (string-append prefix (car items))]
    [(2) (format "~a~a ~a~a" prefix (car items) ult (cadr items))]
    [else (let ([strings (list* (car items) (loop (cdr items)))])
            (apply string-append prefix strings))]))
