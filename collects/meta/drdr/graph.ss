#lang scheme/gui

;; a raw-line is
;;  (list number number (listof (list number number number)))

;;; ========================================

;; a graph is
;;  (make-graph revision-number revision-number (listof line))
(define-struct graph (start end lines) #:transparent)

;; a line is
;;  (make-line number number (listof number) string symbol)
;;  min, max: absolute values from the data
;;  points: numbers between [0,1] indicating the percentage
;;          of the time of the slowest run 
;;  style = (or/c 'overall 'cpu 'real 'gc)
(define-struct line (min max points color style) #:transparent)

(define graph-height 200)
(define graphs-width 1000)
(define graph-gap 4)
(define frame? #f)

(define-values (input-file output-file)
  (command-line
   #:once-each
   [("-w" "--width") 
    width
    "width of the image, defaults to 1000" 
    (set! graphs-width width)]
   [("-e" "--height")
    height
    "height of the image, defaults to 200"
    (set! graph-height height)]
   [("-f" "--frame")
    "open a window containing the image, instead of writing a file"
    (set! frame? #t)]
   #:args (input-file output-file) (values input-file output-file)))




;                                              
;                                              
;                                              
;                                              
;                             ;;               
;                             ;                
;   ;;;;;  ;;;;;  ;;;; ;;;;   ;;  ;;;;;  ;;;;; 
;   ;;;;;  ;; ;;  ;;;  ;; ;;  ;;  ;;;;;  ;;;;; 
;   ;;  ;;   ;;;; ;;   ;;;;   ;;  ;; ;;;;;; ;; 
;   ;;  ;; ;;;;;; ;;    ;;;;  ;;  ;; ;;;;;; ;; 
;   ;;;;;;;;; ;;; ;;  ;;; ;;  ;;  ;; ;;; ;;;;; 
;   ;;;;;  ;;;;;; ;;   ;;;;   ;;  ;; ;;; ;;;;; 
;   ;;                                   ;; ;; 
;   ;;                                   ;;;;; 
;                                              
;                                              



;; this build ex2.ss out of ex.ss
;; adjust : raw-line -> raw-line
(define adjust
  (let ([seen-time #f])
    (lambda (l)
      (match l
        [`(,rev ,time ,times)
         (cond
           [(empty? times)
            l]
           [(not seen-time)
            (set! seen-time 1)
            l]
           [(< seen-time 30)
            (set! seen-time (+ seen-time 1))
            l]
           [else
            (list (list-ref l 0)
                  (list-ref l 1)
                  (list (car (list-ref l 2))
                        (map (λ (x) (* x 10)) (car (list-ref l 2)))))])]))))

;; fetch-data : string -> (listof raw-line)
(define (fetch-data file)
  (call-with-input-file  file 
    (λ (port)
      (let loop ()
        (let ([l (read port)])
          (if (eof-object? l)
              '()
              (cons l #; (adjust l) ;; to build ex2.ss
                    (loop))))))))

;; build-graphs : (listof raw-line) -> (listof graph)
(define (build-graphs data)
  (let loop ([data data]
             [working-graph '()])
    (cond
      [(null? data) 
       (if (null? working-graph)
           '()
           (list (finalize-graph (reverse working-graph))))]
      [else
       (let ([this (car data)])
         (cond
           [(matching-line? this working-graph)
            (loop (cdr data)
                  (cons this working-graph))]
           [else
            (cons (finalize-graph (reverse working-graph))
                  (loop data '()))]))])))

;; match-line? : raw-line (listof raw-line) -> boolean
;; #t if the line fits into this graph
(define (matching-line? line working-graph)
  (or (null? working-graph)
      (match line
        [`(,rev ,time ,line-seqs)
         (match (car working-graph)
           [`(,rev ,time ,working-line-seq)
            (= (length line-seqs)
               (length working-line-seq))])])))

;; finalize-graph : (non-empty-listof raw-line) -> graph
(define (finalize-graph working-graph)
  (restart-colors)
  (make-graph 
   (car (car working-graph))
   (car (last working-graph))
   (cons
    (build-line 'overall (map second working-graph) "black")
    (apply 
     append
     (let ([cpu-real-gcss (map third working-graph)])
       (for/list ([ele (car cpu-real-gcss)]
                  [i (in-naturals)])
         (let ([color (next-color)])
           (list (build-line 'cpu 
                             (map (λ (x) (first (list-ref x i)))
                                  cpu-real-gcss)
                             color)
                 (build-line 'real 
                             (map (λ (x) (second (list-ref x i))) 
                                  cpu-real-gcss)
                             color)
                 (build-line 'gc 
                             (map (λ (x) (third (list-ref x i)))
                                  cpu-real-gcss)
                             color)))))))))

(define (average l)
  (/ (apply + l)
     (length l)))

(define IGNORE-COEFFICIENT 10)
(define (build-line which real-points color)
  (if (empty? real-points)
      (make-line 0 1 empty color which)
      (local [(define average-v (average real-points))
              (define points
                (filter-not (λ (x) (> x (* IGNORE-COEFFICIENT average-v)))
                            real-points))
              (define min-v (apply min points))
              (define max-v (apply max points))]
        (make-line min-v max-v (map (λ (x) (/ x max-v)) real-points) color which))))

(define-values (next-color restart-colors)
  (let ([colors '("darkred" 
                  "mediumvioletred" 
                  "brown"
                  "olive"
                  "darkgreen"
                  "midnightblue")]
        [i 0])
    (values (λ ()
              (begin0
                (list-ref colors i)
                (set! i (modulo (+ i 1) (length colors)))))
            (λ ()
              (set! i 0)))))


;                                                 
;                                                 
;                                                 
;                                                 
;      ;;                        ;;               
;      ;;                        ;                
;   ;;;;;  ;;;; ;;;;; ;;  ;;  ;; ;;  ;;;;;  ;;;;; 
;   ;;;;;  ;;;  ;; ;;  ;  ;;  ;; ;;  ;;;;;  ;;;;; 
;  ;;; ;;  ;;     ;;;; ;;;;;;;;  ;;  ;; ;;;;;; ;; 
;  ;;; ;;  ;;   ;;;;;; ;;;;;;;;  ;;  ;; ;;;;;; ;; 
;   ;;;;;  ;;  ;;; ;;;  ;;  ;;;  ;;  ;; ;;; ;;;;; 
;   ;;;;;  ;;   ;;;;;;  ;;  ;;   ;;  ;; ;;; ;;;;; 
;                                           ;; ;; 
;                                           ;;;;; 
;                                                 
;                                         ;;;;;;;;


(define (draw-graphs dc graphs)
  (let ([tot-points (apply + (map graph-point-count graphs))]
        [tot-space (- graphs-width (* graph-gap (- (length graphs) 1)))])
    (let loop ([sx 0]
               [graphs graphs])
      (unless (null? graphs)
        (let* ([graph (car graphs)]
               [points (graph-point-count graph)]
               [this-w (* (/ points tot-points) tot-space)]
               [next-sx (+ sx this-w graph-gap)])
          (draw-graph dc graph sx this-w)
          (unless (null? (cdr graphs))
            (send dc set-pen "black" 1 'transparent)
            (send dc set-brush "gray" 'solid)
            (send dc set-alpha 1)
            (send dc draw-rectangle 
                  (- next-sx graph-gap)
                  0
                  graph-gap
                  graph-height))
          (loop next-sx 
                (cdr graphs)))))))

(define (graph-point-count graph)
  (length (line-points (car (graph-lines graph)))))

(define (draw-graph dc graph sx w)
  (draw-legend dc sx w)
  (for ([line (in-list (graph-lines graph))])
    (let* ([num-points (length (line-points line))]
           [i->x (λ (i) (+ sx (* (/ i num-points) w)))])
      (send dc set-pen (line->pen line))
      (send dc set-alpha (line->alpha line)) 
      (for ([start (in-list (line-points line))]
            [end (in-list (cdr (line-points line)))]
            [i (in-naturals)])
        (let* ([x-start (i->x i)]
               [x-end (i->x (+ i 1))]
               [y-start (* (- 1 start) graph-height)]
               [y-end (* (- 1 end) graph-height)])
          (send dc draw-line 
                x-start y-start
                x-end y-end))))))

(define (draw-legend dc sx w)
  (send dc set-pen "gray" 3 'solid)
  (send dc set-alpha 1)
  (let ([hline (λ (p)
                 (send dc draw-line 
                       sx
                       (* p graph-height)
                       (+ sx w)
                       (* p graph-height)))])
    (hline 0)
    (hline 1/4)
    (hline 1/2)
    (hline 3/4)
    (hline 1)))

(define (line->alpha line)
  (case (line-style line)
    [(overall) 1]
    [(cpu) 1/2]
    [(gc) 1/4]
    [(real) 1]))

(define (line->pen line)
  (send the-pen-list find-or-create-pen 
        (line-color line)
        1
        'solid))

(define (draw fgs dc)
  (send dc set-smoothing 'aligned)
  (draw-graphs dc fgs))


;                               
;                               
;                               
;                               
;                     ;;        
;                     ;         
;   ;;;;; ;;   ;;;;;  ;;  ;;;;; 
;   ;;;;;;;;;  ;; ;;  ;;  ;;;;; 
;   ;; ;;  ;;    ;;;; ;;  ;; ;;;
;   ;; ;;  ;;  ;;;;;; ;;  ;; ;;;
;   ;; ;;  ;; ;;; ;;; ;;  ;; ;;;
;   ;; ;;  ;;  ;;;;;; ;;  ;; ;;;
;                               
;                               
;                               
;


(define (show fgs)
  (define f (new frame% [label ""] [alignment '(center center)]))
  (define c (new canvas% 
                 [parent f] 
                 [min-width graphs-width]
                 [min-height graph-height]
                 [stretchable-height #f]
                 [stretchable-width #f]
                 [paint-callback (λ (c dc) (draw fgs dc))]))
  (send f show #t))

(define (save fgs)
  (let* ([bm (make-object bitmap% graphs-width graph-height)]
         [bdc (make-object bitmap-dc% bm)])
    (send bdc clear)
    (draw fgs bdc)
    (send bm save-file output-file 'png)
    (void)))

(let ([fgs (build-graphs (fetch-data input-file))])
  (if frame?
      (show fgs)
      (save fgs)))
