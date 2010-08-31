#lang scheme/gui
(require xml)

(require "constants.ss")

;; example data:
;; http://drdr.racket-lang.org/data/collects/tests/mzscheme/benchmarks/common/earley_ss.timing

;;; ========================================

;; a raw-line is
;;  (list number number (listof (list number number number)))

;; a graph is
;;  (make-graph revision-number revision-number (listof line))
(define-struct graph (start end lines) #:transparent)

;; a line is
;;  (make-line number number (listof point) string symbol)
;;  style = (or/c 'overall 'cpu 'real 'gc)
(define-struct line (min max points color style) #:transparent)

;; value : number between [0,1] indicating the percentage
;;         of the time of the slowest run
;; revision : revision number
(define-struct point (value revision) #:transparent)


;; revision : nat
;; x-pixel : nat
(define-struct coordinate (revision x-pixel) #:transparent)

(define graph-gap 4)
(define max-revisions-per-graph (make-parameter 400))

(define max-graphs-per-image (floor (/ graphs-width (+ graph-gap 4))))
(define max-samples-per-image (floor (/ graphs-width 3)))

(define link-format-string #f)

(define image-loc "./")

(define full? #f)

(define-values (input-file image-filename-prefix image-url-prefix html-file)
  (command-line
#|
   #:argv
   #("-l"
     "http://drdr.racket-lang.org/~a/collects/tests/mzscheme/benchmarks/mz/expand-class.scm"
     "expand-class_scm.timing" "out" "out.html" )
|#
   #:once-each
   [("-f" "--full")
    "indicates that a complete html file should be produced; otherwise, a single div is all you get"
    (set! full? #t)]
   [("-l" "--link")
    link-format
    "specifies where revisions link to; expected to be a url with a ~a in the middle"
    (set! link-format-string link-format)]
   [("--image-loc")
    url-path
    "specify the path to the image files for html generation (not just to the dir; to the file itself)"
    (unless (regexp-match #rx"/$" url-path)
      (error 'build-graph.ss "expected the image-loc to end with a /, got ~a" url-path))
    (set! image-loc url-path)]
   #:args (input-file image-filename-prefix image-url-prefix html-file) 
   (values input-file image-filename-prefix image-url-prefix html-file)))

(define dot-image-file (string-append image-loc "dot.png"))
(define before-image-file (string-append image-loc "before.png"))
(define after-image-file (string-append image-loc "after.png"))




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

;; orig-data : hash[revision-number -o> sexp]
;; the original lines from the files, indexed by revision number
(define orig-data (make-hash))

(define (revision->duration revision)
  (let ([info (hash-ref orig-data revision)])
    (floor (inexact->exact (list-ref info 1)))))

(define (revision->timings-array revision)
  (let ([info (hash-ref orig-data revision)])
    (apply
     string-append
     (append (list "[")
             (add-between (map (λ (line)
                                 (format "'cpu time: ~a real time: ~a gc time: ~a'"
                                         (list-ref line 0)
                                         (list-ref line 1)
                                         (list-ref line 2)))
                               (list-ref info 2))
                          ",")
             (list "]")))))

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

;; fetch-data : string -> (listof raw-line)[uniq revision-numbers]
(define (fetch-data file)
  (call-with-input-file file 
    (λ (port)
      (let loop ()
        (let ([l (read port)])
          (cond
            [(eof-object? l) '()]
            [(hash-ref orig-data (car l) #f)
             ;; skip duplicate revisions
             (loop)]
            [else
             (hash-set! orig-data (car l) l) 
             (cons l (loop))]))))))

;; build-graphss : (listof raw-line) -> (listof (listof graph))
(define (build-graphss data)
  (let ([large-graphs 
         (filter
          (λ (x) (<= 2 (length (line-points (car (graph-lines x))))))
          (build-large-graphs data))])
    (let loop ([graphs (reverse large-graphs)])
       (let-values ([(first rest) (split-out-graph graphs)])
         (cond
           [(null? rest)
            (list first)]
           [else
            (cons first (loop rest))])))))

;; split-out-graphs : (listof graph) -> (values (listof graph) (listof graph))
;; first result is a set of graphs to go into a single image
;;   limits the number of graphs and the number of total samples
;; expects the graphs to be in order from the right to the left 
;;  returns the first result in the opposite order and the second result in the same order.
(define (split-out-graph graphs)
  (let loop ([graphs graphs]
             [sample-count 0]
             [graph-count 0]
             [current '()])
    (cond
      [(null? graphs)
       (values current graphs)]
      [(< max-graphs-per-image graph-count)
       (values current graphs)]
      [else
       (let* ([graph (car graphs)]
              [this-graph-samples (graph-sample-count graph)])
         (cond
           [(<= (+ sample-count this-graph-samples) max-samples-per-image)
            ;; if this graph fits, take it.
            (loop (cdr graphs)
                  (+ sample-count this-graph-samples)
                  (+ graph-count 1)
                  (cons graph current))]
           [(<= sample-count (/ max-samples-per-image 2))
            ;; if the graph doesn't fit, and we have less than 1/2 of the samples that fill 
            ;; the page, break this graph into two graphs, taking the first part of the split
            (let-values ([(before after) (split-graph 
                                          graph
                                          (- max-samples-per-image sample-count))])
              (values (cons before current)
                      (cons after (cdr graphs))))]
           [else
            ;; otherwise, just stop with what we have now
            (values current
                    graphs)]))])))

;; split-graph : graph number -> (values graph graph)
;; break graph into two pieces where the first piece has 'max-samples' samples
;; split-point <= number of samples in graph
(define (split-graph graph split-point)
  (let* ([this-graph-samples (graph-sample-count graph)]
         [orig-lines (graph-lines graph)]
         [lines-before (pull-out orig-lines (λ (x) (take x split-point)))]
         [lines-after (pull-out orig-lines (λ (x) (drop x split-point)))]
         [lines-before-last-revision
          (apply max (map point-revision (line-points (car lines-before))))]
         [lines-after-first-revision
          (apply min (map point-revision (line-points (car lines-after))))])
    (values (make-graph (graph-start graph) 
                        lines-before-last-revision
                        lines-before)
            (make-graph lines-after-first-revision 
                        (graph-end graph)
                        lines-after))))

;; pull-out : (listof line) (-> (listof point) (listof point)) -> (listof line)
;; makes lines like 'lines', but using puller to select the relevant points
(define/contract (pull-out lines puller)
  (-> (listof line?) (-> (listof point?) (listof point?)) (listof line?))
  (map (λ (line)
         (let* ([new-points (puller (line-points line))]
                [max-v (apply max (map point-value new-points))]
                [min-v (apply min (map point-value new-points))])
           (make-line min-v
                      max-v
                      new-points
                      (line-color line)
                      (line-style line))))
       lines))

(define (graph-sample-count graph)
  (length (line-points (car (graph-lines graph)))))

;; build-large-graphs : (listof raw-line) -> (listof graph)
(define (build-large-graphs data)
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

;; matching-line? : raw-line (listof raw-line) -> boolean
;; #t if the line fits into this graph
(define (matching-line? line working-graph)
  (or (null? working-graph)
      (match line
        [`(,rev ,time ,line-seqs)
         (match (car working-graph)
           [`(,rev ,time ,working-line-seq)
            (= (length line-seqs)
               (length working-line-seq))])])))

;; split-up : (listof X) -> (listof (listof X))
;; splits up the working graph into at chunks of size at most
;; max-revisions-per-graph.
(define (split-up working-graph)
  (reverse
   (let loop ([working-graph (reverse working-graph)]
              [i 0]
              [pending '()])
     (cond
       [(null? working-graph) 
        (if (null? pending)
            '()
            (list pending))]
       [else
        (if (< i (max-revisions-per-graph))
            (loop (cdr working-graph)
                  (+ i 1)
                  (cons (car working-graph) pending))
            (cons pending
                  (loop working-graph 0 '())))]))))

;; poor man testing ....
(parameterize ([max-revisions-per-graph 3])
  (unless (and (equal? (split-up '()) '())
               (equal? (split-up '(1)) '((1)))
               (equal? (split-up '(1 2)) '((1 2)))
               (equal? (split-up '(1 2 3)) '((1 2 3)))
               (equal? (split-up '(1 2 3 4)) '((1) (2 3 4)))
               (equal? (split-up '(1 2 3 4 5)) '((1 2) (3 4 5)))
               (equal? (split-up '(1 2 3 4 5 6)) '((1 2 3) (4 5 6)))
               (equal? (split-up '(1 2 3 4 5 6 7)) '((1) (2 3 4) (5 6 7))))
    (error 'tests-failed)))

;; finalize-graph : (non-empty-listof raw-line) -> graph
(define (finalize-graph working-graph)
  (restart-colors)
  (let ([revisions (map first working-graph)])
    (make-graph 
     (car (car working-graph))
     (car (last working-graph))
     (cons
      (build-line 'overall 
                  (map second working-graph) 
                  revisions
                  "black")
      (apply 
       append
       (let ([cpu-real-gcss (map third working-graph)])
         (for/list ([ele (car cpu-real-gcss)]
                    [i (in-naturals)])
           (let ([color (next-color)])
             (list (build-line 'cpu 
                               (map (λ (x) (first (list-ref x i)))
                                    cpu-real-gcss)
                               revisions
                               color)
                   (build-line 'real 
                               (map (λ (x) (second (list-ref x i))) 
                                    cpu-real-gcss)
                               revisions
                               color)
                   (build-line 'gc 
                               (map (λ (x) (third (list-ref x i)))
                                    cpu-real-gcss)
                               revisions
                               color))))))))))

(define (build-line which points revisions color)
  (let ([min-v (apply max points)]
        [max-v (apply max points)])
    (make-line min-v
               max-v
               (map make-point points revisions)
               color which)))

(define-values (next-color restart-colors)
  (let ([colors '("darkred" 
                  "mediumvioletred" 
                  "brown"
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
;


;; record-points : (parameter (or/c #f (-> number[x-coordinate] number[revision] -> void)))
(define record-points (make-parameter #f))

(define (graphs->coordinates graphs)
  (let ([dc (make-object bitmap-dc% (make-object bitmap% 1 1))]
        [points (make-hash)])
    (define (rp x-coord revision)
      (let ([pixel (inexact->exact (floor x-coord))]
            [prev (hash-ref points revision #f)])
        (cond
          [prev
           (unless (equal? prev pixel)
             (error 
              'graphs->coordinates 
              "revision ~s maps to two different pixel values! ~s and ~s" 
              revision
              prev
              pixel))]
          [else
           (hash-set! points revision pixel)])))
    (parameterize ([record-points rp])
      (draw-graphs dc graphs))
    (sort (hash-map points (λ (revision pixel) (make-coordinate revision pixel))) 
          < 
          #:key coordinate-revision)))

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
           [i->x (λ (i) (+ sx (* (/ i (- num-points 1)) w)))]
           [point->y (λ (point) 
                       (let ([lm (line-max line)])
                         (if (zero? lm) ;; everything must be zero in this case
                             graph-height
                             (* (- 1 (/ (point-value point) lm)) 
                                graph-height))))])
      (send dc set-pen (line->pen line))
      (send dc set-alpha (line->alpha line)) 
      
      (for ([start (in-list (line-points line))]
            [end (in-list (cdr (line-points line)))]
            [i (in-naturals)])
        (let* ([x-start (i->x i)]
               [x-end (i->x (+ i 1))]
               [y-start (point->y start)]
               [y-end (point->y end)])
          (let ([rp (record-points)])
            (when rp
              (when (= i 0) (rp x-start (point-revision start)))
              (rp x-end (point-revision end))))
          (send dc draw-line 
                x-start y-start
                x-end y-end))))))

(define (draw-legend dc sx w)
  (send dc set-pen "gray" 3 'solid)
  (send dc set-alpha 1)
  (let ([hline (λ (p [dy 0])
                 (send dc draw-line 
                       sx
                       (+ dy (* p graph-height))
                       (+ sx w)
                       (+ dy (* p graph-height))))])
    (hline 0 1)
    (hline 1/4)
    (hline 1/2)
    (hline 3/4)
    (hline 1 -2)))

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
;   ;;       ;             ;; 
;   ;;      ;;             ;; 
;   ;;      ;;             ;; 
;   ;;;;;; ;;;; ;;;;;;;;;  ;; 
;   ;;  ;;  ;;  ;; ;;; ;;; ;; 
;   ;;  ;;  ;;  ;; ;;; ;;; ;; 
;   ;;  ;;  ;;  ;; ;;; ;;; ;; 
;   ;;  ;;  ;;  ;; ;;; ;;; ;; 
;   ;; ;;;  ;;;;;; ;;; ;;; ;; 
;                             
;                             
;                             

(define (write-html graphss)
  (let ([xml (xexpr->xml (graphs->complete-xexpr graphss))])
    (call-with-output-file html-file
      (λ (port) (display-xml/content xml port))
      #:exists 'truncate)))

(define (graphs->complete-xexpr graphss)
  (let ([xexpr (graphs->xexpr graphss)])
    (if full?
        `(html (head)
               (body ,xexpr))
        xexpr)))
       

(define (graphs->xexpr graphss)
  (let ([last-one (- (length graphss) 1)])
    `(div
      (script ((type "text/javascript"))
              ,(format "var current_pane=~a;\n" last-one)
              "function move_bar(rev,x,w,duration,timing_strings) {\n"
              "  var suffix = ' msec'\n"
              "  if (duration > 1000) {\n"
              "    duration = duration/1000;"
              "    suffix = ' sec'"
              "  }\n"
              "  document.getElementById(\"rev_and_duration\").innerHTML='revision '+rev+' duration '+duration+suffix;\n"
              ,(make-cdata 
                'here
                'there
                "  document.getElementById(\"timings\").innerHTML=timing_strings.join('<br />');\n")
              "  var barimg = document.getElementById(\"barimg\");\n"
              "  barimg.width=w;\n"
              "  barimg.height=200;\n"
              "  document.getElementById(\"bar\").style.left=x;\n"
              ,@(if link-format-string
                    (list (format
                           "  document.getElementById(\"bara\").href='~a'+'?pane='+(current_pane+1)\n"
                           (format link-format-string "'+rev+'")))
                    '())
              "  return true;\n"
              "}\n"
              "function do_before(){\n"
              "  current_pane = current_pane-1;\n"
              ,(format "  if (current_pane == -1) { current_pane=~a; }\n" last-one)
              " update_pane();\n"
              "}\n"
              "function do_after(){\n"
              "  current_pane = current_pane+1;\n"
              ,(format "  if (current_pane == ~a) { current_pane=0; }\n" (+ 1 last-one))
              " update_pane();\n"
              "}\n"
              "function update_pane(){\n"
              "  var img = document.getElementById(\"img\");\n"
              "  img.useMap='#revmap'+current_pane;\n"
              ,(format "  img.src='~a'+current_pane+'.png';\n" image-url-prefix)
              "  var p = current_pane+1;\n"
              ,(format "  document.getElementById(\"paneid\").innerHTML=('Pane '+p+' of ~a');\n"
                       (+ last-one 1))
              "}\n"
              "// this function from http://www.netlobo.com/url_query_string_javascript.html\n"
              "function gup (name) {\n"
              "  name = name.replace(/[\\[]/,\"\\\\\\[\").replace(/[\\]]/,\"\\\\\\]\");\n"
              ,(make-cdata 'here 'there "  var regexS = \"[\\\\?&]\"+name+\"=([^&#]*)\";\n")
              "  var regex = new RegExp( regexS );\n"
              "  var results = regex.exec( window.location.href );\n"
              "  if( results == null )\n"
              "    return \"\";\n"
              "  else\n"
              "    return results[1];\n"
              "}\n"
              "function startup() {\n"
              "  current_pane = parseInt(gup('pane'));\n"
              "  if (isNaN(current_pane))\n"
              ,(format "     current_pane = ~a;\n" last-one)
              "  else\n"
              ,(format "     current_pane = Math.min(Math.max(current_pane,1),~a)-1\n" (+ last-one 1))
              "  update_pane();"
              "}\n"
              )
      (table
       ((cellpadding "0")
        (cellspacing "0"))
       (tr
        (td (a ((href "#") (onclick "javascript:do_before(); return false;"))
               (img ((border "0")
                     (src ,before-image-file)))))
        (td
         (div ((style "position: relative;"))
              (img ((src ,dot-image-file)
                    (border "0")
                    (id "img")
                    (height ,(format "~a" graph-height))
                    (width ,(format "~a" graphs-width))))
              (div ((id "bar")
                    (style "position: absolute; top: 0px; left: 20px"))
                   (a ((id "bara"))
                      (img ((style "border:none")
                            (id "barimg")
                            (width "0") 
                            (height ,(format "~a" graph-height)) 
                            (src ,dot-image-file)))))))
        (td (a ((href "#") (onclick "javascript:do_after(); return false;")) 
               (img ((border "0")
                     (src ,after-image-file)))))))
      (div (span ((id "paneid")) "")
           (span ((id "rev_and_duration")) ""))
      (tt (span ((id "timings")) ""))
      
      ,@(for/list ((graphs (in-list graphss))
                   (i (in-naturals)))
          `(map ((name ,(format "revmap~a" i)))
                ,@(graphs->areas graphs i)))
      
      (script ((type "text/javascript"))
              "startup()"))))

(define (graphs->areas graphs i)
  (let ([coordinates (graphs->coordinates graphs)])
    (for/list ([c-1 (cons #f coordinates)]
               [c coordinates]
               [c+1 (append (cdr coordinates) (list #f))])
      (let ([left (if c-1
                      (floor (average (coordinate-x-pixel c-1)
                                      (coordinate-x-pixel c)))
                      (coordinate-x-pixel c))]
            [right (if c+1
                       (floor (average (coordinate-x-pixel c) 
                                       (coordinate-x-pixel c+1)))
                       (coordinate-x-pixel c))])
        `(area ((shape "rect")
                (coords ,(format "~a,~a,~a,~a" left 0 right graph-height))
                (onmouseover ,(format "move_bar('~a','~apx',~a,'~a',~a)" 
                                      (coordinate-revision c)
                                      left
                                      (- right left)
                                      (revision->duration (coordinate-revision c))
                                      (revision->timings-array
                                       (coordinate-revision c))))))))))

(define (timing-strings c) (format "~s" c))

(define (average . l) (/ (apply + l) (length l)))


;; note: there is javascript code doing this same computation.
(define (i->image-file i) (format "~a~a.png" image-filename-prefix i))


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

(define (save fgs i)
  (let* ([bm (make-object bitmap% graphs-width graph-height)]
         [bdc (make-object bitmap-dc% bm)])
    (send bdc clear)
    (draw fgs bdc)
    (send bdc set-bitmap #f)
    (send bm save-file (i->image-file i) 'png)
    (void)))

(let ()
  (define the-data (fetch-data input-file))
  (define the-graphss (build-graphss the-data))

  (write-html the-graphss)
  (for ((the-graphs (in-list the-graphss))
        (i (in-naturals)))
    (save the-graphs i)))
