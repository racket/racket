#lang racket/base
(require racket/class
         racket/gui/base
         racket/system
         "graph.rkt")

(provide dot-positioning find-dot
         dot-label neato-label neato-hier-label neato-ipsep-label)
(define dot-label "dot")
(define neato-label "neato")
(define neato-hier-label "neato – hier")
(define neato-ipsep-label "neato – ipsep")

;; the code that finds the dot executable is the same as
;; pkg/contract-profile/dot.rkt; please check the other 
;; place if you find a bug here. the only reason it isn't
;; shared is dependencies and lack of an obvious common
;; place in between to put it.

;; these paths are explicitly checked (when find-executable-path
;; fails) because starting drracket from the finder (or the dock) 
;; under mac os x generally does not get the path right.
(define dot-paths 
  '("/usr/bin"
    "/bin"
    "/usr/local/bin"
    "/opt/local/bin/"))

(define dot.exe (if (eq? (system-type) 'windows) "dot.exe" "dot"))
(define neato.exe (if (eq? (system-type) 'windows) "neato.exe" "neato"))

(define (find-dot [neato? #f])
  (with-handlers ([(lambda (e) ; may not have permission
                     (and (exn:fail? e)
                          (regexp-match "access denied" (exn-message e))))
                   (λ (x) #f)])
    (define dp (find-executable-path dot.exe))
    (define np (find-executable-path neato.exe))
    (cond
      [(and dp np)
       (if neato? np dp)]
      [else
       (ormap (λ (x) (and (file-exists? (build-path x dot.exe))
                          (file-exists? (build-path x neato.exe))
                          (build-path x (if neato? neato.exe dot.exe))))
              dot-paths)])))

(define (dot-positioning pb [option dot-label] [overlap? #f])
  (define dot-path (find-dot (regexp-match #rx"neato" option)))
  (when dot-path
    (define info (snip-info pb))
    (define-values (positions max-y) (run-dot dot-path info option overlap?))
    (send pb begin-edit-sequence)
    (for ([position-line (in-list positions)])
      (define id (list-ref position-line 0))
      (define x (list-ref position-line 1))
      (define y (list-ref position-line 2))
      (define snip (car (hash-ref info id)))
      (send pb move-to snip x (- max-y y)))
    (send pb invalidate-bitmap-cache)
    (send pb end-edit-sequence)))

;; with-snips : pasteboard% 
;;           -> hash-table[snip -> (list i number[width] number[height] (listof number))]
(define (snip-info pb)
  (let ([num-ht (make-hasheq)]
        [children-ht (make-hasheq)])
    (let loop ([snip (send pb find-first-snip)]
               [i 0])
      (when snip
        (when (is-a? snip graph-snip<%>)
          (hash-set! num-ht snip i))
        (loop (send snip next) (+ i 1))))
    (let ([lb (box 0)]
          [tb (box 0)]
          [rb (box 0)]
          [bb (box 0)])
      (hash-for-each
       num-ht
       (λ (snip num)
         (send pb get-snip-location snip lb tb #f)
         (send pb get-snip-location snip rb bb #t)
         (hash-set! children-ht
                    (hash-ref num-ht snip)
                    (list 
                     snip
                     (- (unbox rb) (unbox lb))
                     (- (unbox bb) (unbox tb))
                     (map (λ (c) (hash-ref num-ht c))
                          (send snip get-children))))))
      children-ht)))

;; run-dot : hash-table[snip -> (list i (listof number))] string -> void
(define (run-dot dot-path ht option overlap?)
  (define info (sort (hash-map ht (λ (k v) (cons k v)))
                     (λ (x y) (< (car x) (car y)))))
  (let-values ([(in1 out1) (make-pipe)]
               [(in2 out2) (make-pipe)])
    (thread
     (λ ()
       (parameterize ([current-output-port out1])
         (graph->dot info option overlap?))
       (close-output-port out1)))
    (thread
     (λ ()
       (parameterize ([current-input-port in1]
                      [current-output-port out2])
         (system (format "~a -Tplain" (path->string dot-path))))
       (close-output-port out2)
       (close-input-port in1)))
    (parse-plain in2)))

;; graph->dot (listof (list snip number (listof number)) -> void
;; prints out the dot input file based on `g'
(define (graph->dot g option overlap?)
  (printf "digraph {\n")
  (cond
    [(equal? option dot-label)
     (printf "  rankdir=\"~a\"\n" (if overlap? "LR" "TB"))]
    [(equal? option neato-label)
     (printf "  overlap=\"~a\"\n" (if overlap? "true" "false"))
     (printf "  splines=\"true\"\n")]
    [(equal? option neato-hier-label)
     (printf "  overlap=\"~a\"\n" (if overlap? "true" "false"))
     (printf "  mode=\"hier\"\n")
     (printf "  splines=\"true\"\n")]
    [(equal? option neato-ipsep-label)
     (printf "  mode=\"ipsep\"\n")
     (printf "  splines=\"true\"\n")
     (printf "  overlap=\"~a\"\n" (if overlap? "true" "ipsep"))])
  (for-each
   (λ (line)
     (let* ([snip (list-ref line 1)]
            [id (list-ref line 0)]
            [w (list-ref line 2)]
            [h (list-ref line 3)]
            [children (list-ref line 4)])
       (printf "  ~a [width=~a height=~a shape=box label=\"\"]\n" 
               (num->id id) 
               (format-number (pixels->inches w))
               (format-number (pixels->inches h)))
       (for-each
        (λ (child)
          (printf "  ~a -> ~a\n" (num->id id) (num->id child)))
        children)))
   g)
  (printf "}\n"))

(define (num->id n) (format "s~a" n))
(define (id->num s) (string->number (substring s 1 (string-length s))))

(define (format-number n)
  (let ([candidate (number->string (exact->inexact n))])
    (cond
      [(regexp-match #rx"^([0-9]*)\\.([0-9]*)$" candidate)
       =>
       (λ (m)
         (let ([prefix (list-ref m 1)]
               [suffix (list-ref m 2)])
           (if (< (string-length suffix) 5)
               candidate
               (string-append prefix "." (substring suffix 0 4)))))]
      [else
       candidate])))
  
(define (parse-plain port)
  
  (define max-y 0)
  
  (define positions '())
  
  (define (main)
    (let ([graph-line (read-line port)])
      (let loop ()
        (let ([line (read-line port)])
          (cond
            [(regexp-match #rx"^node" line)
             (join (parse-node line)
                   (loop))]
            [(regexp-match #rx"^edge" line)
             (join (parse-edge line)
                   (loop))]
            [(regexp-match #rx"stop" line)
             void]
            [else
             (error 'parse-file "didn't recognize line:\n  ~s" line)])))))
  
  (define (join p1 p2)
    (cond
      [(eq? p1 void) p2]
      [(eq? p2 void) p1]
      [else 
       (λ (dc left top right bottom dx dy) 
         (p1 dc left top right bottom dx dy)
         (p2 dc left top right bottom dx dy))]))

  (define (parse-node line) 
    (let*-values ([(node line) (chomp line)]
                  [(id line) (chomp line)]
                  [(raw-x line) (chomp line)]
                  [(raw-y line) (chomp line)]
                  [(raw-w line) (chomp line)]
                  [(raw-h line) (chomp line)]
                  [(label line) (chomp line)]
                  [(style line) (chomp line)]
                  [(raw-shape line) (chomp line)]
                  [(color line) (chomp line)]
                  [(fillcolor line) (chomp line)])
      
      (define x (inches->pixels (string->number raw-x)))
      (define y (inches->pixels (string->number raw-y)))
      (define w (inches->pixels (string->number raw-w)))
      (define h (inches->pixels (string->number raw-h)))
      (define shape (string->symbol raw-shape))
      
      (set! positions (cons (list (id->num id) 
                                  (- x (/ w 2)) 
                                  (+ y (/ h 2)))
                            positions))
      
      (set! max-y (max (+ y h) max-y))
      void))
        
  (define (parse-edge line)
    (define (give-up)
      (error 'redex "could not parse edge line:\n  ~s\n" line))
    (let* ([m (regexp-match #rx"edge ([^ ]+) ([^ ]+) ([0-9]+) (.*)$" line)]
           [_ (unless m (give-up))]
           [from (list-ref m 1)]
           [to (list-ref m 2)]
           [point-count (string->number (list-ref m 3))]
           [rest (list-ref m 4)]
           [points 
            (let loop ([pts point-count]
                       [rest rest])
              (if (zero? pts) 
                  '()
                  (let* ([m (regexp-match #rx"^([-0-9.]+) ([-0-9.]+) (.*)$" rest)]
                         [_ (unless m (give-up))]
                         [x (string->number (list-ref m 1))]
                         [y (string->number (list-ref m 2))])
                    (set! max-y (max y max-y))
                    (cons (list x y)
                          (loop (- pts 1)
                                (list-ref m 3))))))])
      (λ (dc left top right bottom dx dy)
        (draw-edges dc dx dy points))))
  
  ;; chomp : string -> (values string (union #f string))
  ;; returns the first word at the beginning of the string
  ;; and the remainder of the string (or #f is there is no more)
  (define (chomp s)
    (let ([s (regexp-replace #rx"^ *" s "")])
      (cond
        [(equal? s "")
         (values "" #f)]
        [else
         (case (string-ref s 0)
           [(#\") (let ([m (regexp-match #rx"^\"([^\"]*)\"(.*)$" s)])
                    (values (list-ref m 1)
                            (list-ref m 2)))]
           [else
            (cond
              [(regexp-match #rx"^([^ ]*) (.*)$" s)
               =>
               (λ (m)
                 (values (list-ref m 1)
                         (list-ref m 2)))]
              [(regexp-match #rx"^([^ ]*)$" s)
               =>
               (λ (m)
                 (values (list-ref m 1)
                         #f))]
              [else
               (error 'chomp "~s" s)])])])))
            
  (define (draw-edges dc dx dy raw-points)
    (let ([points (map (λ (x) (list (inches->pixels (car x))
                                    (inches->pixels (list-ref x 1))))
                       raw-points)])
      (send dc set-pen "blue" 1 'solid)
      (send dc set-brush "black" 'transparent)
      (let ([path (new dc-path%)])
        (send path move-to 
              (car (car points))
              (- max-y (cadr (car points))))
        (let loop ([points (cdr points)])
          (cond
            [(null? points) (void)]
            [else (let ([p1 (list-ref points 0)]
                        [p2 (list-ref points 1)]
                        [p3 (list-ref points 2)])
                    (send path curve-to 
                          (list-ref p1 0) (- max-y (list-ref p1 1))
                          (list-ref p2 0) (- max-y (list-ref p2 1))
                          (list-ref p3 0) (- max-y (list-ref p3 1)))
                    (loop (cdddr points)))]))
        (send dc draw-path path dx dy))))
  (main)
  (values positions
          max-y))

(define (pixels->inches x) (/ x 72))
(define (inches->pixels x) (* x 72))
