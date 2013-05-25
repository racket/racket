;;; MAZE -- Constructs a maze on a hexagonal grid, written by Olin Shivers.

;------------------------------------------------------------------------------
; Was file "rand.scm".

; Minimal Standard Random Number Generator
; Park & Miller, CACM 31(10), Oct 1988, 32 bit integer version.
; better constants, as proposed by Park.
; By Ozan Yigit

;;; Rehacked by Olin 4/1995.

(define (random-state n)
  (cons n #f))

(define (rand state)
  (let ((seed (car state))
        (A 2813) ; 48271
        (M 8388607) ; 2147483647
        (Q 2787) ; 44488
        (R 2699)) ; 3399
    (let* ((hi (quotient seed Q))
           (lo (modulo seed Q))
           (test (- (* A lo) (* R hi)))
           (val (if (> test 0) test (+ test M))))
      (set-car! state val)
      val)))

(define (random-int n state)
  (modulo (rand state) n))

; poker test
; seed 1
; cards 0-9 inclusive (random 10)
; five cards per hand
; 10000 hands
;
; Poker Hand     Example    Probability  Calculated
; 5 of a kind    (aaaaa)      0.0001      0
; 4 of a kind    (aaaab)      0.0045      0.0053
; Full house     (aaabb)      0.009       0.0093
; 3 of a kind    (aaabc)      0.072       0.0682
; two pairs      (aabbc)      0.108       0.1104
; Pair           (aabcd)      0.504       0.501
; Bust           (abcde)      0.3024      0.3058

; (define (random n)
;   (let* ((M 2147483647)
;        (slop (modulo M n)))
;     (let loop ((r (rand)))
;       (if (> r slop)
;         (modulo r n)  
;         (loop (rand))))))
; 
; (define (rngtest)
;   (display "implementation ")
;   (srand 1)
;   (let loop ((n 0))
;     (if (< n 10000)
;         (begin
;          (rand)
;          (loop (1+ n)))))
;   (if (= *seed* 399268537)
;       (display "looks correct.")
;       (begin
;        (display "failed.")
;        (newline)
;        (display "   current seed ") (display *seed*)
;        (newline)
;        (display "   correct seed 399268537")))
;   (newline))

;------------------------------------------------------------------------------
; Was file "uf.scm".

;;; Tarjan's amortised union-find data structure.
;;; Copyright (c) 1995 by Olin Shivers.

;;; This data structure implements disjoint sets of elements.
;;; Four operations are supported. The implementation is extremely
;;; fast -- any sequence of N operations can be performed in time
;;; so close to linear it's laughable how close it is. See your
;;; intro data structures book for more. The operations are:
;;;
;;; - (base-set nelts) -> set
;;;   Returns a new set, of size NELTS.
;;;
;;; - (set-size s) -> integer
;;;   Returns the number of elements in set S.
;;;
;;; - (union! set1 set2)
;;;   Unions the two sets -- SET1 and SET2 are now considered the same set
;;;   by SET-EQUAL?.
;;;
;;; - (set-equal? set1 set2)
;;;   Returns true <==> the two sets are the same.

;;; Representation: a set is a cons cell. Every set has a "representative"
;;; cons cell, reached by chasing cdr links until we find the cons with
;;; cdr = (). Set equality is determined by comparing representatives using
;;; EQ?. A representative's car contains the number of elements in the set.

;;; The speed of the algorithm comes because when we chase links to find 
;;; representatives, we collapse links by changing all the cells in the path
;;; we followed to point directly to the representative, so that next time
;;; we walk the cdr-chain, we'll go directly to the representative in one hop.


(define (base-set nelts) (cons nelts '()))

;;; Sets are chained together through cdr links. Last guy in the chain
;;; is the root of the set.

(define (get-set-root s)
  (let lp ((r s))                       ; Find the last pair
    (let ((next (cdr r)))               ; in the list. That's
      (cond ((pair? next) (lp next))    ; the root r.

            (else
             (if (not (eq? r s))        ; Now zip down the list again,
                 (let lp ((x s))        ; changing everyone's cdr to r.
                   (let ((next (cdr x)))        
                     (cond ((not (eq? r next))
                            (set-cdr! x r)
                            (lp next))))))
             r)))))                     ; Then return r.

(define (set-equal? s1 s2) (eq? (get-set-root s1) (get-set-root s2)))

(define (set-size s) (car (get-set-root s)))

(define (union! s1 s2)
  (let* ((r1 (get-set-root s1))
         (r2 (get-set-root s2))
         (n1 (set-size r1))
         (n2 (set-size r2))
         (n  (+ n1 n2)))

    (cond ((> n1 n2)
           (set-cdr! r2 r1)
           (set-car! r1 n))
          (else
           (set-cdr! r1 r2)
           (set-car! r2 n)))))

;------------------------------------------------------------------------------
; Was file "maze.scm".

;;; Building mazes with union/find disjoint sets.
;;; Copyright (c) 1995 by Olin Shivers.

;;; This is the algorithmic core of the maze constructor.
;;; External dependencies:
;;; - RANDOM-INT
;;; - Union/find code
;;; - bitwise logical functions

; (define-record wall
;   owner         ; Cell that owns this wall.
;   neighbor      ; The other cell bordering this wall.
;   bit)          ; Integer -- a bit identifying this wall in OWNER's cell.

; (define-record cell
;   reachable     ; Union/find set -- all reachable cells.
;   id            ; Identifying info (e.g., the coords of the cell).
;   (walls -1)    ; A bitset telling which walls are still standing.
;   (parent #f)   ; For DFS spanning tree construction.
;   (mark #f))    ; For marking the solution path.

(define (make-wall owner neighbor bit)
  (vector 'wall owner neighbor bit))

(define (wall:owner o)          (vector-ref o 1))
(define (set-wall:owner o v)    (vector-set! o 1 v))
(define (wall:neighbor o)       (vector-ref o 2))
(define (set-wall:neighbor o v) (vector-set! o 2 v))
(define (wall:bit o)            (vector-ref o 3))
(define (set-wall:bit o v)      (vector-set! o 3 v))

(define (make-cell reachable id)
  (vector 'cell reachable id -1 #f #f))

(define (cell:reachable o)       (vector-ref o 1))
(define (set-cell:reachable o v) (vector-set! o 1 v))
(define (cell:id o)              (vector-ref o 2))
(define (set-cell:id o v)        (vector-set! o 2 v))
(define (cell:walls o)           (vector-ref o 3))
(define (set-cell:walls o v)     (vector-set! o 3 v))
(define (cell:parent o)          (vector-ref o 4))
(define (set-cell:parent o v)    (vector-set! o 4 v))
(define (cell:mark o)            (vector-ref o 5))
(define (set-cell:mark o v)      (vector-set! o 5 v))

;;; Iterates in reverse order.

(define (vec-for-each proc v)
  (let lp ((i (- (vector-length v) 1)))
    (cond ((>= i 0)
           (proc (vector-ref v i))
           (lp (- i 1))))))


;;; Randomly permute a vector.

(define (permute-vec! v random-state)
  (let lp ((i (- (vector-length v) 1)))
    (cond ((> i 1)
           (let ((elt-i (vector-ref v i))
                 (j (random-int i random-state)))       ; j in [0,i)
             (vector-set! v i (vector-ref v j))
             (vector-set! v j elt-i))
           (lp (- i 1)))))
  v)


;;; This is the core of the algorithm.

(define (dig-maze walls ncells)
  (call-with-current-continuation
    (lambda (quit)
      (vec-for-each
       (lambda (wall)                   ; For each wall,
         (let* ((c1   (wall:owner wall)) ; find the cells on
                (set1 (cell:reachable c1))

                (c2   (wall:neighbor wall)) ; each side of the wall
                (set2 (cell:reachable c2)))

           ;; If there is no path from c1 to c2, knock down the
           ;; wall and union the two sets of reachable cells.
           ;; If the new set of reachable cells is the whole set
           ;; of cells, quit.
           (if (not (set-equal? set1 set2))
               (let ((walls (cell:walls c1))    
                     (wall-mask (bitwise-not (wall:bit wall))))
                 (union! set1 set2)
                 (set-cell:walls c1 (bitwise-and walls wall-mask))
                 (if (= (set-size set1) ncells) (quit #f))))))
       walls))))


;;; Some simple DFS routines useful for determining path length 
;;; through the maze.

;;; Build a DFS tree from ROOT. 
;;; (DO-CHILDREN proc maze node) applies PROC to each of NODE's children.
;;; We assume there are no loops in the maze; if this is incorrect, the
;;; algorithm will diverge.

(define (dfs-maze maze root do-children)
  (let search ((node root) (parent #f))
    (set-cell:parent node parent)
    (do-children (lambda (child)
                   (if (not (eq? child parent))
                       (search child node)))
                 maze node)))

;;; Move the root to NEW-ROOT.

(define (reroot-maze new-root)
  (let lp ((node new-root) (new-parent #f))
    (let ((old-parent (cell:parent node)))
      (set-cell:parent node new-parent)
      (if old-parent (lp old-parent node)))))

;;; How far from CELL to the root?

(define (path-length cell)
  (do ((len 0 (+ len 1))
       (node (cell:parent cell) (cell:parent node)))
      ((not node) len)))

;;; Mark the nodes from NODE back to root. Used to mark the winning path.

(define (mark-path node)
  (let lp ((node node))
    (set-cell:mark node #t)
    (cond ((cell:parent node) => lp))))

;------------------------------------------------------------------------------
; Was file "harr.scm".

;;; Hex arrays
;;; Copyright (c) 1995 by Olin Shivers.

;;; External dependencies:
;;; - define-record

;;;        ___       ___       ___
;;;       /   \     /   \     /   \
;;;   ___/  A  \___/  A  \___/  A  \___
;;;  /   \     /   \     /   \     /   \
;;; /  A  \___/  A  \___/  A  \___/  A  \
;;; \     /   \     /   \     /   \     /
;;;  \___/     \___/     \___/     \___/
;;;  /   \     /   \     /   \     /   \
;;; /     \___/     \___/     \___/     \
;;; \     /   \     /   \     /   \     /
;;;  \___/     \___/     \___/     \___/
;;;  /   \     /   \     /   \     /   \
;;; /     \___/     \___/     \___/     \
;;; \     /   \     /   \     /   \     /
;;;  \___/     \___/     \___/     \___/

;;; Hex arrays are indexed by the (x,y) coord of the center of the hexagonal
;;; element. Hexes are three wide and two high; e.g., to get from the center
;;; of an elt to its {NW, N, NE} neighbors, add {(-3,1), (0,2), (3,1)}
;;; respectively.
;;;
;;; Hex arrays are represented with a matrix, essentially made by shoving the
;;; odd columns down a half-cell so things line up. The mapping is as follows:
;;;     Center coord      row/column
;;;     ------------      ----------
;;;     (x,  y)        -> (y/2, x/3)
;;;     (3c, 2r + c&1) <- (r,   c)


; (define-record harr
;   nrows
;   ncols
;   elts)

(define (make-harr nrows ncols elts)
  (vector 'harr nrows ncols elts))

(define (harr:nrows o)       (vector-ref o 1))
(define (set-harr:nrows o v) (vector-set! o 1 v))
(define (harr:ncols o)       (vector-ref o 2))
(define (set-harr:ncols o v) (vector-set! o 2 v))
(define (harr:elts o)        (vector-ref o 3))
(define (set-harr:elts o v)  (vector-set! o 3 v))

(define (harr r c)
  (make-harr r c (make-vector (* r c))))



(define (href ha x y)
  (let ((r (quotient y 2))
        (c (quotient x 3)))
    (vector-ref (harr:elts ha)
                (+ (* (harr:ncols ha) r) c))))

(define (hset! ha x y val)
  (let ((r (quotient y 2))
        (c (quotient x 3)))
    (vector-set! (harr:elts ha)
                 (+ (* (harr:ncols ha) r) c)
                 val)))

(define (href/rc ha r c)
    (vector-ref (harr:elts ha)
                (+ (* (harr:ncols ha) r) c)))

;;; Create a nrows x ncols hex array. The elt centered on coord (x, y)
;;; is the value returned by (PROC x y).

(define (harr-tabulate nrows ncols proc)
  (let ((v (make-vector (* nrows ncols))))

    (do ((r (- nrows 1) (- r 1)))
        ((< r 0))
      (do ((c 0 (+ c 1))
           (i (* r ncols) (+ i 1)))
          ((= c ncols))
        (vector-set! v i (proc (* 3 c) (+ (* 2 r) (bitwise-and c 1))))))

    (make-harr nrows ncols v)))


(define (harr-for-each proc harr)
  (vec-for-each proc (harr:elts harr)))

;------------------------------------------------------------------------------
; Was file "hex.scm".

;;; Hexagonal hackery for maze generation.
;;; Copyright (c) 1995 by Olin Shivers.

;;; External dependencies:
;;; - cell and wall records
;;; - Functional Postscript for HEXES->PATH
;;; - logical functions for bit hacking
;;; - hex array code.

;;; To have the maze span (0,0) to (1,1):
;;; (scale (/ (+ 1 (* 3 ncols))) (/ (+ 1 (* 2 nrows)))
;;;        (translate (point 2 1) maze))

;;; Every elt of the hex array manages his SW, S, and SE wall.
;;; Terminology: - An even column is one whose column index is even. That
;;;                means the first, third, ... columns (indices 0, 2, ...).
;;;              - An odd column is one whose column index is odd. That
;;;                means the second, fourth... columns (indices 1, 3, ...).
;;;              The even/odd flip-flop is confusing; be careful to keep it
;;;              straight. The *even* columns are the low ones. The *odd*
;;;              columns are the high ones.
;;;    _   _
;;;  _/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/
;;;  0 1 2 3

(define south-west 1)
(define south      2)
(define south-east 4)

(define (gen-maze-array r c)
  (harr-tabulate r c (lambda (x y) (make-cell (base-set 1) (cons x y)))))

;;; This could be made more efficient.
(define (make-wall-vec harr)
  (let* ((nrows (harr:nrows harr))
         (ncols (harr:ncols harr))
         (xmax (* 3 (- ncols 1)))

         ;; Accumulate walls.
         (walls '())
         (add-wall (lambda (o n b) ; owner neighbor bit
                     (set! walls (cons (make-wall o n b) walls)))))
        
    ;; Do everything but the bottom row.
    (do ((x (* (- ncols 1) 3) (- x 3)))
        ((< x 0))
      (do ((y (+ (* (- nrows 1) 2) (bitwise-and x 1))
              (- y 2)))
          ((<= y 1))    ; Don't do bottom row.
          (let ((hex (href harr x y)))
            (if (not (zero? x))
                (add-wall hex (href harr (- x 3) (- y 1)) south-west))
            (add-wall hex (href harr x (- y 2)) south)
            (if (< x xmax)
                (add-wall hex (href harr (+ x 3) (- y 1)) south-east)))))

    ;; Do the SE and SW walls of the odd columns on the bottom row.
    ;; If the rightmost bottom hex lies in an odd column, however,
    ;; don't add it's SE wall -- it's a corner hex, and has no SE neighbor.
    (if (> ncols 1)
        (let ((rmoc-x (+ 3 (* 6 (quotient (- ncols 2) 2)))))
          ;; Do rightmost odd col.
          (let ((rmoc-hex (href harr rmoc-x 1)))
            (if (< rmoc-x xmax) ; Not  a corner -- do E wall.
                (add-wall rmoc-hex (href harr xmax 0) south-east))
            (add-wall rmoc-hex (href harr (- rmoc-x 3) 0) south-west))

          (do ((x (- rmoc-x 6) ; Do the rest of the bottom row's odd cols.
                  (- x 6)))
              ((< x 3)) ; 3 is X coord of leftmost odd column.
            (add-wall (href harr x 1) (href harr (- x 3) 0) south-west)
            (add-wall (href harr x 1) (href harr (+ x 3) 0) south-east))))

    (list->vector walls)))


;;; Find the cell ctop from the top row, and the cell cbot from the bottom
;;; row such that cbot is furthest from ctop. 
;;; Return [ctop-x, ctop-y, cbot-x, cbot-y].

(define (pick-entrances harr)
  (dfs-maze harr (href/rc harr 0 0) for-each-hex-child)
  (let ((nrows (harr:nrows harr))
        (ncols (harr:ncols harr)))
    (let tp-lp ((max-len -1)
                (entrance #f)
                (exit #f)
                (tcol (- ncols 1)))
      (if (< tcol 0) (vector entrance exit)
          (let ((top-cell (href/rc harr (- nrows 1) tcol)))
            (reroot-maze top-cell)
            (let ((result
                    (let bt-lp ((max-len max-len)
                                (entrance entrance)
                                (exit exit)
                                (bcol (- ncols 1)))
;                     (format #t "~a ~a ~a ~a~%" max-len entrance exit bcol)
                      (if (< bcol 0) (vector max-len entrance exit)
                          (let ((this-len (path-length (href/rc harr 0 bcol))))
                            (if (> this-len max-len)
                                (bt-lp this-len tcol bcol (- bcol 1))
                                (bt-lp max-len  entrance exit (- bcol 1))))))))
              (let ((max-len (vector-ref result 0))
                    (entrance (vector-ref result 1))
                    (exit (vector-ref result 2)))
                (tp-lp max-len entrance exit (- tcol 1)))))))))
                


;;; Apply PROC to each node reachable from CELL.
(define (for-each-hex-child proc harr cell)
  (let* ((walls (cell:walls cell))
         (id (cell:id cell))
         (x (car id))
         (y (cdr id))
         (nr (harr:nrows harr))
         (nc (harr:ncols harr))
         (maxy (* 2 (- nr 1)))
         (maxx (* 3 (- nc 1))))
    (if (not (bit-test walls south-west)) (proc (href harr (- x 3) (- y 1))))
    (if (not (bit-test walls south))      (proc (href harr x       (- y 2))))
    (if (not (bit-test walls south-east)) (proc (href harr (+ x 3) (- y 1))))

    ;; NW neighbor, if there is one (we may be in col 1, or top row/odd col)
    (if (and (> x 0)    ; Not in first column.
             (or (<= y maxy)            ; Not on top row or
                 (zero? (modulo x 6)))) ; not in an odd column.
        (let ((nw (href harr (- x 3) (+ y 1))))
          (if (not (bit-test (cell:walls nw) south-east)) (proc nw))))

    ;; N neighbor, if there is one (we may be on top row).
    (if (< y maxy)              ; Not on top row
        (let ((n (href harr x (+ y 2))))
          (if (not (bit-test (cell:walls n) south)) (proc n))))

    ;; NE neighbor, if there is one (we may be in last col, or top row/odd col)
    (if (and (< x maxx) ; Not in last column.
             (or (<= y maxy)            ; Not on top row or
                 (zero? (modulo x 6)))) ; not in an odd column.
        (let ((ne (href harr (+ x 3) (+ y 1))))
          (if (not (bit-test (cell:walls ne) south-west)) (proc ne))))))



;;; The top-level
(define (make-maze nrows ncols)
  (let* ((cells (gen-maze-array nrows ncols))
         (walls (permute-vec! (make-wall-vec cells) (random-state 20))))
    (dig-maze walls (* nrows ncols))
    (let ((result (pick-entrances cells)))
      (let ((entrance (vector-ref result 0))
            (exit (vector-ref result 1)))
        (let* ((exit-cell (href/rc cells 0 exit))
               (walls (cell:walls exit-cell)))
          (reroot-maze (href/rc cells (- nrows 1) entrance))
          (mark-path exit-cell)
          (set-cell:walls exit-cell (bitwise-and walls (bitwise-not south)))
          (vector cells entrance exit))))))


(define (pmaze nrows ncols)
  (let ((result (make-maze nrows ncols)))
    (let ((cells (vector-ref result 0))
          (entrance (vector-ref result 1))
          (exit (vector-ref result 2)))
      (print-hexmaze cells entrance))))

;------------------------------------------------------------------------------
; Was file "hexprint.scm".

;;; Print out a hex array with characters.
;;; Copyright (c) 1995 by Olin Shivers.

;;; External dependencies:
;;; - hex array code
;;; - hex cell code

;;;    _   _
;;;  _/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ 

;;; Top part of top row looks like this:
;;;    _   _  _   _
;;;  _/ \_/ \/ \_/ \
;;; /        

(define output #f) ; the list of all characters written out, in reverse order.

(define (write-ch c)
  (set! output (cons c output)))

(define (print-hexmaze harr entrance)
  (let* ((nrows  (harr:nrows harr))
         (ncols  (harr:ncols harr))
         (ncols2 (* 2 (quotient ncols 2))))

    ;; Print out the flat tops for the top row's odd cols.
    (do ((c 1 (+ c 2)))
        ((>= c ncols))
;     (display "   ")
      (write-ch #\space)
      (write-ch #\space)
      (write-ch #\space)
      (write-ch (if (= c entrance) #\space #\_)))
;   (newline)
    (write-ch #\newline)

    ;; Print out the slanted tops for the top row's odd cols
    ;; and the flat tops for the top row's even cols.
    (write-ch #\space)
    (do ((c 0 (+ c 2)))
        ((>= c ncols2))
;     (format #t "~a/~a\\"
;             (if (= c entrance) #\space #\_)
;             (dot/space harr (- nrows 1) (+ c 1)))
      (write-ch (if (= c entrance) #\space #\_))
      (write-ch #\/)
      (write-ch (dot/space harr (- nrows 1) (+ c 1)))
      (write-ch #\\))
    (if (odd? ncols)
        (write-ch (if (= entrance (- ncols 1)) #\space #\_)))
;   (newline)
    (write-ch #\newline)

    (do ((r (- nrows 1) (- r 1)))
        ((< r 0))

      ;; Do the bottoms for row r's odd cols.
      (write-ch #\/)
      (do ((c 1 (+ c 2)))
          ((>= c ncols2))
        ;; The dot/space for the even col just behind c.
        (write-ch (dot/space harr r (- c 1)))
        (display-hexbottom (cell:walls (href/rc harr r c))))    

      (cond ((odd? ncols)
             (write-ch (dot/space harr r (- ncols 1)))
             (write-ch #\\)))
;     (newline)
      (write-ch #\newline)

      ;; Do the bottoms for row r's even cols.
      (do ((c 0 (+ c 2)))
          ((>= c ncols2))
        (display-hexbottom (cell:walls (href/rc harr r c)))
        ;; The dot/space is for the odd col just after c, on row below.
        (write-ch (dot/space harr (- r 1) (+ c 1))))
      
      (cond ((odd? ncols)
             (display-hexbottom (cell:walls (href/rc harr r (- ncols 1)))))
            ((not (zero? r)) (write-ch #\\)))
;     (newline)
      (write-ch #\newline))))

(define (bit-test j bit)
  (not (zero? (bitwise-and j bit))))

;;; Return a . if harr[r,c] is marked, otherwise a space.
;;; We use the dot to mark the solution path.
(define (dot/space harr r c)
  (if (and (>= r 0) (cell:mark (href/rc harr r c))) #\. #\space))

;;; Print a \_/ hex bottom.
(define (display-hexbottom hexwalls)
  (write-ch (if (bit-test hexwalls south-west) #\\ #\space))
  (write-ch (if (bit-test hexwalls south     ) #\_ #\space))
  (write-ch (if (bit-test hexwalls south-east) #\/ #\space)))

;;;    _   _
;;;  _/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \_/
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \
;;; / \_/ \_/
;;; \_/ \_/ \_/

;------------------------------------------------------------------------------

(let ((input (with-input-from-file "input.txt" read)))
  (time (let loop ((n 10000) (v 0))
          (if (zero? n)
              (length v)
              (begin
                (set! output '())
                (pmaze 20 (if input 7 0))
                (loop (- n 1) output))))))
