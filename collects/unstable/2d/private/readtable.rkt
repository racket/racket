#lang racket/base
#|

ideas:
- 2dcond
- 2dmatch
- literal tables in scribble layout?
- something for graphics?

example uses:
- unifier
- subtyping relation

|#

(require racket/port
         syntax/readerr
         racket/match
         racket/set
         ;syntax/rect
         framework/private/dir-chars
         (for-syntax racket/base
                     racket/list))


(provide make-2d-readtable
         
         ;; provided for the test suite
         chars->desc
         smallest-representative
         parse-2dcond
         all-line-of-interest
         current-lines
         close-cell-graph
         compare/xy
         fill-scratch-string)

(define all-line-of-interest (make-hash))
(define current-lines (make-parameter #f))
(define-syntax (line-of-interest stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-local-lift-expression #'(hash-set! all-line-of-interest line #t))
    #'(visited line)))
(define (visited line) 
  (define t (current-lines))
  (when t
    (hash-remove! t line)))

(define (make-2d-readtable)
  (define previous-readtable (current-readtable))
  (make-readtable
   #f
   #\2
   'dispatch-macro
   (case-lambda
     [(char port)
      (define-values (line col pos) (port-next-location port))
      (dispatch-proc char port #f line col pos read/recursive previous-readtable)]
     [(char port source _line _col _pos)
      (dispatch-proc char port source _line _col _pos 
                     (λ (a b c) (read-syntax/recursive source a b c))
                     previous-readtable)])))

(define (dispatch-proc char port source _line _col _pos /recursive previous-readtable)
  (define next-char (peek-char port))
  (cond
    [(equal? next-char #\d)
     (define chars-read 2) ;; account for the # and the 2
     (define (rc) 
       (set! chars-read (+ chars-read 1))
       (read-char port))
     (rc) ;; get the #\d
     (define kwd-chars
       (let loop ()
         (define c (rc))
         (cond
           [(eof-object? c)
            (line-of-interest)
            (raise (make-exn:fail:read:eof 
                    "expected a newline to follow #2d"
                    (current-continuation-marks)
                    (list (srcloc source 
                                  _line _col _pos
                                  (+ _pos chars-read)))))]
           [(equal? c #\newline) '()]
           [(equal? c #\return) 
            (when (equal? #\newline (peek-char port))
              (rc))
            '()]
           [else (cons c (loop))])))
     (define-values (post-2d-line post-2d-col post-2d-span) (port-next-location port))
     (define-values (edges lines table-column-breaks initial-space-count)
       (parse-2dcond port source _line _col _pos chars-read))
     (define lhses (close-cell-graph edges (length table-column-breaks) (vector-length lines)))
     (define scratch-string (make-string (for/sum ([ss (in-vector lines)])
                                           (for/sum ([s (in-list ss)])
                                             (string-length s)))
                                         #\space))
     
     (define heights
       (for/list ([line (in-vector lines)])
         (length line)))
     
     `(,(string->symbol (string-append "2d" (apply string kwd-chars)))
       
       ,table-column-breaks
       ,heights
       
       ,@(for/list ([set-of-indicies (in-list (sort (set->list lhses) compare/xy 
                                                    #:key smallest-representative))])
           (fill-scratch-string set-of-indicies 
                                lines 
                                scratch-string 
                                table-column-breaks 
                                initial-space-count)
           (define scratch-port (open-input-string scratch-string))
           (when post-2d-line (port-count-lines! scratch-port))
           (set-port-next-location! scratch-port post-2d-line post-2d-col post-2d-span)
           `[,(sort (set->list set-of-indicies) compare/xy)
             ,@(read-subparts source scratch-port 
                              initial-space-count table-column-breaks heights set-of-indicies
                              previous-readtable /recursive)]))]
    [else
     (/recursive 
      (input-port-append #f (open-input-string "#2") port)
      #f
      previous-readtable)]))

(define (read-subparts source scratch-port 
                       initial-space-count table-column-breaks heights lhs
                       previous-readtable /recursive)
  (with-handlers (#;
                  [exn:fail:read?
                   (λ (exn)
                     (define constructor
                       (cond
                         [(exn:fail:read:eof? exn) exn:fail:read:eof/rects]
                         [(exn:fail:read:non-char? exn) exn:fail:read:non-char/rects]
                         [else exn:fail:read/rects]))
                     (raise 
                      (constructor (exn-message exn)
                                   (exn-continuation-marks exn)
                                   (exn:fail:read-srclocs exn)
                                   (build-rectangles
                                    source
                                    initial-space-count table-column-breaks heights lhs))))])
    (let loop ()
      (define o (/recursive scratch-port #f previous-readtable))
      (cond
        [(eof-object? o) '()]
        [else (cons o (loop))]))))

#;
(define (build-rectangles source table-column-breaks heights set-of-indicies)
  (for/list ([pr (in-set set-of-indicies)])
    (define x (list-ref pr 0))
    (define y (list-ref pr 1))
    (srcloc-rect source 
                 ?-start-position
                 (list-ref table-column-breaks x)
                 (list-ref heights y))))

(define (fill-scratch-string set-of-indicies 
                             lines
                             scratch-string
                             table-column-breaks
                             initial-space-count)
  (define scratch-pos 0)
  (define-syntax-rule 
    (set-scratch! c) 
    (let ([x c])
      ;(unless (char-whitespace? x) (printf "putting ~s @ ~s\n" x scratch-pos))
      (string-set! scratch-string scratch-pos x)))
  (define-syntax-rule
    (inc-scratch-pos! e)
    (set! scratch-pos (+ scratch-pos e)))
  (for ([lines (in-vector lines)]
        [y (in-naturals)])
    (for ([line (in-list lines)]
          [l-num (in-naturals)])
      (define first-line? (zero? l-num))
      ;; skip over initial spaces: we know that the string is already right here
      ;; because it is initialized with spaces and never changed
      ;; the +1 is for the first character (in the current line)
      ;; of the table, which is always a table edge character
      (inc-scratch-pos! (+ initial-space-count 1))
      (define end-of-table-position
        (for/fold ([start-pos-in-line (+ initial-space-count 1)])
          ([table-column-break (in-list table-column-breaks)]
           [x (in-naturals)])
          (cond
            [(and (set-member? set-of-indicies (list x y))
                  (or (not first-line?)
                      (set-member? set-of-indicies (list x (- y 1)))))
             (for ([j (in-range table-column-break)])
               (set-scratch! (string-ref line (+ j start-pos-in-line)))
               (inc-scratch-pos! 1))
             ;(printf "first-line? ~s x ~s y ~s\n" first-line? x y)
             (set-scratch! (if (if first-line?
                                   (and (set-member? set-of-indicies (list (+ x 1) (- y 1)))
                                        (set-member? set-of-indicies (list (+ x 1) y))
                                        (set-member? set-of-indicies (list x (- y 1))))
                                   (set-member? set-of-indicies (list (+ x 1) y)))
                               (string-ref line (+ table-column-break start-pos-in-line))
                               #\space))
             ;(printf "set\n")
             (inc-scratch-pos! 1)]
            [else
             (for ([j (in-range table-column-break)])
               (set-scratch! #\space)
               (inc-scratch-pos! 1))
             (set-scratch! #\space)
             (inc-scratch-pos! 1)])
          (+ start-pos-in-line table-column-break 1)))
      (for ([j (in-range end-of-table-position (string-length line))])
        (set-scratch! (string-ref line j))
        (inc-scratch-pos! 1)))))

(define (compare/xy p1 p2)
  (cond
    [(= (list-ref p1 0) (list-ref p2 0))
     (< (list-ref p1 1) (list-ref p2 1))]
    [else
     (< (list-ref p1 0) (list-ref p2 0))]))

(define (smallest-representative set)
  (define lst (set->list set))
  (let loop ([best (car lst)]
             [rest (cdr lst)])
    (cond
      [(null? rest) best]
      [else
       (cond
         [(compare/xy best (car rest))
          (loop best (cdr rest))]
         [else
          (loop (car rest) (cdr rest))])])))

(define (close-cell-graph edges width height)
  (define res (make-hash))
  (for ([x (in-range width)])
    (for ([y (in-range height)])
      (hash-set! res (list x y) (set (list x y)))))
  
  (let loop ()
    (define something-changed? #f)
    (define (add-all n1 n2)
      (define in-n1 (hash-ref res n1))
      (define in-n2 (hash-ref res n2))
      (for ([new-node (in-set in-n1)])
        (unless (set-member? in-n2 new-node)
          (set! something-changed? #t)
          (hash-set! res n2 (set-add in-n2 new-node)))))
    
    (for ([(node-src nodes) (in-hash edges)])
      (for ([node-dest (in-set nodes)])
        (add-all node-dest node-src)
        (add-all node-src node-dest)))
    
    (when something-changed? (loop)))
  
  (apply set (hash-map res (λ (x y) y))))
  
;; parse-2dcond returns three values:
;;  - a hash table encoding a graph that shows where the 
;;    broken walls are in the 2d
;;  - a vector of lists of strings containing the all of the line
;;    of the table except the last one; the first string in each
;;    list is the boundary line between the two rows
;;  - a list of numbers showing the size of each column, not 
;;    counting the separator character (and not taking into
;;    acount broken walls)
;;  - the number of spaces to the left of the 2d (same for all lines)
(define (parse-2dcond port source _line _col _pos chars-read)
  (define current-line-number _line)
  (define current-line-start-position (+ (or _pos 0) chars-read))
  (define current-line #f)
  (define current-line-length 0)
  (define initial-space-count 0) 
  (define initial-column-guide #f)
  (define newline-char-count 0)
  (define table-column-breaks '())
  (define table-column-guides '())
  (define right-edge-column #f)
  
  ;; saving the previous lines to build
  ;; the result for this function
  (define pending-row '())
  (define rows '())
  
  (define current-row 0)
  (define cell-connections (make-hash))
  (define (add-node col row) 
    (define k (list col row))
    (unless (hash-ref cell-connections k #f)
      (hash-set! cell-connections k (set))))
  (define (add-edge col1 row1 col2 row2)
    (define (add-->edge col1 row1 col2 row2)
      (add-node col1 row1)
      (define k (list col1 row1))
      (hash-set! cell-connections k (set-add (hash-ref cell-connections k) (list col2 row2))))
    (add-->edge col1 row1 col2 row2)
    (add-->edge col2 row2 col1 row1))
    
  (define (fetch-next-line)
    (when current-line
      (set! pending-row (cons current-line pending-row)))
    (set! current-line-start-position 
          (+ current-line-start-position 
             current-line-length
             newline-char-count))
    (when current-line-number
      (set! current-line-number (+ current-line-number 1)))
    (define chars
      (let loop ([chars-read 0])
        (define c (read-char port))
        (cond
          [(eof-object? c) 
           (raise-read-eof-error
            "expected eof; "
            source _line _col _pos 
            (and _pos (- _pos (+ current-line-start-position chars-read))))]
          [(equal? c #\return)
           (cond
             [(equal? #\newline (peek-char port))
              (set! newline-char-count 2)
              (list c (read-char port))]
             [else
              (set! newline-char-count 1)
              (list c)])]
          [(equal? c #\newline)
           (set! newline-char-count 1)
           (list c)]
          [(and (equal? c #\╝) (equal? right-edge-column chars-read))
           ;; if we find a ╝ at the width of the table, 
           ;; then we don't want
           ;; to consume any more characters and
           ;; instead to allow subsequent characters
           ;; to be part of some other thing that's 
           ;; being read (presumably a close paren)
           (set! newline-char-count 0)
           (list c)]
          [else
           (cons c (loop (+ chars-read 1)))])))
    (set! current-line (apply string chars))
    (set! current-line-length (- (string-length current-line) newline-char-count)))
  
  (define (process-first-line)
    (fetch-next-line)
    (let loop ([pos 0])
      (cond
        [(< pos current-line-length)
         (cond
           [(equal? #\space (string-ref current-line pos))
            (loop (+ pos 1))]
           [(equal? #\╔ (string-ref current-line pos))
            (set! initial-column-guide (make-a-guide pos))
            (set! initial-space-count pos)]
           [else
            (line-of-interest)
            (readerr "expected the first non-whitespace character in the table to be ╔"
                     pos)])]
        [else
         (line-of-interest)
         (readerr "expected some non-whitespace characters in the first line of the table"
                  0
                  pos)]))
    (let loop ([pos (+ initial-space-count 1)]
               [current-column-width 0]
               [column 0]
               [column-breaks '()]
               [column-guides '()])
      (cond
        [(< pos current-line-length)
         (case (string-ref current-line pos)
           [(#\╦) 
            (add-node column 0)
            (loop (+ pos 1) 0 (+ column 1)
                  (cons current-column-width column-breaks)
                  (cons (make-a-guide pos) column-guides))]
           [(#\═) (loop (+ pos 1) (+ current-column-width 1) column 
                        column-breaks column-guides)]
           [(#\╗) 
            (add-node column 0)
            (whitespace-to-end (+ pos 1))
            (set! table-column-breaks (reverse (cons current-column-width column-breaks)))
            (set! right-edge-column pos)
            (set! table-column-guides (reverse (cons (make-a-guide pos) column-guides)))])]
        [else 
         (line-of-interest)
         (readerr "expected ╗ to terminate the first line" pos)])))
  
  (define (process-a-line current-map)
    (fetch-next-line)
    ;; check leading space
    (let loop ([n 0])
      (cond
        [(= n initial-space-count) (void)]
        [(and (< n current-line-length)
              (equal? #\space (string-ref current-line n)))
         (loop (+ n 1))]
        [else
         (line-of-interest)
         (readerr "expected leading space" n)]))
    (case (string-ref current-line initial-space-count)
      [(#\║) (continue-line current-map)]
      [(#\╠) (start-new-block current-map)]
      [(#\╚) (finish-table current-map)]
      [else 
       (line-of-interest)
       (readerr/expected '(#\║ #\╠ #\╚)
                         initial-space-count
                         #:guides (list initial-column-guide))]))
  
  (define (start-new-block previous-map)
    (set! current-row (+ current-row 1))
    (add-node 0 current-row)

    (set! rows (cons (reverse pending-row) rows))
    (set! pending-row '())
    
    (let loop ([current-cell-size (car table-column-breaks)]
               [table-column-breaks (cdr table-column-breaks)]
               [pos (+ initial-space-count 1)]
               
               ;; whether or not the section of the line
               ;; we're currently traversing is there (or not)
               [cell-wall-broken? #f]
               
               ;; the srcloc of the spot that led us to the decision
               ;; of which boolean that cell-wall-broken? should be
               [cell-wall-guide (make-a-guide initial-space-count)]
               
               ;; this is the result, being built up backwards
               [map '()]
               
               ;; this is the map from the previous cell;
               ;; it tells us which characters here have to point upwards
               [previous-map previous-map]
               
               [current-column 0])
      (cond
        [(zero? current-cell-size)
         (unless (< pos current-line-length)
           (line-of-interest)
           (readerr "line ended too soon" pos))
         (define sep (string-ref current-line pos))
         (cond
           [(and cell-wall-broken? (not (car previous-map)))
            (unless (equal? sep #\╔)
              (when (double-barred-char? sep)
                (line-of-interest)
                (readerr "expected not to find a cell boundary character" pos)))]
           [else
            (define allowed-chars
              (if (null? table-column-breaks)
                  (list (get-one (not cell-wall-broken?) (car previous-map) #f #f)
                        (get-one (not cell-wall-broken?) (car previous-map) #f #t))
                  (list (get-one (not cell-wall-broken?) (car previous-map) #f #f)
                        (get-one (not cell-wall-broken?) (car previous-map) #f #t)
                        (get-one (not cell-wall-broken?) (car previous-map) #t #f)
                        (get-one (not cell-wall-broken?) (car previous-map) #t #t))))
            (unless (member sep allowed-chars)
              (line-of-interest)
              (readerr/expected (filter values allowed-chars) pos))])
         (cond
           [(null? table-column-breaks) 
            (whitespace-to-end (+ pos 1))
            (reverse (cons #t map))]
           [else
            (define next-cell-wall-broken? (not (member sep rt-chars)))
            (define edge-going-down? (and (member sep dn-chars) #t))
            (define next-column (+ current-column 1))
            (add-node next-column current-row)
            (when next-cell-wall-broken?
              (add-edge next-column current-row
                        next-column (- current-row 1)))
            (unless edge-going-down?
              (add-edge next-column current-row
                        (- next-column 1) current-row))
            (loop (car table-column-breaks)
                  (cdr table-column-breaks)
                  (+ pos 1)
                  next-cell-wall-broken? 
                  (make-a-guide pos)
                  (cons edge-going-down? map)
                  (cdr previous-map)
                  next-column)])]
        [else
         (unless (< pos current-line-length)
           (line-of-interest)
           (readerr "line ended in the middle of a cell" pos))
         (cond
           [cell-wall-broken?
            (when (double-barred-char? (string-ref current-line pos))
              (line-of-interest)
              (readerr 
               (format "expected not to find a cell boundary character (based on earlier ~a)"
                       (guide-char cell-wall-guide))
               pos
               #:guides (list cell-wall-guide)))]
           [else
            (unless (equal? (string-ref current-line pos) #\═)
              (line-of-interest)
              (readerr/expected '(#\═) pos #:guides (list cell-wall-guide)))])
         (loop (- current-cell-size 1) 
               table-column-breaks 
               (+ pos 1)
               cell-wall-broken?
               cell-wall-guide
               map
               previous-map
               current-column)])))
  
  (define (continue-line map)
    (let loop ([current-cell-size (car table-column-breaks)]
               [table-column-breaks (cdr table-column-breaks)]
               [map map]
               [pos (+ initial-space-count 1)]
               [column-number 0])
      (cond
        [(zero? current-cell-size)
         (unless (< pos current-line-length)
           (line-of-interest)
           (readerr "line ended at the boundary of a cell, expected the edge of the cell" pos))
         (cond
           [(car map)
            (unless (equal? (string-ref current-line pos) #\║)
              (line-of-interest)
              (readerr/expected '(#\║) pos))]
           [else
            (when (double-barred-char? (string-ref current-line pos))
              (line-of-interest)
              (readerr "expected not to find a cell boundary character" pos))])
         (cond
           [(null? table-column-breaks)
            (whitespace-to-end (+ pos 1))]
           [else
            (loop (car table-column-breaks)
                  (cdr table-column-breaks)
                  (cdr map)
                  (+ pos 1)
                  (+ column-number 1))])]
        [else
         (unless (< pos current-line-length)
           (line-of-interest)
           (readerr "line ended in the middle of a cell" pos))
         (when (double-barred-char? (string-ref current-line pos))
           (line-of-interest)
           (readerr "expected not to find a cell boundary character" pos))
         (loop (- current-cell-size 1)
               table-column-breaks
               map
               (+ pos 1)
               column-number)]))
    map)
  
  
  (define (finish-table map)
    (set! rows (cons (reverse pending-row) rows))
    (let loop ([current-cell-size (car table-column-breaks)]
               [table-column-breaks (cdr table-column-breaks)]
               [map map]
               [pos (+ initial-space-count 1)])
      (cond
        [(zero? current-cell-size) 
         (unless (< pos current-line-length)
           (line-of-interest)
           (readerr "line ended in the middle of a cell" pos))
         (define expected-char
           (cond
             [(null? table-column-breaks) #\╝]
             [(car map) #\╩]
             [else #\═]))
         (unless (equal? (string-ref current-line pos) expected-char)
           (line-of-interest)
           (readerr/expected (list expected-char) pos))
         (cond
           [(null? table-column-breaks)
            #f]
           [else
            (loop (car table-column-breaks)
                  (cdr table-column-breaks)
                  (cdr map)
                  (+ pos 1))])]
        [else
         (unless (< pos current-line-length)
           (line-of-interest)
           (readerr "line ended in the middle of a cell" pos))
         (unless (equal? (string-ref current-line pos) #\═)
           (line-of-interest)
           (readerr/expected '(#\═) pos))
         (loop (- current-cell-size 1)
               table-column-breaks
               map 
               (+ pos 1))])))
  
  (define (whitespace-to-end pos)
    (let loop ([pos pos])
      (when (< pos current-line-length)
        (define c (string-ref current-line pos))
        (cond
          [(equal? #\space c)
           (loop (+ pos 1))]
          [(equal? #\; c)
           (void)]
          [else
           (line-of-interest)
           (readerr "expected only whitespace outside of the table" pos)]))))  
  
  (struct guide (char srcloc))
  
  (define (make-a-guide pos-in-line)
    (guide (string-ref current-line pos-in-line)
           (srcloc source current-line-number pos-in-line
                   (+ current-line-start-position pos-in-line)
                   1)))

  (define (readerr/expected chars pos-in-line #:guides [guides '()])
    (readerr (format "expected ~a~a~a" 
                     (if (null? (cdr chars))
                         ""
                         "one of ")
                     (chars->desc chars "or")
                     (if (null? guides)
                         ""
                         (format " (based on earlier ~a)"
                                 (chars->desc (map guide-char guides)
                                              "and"))))
             pos-in-line
             #:guides guides))
  
  (define (readerr msg pos-in-line [span 1] #:guides [guides '()])
    (raise-read-error msg 
                      source
                      current-line-number
                      pos-in-line
                      (+ current-line-start-position pos-in-line)
                      span
                      #:extra-srclocs (map guide-srcloc guides)))
  
  (process-first-line)
  (let loop ([map (map (λ (x) #t) table-column-breaks)])
    (define next-map (process-a-line map))
    (cond
      [next-map (loop next-map)]
      [else 
       (values cell-connections 
               (apply vector (reverse rows))
               table-column-breaks
               initial-space-count)])))




;; chars : non-empty-list-of-char -> string
(define (chars->desc chars sep)
  (cond
    [(null? (cdr chars))
     (format "~a" (car chars))]
    [else
     (define commas? (pair? (cddr chars)))
     (apply 
      string-append
      (let loop ([chars chars]
                 [first? #t])
        (cond
          [(null? (cdr chars))
           (list (format "~a~a ~a"
                         (if first? "" " ")
                         sep
                         (car chars)))]
          [else
           (cons (format "~a~a~a" 
                         (if first? "" " ")
                         (car chars)
                         (if commas? "," ""))
                 (loop (cdr chars) #f))])))]))

(define (double-barred-char? c) (member c double-barred-chars))

(define (get-one lt? up? rt? dn?)
  (define (cmb a b) (if a b (not b)))
  (for/or ([c (in-list double-barred-chars)])
    (and (cmb lt? (member c lt-chars))
         (cmb up? (member c up-chars))
         (cmb rt? (member c rt-chars))
         (cmb dn? (member c dn-chars))
         c)))

