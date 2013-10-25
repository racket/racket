#lang racket/base
#|

ideas:
- 2dcond
- 2dmatch
- literal tables in scribble layout?
- something for 2d graphics?

example uses:
- unifier
- subtyping relation
- merge (from merge-sort)

|#

(require racket/port
         syntax/readerr
         racket/match
         racket/set
         ;syntax/rect
         "../dir-chars.rkt"
         (for-syntax racket/base
                     racket/list))


(provide parse-2dcond
         parse-2dcond-one-step
         
         setup-state
         make-state
         copy-state
         
         chars->desc
         smallest-representative
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

;; fill-scratch-string : (setof (list/c number? number?))
;;                       (vector (listof string?))
;;                       (or/c string? #f)
;;                       (listof number)
;;                       number
;;                       [boolean?]
;;                    -> (if scratch-string 
;;                           (listof (cons/c number? number?)) 
;;                           void?)
;; scratch-string gets filled in from the 'lines' argument. 
;; If compute-regions? is #t, then this function constructs the regions of the
;; string that are have been filled in (as a list of pairs of start/end coordinates)
;; and returns that (not counting the regions outside of the table to the right--
;; these get filled in to the string, but the regions are not included)
;; the resulting regions are sorted (smaller to bigger values) and non-overlapping
(define (fill-scratch-string set-of-indicies 
                             lines
                             scratch-string
                             table-column-breaks
                             initial-space-count
                             [compute-regions? #f])
  
  (define scratch-pos 0)
  
  (define eols '())
  (define segments '())
  (define cur-seg-start #f)
  (define cur-seg-end #f)
  (define (add-keeper)
    (cond
      [(equal? cur-seg-end scratch-pos)
       (set! cur-seg-end (+ cur-seg-end 1))]
      [else
       (record-position)
       (set! cur-seg-start scratch-pos)
       (set! cur-seg-end (+ scratch-pos 1))]))
  (define (record-position)
    (when (and cur-seg-start cur-seg-end)
      ;; port positions count from 1, but here
      ;; we're counting from 0 in the string, so inc
      (set! segments (cons (cons (+ cur-seg-start 1)
                                 (+ cur-seg-end 1))
                           segments))))
  
  (define-syntax-rule 
    (set-scratch! in? c) 
    (begin
      (let ([x c])
        ;(unless (char-whitespace? x) (printf "putting ~s @ ~s\n" x scratch-pos))
        (string-set! scratch-string scratch-pos x))
      (when in? (when compute-regions? (add-keeper)))))
  (define-syntax-rule
    (clear-scratch!)
    (when scratch-string (string-set! scratch-string scratch-pos #\space)))
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
               (set-scratch! #t (string-ref line (+ j start-pos-in-line)))
               (inc-scratch-pos! 1))
             (if (if first-line?
                     (and (set-member? set-of-indicies (list (+ x 1) (- y 1)))
                          (set-member? set-of-indicies (list (+ x 1) y))
                          (set-member? set-of-indicies (list x (- y 1))))
                     (set-member? set-of-indicies (list (+ x 1) y)))
                 (set-scratch! #t (string-ref line (+ table-column-break start-pos-in-line)))
                 (clear-scratch!))
             (inc-scratch-pos! 1)]
            [else
             (for ([j (in-range table-column-break)])
               (clear-scratch!)
               (inc-scratch-pos! 1))
             (clear-scratch!)
             (inc-scratch-pos! 1)])
          (+ start-pos-in-line table-column-break 1)))
      (set! eols (cons (cons end-of-table-position (string-length line))
                       eols))
      (for ([j (in-range end-of-table-position (string-length line))])
        (set-scratch! #f (string-ref line j))
        (inc-scratch-pos! 1))))
  
  (when compute-regions?
    (record-position)
    (reverse segments)))

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

(begin-for-syntax
  (define state-components
    ;; these are the state variables for the parse-2d-cond procedure
    '((current-line-number _line)
      (current-line-start-position (+ (or _pos 0) chars-read))
      (current-line #f)
      (current-line-length 0)
      (initial-space-count 0)
      (initial-column-guide #f)
      (newline-char-count 0)
      (table-column-breaks '())
      (table-column-guides '())
      (right-edge-column #f)
      (pending-row '())
      (rows '())
      (current-row 0)
      (cell-connections (make-hash))
      (position-of-first-cell (hash)))))

(define-syntax (setup-state stx)
  (syntax-case stx ()
    [(_ state-struct-id #;state-accessor #;state-mutator)
     #`(begin
         #,@(for/list ([state-component (in-list state-components)]
                       [i (in-naturals)])
              (with-syntax ([id (datum->syntax #'state-struct-id (car state-component))]
                            [i i])
                #'(define-syntax id
                    (make-set!-transformer
                     (λ (stx)
                       (syntax-case stx (set!)
                         [(set! x e)
                          #'(state-mutator state-struct-id i e)]
                         [x
                          (identifier? #'x)
                          #'(state-accessor state-struct-id i)])))))))]))

(define-syntax (state-struct stx)
  (syntax-case stx ()
    [(_ make-state state-accessor state-mutator copy-state)
     #`(begin
         (define-values (state-type state-constructor state? state-accessor state-mutator)
           (make-struct-type 'parse-2d-cond-state #f #,(length state-components) 0 #f '() #f))
         (define (make-state _line _pos chars-read)
           (state-constructor #,@(for/list ([state-component (in-list state-components)])
                                   (list-ref state-component 1))))
         (define (copy-state the-state)
           (state-constructor #,@(for/list ([state-component (in-list state-components)]
                                            [i (in-naturals)])
                                   #`(state-accessor the-state #,i)))))]))

(state-struct make-state state-accessor state-mutator copy-state)

(define (parse-2dcond port source _line _col _pos chars-read)
  (define the-state (make-state _line _pos chars-read))
  (let loop ([map #f])
    (define new-map
      (parse-2dcond-one-step port source _line _col _pos the-state map))
    (cond
      [new-map
       (loop new-map)]
      [else
       (setup-state the-state)
       (values cell-connections 
               (apply vector (reverse rows))
               table-column-breaks
               initial-space-count
               position-of-first-cell)])))

(struct guide (char srcloc) #:transparent)

  
;; parse-2dcond returns four values:
;;  - a hash table encoding a graph that shows where the 
;;    broken walls are in the 2d
;;  - a vector of lists of strings containing the all of the line
;;    of the table except the last one; the first string in each
;;    list is the boundary line between the two rows
;;  - a list of numbers showing the size of each column, not 
;;    counting the separator character (and not taking into
;;    acount broken walls)
;;  - the number of spaces to the left of the 2d (same for all lines)
(define (parse-2dcond-one-step port source _line _col _pos the-state last-left-map)
  
  ;; this sets up all of the state variables so they 
  ;; look up the fields of 'the-state' and mutate
  ;; the fields of 'the-state'; state-components lists
  ;; of the state variables and their initial values
  (setup-state the-state)
  
  
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
        (define c (read-char-or-special port))
        (cond
          [(eof-object? c) 
           (raise-read-eof-error
            "unexpected eof; "
            source _line _col _pos 
            (and _pos (- (+ current-line-start-position chars-read) _pos)))]
          [(not (char? c))
           (readerr "unexpected special" chars-read)]
          [(equal? c #\return)
           (cond
             [(equal? #\newline (peek-char-or-special port))
              (set! newline-char-count 2)
              (list c (read-char-or-special port))]
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
            (set! table-column-guides (reverse (cons (make-a-guide pos) column-guides)))]
           [else 
            (line-of-interest)
            (readerr "expected only ═ ╦ and ╗ characters along the top of the grid" pos)])]
        [else 
         (line-of-interest)
         (readerr "expected ╗ to terminate the first line" pos)])))
  
  (define (process-a-line current-map previous-line-separator?)
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
      [(#\║) (values (continue-line current-map previous-line-separator?) #t)]
      [(#\╠) (values (start-new-block current-map) #f)]
      [(#\╚) (values (finish-table current-map) #f)]
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
  
  (define (continue-line map previous-line-separator?)
    (let loop ([current-cell-size (car table-column-breaks)]
               [table-column-breaks (cdr table-column-breaks)]
               [map map]
               [pos (+ initial-space-count 1)]
               [column-number 0]
               [starting-a-new-cell? #t])
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
                  (+ column-number 1)
                  #t)])]
        [else
         (unless (< pos current-line-length)
           (line-of-interest)
           (readerr "line ended in the middle of a cell" pos))
         (when (double-barred-char? (string-ref current-line pos))
           (line-of-interest)
           (readerr "expected not to find a cell boundary character" pos))
         (when previous-line-separator?
           (when starting-a-new-cell?
             (set! position-of-first-cell
                   (hash-set
                    position-of-first-cell
                    (list column-number current-row)
                    (guide-srcloc (make-a-guide pos))))))
         (loop (- current-cell-size 1)
               table-column-breaks
               map
               (+ pos 1)
               column-number
               #f)]))
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
  
  (let loop ([map (or last-left-map
                      (begin
                        (process-first-line)
                        (map (λ (x) #t) table-column-breaks)))]
             [previous-line-separator? #t])
    (define-values (next-map continue?) (process-a-line map previous-line-separator?))
    (cond
      [continue? (loop next-map #f)]
      [next-map next-map]
      [else #f])))
     
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
