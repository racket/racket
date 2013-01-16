#lang racket/base
(require racket/port
         syntax/readerr
         racket/match
         racket/set
         framework/private/dir-chars
         (for-syntax racket/base
                     racket/list))

(define rt
  (make-readtable
   #f
   #\2
   'dispatch-macro
   (λ (char port source _line _col _pos)
     (define pport (peeking-input-port port))
     (define dcond-count 5)
     (define dcond (for/list ([i (in-range dcond-count)]) (read-char pport)))
     (cond
       [(equal? dcond '(#\d #\c #\o #\n #\d))
        (define chars-read 0)
        (define (rc) 
          (set! chars-read (+ chars-read 1))
          (read-char port))
        (for ([i (in-range dcond-count)]) (rc))
        (define nl (rc))
        (cond
          [(equal? nl #\newline) (void)]
          [(equal? nl #\return) 
           (when (equal? #\newline (peek-char port))
             (rc))]
          [else
           (raise-read-error "expected a newline to follow #2dcond"
                             (object-name port) 
                             _line _col _pos 
                             6)])
        (parse-2dcond port source _line _col _pos chars-read)]
       [else
        (read/recursive 
         (input-port-append #f (open-input-string "#2") port)
         #f
         #f)]))))

(define (parse-2dcond port source _line _col _pos chars-read)
  (define current-line-number _line)
  (define current-line-start-position (+ _pos chars-read))
  (define current-line "")
  (define table-column-breaks '())
  (define initial-space-count 0) 
  (define newline-char-count 0)
  
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
    (set! current-line-start-position 
          (+ current-line-start-position 
             (string-length current-line)
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
            (- _pos (+ current-line-start-position chars-read)))]
          [(equal? c #\return)
           (cond
             [(equal? #\newline (peek-char c))
              (read-char c)
              (set! newline-char-count 2)]
             [else
              (set! newline-char-count 1)])
           '()]
          [(equal? c #\newline)
           (set! newline-char-count 1)
           '()]
          [else
           (cons c (loop (+ chars-read 1)))])))
    (set! current-line (apply string chars)))
  
  (define (process-first-line)
    (fetch-next-line)
    (cond
      [(eq? current-line 'terminator)
       (readerr "expected to find a table" port)]
      [else
       (let loop ([pos 0])
         (cond
           [(< pos (string-length current-line))
            (cond
              [(equal? #\space (string-ref current-line pos))
               (loop (+ pos 1))]
              [(equal? #\╔ (string-ref current-line pos))
               (set! initial-space-count pos)]
              [else
               (readerr "expected the first character in the table to be ╔"
                        pos 1)])]
           [else
            (readerr "expected some non-whitespace characters in the first line of the table"
                     0
                     pos)]))
       (set! table-column-breaks
             (let loop ([pos (+ initial-space-count 1)]
                        [current-column-width 0]
                        [column 0])
               (cond
                 [(< pos (string-length current-line))
                  (case (string-ref current-line pos)
                    [(#\╦) 
                     (add-node column 0)
                     (cons current-column-width (loop (+ pos 1) 0 (+ column 1)))]
                    [(#\═) (loop (+ pos 1) (+ current-column-width 1) column)]
                    [(#\╗) 
                     (add-node column 0)
                     (let loop ([pos (+ pos 1)])
                       (when (< pos (string-length current-line))
                         (cond
                           [(char-whitespace? (string-ref current-line pos))
                            (loop (+ pos 1))]
                           [else
                            (readerr "expected only whitespace to follow ╗ (on the same line)" pos)]))
                       (list current-column-width))])]
                 [else 
                  (readerr "expected ╗ to terminate the first line" pos)])))]))
  
  (define (process-a-line current-map)
    (fetch-next-line)
    (cond
      [(eq? current-line 'terminator)
       (readerr "incomplete table" port)]
      [else
       ;; check leading space
       (let loop ([n 0])
         (cond
           [(= n initial-space-count) (void)]
           [(and (< n (string-length current-line))
                 (equal? #\space (string-ref current-line n)))
            (loop (+ n 1))]
           [else
            (readerr "expected leading space" n)]))
       (case (string-ref current-line initial-space-count)
         [(#\║) (continue-line current-map)]
         [(#\╠) (start-new-block current-map)]
         [(#\╚) (finish-table current-map)]
         [else (readerr/expected '(#\║ #\╠ #\╚)
                                 initial-space-count)])]))
  
  (define (start-new-block previous-map)
    (set! current-row (+ current-row 1))
    (add-node 0 current-row)
    (let loop ([current-cell-size (car table-column-breaks)]
               [table-column-breaks (cdr table-column-breaks)]
               [pos (+ initial-space-count 1)]
               
               ;; whether or not the section of the line
               ;; we're currently traversing is there (or not)
               [cell-wall-broken? #f]
               
               ;; this is the result, being built up backwards
               [map '()]
               
               ;; this is the map from the previous cell;
               ;; it tells us which characters here have to point upwards
               [previous-map previous-map]
               
               [current-column 0])
      (cond
        [(zero? current-cell-size)
         (unless (< pos (string-length current-line))
           (readerr "line ended too soon"))
         (define sep (string-ref current-line pos))
         (cond
           [(and cell-wall-broken? (not (car previous-map)))
            (when (table-char? (string-ref current-line pos))
              (readerr "expected not to find a cell boundary character" pos))]
           [else
            (define allowed-chars
              (if (null? table-column-breaks)
                  (list (get-one (not cell-wall-broken?) (car previous-map) #f #f)
                        (get-one (not cell-wall-broken?) (car previous-map) #f #t))
                  (list (get-one (not cell-wall-broken?) (car previous-map) #f #f)
                        (get-one (not cell-wall-broken?) (car previous-map) #f #t)
                        (get-one (not cell-wall-broken?) (car previous-map) #t #f)
                        (get-one (not cell-wall-broken?) (car previous-map) #t #t))))
            (set! allowed-chars (filter values allowed-chars))
            (unless (member sep allowed-chars)
              (readerr/expected allowed-chars pos))])
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
                  (cons edge-going-down? map)
                  (cdr previous-map)
                  next-column)])]
        [else
         (unless (< pos (string-length current-line))
           (readerr "line ended in the middle of a cell" pos))
         (cond
           [cell-wall-broken?
            (when (table-char? (string-ref current-line pos))
              (readerr "expected not to find a cell boundary character" pos))]
           [else
            (unless (equal? (string-ref current-line pos) #\═)
              (readerr/expected '(#\═) pos))])
         (loop (- current-cell-size 1) 
               table-column-breaks 
               (+ pos 1)
               cell-wall-broken?
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
         (unless (< pos (string-length current-line))
           (readerr "line ended at the boundary of a cell, expected the edge of the cell" pos))
         (cond
           [(car map)
            (unless (equal? (string-ref current-line pos) #\║)
              (readerr/expected '(#\║) pos))]
           [else
            (when (table-char? (string-ref current-line pos))
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
         (unless (< pos (string-length current-line))
           (readerr "line ended in the middle of a cell" pos))
         (when (table-char? (string-ref current-line pos))
           (readerr "expected not to find a cell boundary character" pos))
         (loop (- current-cell-size 1)
               table-column-breaks
               map
               (+ pos 1)
               column-number)]))
    map)
  
  
  (define (finish-table map)
    (let loop ([current-cell-size (car table-column-breaks)]
               [table-column-breaks (cdr table-column-breaks)]
               [map map]
               [pos (+ initial-space-count 1)])
      (cond
        [(zero? current-cell-size) 
         (unless (< pos (string-length current-line))
           (readerr "line ended in the middle of a cell" pos))
         (define expected-char
           (cond
             [(null? table-column-breaks) #\╝]
             [(car map) #\╩]
             [else #\═]))
         (unless (equal? (string-ref current-line pos) expected-char)
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
         (unless (< pos (string-length current-line))
           (readerr "line ended in the middle of a cell" pos))
         (unless (equal? (string-ref current-line pos) #\═)
           (readerr/expected '(#\═) pos))
         (loop (- current-cell-size 1)
               table-column-breaks
               map 
               (+ pos 1))])))
  
  (define (whitespace-to-end pos)
    (let loop ([pos pos])
      (when (< pos (string-length current-line))
        (unless (equal? #\space (string-ref current-line pos))
          (readerr "expected only whitespace outside of the table" pos))
        (loop (+ pos 1)))))
  
  (define (readerr/expected chars pos-in-line)
    (readerr (format "expected ~a" (chars->desc chars))
             pos-in-line))
  
  (define (readerr msg pos-in-line [span 1])
    (raise-read-error msg (object-name port)
                      current-line-number
                      pos-in-line
                      (+ current-line-start-position pos-in-line)
                      span))
  
  (process-first-line)
  (let loop ([map (map (λ (x) #t) table-column-breaks)])
    (define next-map (process-a-line map))
    (cond
      [next-map (loop next-map)]
      [else cell-connections])))




;; chars : non-empty-list-of-char -> string
(define (chars->desc chars)
  (cond
    [(null? (cdr chars))
     (format "~a" (car chars))]
    [else
     (define commas? (pair? (cddr chars)))
     (apply 
      string-append
      "one of"
      (let loop ([chars chars])
        (cond
          [(null? (cdr chars))
           (list (format " or ~a" (car chars)))]
          [else
           (cons (format " ~a~a" 
                         (car chars)
                         (if commas? "," ""))
                 (loop (cdr chars)))])))]))

(define table-chars '(#\═ #\║ #\╔ #\╦ #\╗ #\╠ #\╬ #\╣ #\╚ #\╩ #\╝))
(define (table-char? c) (member c table-chars))

(define (get-one  lt? up? rt? dn?)
  (define (cmb a b) (if a b (not b)))
  (for/or ([c (in-list table-chars)])
    (and (cmb lt? (member c lt-chars))
         (cmb up? (member c up-chars))
         (cmb rt? (member c rt-chars))
         (cmb dn? (member c dn-chars))
         c)))

(module+ test
  (require rackunit)
  
  (check-equal? (chars->desc '(#\a))
                "a")
  (check-equal? (chars->desc '(#\a #\b))
                "one of a or b")
  (check-equal? (chars->desc '(#\a #\b #\c))
                "one of a, b, or c")
  (check-equal? (chars->desc '(#\a #\b #\c #\d))
                "one of a, b, c, or d")
  (check-equal? (chars->desc '(#\a #\b #\c #\d #\e))
                "one of a, b, c, d, or e")
  
  (check-equal? (read (open-input-string "#2(x)"))
                (parameterize ([current-readtable rt])
                  (read (open-input-string "#2(x)"))))
  (check-equal? (with-handlers ((exn:fail? exn-message))
                  (read (open-input-string "#2x(x)")))
                (with-handlers ((exn:fail? exn-message))
                  (parameterize ([current-readtable rt])
                    (read (open-input-string "#2x(x)")))))
  (check-regexp-match #rx"expected a newline"
                      (with-handlers ((exn:fail? exn-message))
                        (parameterize ([current-readtable rt])
                          (read (open-input-string "#2dcond")))))
  
  (define (get-err-positions inputs)
    (with-handlers ((exn:fail:read? 
                     (λ (exn)
                       (map srcloc-position (exn:fail:read-srclocs exn)))))
      (parameterize ([current-readtable rt])
        (read (open-input-string (apply string-append inputs))))
      #f))
  
  (define (get-graph inputs)
    (define p (open-input-string (apply string-append inputs)))
    (port-count-lines! p)
    ;; account for the "#2dcond" that was read from the first line
    (parse-2dcond p "source" 1 0 1 6))

  
  (check-equal? (get-err-positions 
                 '("#2dcond\n"
                   "╔══╦══╗\n"
                   "║1 ║2 ║\n"
                   "╠══╬══╣\n"
                   "║4 ║3 ║\n"
                   "╚══╩══╝\n"))
                #f)
  (check-equal? (get-err-positions 
                 '("#2dcond\n"
                   "  ╔══╦══╗\n"
                   "  ║1 ║4 ║\n"
                   "  ╠══╬══╣\n"
                   "  ║2 ║3 ║\n"
                   "  ╚══╩══╝\n"))
                #f)
  (check-equal? (get-err-positions 
                 '("#2dcond\n"
                   "  ╔══╦══╦══╗\n"
                   "  ║1 ║2 ║3 ║\n"
                   "  ╠══╬══╬══╣\n"
                   "  ║6 ║5 ║4 ║\n"
                   "  ╠══╬══╬══╣\n"
                   "  ║7 ║8 ║9 ║\n"
                   "  ╚══╩══╩══╝\n"))
                #f)
  (check-equal? (get-err-positions 
                 '("#2dcond\n"
                   "  ╔══╦══╦══╗\n"
                   "  ║ 1║ 2║ 3║\n"
                   "  ╠══╬══╩══╣\n"
                   "  ║ 4║     ║\n"
                   "  ╠══╣  6  ║\n"
                   "  ║ 5║     ║\n"
                   "  ╚══╩═════╝\n"))
                #f)
  (check-equal? (get-err-positions 
                 '("#2dcond\n"
                   "  ╔══╦══╦══╗\n"
                   "  ║ 1║ 2║ 3║\n"
                   "  ╠══╬══╩══╣\n"
                   "  ║ 4║5    ║\n"
                   "  ╠══╬═════╣\n"
                   "  ║ 6║7    ║\n"
                   "  ╚══╩═════╝\n"))
                #f)
  (check-equal? (get-err-positions 
                 '("#2dcond\n"
                   "  ╔══╦══╦══╦══╗\n"
                   "  ║ 1║ 2║ 3║ 4║\n"
                   "  ╠══╬══╬══╩══╣\n"
                   "  ║ 4║ 5║  6  ║\n"
                   "  ╚══╩══╩═════╝\n"))
                #f)
  (check-equal? (get-err-positions 
                 '("#2dcond\n"
                   "  ╔══╦══╦══╗\n"
                   "  ║1 ║2 ║3 ║\n"
                   "  ╠══╬══╬══╣\n"
                   "  ║4 ║  ║  ║\n"
                   "  ╠══╣  ║  ║\n"
                   "  ║5 ║6 ║7 ║\n"
                   "  ╚══╩══╩══╝\n"))
                #f)
  (check-equal? (get-err-positions 
                 '("#2dcond\n"
                   "  ╔══╦══╗\n"
                   "  ║  ║  ║\n"
                   "  ╠══╣  ║\n"
                   "  ║  ║  ║\n"
                   "  ╚══╩══╝\n"))
                '(something)) ;; that first ═ line has to go all the way across
  (check-equal? (get-err-positions 
                 '("#2dcond\n"
                   "  ╔══╦══╗\n"
                   "     ║  ║\n"
                   "  ╠══╬══╣\n"
                   "  ║  ║  ║\n"
                   "  ╚══╩══╝\n"))
                '(12 2))
  (check-equal? (get-err-positions 
                 '("#2dcond\n"
                   "  ╔══╦══╗\n"
                   "  ║ ═║  ║\n"
                   "  ╠══╬══╣\n"
                   "  ║  ║  ║\n"
                   "  ╚══╩══╝\n"))
                '(14 4))
  
  
  (check-equal? (get-graph
                 '("  ╔══╦══╦══╗\n"
                   "  ║ 1║ 2║ 3║\n"
                   "  ╠══╬══╩══╣\n"
                   "  ║ 4║     ║\n"
                   "  ╠══╣  6  ║\n"
                   "  ║ 5║     ║\n"
                   "  ╚══╩═════╝\n"))
                (make-hash
                 (list (cons (list 0 0) (set))
                       (cons (list 0 1) (set))
                       (cons (list 0 2) (set))
                       (cons (list 1 0) (set))
                       (cons (list 2 0) (set))
                      
                       (cons (list 1 1) (set (list 1 2) (list 2 1)))
                       (cons (list 2 1) (set (list 1 1) (list 2 2)))
                       (cons (list 1 2) (set (list 1 1) (list 2 2)))
                       (cons (list 2 2) (set (list 1 2) (list 2 1))))))
  
  (check-equal? (get-graph
                 '("  ╔══╦══╦══╗\n"
                   "  ║1 ║2 ║3 ║\n"
                   "  ╠══╬══╬══╣\n"
                   "  ║6 ║5 ║4 ║\n"
                   "  ╠══╬══╬══╣\n"
                   "  ║7 ║8 ║9 ║\n"
                   "  ╚══╩══╩══╝\n"))
                (make-hash
                 (list (cons (list 0 0) (set))
                       (cons (list 0 1) (set))
                       (cons (list 0 2) (set))
                       (cons (list 1 0) (set))
                       (cons (list 1 1) (set))
                       (cons (list 1 2) (set))
                       (cons (list 2 0) (set))
                       (cons (list 2 1) (set))
                       (cons (list 2 2) (set))))))



#;
(module+ main
  (define s (string-append
             "  ╔══╦══╦══╗\n"
             "  ║ 1║ 2║ 3║\n"
             "  ╠══╬══╩══╣\n"
             "  ║ 4║     ║\n"
             "  ╠══╣  6  ║\n"
             "  ║ 5║     ║\n"
             "  ╚══╩═════╝\n"))
  #;
  (define s (string-append
             "  ╔══╦══╦══╗\n"
             "  ║1 ║2 ║3 ║\n"
             "  ╠══╬══╬══╣\n"
             "  ║6 ║5 ║4 ║\n"
             "  ╠══╬══╬══╣\n"
             "  ║7 ║8 ║9 ║\n"
             "  ╚══╩══╩══╝\n"))
  (define p (open-input-string s))
  (port-count-lines! p)
  ;; account for the "#2dcond" that was read from the first line
  (parse-2dcond p "source" 1 0 1 6))
  
#|
╔══╦══╗
║  ║  ║
╠══╬══╣
║  ║  ║
╚══╩══╝
|#