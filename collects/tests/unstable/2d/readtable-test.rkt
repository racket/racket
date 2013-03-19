#lang racket/base

(require rackunit
         unstable/2d/private/read-util
         unstable/2d/private/readtable
         racket/set)

(define touched-lines-table (make-hash))

(check-equal? (chars->desc '(#\a) "or")
              "a")
(check-equal? (chars->desc '(#\a #\b) "or")
              "a or b")
(check-equal? (chars->desc '(#\a #\b #\c) "or")
              "a, b, or c")
(check-equal? (chars->desc '(#\a #\b #\c #\d) "or")
              "a, b, c, or d")
(check-equal? (chars->desc '(#\a #\b #\c #\d #\e) "or")
              "a, b, c, d, or e")

(check-equal? (read (open-input-string "#2(x)"))
              (parameterize ([current-readtable (make-2d-readtable)])
                (read (open-input-string "#2(x)"))))
(check-equal? (with-handlers ((exn:fail? exn-message))
                (read (open-input-string "#2x(x)")))
              (with-handlers ((exn:fail? exn-message))
                (parameterize ([current-readtable (make-2d-readtable)])
                  (read (open-input-string "#2x(x)")))))
(check-true (syntax? (read-syntax 'hi (open-input-string "#2(x)"))))
(check-equal? (read (open-input-string "#2(x)"))
              (parameterize ([current-readtable (make-2d-readtable)])
                (syntax->datum (read-syntax 'hi (open-input-string "#2(x)")))))

(parameterize ([current-readtable (make-2d-readtable)])
  (define sp (open-input-string 
              (string-append "#2d\n"
                             "╔══╦══╗\n"
                             "║1 ║2 ║\n"
                             "╠══╬══╣\n"
                             "║4 ║3 ║\n"
                             "╚══╩══╝\n")))
  (define wp
    (make-input-port 'name sp sp void #f #f
                     (λ () (values #f #f #f))
                     void))
  (port-count-lines! wp)
  ;; make sure that if there is no source location information,
  ;; we still get some result back.
  (check-true (pair? (read wp))))

(parameterize ([current-readtable (make-2d-readtable)])
  (define sp (open-input-string 
              (string-append "#2d\n"
                             "╔══╦══╗\n"
                             "║1 ║2 ║\n"
                             "╠══╬══╣\n"
                             "║4 ║3 ║\n"
                             "╚══╩══╝\n")))
  (port-count-lines! sp)
  ;; make sure that if there is no source location information,
  ;; we still get some result back.
  (define stx (read-syntax "the source" sp))
  (define initial-keyword (car (syntax-e stx)))
  (check-not-false (syntax-source initial-keyword))
  (check-not-false (syntax-line initial-keyword))
  (check-not-false (syntax-column initial-keyword))
  (check-not-false (syntax-position initial-keyword))
  (check-not-false (syntax-span initial-keyword))
  (check-not-false (syntax-original? initial-keyword))
  (check-not-equal? (syntax-position stx) 
                    (syntax-position initial-keyword)))

(define (get-err-locs inputs)
  (with-handlers ([exn:fail:read? exn:fail:read-srclocs])
    (define p (open-input-string (apply string-append inputs)))
    (port-count-lines! p)
    (parameterize ([current-readtable (make-2d-readtable)])
      (read-syntax #f p))
    #f))

(define (get-something inputs i)
  (define p (open-input-string (apply string-append inputs)))
  (port-count-lines! p)
  ;; account for the "#2d" that was read from the first line
  (call-with-values (λ () (parse-2dcond p "source" 1 0 1 2))
                    (λ x (list-ref x i))))

(define (get-graph inputs) (get-something inputs 0))
(define (get-all-lines inputs) (get-something inputs 1))
(define (get-table-column-breaks inputs) (get-something inputs 2))
(define (get-initial-space-count inputs) (get-something inputs 3))


(check-equal? (get-err-locs 
               '("#2d\n"
                 "╔══╦══╗\n"
                 "║1 ║2 ║\n"
                 "╠══╬══╣\n"
                 "║4 ║3 ║\n"
                 "╚══╩══╝\n"))
              #f)
(check-equal? (get-err-locs 
               '("#2d\n"
                 "╔══╦══╗\n"
                 "║λ ║2 ║\n"
                 "╠══╬══╣\n"
                 "║1 ║黃 ║\n"
                 "╚══╩══╝\n"))
              #f)
(check-equal? (get-err-locs 
               '("#2d\n"
                 "  ╔══╦══╗\n"
                 "  ║1 ║4 ║\n"
                 "  ╠══╬══╣\n"
                 "  ║2 ║3 ║\n"
                 "  ╚══╩══╝\n"))
              #f)
(check-equal? (get-err-locs 
               '("#2d\n"
                 "  ╔══╦══╦══╗\n"
                 "  ║1 ║2 ║3 ║\n"
                 "  ╠══╬══╬══╣\n"
                 "  ║6 ║5 ║4 ║\n"
                 "  ╠══╬══╬══╣\n"
                 "  ║7 ║8 ║9 ║\n"
                 "  ╚══╩══╩══╝\n"))
              #f)
(check-equal? (get-err-locs 
               '("#2d\n"
                 "  ╔══╦══╦══╗\n"
                 "  ║ 1║ 2║ 3║\n"
                 "  ╠══╬══╩══╣\n"
                 "  ║ 4║     ║\n"
                 "  ╠══╣  6  ║\n"
                 "  ║ 5║     ║\n"
                 "  ╚══╩═════╝\n"))
              #f)
(check-equal? (get-err-locs 
               '("#2d\n"
                 "  ╔══╦══╦══╗\n"
                 "  ║ 1║ 2║ 3║\n"
                 "  ╠══╬══╩══╣\n"
                 "  ║ 4║5    ║\n"
                 "  ╠══╬═════╣\n"
                 "  ║ 6║7    ║\n"
                 "  ╚══╩═════╝\n"))
              #f)
(check-equal? (get-err-locs 
               '("#2d\n"
                 "  ╔══╦══╦══╦══╗\n"
                 "  ║ 1║ 2║ 3║ 4║\n"
                 "  ╠══╬══╬══╩══╣\n"
                 "  ║ 4║ 5║  6  ║\n"
                 "  ╚══╩══╩═════╝\n"))
              #f)
(check-equal? (get-err-locs 
               '("#2d\n"
                 "  ╔══╦══╦══╗\n"
                 "  ║1 ║2 ║3 ║\n"
                 "  ╠══╬══╬══╣\n"
                 "  ║4 ║  ║  ║\n"
                 "  ╠══╣  ║  ║\n"
                 "  ║5 ║6 ║7 ║\n"
                 "  ╚══╩══╩══╝\n"))
              #f)

(check-equal? (get-err-locs 
               '("#2d\n"
                 "  ╔══╦══╗\n"
                 "  ║1 ║4 ║  ;; comment\n"
                 "  ╠══╬══╣  ;; comment  \n"
                 "  ║2 ║3 ║\n"
                 "  ╚══╩══╝\n"))
              #f)

(define lines-table (hash-copy all-line-of-interest))

(parameterize ([current-lines lines-table])
  (check-regexp-match #rx"expected a newline"
                      (with-handlers ((exn:fail? exn-message))
                        (parameterize ([current-readtable (make-2d-readtable)])
                          (read (open-input-string "#2d")))))
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦══╗\n"
                   "     ║  ║\n"
                   "  ╠══╬══╣\n"
                   "  ║  ║  ║\n"
                   "  ╚══╩══╝\n"))
                (list (srcloc #f 3 2 17 1)
                      (srcloc #f 2 2 7 1)))
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦══╗\n"
                   "  ║ ═║  ║\n"
                   "  ╠══╬══╣\n"
                   "  ║  ║  ║\n"
                   "  ╚══╩══╝\n"))
                (list (srcloc #f 3 4 19 1)))
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦══╗\n"
                   " ║  ║  ║\n"
                   " ╠══╬══╣\n"
                   " ║  ║  ║\n"
                   " ╚══╩══╝\n"))
                (list (srcloc #f 3 1 16 1)))
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦══╗\n"
                   "  ║  ║  ║\n"
                   "  ╠══╬══\n"
                   "  ║  ║  ║\n"
                   "  ╚══╩══╝\n"))
                (list (srcloc #f 4 8 33 1)))
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦══╗\n"
                   "  ║  ║  ║\n"
                   "  ╠═\n"
                   "  ║  ║  ║\n"
                   "  ╚══╩══╝\n"))
                (list (srcloc #f 4 4 29 1)))
  (check-equal? (get-err-locs
                 '("#2d\n"
                   "  +----+\n"
                   "  |    |\n"
                   "  +----+\n"))
                (list (srcloc #f 2 2 7 1)))
  (check-equal? (get-err-locs
                 '("#2d\n"
                   "   \n"))
                (list (srcloc #f 2 0 5 3)))
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦══\n"))
                (list (srcloc #f 2 8 13 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦══╦═══╗\n"
                   "  ║  ║  ║   ║\n"
                   "  ╠══╬══╩═══╣\n"
                   "  ║  ║      ║\n"
                   "  ╠══╣  ═   ║\n"
                   "  ║  ║      ║\n"
                   "  ╚══╩══════╝\n"))
                (list (srcloc #f 6 8 69 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦══╦═══╗\n"
                   "  ║  ║  ║   ║\n"
                   "  ╠══╬══╩═══╣\n"
                   "  ║  ║      ║\n"
                   "  ╠══╬══╝═══╣\n"
                   "  ║  ║      ║\n"
                   "  ╚══╩══════╝\n")) 
                (list (srcloc #f 6 8 69 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦═══╦═══╗\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╬═══╬═══╣\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╣ ═ ╠═══╣\n"
                   "  ║  ║   ║   ║\n"
                   "  ╚══╩═══╩═══╝\n")) 
                (list (srcloc #f 6 7 72 1)
                      (srcloc #f 6 5 70 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦═══╦═══╗\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╬═══╬═══╣\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╬═ ═╬═══╣\n"
                   "  ║  ║   ║   ║\n"
                   "  ╚══╩═══╩═══╝\n")) 
                (list (srcloc #f 6 7 72 1)
                      (srcloc #f 6 5 70 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦═══╦═══╗\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╬═══╬═══╣\n"
                   "  ║  ║   ║   \n"
                   "  ╠══╬═══╬═══╣\n"
                   "  ║  ║   ║   ║\n"
                   "  ╚══╩═══╩═══╝\n")) 
                (list (srcloc #f 5 13 63 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦═══╦═══╗\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╩═══╬═══╣\n"
                   "  ║      ║   ║\n"
                   "  ║          ║\n"
                   "  ║      ║   ║\n"
                   "  ╠══╦═══╬═══╣\n"
                   "  ║  ║   ║   ║\n"
                   "  ╚══╩═══╩═══╝\n")) 
                (list (srcloc #f 6 9 74 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦═══╦═══╗\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╩═══╬═══╣\n"
                   "  ║      ║   ║\n"
                   "  ║  ╩   ║   ║\n"
                   "  ║      ║   ║\n"
                   "  ╠══╦═══╬═══╣\n"
                   "  ║  ║   ║   ║\n"
                   "  ╚══╩═══╩═══╝\n")) 
                (list (srcloc #f 6 5 70 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦═══╦═══╗\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╩═══╬═══╣\n"
                   "  ║      ║   ║\n"
                   "  ║   \n"
                   "  ║      ║   ║\n"
                   "  ╠══╦═══╬═══╣\n"
                   "  ║  ║   ║   ║\n"
                   "  ╚══╩═══╩═══╝\n")) 
                (list (srcloc #f 6 6 71 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦═══╦═══╗\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╩═══╬═══╣\n"
                   "  ║      ║   ║\n"
                   "  ╚══════╩═\n")) 
                (list (srcloc #f 6 11 76 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦═══╦═══╗\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╩═══╬═══╣\n"
                   "  ║      ║   ║\n"
                   "  ╚══════\n")) 
                (list (srcloc #f 6 9 74 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦═══╦═══╗\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╩═══╬═══╣\n"
                   "  ║      ║   ║\n"
                   "  ╚══════╩═══X\n")) 
                (list (srcloc #f 6 13 78 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦═══╦═══╗\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╩═══╬═══╣\n"
                   "  ║      ║   ║\n"
                   "  ╚══════╩══X╝\n")) 
                (list (srcloc #f 6 12 77 1)))
  
  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "  ╔══╦═══╦═══╗\n"
                   "  ║  ║   ║   ║\n"
                   "  ╠══╩═══╬═══╣\n"
                   "  ║      ║   ║     NOT WHITESPACE\n"
                   "  ╚══════╩═══╝\n")) 
                (list (srcloc #f 5 19 69 1)))

  (check-equal? (get-err-locs 
                 '("#2d\n"
                   "╔══╦-══╗\n"
                   "║  ║   ║\n"
                   "╠══╬-══╣\n"
                   "║  ║   ║\n"
                   "╚══╩-══╝\n"))
                (list (srcloc #f 2 4 9 1))))

(let ([lines (hash-map lines-table (λ (x y) x))])
  (unless (null? lines)
    (eprintf "no test case for errors on lines: ~s\n"
             (sort lines <))))


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
                     (cons (list 2 2) (set)))))

(check-equal? (get-graph
               '("  ╔══╦══╦══╦══╗\n"
                 "  ║1 ║2 ║3 ║4 ║\n"
                 "  ╠══╬══╩══╩══╣\n"
                 "  ║6 ║5       ║\n"
                 "  ╠══╣  ╔══╗  ║\n"
                 "  ║7 ║  ║10║  ║\n"
                 "  ╠══╣  ╚══╝  ║\n"
                 "  ║7 ║        ║\n"
                 "  ╚══╩════════╝\n"))
              (make-hash
               (list (cons (list 0 0) (set))
                     (cons (list 0 1) (set))
                     (cons (list 0 2) (set))
                     (cons (list 0 3) (set))
                     
                     (cons (list 1 0) (set))
                     (cons (list 1 1) (set (list 1 2) (list 2 1)))
                     (cons (list 1 2) (set (list 1 1) (list 1 3)))
                     (cons (list 1 3) (set (list 1 2) (list 2 3)))
                     
                     (cons (list 2 0) (set))
                     (cons (list 2 1) (set (list 1 1) (list 3 1)))
                     (cons (list 2 2) (set))
                     (cons (list 2 3) (set (list 1 3) (list 3 3)))
                     
                     (cons (list 3 0) (set))
                     (cons (list 3 1) (set (list 2 1) (list 3 2)))
                     (cons (list 3 2) (set (list 3 1) (list 3 3)))
                     (cons (list 3 3) (set (list 3 2) (list 2 3))))))

(check-equal? (get-all-lines '("  ╔══╦══╗\n"
                               "  ║1 ║  ║\r"
                               "  ╠══╬══╣\r\n"
                               "  ║2 ║  ║\r"
                               "  ╠══╬══╣\n"
                               "  ║3 ║  ║\n"
                               "  ╚══╩══╝\n"))
              '#(("  ╔══╦══╗\n"   "  ║1 ║  ║\r")
                 ("  ╠══╬══╣\r\n" "  ║2 ║  ║\r")
                 ("  ╠══╬══╣\n"   "  ║3 ║  ║\n")))

(check-equal? (get-table-column-breaks '("  ╔══╦══╗\n"
                                         "  ║1 ║  ║\n"
                                         "  ╠══╬══╣\n"
                                         "  ║2 ║  ║\n"
                                         "  ╠══╬══╣\n"
                                         "  ║3 ║  ║\n"
                                         "  ╚══╩══╝\n"))
              (list 2 2))
(check-equal? (get-initial-space-count '("  ╔══╦══╗\n"
                                         "  ║1 ║  ║\n"
                                         "  ╠══╬══╣\n"
                                         "  ║2 ║  ║\n"
                                         "  ╠══╬══╣\n"
                                         "  ║3 ║  ║\n"
                                         "  ╚══╩══╝\n"))
              2)

(check-equal? (close-cell-graph (make-hash) 2 2)
              (set (set (list 0 0))
                   (set (list 0 1))
                   (set (list 1 0))
                   (set (list 1 1))))

(check-equal? (close-cell-graph (make-hash 
                                 (list
                                  (cons (list 0 0) (set (list 0 1)))))
                     2 2)
              (set (set (list 0 0) (list 0 1))
                   (set (list 1 0))
                   (set (list 1 1))))


(check-equal? (close-cell-graph (make-hash 
                                 (list
                                  (cons (list 0 0) (set (list 0 1)))
                                  (cons (list 0 1) (set (list 1 1)))))
                     2 2)
              (set (set (list 0 0) (list 0 1) (list 1 1))
                   (set (list 1 0))))
(check-equal? (close-cell-graph (make-hash 
                                 (list
                                  (cons (list 0 0) (set (list 0 1)))
                                  (cons (list 0 1) (set (list 1 1)))
                                  (cons (list 1 1) (set (list 1 0)))))
                     2 2)
              (set (set (list 0 0) (list 0 1) (list 1 1) (list 1 0))))

(check-true  (compare/xy (list 0 0) (list 1 1)))
(check-false (compare/xy (list 1 1) (list 0 0)))
(check-true  (compare/xy (list 1 0) (list 1 1)))
(check-false (compare/xy (list 1 1) (list 1 0)))
(check-false (compare/xy (list 1 0) (list 1 0)))

(check-equal? (smallest-representative (set (list 0 0) (list 1 0) (list 0 1) (list 1 1)))
              (list 0 0))
(check-equal? (smallest-representative (set (list 1 1) (list 0 1) (list 1 0) (list 0 0)))
              (list 0 0))

(let ()
  (define scratch (string-copy "                                "))
  (fill-scratch-string (set '(0 0))
                       #(("╔══╦══╗\n" "║12║34║\n") ("╠══╬══╣\n" "║56║78║\n"))
                       scratch
                       '(2 2)
                       0)
  (check-equal? scratch
                "       \n 12    \n       \n       \n"))

(let ()
  (define scratch (string-copy "                                "))
  (fill-scratch-string (set '(1 0))
                       #(("╔══╦══╗\n" "║12║34║\n") ("╠══╬══╣\n" "║56║78║\n"))
                       scratch
                       '(2 2)
                       0)
  (check-equal? scratch
                "       \n    34 \n       \n       \n"))

(let ()
  (define scratch (string-copy "                                "))
  (fill-scratch-string (set '(0 1))
                       #(("╔══╦══╗\n" "║12║34║\n") ("╠══╬══╣\n" "║56║78║\n"))
                       scratch
                       '(2 2)
                       0)
  (check-equal? scratch
                "       \n       \n       \n 56    \n"))

(let ()
  (define scratch (string-copy "                                "))
  (fill-scratch-string (set '(1 1))
                       #(("╔══╦══╗\n" "║12║34║\n") ("╠══╬══╣\n" "║56║78║\n"))
                       scratch
                       '(2 2)
                       0)
  (check-equal? scratch
                "       \n       \n       \n    78 \n"))

(let ()
  (define scratch (string-copy "       \n    34 \n       \n       \n"))
  (fill-scratch-string (set '(1 0) '(1 1))
                       #(("╔══╦══╗\n" "║12║34║\n") ("╠══╣56║\n" "║78║90║\n"))
                       scratch
                       '(2 2)
                       0)
  (check-equal? scratch
                "       \n    34 \n    56 \n    90 \n"))

(let ()
  (define scratch (string-copy "       \n    34 \n       \n       \n"))
  (fill-scratch-string (set '(0 1) '(1 1))
                       #(("╔══╦══╗\n" "║12║34║\n") ("╠══╩══╣\n" "║56789║\n"))
                       scratch
                       '(2 2)
                       0)
  (check-equal? scratch
                "       \n       \n       \n 56789 \n"))

(let ()
  (define scratch (string-copy "       \n    34 \n       \n       \n"))
  (fill-scratch-string (set '(0 1) '(1 0) '(1 1))
                       #(("╔══╦══╗\n" "║12║34║\n") ("╠══╝56║\n" "║7890A║\n"))
                       scratch
                       '(2 2) 
                       0)
  (check-equal? scratch
                "       \n    34 \n    56 \n 7890A \n"))

(let ()
  (define scratch (string-copy "                                "))
  (fill-scratch-string (set '(0 0))
                       #(("╔═════╗\n" "║12345║\n" "║67890║\n" "║ABCDE║\n"))
                       scratch
                       '(5)
                       0)
  
  (check-equal? scratch
                "       \n 12345 \n 67890 \n ABCDE \n"))

(let ()
  (define scratch (make-string 66 #\space))
  (fill-scratch-string (set '(1 2) '(1 1) '(2 2) '(2 1))
                       #(("╔══╦══╦══╗\n" "║12║34║56║\n")
                         ("╠══╬══╩══╣\n" "║78║90ABC║\n")
                         ("╠══╣DEFGH║\n" "║IJ║KLMNO║\n"))
                       scratch
                       '(2 2 2)
                       0)
  
  (check-equal? scratch
                "          \n          \n          \n    90ABC \n    DEFGH \n    KLMNO \n"))

(let ()
  (define scratch (make-string 120 #\space))
  (fill-scratch-string 
   (set '(1 2) '(1 3) '(2 3))
   #(("╔═╦════╦═════╗\n" "║1║2345║67890║\n")
     ("╠═╬════╩═════╣\n" "║a║bcdefghijk║\n")
     ("╠═╬════╗lmnop║\n" "║q║rstu║vwxyz║\n")
     ("╠═╣ABCD╚═════╣\n" "║E║FGHIJKLMNO║\n"))
   scratch
   '(1 4 5)
   0)
  (check-equal? (string-append
                 "              \n"
                 "              \n"
                 "              \n"
                 "              \n"
                 "              \n"
                 "   rstu       \n"
                 "   ABCD       \n"
                 "   FGHIJKLMNO \n")
                scratch))

(let ()
  (define scratch (make-string 495 #\space))
  (fill-scratch-string
   (set '(1 2) '(1 3) '(1 4) '(2 3) '(2 4) '(3 4))
   #(("╔════════╦════╦═════╦════════╦═════════════╗\n"
      "║        ║'Int║'Real║'Complex║ `(-> ,c ,d) ║\n")
     ("╠════════╬════╩═════╩════════╬═════════════╣\n"
      "║'Int    ║                   ║             ║\n")
     ("╠════════╬════╗      #t      ║             ║\n"
      "║'Real   ║    ║              ║      #f     ║\n")
     ("╠════════╣    ╚═════╗        ║             ║\n" 
      "║'Complex║          ║        ║             ║\n")
     ("╠════════╣          ╚════════╬═════════════╣\n"
      "║`(-> ,a ║    #f             ║(and (≤ c a) ║\n"
      "║     ,b)║                   ║     (≤ b d))║\n"))
   scratch
   '(8 4 5 8 13)
   0)
  ;; just make sure there are no border characters in there.
  (check-regexp-match #rx"^[\n#f ]*$" scratch))

(let ()
  (define str (make-string 84 #\space))
  (fill-scratch-string (set '(1 2) '(1 1) '(2 1))
                       '#(("╔═══╦═══╦═══╗\n" 
                           "║   ║ a ║ b ║\n")
                          ("╠═══╬═══╩═══╣\n"
                           "║ c ║ 1     ║\n")
                          ("╠═══╣   ╔═══╣\n"
                           "║ d ║   ║ 2 ║\n"))
                       str
                       '(3 3 3)
                       0)
  (check-equal? str
                "             \n             \n             \n      1      \n             \n             \n"))

(let ()
  (define scratch (string-copy "                                "))
  (check-equal? (fill-scratch-string (set '(0 0))
                                     #(("╔══╦══╗\n" "║12║34║\n") ("╠══╬══╣\n" "║56║78║\n"))
                                     scratch
                                     '(2 2)
                                     0
                                     #t)
                '((10 . 12))))

(let ()
  (define sp (open-input-string 
              (string-append "#(#2d\n"
                             "  ╔══╦══╗\n"
                             "  ║1 ║2 ║\n"
                             "  ╠══╬══╣\n"
                             "  ║4 ║3 ║\n"
                             "  ╚══╩══╝)1\n")))
  (parameterize ([current-readtable (make-2d-readtable)])
    (check-true (vector? (read sp)))
    (check-equal? (read sp) 1)))


(let ()
  (define sp (open-input-string "#2dwhatever\n"))
  (port-count-lines! sp)
  (define exn
    (with-handlers ((exn:fail:read:eof? values))
      (parameterize ([current-readtable (make-2d-readtable)])
        (read sp))))
  (check-regexp-match #rx"expected eof" (exn-message exn))
  (check-equal? (exn:fail:read-srclocs exn)
                (list (srcloc #f 1 0 1 12))))
