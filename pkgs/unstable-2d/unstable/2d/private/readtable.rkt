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

(require "read-util.rkt"
         racket/set
         ;syntax/rect
         "../dir-chars.rkt"
         racket/port)


(provide make-2d-readtable)

(define (make-2d-readtable)
  (define previous-readtable (current-readtable))
  (make-readtable 
   previous-readtable 
   #\2
   'dispatch-macro 
   (case-lambda
     [(char port)
      (define-values (line col pos) (port-next-location port))
      
      ;; the "-2"s here are because the initial line and column
      ;; are supposed be at the beginning of the thing read, not
      ;; after the "#2" has been consumed.
      (dispatch-proc char port #f line 
                     (and col (- col 2))
                     (and pos (- pos 2))
                     read/recursive previous-readtable)]
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
     (define-values (cell-connections 
                     lines 
                     table-column-breaks 
                     initial-space-count 
                     position-of-first-cell)
       (parse-2dcond port source _line _col _pos chars-read))
     (define lhses (close-cell-graph cell-connections 
                                     (length table-column-breaks)
                                     (vector-length lines)))
     (define scratch-string (make-string (for/sum ([ss (in-vector lines)])
                                           (for/sum ([s (in-list ss)])
                                             (string-length s)))
                                         #\space))
     
     (define heights
       (for/list ([line (in-vector lines)])
         (length line)))
     
     (define kwd-str (string-append "2d" (apply string kwd-chars)))
     (define kwd-port (open-input-string kwd-str))
     (port-count-lines! kwd-port)
     (set-port-next-location! kwd-port _line (and _col (+ _col 1)) (and _pos (+ _pos 1)))
     (define kwd-stx (read-syntax source kwd-port))
     
     (define line-width (+ initial-space-count 
                           (apply + table-column-breaks)
                           (max 0 (- (length table-column-breaks) 1))))
     
     (define (add-srclocs indicies)
       (for/list ([index (in-list indicies)])
         (define srcloc (hash-ref position-of-first-cell index))
         (datum->syntax #f
                        index
                        (vector (srcloc-source srcloc)
                                #f ;; line
                                #f ;; col
                                (srcloc-position srcloc)
                                1))))
     
     `(,kwd-stx
       
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
           `[,(add-srclocs (sort (set->list set-of-indicies) compare/xy))
             ,@(read-subparts source scratch-port 
                              initial-space-count table-column-breaks heights set-of-indicies
                              /recursive)]))]
    [else
     (/recursive 
      (input-port-append #f (open-input-string "#2") port)
      #f
      previous-readtable)]))


(define (read-subparts source scratch-port 
                       initial-space-count table-column-breaks heights lhs
                       /recursive)
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
      (define o (/recursive scratch-port #f (current-readtable)))
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

