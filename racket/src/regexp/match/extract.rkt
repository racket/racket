#lang racket/base

;; Helpers to extract position, bytes, and string results from a match
;; result.

(provide byte-positions->byte-positions
         byte-positions->bytess
         
         byte-positions->string-positions
         byte-positions->strings

         byte-index->string-index
         
         add-end-bytes)

(define (byte-positions->byte-positions ms-pos me-pos state
                                        #:delta [delta 0])
  (cond
   [(not state)
    (list (cons (+ ms-pos delta) (+ me-pos delta)))]
   [(zero? delta)
    (cons (cons ms-pos me-pos) (vector->list state))]
   [else
    (cons (cons (+ ms-pos delta) (+ me-pos delta))
          (for/list ([p (in-vector state)])
            (and p
                 (cons (+ (car p) delta)
                       (+ (cdr p) delta)))))]))

(define (byte-positions->bytess in ms-pos me-pos state
                                #:delta [delta 0])
  (cons (subbytes in (- ms-pos delta) (- me-pos delta))
        (if state
            (for/list ([p (in-vector state)])
              (and p
                   (subbytes in (- (car p) delta) (- (cdr p) delta))))
            null)))

(define (byte-positions->string-positions bstr-in ms-pos me-pos state
                                          #:start-index [start-index 0]
                                          #:delta [delta 0]
                                          #:result-offset [result-offset 0])
  (define (string-offset pos)
    (+ result-offset (bytes-utf-8-length bstr-in #\? start-index (- pos delta))))
  (cons (cons (string-offset ms-pos) (string-offset me-pos))
        (if state
            (for/list ([p (in-vector state)])
              (and p
                   (cons (string-offset (car p))
                         (string-offset (cdr p)))))
            null)))

(define (byte-positions->strings bstr-in ms-pos me-pos state
                                 #:delta [delta 0])
  (cons (bytes->string/utf-8 bstr-in #\? (- ms-pos delta) (- me-pos delta))
        (if state
            (for/list ([p (in-vector state)])
              (and p
                   (bytes->string/utf-8 bstr-in #\? (- (car p) delta) (- (cdr p) delta))))
            null)))

(define (byte-index->string-index str start-pos pos)
  ;; We assume that pos is on a code-point boundary in the
  ;; UTF-8 encoding of str. Find out how many code points
  ;; are before the index.
  (let loop ([lo-pos 0] [lo 0] [hi (min (- (string-length str) start-pos)
                                        (* pos 6))])
    (cond
      [(= lo hi) lo]
      [(= (add1 lo) hi)
       (if (= lo-pos pos) lo hi)]
      [else
       (define mid (quotient (+ lo hi) 2))
       (define len (string-utf-8-length str (+ start-pos lo) (+ start-pos mid)))
       (define mid-pos (+ lo-pos len))
       (cond
         [(= mid-pos pos) mid]
         [(mid-pos . > . pos) (loop lo-pos lo mid)]
         [else (loop (+ lo-pos len) mid hi)])])))

;; For functions like `regexp-match/end`:
(define (add-end-bytes results end-bytes-count bstr me-pos)
  (if end-bytes-count
      (values results
              (and results
                   (subbytes bstr (max 0 (- me-pos end-bytes-count)) me-pos)))
      results))
