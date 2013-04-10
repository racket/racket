#lang racket

(define ht (make-hash))

(for ([lcp (current-library-collection-paths)])
  (when (directory-exists? lcp)
    (for ([coll (in-list (directory-list lcp))])
      (define coll-path (build-path lcp coll))
      (when (directory-exists? coll-path)
        (hash-set! ht 
                   (path->string coll)
                   (for/sum ([file (in-directory (build-path lcp coll))]
                             #:when (regexp-match? #rx"[.]zo$" (path->string file)))
                     (file-size file)))))))

(define total (for/sum ([(k v) (in-hash ht)]) v))
(printf "The 'time' line is to trick drdr into graphing\n")
(printf "the size of all of the .zo files, in bytes\n")
(printf "cpu time: ~a real time: ~a gc time: ~a\n" total total total)
(printf "This is the breakdown by collection\n")
(sort (hash-map ht list)
      string<=?
      #:key car)
