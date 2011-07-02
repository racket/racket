#lang scheme/base
(require "../core.rkt")

(provide part-style?
         select-suffix
         extract-table-cell-styles
         empty-content?)

(define (part-style? p s)
  (memq s (style-properties (part-style p))))

(define (select-suffix path suggested-suffixes accepted-suffixes)
  (or (ormap (lambda (suggested)
               (and (member suggested accepted-suffixes)
                    (let ([p (bytes->path 
                              (bytes-append (path->bytes (if (string? path)
                                                             (string->path path)
                                                             path))
                                            (string->bytes/utf-8 suggested)))])
                      (and (file-exists? p)
                           p))))
             suggested-suffixes)
      path))

(define (extract-table-cell-styles t)
  (let ([vars (style-properties (table-style t))])
    (or (let ([l (ormap (lambda (v)
                          (and (table-cells? v)
                               (table-cells-styless v)))
                        vars)])
          (and l
               (unless (= (length l) (length (table-blockss t)))
                 (error 'table 
                        "table-cells property list's length does not match row count: ~e vs. ~e"
                        l (length (table-blockss t))))
               (for-each (lambda (l row)
                           (unless (= (length l) (length row))
                             (error 'table
                                    "table-cells property list contains a row whose length does not match the content: ~e vs. ~e"
                                    l (length row))))
                         l (table-blockss t))
               l))
        (let ([cols (ormap (lambda (v) (and (table-columns? v) v)) vars)])
          (and cols
               (let ([cols (table-columns-styles cols)])
                 (map (lambda (row)
                        (unless (= (length cols) (length row))
                          (error 'table
                                 "table-columns property list's length does not match a row length: ~e vs. ~e"
                                 cols (length row)))
                        cols)
                      (table-blockss t)))))
        (map (lambda (row) (map (lambda (c) plain) row)) (table-blockss t)))))

(define (empty-content? c) (null? c))
