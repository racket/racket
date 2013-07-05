(module lw-test-util mzscheme
  (require redex/private/loc-wrapper)
  (provide normalize-lw)
  
  (define (normalize-lw lw)
    (define-values (min-line min-column) (find-min-line/col lw))
    (define (normalize/lw lw)
      (cond
        [(lw? lw)
         (make-lw (normalize/e (lw-e lw))
                  (- (lw-line lw) min-line)
                  (lw-line-span lw)
                  (- (lw-column lw) min-column)
                  (lw-column-span lw)
                  (lw-unq? lw)
                  (lw-metafunction? lw))]
        [else lw]))
    (define (normalize/e e)
      (cond
        [(symbol? e) e]
        [(string? e) e]
        [else (map normalize/lw e)]))
    (normalize/lw lw))
  
  (define (find-min-line/col lw)
    (define min-line #f)
    (define min-col #f)
    (define (find-min/lw lw)
      (when (lw? lw)
        (set! min-line (if min-line
                           (min min-line (lw-line lw))
                           (lw-line lw)))
        (set! min-col (if min-col
                          (min min-col (lw-column lw))
                          (lw-column lw)))
        (find-min/e (lw-e lw))))
    (define (find-min/e e)
      (cond
        [(symbol? e) (void)]
        [(string? e) (void)]
        [else (for-each find-min/lw e)]))
    (find-min/lw lw)
    (values min-line min-col)))
