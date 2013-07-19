(module to-string racket/base
  (require racket/contract/base
           syntax/stx)
  
  (require racket/list)

  (define (syntax->string c)
    (let* ([s (open-output-string)]
           [l (syntax->list c)]
           [init-col (or (syntax-column (first l)) 0)]
           [col init-col]
           [line (or (syntax-line (first l)) 0)])
      (define (advance c init-line!)
        (let ([c (syntax-column c)]
              [l (syntax-line c)])
          (when (and l (l . > . line))
            (newline)
            (set! line l)
            (init-line!))
          (when c
            (display (make-string (max 0 (- c col)) #\space))
            (set! col c))))
      (parameterize ([current-output-port s]
                     [read-case-sensitive #t])
        (define (loop init-line!)
          (lambda (c)
            (cond
              [(eq? 'code:blank (syntax-e c))
               (advance c init-line!)]
              [(eq? '_ (syntax-e c)) (void)]
              [(eq? '... (syntax-e c))
               (void)]
              [(and (pair? (syntax-e c))
                    (eq? (syntax-e (car (syntax-e c))) 'code:comment))
               (advance c init-line!)
               (printf "; ")
               (display (syntax-e (cadr (syntax->list c))))]
              [(and (pair? (syntax-e c))
                    (eq? (syntax-e (car (syntax-e c))) 'code:contract))
               (advance c init-line!)
               (printf "; ")
               (let* ([l (cdr (syntax->list c))]
                      [s-col (or (syntax-column (first l)) col)])
                 (set! col s-col)
                 (for-each (loop (lambda ()
                                   (set! col s-col)
                                   (printf "; ")))
                           l))]
              [(and (pair? (syntax-e c))
                    (eq? (syntax-e (car (syntax-e c))) 'quote))
               (advance c init-line!)
               (printf "'")
               (let ([i (cadr (syntax->list c))])
                 (set! col (or (syntax-column i) col))
                 ((loop init-line!) i))]
              [(pair? (syntax-e c))
               (advance c init-line!)
               (printf "(")
               (set! col (+ col 1))
               (map (loop init-line!) (syntax->list c))
               (printf ")")
               (set! col (+ col 1))]
              [(vector? (syntax-e c))
               (advance c init-line!)
               (printf "#(")
               (set! col (+ col 2))
               (map (loop init-line!) (vector->list (syntax-e c)))
               (printf ")")
               (set! col (+ col 1))]
              [else
               (advance c init-line!)
               (let ([s (format "~s" (syntax-e c))])
                 (set! col (+ col (string-length s)))
                 (display s))])))
        (for-each (loop (lambda () (set! col init-col))) l))
      (get-output-string s)))
  
  (provide/contract [syntax->string (-> (and/c syntax? stx-list?)
                                        string?)]))
