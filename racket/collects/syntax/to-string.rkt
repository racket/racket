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
            (for-each (λ (_) (newline)) (range (- l line)))
            (set! line l)
            (init-line!))
          (when c
            (display (make-string (max 0 (- c col)) #\space))
            (set! col c))))
      (define quotes-table
        #hasheq((quote . "'")
                (quasiquote . "`")
                (unquote . ",")
                (unquote-splicing . ",@")
                (syntax . "#'")
                (quasisyntax . "#`")
                (unsyntax . "#,")
                (unsyntax-splicing . "#,@")))
      (define (get-quote c)
        (hash-ref quotes-table (syntax-e (car (syntax-e c)))))
      (parameterize ([current-output-port s]
                     [read-case-sensitive #t])
        (define (loop init-line!)
          (lambda (c)
            (cond
              [(eq? 'code:blank (syntax-e c))
               (advance c init-line!)]
              [(eq? '_ (syntax-e c)) 
               (advance c init-line!)
               (printf "_")
               (set! col (+ col 1))]
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
                    (hash-has-key? quotes-table (syntax-e (car (syntax-e c))))
                    (eq? (syntax-span (car (syntax-e c)))
                         (string-length (get-quote c))))
               ;; The above conditions detect the shorthand form of quote and friends
               ;; The shorthand form will read, for instance, '<form>
               ;; as (quote <form>), so the result is guaranteed to be a syntax list
               ;; with exactly two elements in it.
               (advance c init-line!)
               (printf (get-quote c))
               (set! col (+ col (string-length (get-quote c))))
               (let ([i (cadr (syntax->list c))])
                 ((loop init-line!) i))]
              [(pair? (syntax-e c))
               (advance c init-line!)
               (define c-paren-shape (syntax-property c 'paren-shape))
               (printf "~a" (or c-paren-shape #\())
               (set! col (+ col 1))
               (define se (syntax-e c))
               (define (build-string-from-pair sp)
                 (cond
                   [(syntax? sp)
                    (printf " . ")
                    (set! col (+ col 3))
                    ((loop init-line!) sp)]
                   [else
                    ((loop init-line!) (car sp))
                    (build-string-from-pair (cdr sp))]))
               (if (list? se)
                   (map (loop init-line!) se)
                   (build-string-from-pair se))
               (printf (case c-paren-shape
                         [(#\[) "]"]
                         [(#\{) "}"]
                         [else ")"]))
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
