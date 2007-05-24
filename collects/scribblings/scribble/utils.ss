
(module utils mzscheme
  (require (lib "struct.ss" "scribble")
           (lib "manual.ss" "scribble")
           (prefix scheme: (lib "scheme.ss" "scribble"))
           (prefix scribble: (lib "reader.ss" "scribble"))
           (lib "string.ss"))

  (provide at
           litchar/lines
           scribble-examples)

  (define at "@")

  (define (litchar/lines s)
    (let ([strs (regexp-split #rx"\n" s)])
      (if (= 1 (length strs))
          (litchar s)
          (make-table
           #f
           (map (lambda (s)
                  (list (make-flow (list (make-paragraph (list (litchar s)))))))
                strs)))))

  (define (as-flow e)
    (make-flow (list (if (flow-element? e)
                         e
                         (make-paragraph (list e))))))

  (define spacer (hspace 2))

  (define ((norm-spacing base) p)
    (cond
     [(and (syntax->list p)
           (not (null? (syntax-e p))))
      (let loop ([e (syntax->list p)]
                 [line (syntax-line (car (syntax-e p)))]
                 [pos base]
                 [second #f]
                 [accum null])
        (cond
         [(null? e)
          (datum->syntax-object
           p
           (reverse accum)
           (list (syntax-source p)
                 (syntax-line p)
                 base
                 (add1 base)
                 (- pos base))
           p)]
         [else
          (let* ([v ((norm-spacing (if (= line (syntax-line (car e)))
                                       pos
                                       (or second pos)))
                     (car e))]
                 [next-pos (+ (syntax-column v) (syntax-span v) 1)])
            (loop (cdr e)
                  (syntax-line v)
                  next-pos
                  (or second next-pos)
                  (cons v accum)))]))]
     [else
      (datum->syntax-object
       p
       (syntax-e p)
       (list (syntax-source p)
             (syntax-line p)
             base
             (add1 base)
             1)
       p)]))

  (define (scribble-examples . lines)
    (make-table
     #f
     (map (lambda (line)
            (let ([line (if (string? line)
                            (list (litchar/lines line)
                                  (scheme:to-paragraph
                                   (let ([p (open-input-string line)])
                                     (port-count-lines! p)
                                     (if (regexp-match? #rx"\n" line)
                                         ((norm-spacing 0) (scribble:read-syntax #f p))
                                         (scribble:read p)))))
                            line)])
              (list (as-flow spacer)
                    (as-flow (if line (car line) ""))
                    (as-flow (if line (make-paragraph (list spacer "reads as" spacer)) ""))
                    (as-flow (if line (cadr line) "")))))
          lines))))
