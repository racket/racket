(require (lib "unitsig.ss"))
(require (lib "servlet-sig.ss" "web-server"))

(let* ([line-size 80]
       [build-a-str
        (lambda (n)
          (list->string (let loop ([n n])
                          (cond
                            [(zero? n) (list #\newline)]
                            [else (cons #\a (loop (sub1 n)))]))))]
       [line (build-a-str (sub1 line-size))]
       [html-overhead 68])
  (unit/sig ()
    (import servlet^)
    
    (define size (- (string->number (cdr (assq 'size bindings))) html-overhead))
    (define nlines (quotient size line-size))
    (define extra (remainder size line-size))
    
    `(html (head (title "A Page"))
           (body (p ,@(vector->list (make-vector nlines line))
                    ,(build-a-str extra))))))
