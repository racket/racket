(module size mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define line-size 80)
  (define (build-a-str n)
    (list->string (let loop ([n n])
                    (cond
                      [(zero? n) (list #\newline)]
                      [else (cons #\a (loop (sub1 n)))]))))
  (define line (build-a-str (sub1 line-size)))
  (define html-overhead 68)
  (define (start initial-request)
    (define bindings (request-bindings initial-request))
    (define size (- (string->number (cdr (assq 'size bindings))) html-overhead))
    (define nlines (quotient size line-size))
    (define extra (remainder size line-size))
    
    `(html (head (title "A Page"))
           (body (p ,@(vector->list (make-vector nlines line))
                    ,(build-a-str extra))))))