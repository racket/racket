#lang racket/base
(require racket/contract/base
         unstable/options)
(provide lexer/c (struct-out dont-stop))

(struct dont-stop (val) #:transparent)

(define lexer/c
  (option/c
   (or/c (->i ([in (and/c input-port? port-counts-lines?)])
              (values [txt any/c]
                      [type symbol?]
                      [paren (or/c symbol? #f)]
                      [start (or/c exact-positive-integer? #f)]
                      [end (start type) (end/c start type)]))
         (->i ([in (and/c input-port? port-counts-lines?)]
               [offset exact-nonnegative-integer?]
               [mode (not/c dont-stop?)])
              (values [txt any/c]
                      [type symbol?]
                      [paren (or/c symbol? #f)]
                      [start (or/c exact-positive-integer? #f)]
                      [end (start type) (end/c start type)]
                      [backup exact-nonnegative-integer?]
                      [new-mode any/c])))
   #:tester (λ (lexer) (try-some-random-streams lexer))))

(define (try-some-random-streams lexer)
  (define 3ary-lexer
    (cond
      [(procedure-arity-includes? lexer 1)
       (λ (in offset mode)
         (define-values (txt type paren start end) (lexer in))
         (values txt type paren start end 0 #f))]
      [else lexer]))
  (for ([x (in-range 10)])
    (define size (random 100))
    (define (quash-backslash-r c)
      ;; it isn't clear the spec is right in
      ;; the case of \r\n combinations, so we
      ;; punt for now
      (if (equal? c #\return) #\newline c))
    (define s (build-string
               size
               (λ (c) 
                 (quash-backslash-r
                  (case (random 3)
                    [(0)
                     (define s " ()@{}\"λΣ\0")
                     (string-ref s (random (string-length s)))]
                    [(1 2)
                     (integer->char (random 255))])))))
    (define in (open-input-string s))
    (port-count-lines! in)
    (let loop ([mode #f][offset 0])
      (define-values (txt type paren start end backup new-mode)
        (3ary-lexer in offset mode))
      (cond
        [(equal? type 'eof) #t]
        [(< end size) (loop new-mode end)]
        [else #f]))))

(define (end/c start type)
  (cond
    [(equal? 'eof type) 
     (or/c exact-positive-integer? #f)]
    [start
     (and/c exact-positive-integer?
            (>/c start))]
    [else
     #f]))
