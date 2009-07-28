#lang scheme/base
(require "scheme-lexer.ss")

(provide scribble-inside-lexer
         scribble-lexer)

(define-struct text (scheme-rx end-rx sub-rx string-rx open-paren close-paren))
(define-struct scheme (status))
(define-struct args ())
(define-struct text-args ())

(define (scribble-inside-lexer in mode)
  (let ([mode (or mode
                  (list
                   (make-text #rx"^@"
                              #f
                              #f
                              #rx".*?(?:(?=[@\r\n])|$)"
                              #f
                              #f)))])
  (let-values ([(line col pos) (port-next-location in)]
               [(l) (car mode)])
    (define (enter-@ comment-k)
      (if (equal? #\; (peek-char in))
          ;; Comment
          (begin
            (read-char in)
            (if (or (equal? #\{ (peek-char in))
                    (equal? #\| (peek-char in)))
                ;; Bracketed comment:
                (let-values ([(end-line end-col end-pos) (port-next-location in)])
                  (comment-k "@;"
                             'comment
                             #f
                             pos
                             end-pos
                             (cons (make-text-args)
                                   mode)))
                ;; Line comment:
                (begin
                  (regexp-match? #rx".*?(?=[\r\n])" in)
                  (let-values ([(end-line end-col end-pos) (port-next-location in)])
                    (comment-k "@;"
                               'comment
                               #f
                               pos
                               end-pos
                               mode)))))
          (let ([new-mode
                 (cond
                  [(equal? #\| (peek-char in))
                   (read-char in)
                   (list* (make-scheme 'bar)
                          mode)]
                  [else
                   (list* (make-scheme 'one)
                          (make-args)
                          mode)])])
            (let-values ([(end-line end-col end-pos) (port-next-location in)])
              (values "@"
                      'other
                      #f
                      pos
                      end-pos
                      new-mode)))))

    (if (eof-object? (peek-char in))
        (values eof
                'eof
                #f
                pos 
                pos
                #f)
        (cond
         [(text? l)
          (cond
           [(and (text-scheme-rx l)
                 (regexp-try-match (text-scheme-rx l) in))
            ;; Found @
            (enter-@ values)]
           [(and (text-end-rx l)
                 (regexp-try-match (text-end-rx l) in))
            (let-values ([(end-line end-col end-pos) (port-next-location in)])
              (values "}"
                      'other
                      (text-close-paren l)
                      pos
                      end-pos
                      (cdr mode)))]
           [(and (text-sub-rx l)
                 (regexp-try-match (text-sub-rx l) in))
            (let-values ([(end-line end-col end-pos) (port-next-location in)])
              (values "{"
                      'other
                      (text-open-paren l)
                      pos
                      end-pos
                      (cons (car mode) mode)))]
           [(regexp-try-match #px"^(?:[\r\n])\\s*" in)
            ;; Treat a newline and leading whitespace in text mode as whitespace
            ;; instead of as a string:
            (let-values ([(end-line end-col end-pos) (port-next-location in)])
              (values " "
                      'white-space
                      #f
                      pos
                      end-pos
                      mode))]
           [else
            ;; Read string up to @, }, or newline
            (regexp-match? (text-string-rx l) in)
            (let-values ([(end-line end-col end-pos) (port-next-location in)])
              (values 'string
                      'string
                      #f
                      pos
                      end-pos
                      mode))])]
         [(scheme? l)
          (let ([status (scheme-status l)])
            (cond
             [(and (eq? status 'bracket)
                   (regexp-try-match #px"^\\s*?[]]" in))
              (let-values ([(end-line end-col end-pos) (port-next-location in)])
                (values "]"
                        'other
                        '|]|
                        pos
                        end-pos
                        (cdr mode)))]
             [(and (memq status '(bar bar-no-more))
                   (regexp-try-match #px"^\\s*?[|]" in))
              (let-values ([(end-line end-col end-pos) (port-next-location in)])
                (values "|"
                        'other
                        #f
                        pos
                        end-pos
                        (cdr mode)))]
             [(regexp-try-match #rx"^@" in)
              (enter-@ (lambda (lexeme type paren start end mode)
                         (values lexeme
                                 (if (eq? status 'one)
                                     'error
                                     type)
                                 paren
                                 start
                                 end
                                 mode)))]
             [else
              (let-values ([(lexeme type paren start end adj) 
                            (case status
                              [(bar bar-no-more one) (scheme-nobar-lexer/status in)]
                              [else (scheme-lexer/status in)])]
                           [(consume) (lambda (status)
                                        (case status
                                          [(many) mode]
                                          [(one) (cdr mode)]
                                          [(bracket bar-no-more) 
                                           (cons (make-scheme status) (cdr mode))]
                                          [(bar) (cons (make-scheme 'bar-no-more) (cdr mode))]
                                          [else (error "bad status")]))])
                (values lexeme
                        (cond
                         [(or (eq? type 'comment) 
                              (eq? type 'white-space))
                          (if (eq? status 'one)
                              'error
                              type)]
                         [(eq? status 'bar-no-more) 
                          ;; Too many S-expressions in @| ... |
                          'error]
                         [else type])
                        paren 
                        start 
                        end
                        (case adj
                          [(continue) mode]
                          [(datum) 
                           (cond
                            [(pair? status) mode]
                            [else (consume status)])]
                          [(open)
                           (cons (make-scheme (cons #t status))
                                 (cdr mode))]
                          [(close)
                           (if (pair? status)
                               (let ([v (cdr status)])
                                 (if (symbol? v)
                                     (consume v)
                                     (cons (make-scheme v) (cdr mode))))
                               (consume status))]
                          [(bad) (consume status)]
                          [else (error "bad adj")])))]))]
         [(args? l)
          (cond
           [(regexp-try-match #rx"^\\[" in)
            (let-values ([(end-line end-col end-pos) (port-next-location in)])
              (values "["
                      'other
                      '|[|
                      pos
                      end-pos
                      (list* (make-scheme 'bracket)
                             mode)))]
           [else
            (scribble-lexer in (cons (make-text-args) (cdr mode)))])]
         [(text-args? l)
          (cond
           [(regexp-try-match #rx"^[|]([^a-zA-Z0-9 \t\r\n\f@\\\177-\377{]*){" in)
            => (lambda (m)
                 (let-values ([(end-line end-col end-pos) (port-next-location in)])
                   (values (cadr m)
                           'other
                           #f
                           pos
                           end-pos
                           (let ([closer (regexp-quote
                                          (bytes-append #"}"
                                                        (regexp-replace** (list #rx"[(]" #rx"[[]" #rx"{" #rx"<")
                                                                          (cadr m)
                                                                          (list #")" #"]" #"}" #">"))
                                                        #"|"))]
                                 [re-opener (regexp-quote (cadr m))])
                             (cons (make-text (byte-regexp (bytes-append #"^[|]" re-opener #"@"))
                                              (byte-regexp (bytes-append #"^" closer))
                                              (byte-regexp (bytes-append #"^[|]" re-opener #"{"))
                                              (byte-regexp (bytes-append
                                                            #".*?(?:(?=[|]"
                                                            re-opener
                                                            #"[@{])|(?="
                                                            closer
                                                            #")|(?=[\r\n])|$)"))
                                              #f
                                              #f)
                                   (cdr mode))))))]
           [(regexp-try-match #rx"^{" in)
            (let-values ([(end-line end-col end-pos) (port-next-location in)])
              (values "{"
                      'other
                      '|{|
                      pos
                      end-pos
                      (cons (make-text #rx"^@"
                                       #rx"^}"
                                       #rx"^{"
                                       #rx".*?(?:(?=[@{}\r\n])|$)"
                                       '|{|
                                       '|}|)
                            (cdr mode))))]
           [else
            (scribble-lexer in (cdr mode))])]
         [else (error "bad mode")])))))

(define (scribble-lexer in mode)
  (scribble-inside-lexer in (or mode (list (make-scheme 'many)))))

(define (regexp-replace** rxs str strs)
  (if (null? rxs)
      str
      (regexp-replace** (cdr rxs)
                        (regexp-replace* (car rxs) str (car strs))
                        (cdr strs))))
