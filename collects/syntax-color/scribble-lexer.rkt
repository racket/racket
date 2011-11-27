#lang scheme/base
(require "scheme-lexer.rkt"
         racket/port)

(provide scribble-inside-lexer
         scribble-lexer)

(define-struct text (scheme-rx end-rx sub-rx string-rx open-paren close-paren) #:transparent)
(define-struct scheme (status backup) #:transparent)
(define-struct args () #:transparent)
(define-struct text-args () #:transparent)

(define rx:opener #rx"^[|]([^a-zA-Z0-9 \t\r\n\f@\\\177-\377{]*){")

(define rxes (make-weak-hash))
(define rx-keys (make-weak-hasheq))

(define (intern-byte-regexp bstr)
  (let ([v (hash-ref rxes bstr #f)])
    (or v
        (let ([rx (byte-regexp bstr)])
          (hash-set! rxes bstr rx)
          (hash-set! rx-keys rx (make-ephemeron rx bstr))
          rx))))

(define (scribble-inside-lexer orig-in offset mode)
  (let ([mode (or mode
                  (list
                   (make-text #rx"^@"
                              #f
                              #f
                              #rx".*?(?:(?=[@\r\n])|$)"
                              #f
                              #f)))]
        [in (special-filter-input-port orig-in
                                       (lambda (v s)
                                         (bytes-set! s 0 (char->integer #\.))
                                         1))])
    (let-values ([(line col pos) (port-next-location orig-in)])
      (when line 
        (port-count-lines! in)))
  (let-values ([(line col pos) (port-next-location in)]
               [(l) (car mode)])

    ;; If we don't match rx:opener in a place where we might otherwise
    ;;  match, and there is a "|" at that point, then a change later
    ;;  could turn the non-match into a match, AND there could be
    ;;  arbitrarily many Scheme tokens in between. So we carry the backup
    ;;  position, use it as necessary (from places that might be between a "|"
    ;;  and a potential match creator), and cancel it when it's clearly
    ;;  not needed anymore (which includes after any token that isn't a 
    ;;  Scheme token).
    (define (backup-if-needed pos)
      (if (and (scheme? (car mode))
               (scheme-backup (car mode)))
          (- (+ pos offset) (scheme-backup (car mode)))
          0))
    (define (no-backup mode)
      (if (and (scheme? (car mode))
               (scheme-backup (car mode)))
          (cons (make-scheme (scheme-status (car mode)) #f)
                (cdr mode))
          mode))
    (define (maybe-no-backup type mode)
      (if (eq? type 'white-space)
          ;; white space definitely ends the need for backup
          (no-backup mode)
          mode))
    
    (define (enter-@ comment-k)
      (cond
       [(equal? #\; (peek-char-or-special in))
        ;; Comment
        (read-char in)
        (if (or (equal? #\{ (peek-char-or-special in))
                (equal? #\| (peek-char-or-special in)))
            ;; Bracketed comment:
            (let-values ([(end-line end-col end-pos) (port-next-location in)])
              (comment-k "@;"
                         'comment
                         #f
                         pos
                         end-pos
                         (backup-if-needed pos)
                         (cons (make-text-args)
                               (no-backup mode))))
            ;; Line comment:
            (begin
              (regexp-match? #rx".*?(?=[\r\n])" in)
              (let-values ([(end-line end-col end-pos) (port-next-location in)])
                (comment-k "@;"
                           'comment
                           #f
                           pos
                           end-pos
                           (backup-if-needed pos)
                           (no-backup mode)))))]
       [(regexp-try-match rx:opener in)
        => (lambda (m) (enter-opener m mode))]
       [(regexp-try-match #rx"^{" in)
        (enter-simple-opener mode)]
       [else
        (let ([new-mode
               (cond
                [(equal? #\| (peek-char-or-special in))
                 (read-char in)
                 (list* (make-scheme 'bar (+ offset pos))
                        (no-backup mode))]
                [else
                 (list* (make-scheme 'one #f)
                        (make-args)
                        (no-backup mode))])])
          (let-values ([(end-line end-col end-pos) (port-next-location in)])
            (values "@"
                    'parenthesis
                    #f
                    pos
                    end-pos
                    (backup-if-needed pos)
                    new-mode)))]))

    (define (enter-simple-opener mode)
      (let-values ([(end-line end-col end-pos) (port-next-location in)])
        (values "{"
                'parenthesis
                '|{|
                pos
                end-pos
                (backup-if-needed pos)
                (cons (make-text #rx"^@"
                                 #rx"^}"
                                 #rx"^{"
                                 #rx".*?(?:(?=[@{}\r\n])|$)"
                                 '|{|
                                 '|}|)
                       (no-backup mode)))))

    (define (enter-opener m mode)
      (let-values ([(end-line end-col end-pos) (port-next-location in)])
        (values (cadr m)
                'parenthesis
                '|{| ;; Better complex paren?
                pos
                end-pos
                (backup-if-needed pos)
                (let ([closer (regexp-quote
                               (bytes-append #"}"
                                             (flip (cadr m))
                                             #"|"))]
                      [re-opener (regexp-quote (cadr m))])
                  (cons (make-text (intern-byte-regexp (bytes-append #"^[|]" re-opener #"@"))
                                   (intern-byte-regexp (bytes-append #"^" closer))
                                   (intern-byte-regexp (bytes-append #"^[|]" re-opener #"{"))
                                   (intern-byte-regexp (bytes-append
                                                         #".*?(?:(?=[|]"
                                                         re-opener
                                                         #"[@{])|(?="
                                                         closer
                                                         #")|(?=[\r\n])|$)"))
                                   '|{|  ;; Better complex paren?
                                   '|}|) ;; Better complex paren?
                        (no-backup mode))))))

    (if (eof-object? (peek-char-or-special in))
        (values eof
                'eof
                #f
                pos 
                pos
                0
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
                      'parenthesis
                      (text-close-paren l)
                      pos
                      end-pos
                      0
                      (cdr mode)))]
           [(and (text-sub-rx l)
                 (regexp-try-match (text-sub-rx l) in))
            (let-values ([(end-line end-col end-pos) (port-next-location in)])
              (values "{"
                      'parenthesis
                      (text-open-paren l)
                      pos
                      end-pos
                      0
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
                      0
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
                      0
                      mode))])]
         [(scheme? l)
          (let ([status (scheme-status l)])
            (cond
             [(and (eq? status 'bracket)
                   (regexp-try-match #px"^\\s*?[]]" in))
              (let-values ([(end-line end-col end-pos) (port-next-location in)])
                (values "]"
                        'parenthesis
                        '|]|
                        pos
                        end-pos
                        0
                        (cdr mode)))]
             [(and (memq status '(bar bar-no-more))
                   (regexp-try-match #px"^\\s*?[|]" in))
              (let-values ([(end-line end-col end-pos) (port-next-location in)])
                (values "|"
                        'parenthesis
                        #f
                        pos
                        end-pos
                        (backup-if-needed pos)
                        (cdr mode)))]
             [(regexp-try-match #rx"^@" in)
              ;; If we have a backup at this point, we can drop it, because
              ;; edits after here cannot lead to a rx:opener match.
              (enter-@ (lambda (lexeme type paren start end backup mode)
                         (values lexeme
                                 (if (eq? status 'one)
                                     'error
                                     type)
                                 paren
                                 start
                                 end
                                 backup
                                 mode)))]
             [(and (eq? status 'one)
                   (regexp-try-match rx:opener in))
              ;; Must have consumed a special before an opener
              => (lambda (m) (enter-opener m (cdr mode)))]
             [(and (eq? status 'one)
                   (regexp-try-match #rx"^{" in))
              ;; Must have consumed a special before an opener
              (enter-simple-opener (cdr mode))]
             [(and (eq? status 'one)
                   (regexp-try-match #rx"^#?['`]" in))
              ;; Value special:
              (let-values ([(end-line end-col end-pos) (port-next-location in)])
                (values "'"
                        'constant
                        #f
                        pos
                        end-pos
                        (backup-if-needed pos)
                        mode))]
             [(and (eq? status 'one)
                   (regexp-try-match #rx"^#?,@?" in))
              ;; Other special:
              (let-values ([(end-line end-col end-pos) (port-next-location in)])
                (values ","
                        'other
                        #f
                        pos
                        end-pos
                        (backup-if-needed pos)
                        mode))]
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
                                           (cons (make-scheme status (scheme-backup l)) 
                                                 (cdr mode))]
                                          [(bar) (cons (make-scheme 'bar-no-more (scheme-backup l))
                                                       (cdr mode))]
                                          [else (error "bad status" status)]))])
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
                        (backup-if-needed start)
                        (maybe-no-backup
                         type
                         (case adj
                           [(continue) mode]
                           [(datum) 
                            (cond
                             [(pair? status) mode]
                             [else (consume status)])]
                           [(open)
                            (cons (make-scheme (cons #t status) (scheme-backup l))
                                  (cdr mode))]
                           [(close)
                            (if (pair? status)
                                (let ([v (cdr status)])
                                  (if (symbol? v)
                                      (consume v)
                                      (cons (make-scheme v (scheme-backup l)) (cdr mode))))
                                (consume status))]
                           [(bad) (if (pair? status)
                                      mode
                                      (consume status))]
                           [else (error "bad adj")]))))]))]
         [(args? l)
          (cond
           [(regexp-try-match #rx"^\\[" in)
            (let-values ([(end-line end-col end-pos) (port-next-location in)])
              (values "["
                      'parenthesis
                      '|[|
                      pos
                      end-pos
                      0
                      (list* (make-scheme 'bracket #f)
                             mode)))]
           [else
            (scribble-inside-lexer in offset (cons (make-text-args) (cdr mode)))])]
         [(text-args? l)
          (cond
           [(regexp-try-match rx:opener in)
            => (lambda (m) (enter-opener m (cdr mode)))]
           [(regexp-try-match #rx"^{" in)
            (enter-simple-opener (cdr mode))]
           [else
            (scribble-inside-lexer in offset (cdr mode))])]
         [else (error "bad mode")])))))

(define (scribble-lexer in offset mode)
  (scribble-inside-lexer in offset (or mode (list (make-scheme 'many #f)))))

(define (flip s)
  (list->bytes
   (for/list ([c (in-bytes s)])
     (cond
      [(equal? c (char->integer #\()) (char->integer #\))]
      [(equal? c (char->integer #\[)) (char->integer #\])]
      [(equal? c (char->integer #\<)) (char->integer #\>)]
      [(equal? c (char->integer #\))) (char->integer #\()]
      [(equal? c (char->integer #\])) (char->integer #\[)]
      [(equal? c (char->integer #\>)) (char->integer #\<)]
      [else c]))))
