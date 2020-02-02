
(define (make-known-char-range-list)
  (let-syntax ([mk
                (lambda (stx)
                  (let-syntax ([define/who (lambda (stx)
                                             (syntax-case stx ()
                                               [(_ . r) #'(define . r)]))]
                               [check (lambda (stx) #'(#%void))]
                               [with-global-lock* (lambda (stx)
                                                    (syntax-case stx ()
                                                      [(_ e) #'e]))])
                    (include "rumble/char.ss")

                    (define (delta op c)
                      (- (char->integer (op c))
                         (char->integer c)))

                    (define (non-decomp? a)
                      (let ([s (string a)])
                        (string=? (string-normalize-nfkd s)
                                  s)))

                    (define (char-same? a b)
                      (and (eqv? (char-alphabetic? a)
                                 (char-alphabetic? b))
                           (eqv? (char-lower-case? a)
                                 (char-lower-case? b))
                           (eqv? (char-upper-case? a)
                                 (char-upper-case? b))
                           (eqv? (char-title-case? a)
                                 (char-title-case? b))
                           (eqv? (char-numeric? a)
                                 (char-numeric? b))
                           (eqv? (char-symbolic? a)
                                 (char-symbolic? b))
                           (eqv? (char-punctuation? a)
                                 (char-punctuation? b))
                           (eqv? (char-graphic? a)
                                 (char-graphic? b))
                           (eqv? (char-whitespace? a)
                                 (char-whitespace? b))
                           (eqv? (char-blank? a)
                                 (char-blank? b))
                           (eqv? (char-iso-control? a)
                                 (char-iso-control? b))
                           (eqv? (char-general-category a)
                                 (char-general-category b))
                           ;; In addition to predicates, sameness checking
                           ;; has historcially counted deltas between upcase
                           ;; and downcase --- just because that was convenient,
                           ;; and not because it makes complete sense
                           (eqv? (delta char-upcase a)
                                 (delta char-upcase b))
                           (eqv? (delta char-downcase a)
                                 (delta char-downcase b))
                           (eqv? (delta char-titlecase a)
                                 (delta char-titlecase b))
                           ;; Check whether decomposition behavior is different
                           (eqv? (non-decomp? a)
                                 (non-decomp? b))
                           ;; Detect different combining classes
                           (eqv? (#%equal? (string a b)
                                           (string-normalize-nfkd (string a b)))
                                 (#%equal? (string b a)
                                           (string-normalize-nfkd (string b a))))))

                    (define ranges
                      (let loop ([i 1] [start 0] [same? #t])
                        (cond
                         [(= i #xD800)
                          (cons `(,start ,(sub1 i) ,same?)
                                (loop #xE001 #xE000 #t))]
                         [(= i #x110000) `((,start #x10FFFF ,same?))]
                         [(eq? 'cn (char-general-category (integer->char i)))
                          (cons `(,start ,(sub1 i) ,same?)
                                (let nloop ([i (add1 i)])
                                  (cond
                                   [(= i #xD800)
                                    (nloop #xE000)]
                                   [(= i #x110000) '()]
                                   [(eq? 'cn (char-general-category (integer->char i)))
                                    (nloop (add1 i))]
                                   [else
                                    (loop (add1 i) i #t)])))]
                         [(and same?
                               (char-same? (integer->char i)
                                           (integer->char (sub1 i))))
                          (loop (add1 i) start #t)]
                         [else
                          (loop (add1 i) start #f)])))

                    (datum->syntax #'here (list 'quote ranges))))])
    (mk)))
