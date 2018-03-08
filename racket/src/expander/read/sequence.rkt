#lang racket/base
(require "../common/struct-star.rkt"
         "config.rkt"
         "special.rkt"
         "readtable.rkt"
         "whitespace.rkt"
         "delimiter.rkt"
         "consume.rkt"
         "closer.rkt"
         "error.rkt"
         "indentation.rkt"
         "parameter.rkt"
         "wrap.rkt"
         "location.rkt"
         "special-comment.rkt")

(provide read-unwrapped-sequence)

(define (read-unwrapped-sequence read-one opener-c opener closer in seq-config
                                 #:elem-config [elem-config (next-readtable seq-config)]
                                 #:dot-mode [dot-mode 'all]
                                 #:shape-tag? [shape-tag? #f]
                                 #:whitespace-read-one [whitespace-read-one read-one]
                                 #:first-read-one [first-read-one read-one])
  (define head #f)
  (define indentation (make-indentation closer in seq-config))
  (define config (struct*-copy read-config elem-config
                               [indentations (cons indentation
                                                   (read-config-indentations seq-config))]))
  (define-values (open-end-line open-end-col open-end-pos) (port-next-location in))

  (define config/keep-comment (keep-comment config))

  (define (read-one/not-eof init-c read-one config)
    (define e (read-one init-c in config))
    (when (eof-object? e)
      (reader-error in seq-config #:due-to e #:end-pos open-end-pos
                    "expected a ~a to close `~a`~a"
                    (closer-name closer config)
                    opener-c
                    (indentation-possible-cause config)))
    e)

  (define seq
    (let loop ([depth 0] [accum null] [init-c #f] [first? #t] [first-read-one first-read-one])
      (define c (read-char/skip-whitespace-and-comments init-c whitespace-read-one in seq-config))
      (define ec (effective-char c seq-config))
      (cond
       [(eqv? ec closer)
        (if (null? accum)
            null
            (reverse accum))]
       [(and (not first?)
             (eqv? ec #\.)
             (check-parameter read-accept-dot config)
             (char-delimiter? (peek-char/special in config) seq-config))
        ;; Found a `.`: maybe improper or maybe infix
        (define-values (dot-line dot-col dot-pos) (port-next-location* in c))
        (track-indentation! config dot-line dot-col)
        
        (unless (and dot-mode
                     ;; don't allow another `.` if we've seen an infix
                     (not head))
          (reader-error in (reading-at config dot-line dot-col dot-pos)
                        "illegal use of `.`"))
        
        ;; Read one item for improper list or for infix:
        (define v (read-one/not-eof #f first-read-one config))
        
        ;; Check for infix or list termination:
        (define rest-c (read-char/skip-whitespace-and-comments #f whitespace-read-one in seq-config))
        (define rest-ec (effective-char rest-c seq-config))
        
        (cond
         [(eqv? rest-ec closer)
          ;; Improper list
          (if (null? accum)
              v
              (append (reverse accum) v))]
         [(and (eqv? rest-ec #\.)
               (check-parameter read-accept-dot config)
               (check-parameter read-accept-infix-dot config)
               (char-delimiter? (peek-char/special in config) seq-config))
          ;; Infix mode
          (set! head (box v))
          
          (define-values (dot2-line dot2-col dot2-pos) (port-next-location in))
          (track-indentation! config dot2-line dot2-col)
          
          ;; Check for a closer right after the second dot:
          (define post-c (read-char/skip-whitespace-and-comments #f whitespace-read-one in seq-config))
          (define post-ec (effective-char post-c seq-config))
          (when (or (eof-object? post-ec)
                    (eqv? post-ec closer))
            (reader-error in (reading-at config dot-line dot-col dot-pos)
                          #:due-to post-ec
                          "illegal use of `.`"))
          
          ;; No closer => another item or EOF
          (loop depth accum post-c #f read-one)]
         [else
          ;; Something else after a single element after a single dot
          (reader-error in (reading-at config dot-line dot-col dot-pos)
                        #:due-to rest-c
                        "illegal use of `.`")])]
       [else
        (define v (read-one/not-eof c first-read-one config/keep-comment))
        (cond
         [(special-comment? v) (loop depth accum #f #f read-one)]
         [(depth . > . 1024)
          ;; At some large depth, it's better to accumlate than recur
          (loop depth (cons v accum) #f #f read-one)]
         [else 
          (cons v (loop (add1 depth) null #f #f read-one))])])))
  (define full-seq (if head
                       (cons (unbox head) seq)
                       seq))
  (if shape-tag?
      (add-shape-tag opener in config full-seq)
      full-seq))

;; ----------------------------------------

(define (add-shape-tag opener in config seq)
  (define tag
    (case opener
      [(#\[) (and (check-parameter read-square-bracket-with-tag config) '#%brackets)]
      [(#\{) (and (check-parameter read-curly-brace-with-tag config) '#%braces)]
      [else #f]))
  (if tag
      (cons (wrap tag in config #f) seq)
      seq))
