#lang racket/base

(require racket/match setup/dirs)

(define (show-tree t0 [format values])
  (let loop ([t t0] [last? #t] [indent '()])
    (define (I mid last) (cond [(eq? t t0) ""] [last? mid] [else last]))
    (for-each display (reverse indent))
    (unless (eq? t t0) (printf "|\n"))
    (for-each display (reverse indent))
    (printf "~a~a\n" (I "\\-" "+-") (format (car t)))
    (for ([s (cdr t)] [n (in-range (- (length t) 2) -1 -1)])
      (loop s (zero? n) (cons (I "  " "| ") indent)))))

(define (zo-size-tree dir)
  (parameterize ([current-directory dir])
    (define subs
      (filter values
        (for/list ([p (in-list (sort (map path->string (directory-list))
                                     string<?))])
          (cond [(directory-exists? p) (zo-size-tree p)]
                [(regexp-match? #rx"\\.zo$" p) (file-size p)]
                [else #f]))))
    (match subs
      ;; fold a single-directory up
      [`(((,name . ,size) . ,rest))
       `((,(format "~a/~a" dir name) . ,size) . ,rest)]
      [_ (define total
           (for/sum ([x (in-list subs)]) (cond [(pair? x) (cdar x)] [else x])))
         (and (< 0 total) (cons (cons dir total) (filter pair? subs)))])))

(define tree (zo-size-tree (find-collects-dir)))

(show-tree tree (λ(x) (format "~a: ~a" (car x) (cdr x))))

(module+ test ; (do this hack only as a test)
  (printf "\n\nThe following \"time\" line is to trick drdr into graphing\n")
  (printf "the size of all of the .zo files, in bytes\n")
  (printf "cpu time: ~a real time: ~a gc time: ~a\n"
          (cdar tree) (cdar tree) (cdar tree))
  (module config info
    (define random? #t)))
