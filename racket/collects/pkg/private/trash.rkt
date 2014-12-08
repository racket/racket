#lang racket/base
(require racket/list
         racket/file
         "config.rkt"
         "dirs.rkt"
         "print.rkt"
         "params.rkt")

(provide select-trash-dest
         pkg-empty-trash)

(define (find-trash-dir) (build-path (pkg-dir #f) ".trash"))

(define (select-trash-dest pkg-name
                           #:remove-printf [remove-printf void])
  (define max-packages (get-trash-max-packages))
  (define max-seconds (get-trash-max-seconds))
  (define trash-dir (find-trash-dir))
  (make-directory* trash-dir)
  (define (delete-trash f)
    (define p (build-path trash-dir f))
    (remove-printf "Removing trash directory ~a\n" p)
    (log-pkg-info "Removing trash directory ~a" p)
    (delete-directory/files p))
  (define l (if (directory-exists? trash-dir)
                (directory-list trash-dir)
                null))
  (let loop ([l l])
    (cond
     [(and (pair? l)
           ((length l) . > . (sub1 max-packages)))
      (delete-trash (car l))
      (loop (cdr l))]
     [(and (pair? l)
           (let ([m (regexp-match #rx"^([0-9]+)-" (car l))])
             (and m
                  ((string->number (cadr m)) . < . (- (current-seconds)
                                                      max-seconds)))))
      (delete-trash (car l))
      (loop (cdr l))]
     [(max-packages . < . 1) #f]
     [(not pkg-name) #f]
     [else
      (let loop ([n 0])
        (define name (format "~a-~a-~a" (current-seconds) n pkg-name))
        (define p (build-path trash-dir name))
        (if (or (directory-exists? p)
                (file-exists? p)
                (link-exists? p))
            (loop (add1 n))
            p))])))

(define (pkg-empty-trash #:list? [show-list? #f]
                         #:quiet? [quiet? #t])
  (cond
   [show-list?
    (define trash-dir (find-trash-dir))
    (printf "Trash directory for ~s scope:\n  ~a\nContent:\n"
            (current-pkg-scope)
            trash-dir)
    (define exists? (directory-exists? trash-dir))
    (define l (if exists?
                  (directory-list trash-dir)
                  null))
    (cond
     [(not exists?)
      (printf "  [does not exist]\n")]
     [(null? l)
      (printf "  [none]\n")]
     [else
      (for ([i (in-list l)])
        (printf "  ~a\n" i))])]
   [else
    (parameterize ([current-pkg-trash-max-packages 0])
      (select-trash-dest #t #:remove-printf (if quiet? void printf/flush)))
    (void)]))
