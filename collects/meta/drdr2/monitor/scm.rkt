#lang racket/base
(require net/url
         racket/system
         racket/function
         racket/list
         racket/match
         racket/port
         racket/contract
         "../lib/scm.rkt")

(define git-path (find-executable-path "git"))
(define git-url-base "http://git.racket-lang.org/plt.git") 
(define (get-newest-push)
  (string->number (port->string (get-pure-port (string->url (format "~a/push-counter" git-url-base))))))

(define (pad2zeros n)
  (format "~a~a"
          (if (n . < . 10)
              "0" "")
          (number->string n)))

(struct push-data (who end-commit branches) #:prefab)

(define (get-push-data push-n)
  (define push-n100s (quotient push-n 100))
  (define push-nrem (pad2zeros (modulo push-n 100)))
  (define ls 
    (port->lines 
     (get-pure-port 
      (string->url 
       (format "~a/pushes/~a/~a" git-url-base push-n100s push-nrem)))))
  (match ls
    [(list (regexp #rx"^([^ ]+) +([0-9abcdef]+)$" (list _ who end-commit))
           (regexp #rx"^([0-9abcdef]+) +([0-9abcdef]+) +(.+)$" (list _ bstart bend branch))
           ...)
     (push-data who end-commit 
                (make-immutable-hash
                 (map (lambda (b bs be) (cons b (vector bs be)))
                      branch bstart bend)))]
    [_
     #f]))

(define (close-input-port* p)
  (when p (close-input-port p)))
(define (close-output-port* p)
  (when p (close-output-port p)))

(define (system/output-port #:k k #:stdout [init-stdout #f] . as)
  (define-values (sp stdout stdin stderr)
    (apply subprocess init-stdout #f #f as))
  (begin0 (k stdout)
          (subprocess-wait sp)
          (subprocess-kill sp #t)
          (close-input-port* stdout)
          (close-output-port* stdin)
          (close-input-port* stderr)))

(define (read-until-empty-line in-p)
  (let loop ()
    (let ([l (read-line in-p)])
      (cond
        [(eof-object? l)
         (close-input-port in-p)
         empty]
        [(string=? l "")
         empty]
        [else
         (list* (regexp-replace #rx"^ +" l "") (loop))]))))

(define (read-commit in-p)
  (match (read-line in-p)
    [(? eof-object?)
     #f]
    [(regexp #rx"^commit +(.+)$" (list _ hash))
     (match (read-line in-p)
       [(regexp #rx"^Merge: +(.+) +(.+)$" (list _ from to))
        (match-define (regexp #rx"^Author: +(.+)$" (list _ author)) (read-line in-p))
        (match-define (regexp #rx"^Date: +(.+)$" (list _ date)) (read-line in-p))
        (define _1 (read-line in-p))
        (define msg (read-until-empty-line in-p))
        (git-merge hash author date msg from to)]
       [(regexp #rx"^Author: +(.+)$" (list _ author))
        (match-define (regexp #rx"^Date: +(.+)$" (list _ date)) (read-line in-p))
        (define _1 (read-line in-p))
        (define msg (read-until-empty-line in-p))
        (define mfiles (read-until-empty-line in-p))
        (git-diff hash author date msg mfiles)])]))

(define master-branch "refs/heads/master")
(define (git-pushes-after cur-rev)
  (define newest-rev (get-newest-push))
  (for/list ([rev (in-range (add1 cur-rev) (add1 newest-rev))]
             #:when
             (let ([info (get-push-data rev)])
               (and info (hash-has-key? (push-data-branches info) master-branch))))
    rev))

(define (git-update repo)
  (parameterize ([current-directory repo])
    (system* git-path "fetch" git-url-base))
  (void))

(define (read-commits in-p)
  (cond
    [(port-closed? in-p)
     empty]
    [(read-commit in-p)
     => (lambda (c) 
          (printf "~S\n" c)
          (list* c (read-commits in-p)))]
    [else
     empty]))
(define (parse-push repo num author in-p)
  (define commits (read-commits in-p))
  (define start (git-commit-hash (last commits)))
  (define previous-commit 
    (parameterize ([current-directory repo])
      (system/output-port 
       #:k (λ (port) (read-line port))
       git-path "--no-pager" "log" "--format=format:%P" start "-1")))
  (git-push num author previous-commit commits))

(define (get-git-push repo rev)
  (match-define (push-data who _ branches) (get-push-data rev))
  (match-define (vector start-commit end-commit) (hash-ref branches master-branch))  
  (parameterize ([current-directory repo])
    (system/output-port 
     #:k (curry parse-push repo rev who)
     git-path
     "--no-pager" "log" "--date=iso" "--name-only" "--no-merges"
     (format "~a..~a" start-commit end-commit))))

(provide/contract
 [git-pushes-after (exact-nonnegative-integer? . -> . (listof exact-nonnegative-integer?))]
 [git-update (path? . -> . void?)]
 [get-git-push (path? exact-nonnegative-integer? . -> . git-push?)])
