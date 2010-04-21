#lang scheme
(require "svn.ss"
         net/url
         scheme/system)
(provide
 (all-from-out "svn.ss"))

(define git-path (make-parameter "/opt/local/bin/git"))
(provide/contract
 [git-path (parameter/c string?)])

(define git-url-base "http://git.racket-lang.org/plt.git")

(define (newest-push)
  (string->number (port->string (get-pure-port (string->url (format "~a/push-counter" git-url-base))))))

(define (pad2zeros n)
  (format "~a~a"
          (if (n . < . 10)
              "0" "")
          (number->string n)))

(define-struct push-data (who start-commit end-commit branches) #:prefab)

(define (push-info push-n)
  (define push-n100s (quotient push-n 100))
  (define push-nrem (pad2zeros (modulo push-n 100)))
  (define ls 
    (port->lines 
     (get-pure-port 
      (string->url 
       (format "~a/pushes/~a/~a" git-url-base push-n100s push-nrem)))))
  (match ls
    [(list (regexp #rx"^([^ ]+) +([0-9abcdef]+) +([0-9abcdef]+)$" (list _ who start-commit end-commit))
           (regexp #rx"^([0-9abcdef]+) +([0-9abcdef]+) +(.+)$" (list _ _ _ branch))
           ...)
     (make-push-data who start-commit end-commit branch)]
    [_
     #f]))

(define (system/output-port #:stdout [init-stdout #f] . as)
  (define-values (sp stdout stdin stderr)
    (apply subprocess init-stdout #f #f as))
  (subprocess-wait sp)
  stdout)

(define-struct git-push (num author commits) #:prefab)
(define-struct git-commit (hash author date msg mfiles) #:prefab)

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
  (match-define (regexp #rx"^commit +(.+)$" (list _ hash)) (read-line in-p))
  (match-define (regexp #rx"^Author: +(.+)$" (list _ author)) (read-line in-p))
  (match-define (regexp #rx"^Date: +(.+)$" (list _ date)) (read-line in-p))
  (define _1 (read-line in-p))
  (define msg (read-until-empty-line in-p))
  (define mfiles (read-until-empty-line in-p))
  (make-git-commit hash author date msg mfiles))

(define port-empty? port-closed?)

(define (read-commits in-p)
  (if (port-empty? in-p)
      empty
      (list* (read-commit in-p)
             (read-commits in-p))))

(define (parse-push num author in-p)
  (make-git-push num author (read-commits in-p)))

(define (get-scm-commit-msg rev repo)
  (match-define (struct push-data (who start-commit end-commit branches)) (push-info rev))
  (scm-update repo)
  (parse-push 
   rev who 
   (parameterize ([current-directory repo])
     (system/output-port (git-path) "log" "--date=iso" "--name-only" (format "~a..~a" start-commit end-commit)))))
(provide/contract
 [struct git-push ([num exact-nonnegative-integer?]
                   [author string?]
                   [commits (listof git-commit?)])]
 [struct git-commit ([hash string?]
                     [author string?]
                     [date string?]
                     [msg (listof string?)]
                     [mfiles (listof string?)])]
 [get-scm-commit-msg (exact-nonnegative-integer? string? . -> . git-push?)])

(define (scm-export rev repo file dest)
  (define commit
    (push-data-end-commit (push-info rev)))
  (scm-update repo)
  (call-with-output-file* 
   dest
   #:exists 'truncate/replace
   (lambda (file-port)
     (parameterize ([current-directory repo])
       (system/output-port #:stdout file-port
                           (git-path)
                           "archive" commit file))))
  (void))

(define (scm-checkout rev repo dest)
  (scm-update repo)
  (system* (git-path) "clone" (path->string repo) (path->string dest))
  (parameterize ([current-directory dest])
    (system* (git-path) "checkout" (push-data-end-commit (push-info rev)))))

(define (scm-update repo)
  (parameterize ([current-directory repo])
    (system* (git-path) "fetch" git-url-base)))

(define (scm-revisions-after cur-rev repo)
  (define newest-rev (newest-push))
  (for/list ([rev (in-range (add1 cur-rev) newest-rev)]
             #:when
             (let ([info (push-info rev)])
               (and info
                    (member "refs/heads/master" (push-data-branches info)))))
    rev))

(provide/contract
 [scm-revisions-after (exact-nonnegative-integer? path? . -> . void?)]
 [scm-export (exact-nonnegative-integer? path? string? path? . -> . void?)]
 [scm-checkout (exact-nonnegative-integer? path? path? . -> . void?)])