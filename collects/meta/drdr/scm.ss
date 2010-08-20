#lang scheme
(require "svn.ss"
         "path-utils.ss"
         "dirstruct.ss"
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

(define-struct push-data (who end-commit branches) #:prefab)

(define (push-info push-n)
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
     (make-push-data who end-commit 
                     (make-immutable-hash
                      (map (lambda (b bs be) (cons b (vector bs be)))
                           branch bstart bend)))]
    [_
     #f]))

(define (pipe/proc cmds)
  (if (null? (cdr cmds))
    ((car cmds))
    (let-values ([(i o) (make-pipe 4096)])
      (parameterize ([current-output-port o])
        (thread (lambda () ((car cmds)) (close-output-port o))))
      (parameterize ([current-input-port i])
        (pipe/proc (cdr cmds))))))
(define-syntax-rule (pipe expr exprs ...)
  (pipe/proc (list (lambda () expr) (lambda () exprs) ...)))

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

(define-struct git-push (num author commits) #:prefab)
(define-struct git-commit (hash author date msg) #:prefab)
(define-struct (git-diff git-commit) (mfiles) #:prefab)
(define-struct (git-merge git-commit) (from to) #:prefab)

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
        (make-git-merge hash author date msg from to)]
       [(regexp #rx"^Author: +(.+)$" (list _ author))
        (match-define (regexp #rx"^Date: +(.+)$" (list _ date)) (read-line in-p))
        (define _1 (read-line in-p))
        (define msg (read-until-empty-line in-p))
        (define mfiles (read-until-empty-line in-p))
        (make-git-diff hash author date msg mfiles)])]))

(define port-empty? port-closed?)

(define (read-commits in-p)
  (cond
    [(port-empty? in-p)
     empty]
    [(read-commit in-p)
     => (lambda (c) 
          (printf "~S~n" c)
          (list* c (read-commits in-p)))]
    [else
     empty]))

(define (parse-push num author in-p)
  (make-git-push num author (read-commits in-p)))

(define (get-scm-commit-msg rev repo)
  (match-define (struct push-data (who _ branches)) (push-info rev))
  (match-define (vector start-commit end-commit) (hash-ref branches master-branch))
  (parameterize ([current-directory repo])
    (system/output-port 
     #:k (curry parse-push rev who)
     (git-path)
     "--no-pager" "log" "--date=iso" "--name-only" "--no-merges"
     (format "~a..~a" start-commit end-commit))))
(provide/contract
 [struct git-push 
         ([num exact-nonnegative-integer?]
          [author string?]
          [commits (listof git-commit?)])]
 [struct git-commit 
         ([hash string?]
          [author string?]
          [date string?]
          [msg (listof string?)])]
 [struct git-diff 
         ([hash string?]
          [author string?]
          [date string?]
          [msg (listof string?)]
          [mfiles (listof string?)])]
 [struct git-merge 
         ([hash string?]
          [author string?]
          [date string?]
          [msg (listof string?)]
          [from string?]
          [to string?])]
 [get-scm-commit-msg (exact-nonnegative-integer? path-string? . -> . git-push?)])

(define (git-push-previous-commit gp)
  (define start (git-push-start-commit gp))
  (parameterize ([current-directory (plt-repository)])
    (system/output-port 
     #:k (λ (port) (read-line port))
     (git-path)
     "--no-pager" "log" "--format=format:%P" start "-1")))  
(define (git-push-start-commit gp)
  (git-commit-hash (last (git-push-commits gp))))
(define (git-push-end-commit gp)
  (git-commit-hash (first (git-push-commits gp))))
(provide/contract
 [git-push-previous-commit (git-push? . -> . string?)]
 [git-push-start-commit (git-push? . -> . string?)]
 [git-push-end-commit (git-push? . -> . string?)])

(define scm-commit-author
  (match-lambda
    [(? git-push? gp) (git-push-author gp)]
    [(? svn-rev-log? srl) (svn-rev-log-author srl)]))
(provide/contract
 [scm-commit-author ((or/c git-push? svn-rev-log?) . -> . string?)])

(define (scm-export-file rev repo file dest)
  (define commit
    (push-data-end-commit (push-info rev)))
  (call-with-output-file*
   dest
   #:exists 'truncate/replace
   (lambda (file-port)
     (parameterize ([current-directory repo])
       (system/output-port
        #:k void
        #:stdout file-port
        (git-path) "--no-pager" "show" (format "~a:~a" commit file)))))
  (void))

(define (scm-export-repo rev repo dest)
  (pipe
   (system*
    (git-path) "archive"
    (format "--remote=~a" repo)
    (format "--prefix=~a/" (regexp-replace #rx"/+$" (path->string* dest) ""))
    "--format=tar"
    (push-data-end-commit (push-info rev)))
   (system* (find-executable-path "tar") "xf" "-" "--absolute-names"))
  (void))

(define (scm-update repo)
  (parameterize ([current-directory repo])
    (system* (git-path) "fetch" git-url-base))
  (void))

(define master-branch "refs/heads/master")

(define (scm-revisions-after cur-rev)
  (define newest-rev (newest-push))
  (for/list ([rev (in-range (add1 cur-rev) (add1 newest-rev))]
             #:when
             (let ([info (push-info rev)])
               (and info (hash-has-key? (push-data-branches info) master-branch))))
    rev))

(provide/contract
 [scm-update (path? . -> . void?)]
 [scm-revisions-after (exact-nonnegative-integer? . -> . (listof exact-nonnegative-integer?))]
 [scm-export-file (exact-nonnegative-integer? path-string? string? path-string? . -> . void?)]
 [scm-export-repo (exact-nonnegative-integer? path-string? path-string? . -> . void?)])
