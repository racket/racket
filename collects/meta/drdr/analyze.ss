#lang scheme
(require scheme/file
         "diff.ss"
         "svn.ss"
         "list-count.ss"
         "notify.ss"
         "cache.ss"
         "dirstruct.ss"
         "run-collect.ss"
         "path-utils.ss"
         "rendering.ss")
(provide (all-from-out "rendering.ss"))

; Email
(require net/sendmail
         "formats.ss")

(define list@
  (match-lambda
    [(and c (cons x y))
     (if (lc-zero? x)
         empty
         (list c))]))

(define (list-limit l n)
  (for/list ([e (in-list l)]
             [i (in-range n)])
    e))

(define (notify cur-rev 
                start end
                duration
                timeout unclean stderr changes)
  (define abs-dur (- end start))
  (define nums
    (map lc->number
         (list timeout unclean stderr changes)))
  (define totals
    (apply format "(timeout ~a) (unclean ~a) (stderr ~a) (changes ~a)" (map number->string nums)))
  (define log-dir (revision-log-dir cur-rev))
  (define base-path 
    (rebase-path log-dir "/"))
  (define (path->url pth)
    (format "http://drdr.plt-scheme.org/~a~a" cur-rev (base-path pth)))
  (define responsible-ht (make-hash))
  (define problems
    (for/list ([(l id)
                (in-dict
                 (append
                  (list@ (cons timeout "timed out"))
                  (list@ (cons unclean "exited uncleanly"))
                  (list@ (cons stderr "had standard error output"))
                  (list@ (cons changes "changed output since the last build"))))])
      (list* ""
             (format "~a files ~a:"
                     (lc->number l)
                     id)
             (for/list ([p (in-list (lc->list l))])
               (define ps (bytes->string/utf-8 p))
               (unless (string=? id "changed output since the last build")
                 (for ([responsible (in-list (rendering-responsibles (log-rendering ps)))])
                   (hash-set! responsible-ht responsible #t)))
               (format "\t~a" (path->url ps))))))
  (define responsibles (hash-map responsible-ht (lambda (k v) k)))
  (define committer
    (with-handlers ([exn:fail? (lambda (x) #f)])
      (svn-rev-log-author 
       (read-cache*
        (revision-commit-msg cur-rev)))))
  (define include-committer?
    (and committer (not (empty? responsibles))))
  (unless (andmap zero? nums)
    (send-mail-message "drdr@plt-scheme.org"
                       (format "[DrDr] R~a ~a"
                               cur-rev totals)
                       (list* "jay.mccarthy@gmail.com"
                              (map (curry format "~a@plt-scheme.org")
                                   (append (if include-committer?
                                               (list committer)
                                               empty)
                                           responsibles)))                              
                       empty empty
                       (apply
                        append
                        (list* (format "DrDr has finished building revision ~a after ~a."
                                       cur-rev
                                       (format-duration-ms abs-dur))
                               ""
                               (format "http://drdr.plt-scheme.org/~a/" 
                                       cur-rev)
                               ""
                               (format "~a:" (apply string-append (add-between responsibles " ")))
                               "You are receiving this email because a file you are responsible for has a condition that may need inspecting."
                               (if include-committer?
                                   (list
                                    ""
                                    (format "~a:" committer)
                                    (format "You are receiving this email because the DrDr test of revision ~a (which you committed) contained a condition that may need inspecting" cur-rev))
                                   empty))
                        (list-limit problems 2))))
  
  (send-mail-message "drdr"
                     (format "http://drdr.plt-scheme.org/~a/" 
                             cur-rev)
                     (list "eli+ircbot@eli.barzilay.org")
                     empty empty
                     (list* (format " (abs ~a) (sum ~a) ~a"
                                    (format-duration-ms abs-dur)
                                    (format-duration-ms duration)
                                    totals)
                            (if (empty? responsibles)
                                empty
                                (list (apply string-append (add-between responsibles " ")))))))
; End Email

(define (trunk-path pth)
  (define rev (current-rev))
  ((rebase-path (revision-log-dir rev) (revision-trunk-dir rev)) pth))

(define (analyze-path pth dir?)
  (define rev (current-rev))
  (define log-dir (revision-log-dir rev))
  (define analyze-dir (revision-analyze-dir rev))
  (define the-analyze-path
    ((rebase-path log-dir analyze-dir) pth))
  (if dir?
      (build-path the-analyze-path "index.analyze")
      (path-add-suffix the-analyze-path ".analyze")))

(define (analyze-revision cur-rev)
  (cache/file/timestamp
   (build-path (revision-dir cur-rev) "analyzed")
   (lambda ()
     (match (analyze-logs cur-rev)
       [(struct rendering (start end duration timeout unclean stderr _ changes))
        (notify cur-rev 
                start end
                duration
                timeout unclean stderr changes)]
       [#f
        (void)])
     (safely-delete-directory (revision-trunk-dir cur-rev))
     (void))))

(define (analyze-logs rev)
  (define log-dir (revision-log-dir rev))
  (define analyze-dir (revision-analyze-dir rev))
  (make-directory* analyze-dir)
  (parameterize ([current-rev rev])
    (dir-rendering log-dir #:committer? #t)))

(define (log-rendering log-pth)
  ; XXX
  (if (or #t (file-exists? log-pth))
      (cache/file
       (analyze-path log-pth #f)
       (lambda ()
         #;(notify! "Analyzing log: ~S" log-pth)
         (match (read-cache log-pth)
           [(and log (struct status (start end command-line output-log)))
            (define dur (status-duration log))
            (define any-stderr? (ormap stderr? output-log))
            (define changed?
              (if (previous-rev)
                  (with-handlers ([exn:fail? (lambda (x) #t)])
                    (define prev-log-pth ((rebase-path (revision-log-dir (current-rev)) (revision-log-dir (previous-rev))) log-pth))
                    (log-different? output-log (status-output-log (read-cache prev-log-pth))))
                  #f))
            (define responsible 
              (or (svn-property-value/root (trunk-path log-pth) plt:responsible)
                  (and (regexp-match #rx"^/planet" (path->string* log-pth))
                       "jay")
                  ; XXX maybe mflatt, eli, or tewk
                  (and (regexp-match #rx"^/src" (path->string* log-pth))
                       "jay")
                  "unknown"))
            (define lc
              (list (path->bytes log-pth)))
            (make-rendering start end dur 
                            (if (timeout? log) lc empty)
                            (if (exit? log) 
                                (if (zero? (exit-code log)) empty lc)
                                empty)
                            (if any-stderr? lc empty)
                            responsible
                            (if changed? lc empty))])))
      #f))

(define (dir-rendering dir-pth 
                       #:committer? [committer? #f])
  ; XXX
  (if (or #t (directory-exists? dir-pth))
      (cache/file
       (analyze-path dir-pth #t)
       (lambda ()
         (notify! "Analyzing dir: ~S" dir-pth)
         (foldl (lambda (sub-pth acc)
                  (define pth (build-path dir-pth sub-pth))
                  (define directory? (directory-exists? pth))
                  (define (next-rendering)
                    (if directory?
                        (dir-rendering pth)
                        (log-rendering pth)))
                  (match (next-rendering)
                    [(struct rendering (pth-start pth-end pth-dur pth-timeouts pth-unclean-exits pth-stderrs _pth-responsible pth-changed))
                     (match acc
                       [(struct rendering (acc-start acc-end acc-dur acc-timeouts acc-unclean-exits acc-stderrs acc-responsible acc-changed))
                        (make-rendering (min pth-start acc-start)
                                        (max pth-end acc-end)
                                        (+ pth-dur acc-dur)
                                        (lc+ pth-timeouts acc-timeouts)
                                        (lc+ pth-unclean-exits acc-unclean-exits)
                                        (lc+ pth-stderrs acc-stderrs)
                                        acc-responsible
                                        (lc+ pth-changed acc-changed))])]))
                (make-rendering 
                 +inf.0 -inf.0 0
                 empty empty empty
                 
                 (or
                  (and committer? 
                       (with-handlers ([exn:fail? (lambda (x) #f)])
                         (svn-rev-log-author (read-cache (revision-commit-msg (current-rev))))))
                  (or (svn-property-value/root (trunk-path dir-pth) plt:responsible)
                      "unknown"))
                 
                 empty)
                (directory-list* dir-pth))))
      #f))

(provide/contract
 [analyze-revision (exact-nonnegative-integer? . -> . void)]
 [analyze-logs (exact-nonnegative-integer? . -> . void)]
 [log-rendering (path-string? . -> . (or/c rendering? false/c))]
 [dir-rendering (path-string? . -> . (or/c rendering? false/c))])