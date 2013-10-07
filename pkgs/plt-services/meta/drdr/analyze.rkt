#lang racket
(require racket/file
         "diff.rkt"
         "scm.rkt"
         "list-count.rkt"
         "notify.rkt"
         "cache.rkt"
         "dirstruct.rkt"
         "status.rkt"
         "metadata.rkt"
         "path-utils.rkt"
         "rendering.rkt")
(provide (all-from-out "rendering.rkt"))

; Email
(require net/sendmail
         "formats.rkt")

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

(define responsible-ht-id->str
  #hasheq([timeout . "Timeout"]
          [unclean . "Unclean Exit"]
          [stderr . "STDERR Output"]
          [changes . "Changes"]))
(define responsible-ht-severity
  '(timeout unclean stderr changes))
(define (rev->responsible-ht rev)
  (define log-dir (revision-log-dir rev))
  (define top-analyze
    (parameterize ([cache/file-mode 'no-cache]
                   [current-rev rev])
      (dir-rendering log-dir)))
  (rendering->responsible-ht rev top-analyze))

(define (rendering->responsible-ht rev top-analyze)
  (match-define
   (struct rendering (_ _ _ timeout unclean stderr _ changes))
   top-analyze)
  (statuses->responsible-ht rev timeout unclean stderr changes))

(define (statuses->responsible-ht rev timeout unclean stderr changes)
  (parameterize ([current-rev rev])
    (define log-dir (revision-log-dir rev))
    (define base-path 
      (rebase-path log-dir "/"))
    
    (define responsible->problems (make-hash))
    (for ([lc (in-list (list timeout unclean stderr changes))]
          [id (in-list responsible-ht-severity)])
      (for ([pp (in-list (lc->list lc))])
        (define p (bytes->string/utf-8 pp))
        (define bp (base-path p))
        (for ([responsible 
               (in-list
                (rendering-responsibles (log-rendering p)))])
          (hash-update!
           (hash-ref! responsible->problems responsible make-hasheq)
           id
           (curry list* bp)
           empty))))
    
    responsible->problems))

(define (2hash-copy ht)
  (define 2ht (make-hash))
  (for ([(r ht) (in-hash ht)])
    (hash-set! 2ht r (hash-copy ht)))
  2ht)
(define (responsible-ht-difference old new)
  (let ([ht (2hash-copy new)])
    (for ([(r rht) (in-hash old)])
      (define nrht (hash-ref! ht r make-hash))
      (for ([(id ps) (in-hash rht)])
        (hash-update! nrht id
                      (curry remove* ps)
                      empty)
        (when (zero? (length (hash-ref nrht id)))
          (hash-remove! nrht id)))
      (when (zero? (hash-count nrht))
        (hash-remove! ht r)))
    ht))

(define responsible-ht/c
  (hash/c string? (hash/c symbol? (listof path?))))

(define (responsible-ht->status-ht diff)
  (for/hash ([id (in-list responsible-ht-severity)])
    (define id-l
      (for*/list ([(_ ht) (in-hash diff)]
                  [f (in-list (hash-ref ht id empty))])
        f))
    (values id (remove-duplicates id-l))))

(provide/contract
 [rendering->responsible-ht
  (exact-positive-integer? rendering? . -> . responsible-ht/c)]
 [statuses->responsible-ht 
  (exact-positive-integer? list/count list/count list/count list/count . -> . responsible-ht/c)]
 [responsible-ht-severity (listof symbol?)]
 [responsible-ht-id->str (hash/c symbol? string?)]
 [responsible-ht-difference (responsible-ht/c responsible-ht/c . -> . responsible-ht/c)])  

(define ERROR-LIMIT 50)
(define (notify cur-rev 
                start end
                duration
                timeout unclean stderr changes)
  (define abs-dur (- end start))
  (define nums
    (map lc->number
         (list timeout unclean stderr changes)))
  (define totals
    (apply 
     format
     "(timeout ~a) (unclean ~a) (stderr ~a) (changes ~a)"
     (map number->string nums)))
  (define (path->url pth)
    (format "http://drdr.racket-lang.org/~a~a" cur-rev pth))
  (define responsible-ht
    (statuses->responsible-ht 
     cur-rev
     timeout
     unclean
     stderr
     changes))
  (define responsibles 
    (for/list ([(responsible ht) (in-hash responsible-ht)]
               #:when (ormap (curry hash-has-key? ht)
                             (take responsible-ht-severity 3)))
      (match responsible
        ["nobody" "drdr-nobody"]
        [x x])))
  (define committer
    (with-handlers ([exn:fail? (lambda (x) #f)])
      (scm-commit-author 
       (read-cache*
        (revision-commit-msg cur-rev)))))
  (define diff
    (with-handlers ([exn:fail? (lambda (x) #t)])
      (define old (rev->responsible-ht (previous-rev)))
      (responsible-ht-difference old responsible-ht)))
  (define include-committer?
    (and ; The committer can be found
     committer 
     ; There is a condition
     (not (empty? responsibles))
     ; It is different from before
     (hash? diff)
     (for*/or ([(r ht) (in-hash diff)]
               [(id ps) (in-hash ht)])
       (and 
        (for/or ([p (in-list ps)])
          ; XXX This squelch should be disabled if the committer changed this file
          ; XXX But even then it can lead to problems
          (not (path-random? (build-path (revision-trunk-dir cur-rev)
                                         (substring (path->string* p) 1)))))
        (not (symbol=? id 'changes))))))
  (define mail-recipients
    (remove-duplicates
     (append (if include-committer?
                 (list committer)
                 empty)
             responsibles)))
  
  ; Send messages to everyone...
  (unless (andmap zero? nums)
    (for ([r (in-list mail-recipients)])
      (send-mail-message 
       "drdr@racket-lang.org"
       (format "[DrDr] R~a ~a"
               cur-rev totals)
       (list (format "~a@racket-lang.org" r))
       empty empty
       (flatten
        (list (format "DrDr has finished building push #~a after ~a."
                      cur-rev
                      (format-duration-ms abs-dur))
              ""
              (format "http://drdr.racket-lang.org/~a/" 
                      cur-rev)
              ""
              (if (and include-committer? (equal? committer r))
                  (list
                   (format "Push #~a (which you did) contained a NEW condition that may need inspecting." cur-rev)
                   (let ([diff-smash (responsible-ht->status-ht diff)])
                     (for/list ([(id paths) (in-hash diff-smash)]
                                #:when (not (symbol=? id 'changes)))
                       (if (empty? paths)
                           empty
                           (list (format "  ~a" id)
                                 (for/list ([f (in-list paths)]
                                            [i (in-range ERROR-LIMIT)]
                                            #:when (not (path-random?
                                                         (build-path (revision-trunk-dir cur-rev)
                                                                     (substring (path->string* f) 1)))))
                                   (format "    ~a" (path->url f)))
                                 ""))))
                   "")
                  empty)
              (if (hash-has-key? responsible-ht r)
                  (list* "A file you are responsible for has a condition that may need inspecting."
                         (for/list ([(id files) (in-hash (hash-ref responsible-ht r))]
                                    #:when (not (symbol=? id 'changes)))
                           (list (format "  ~a:" id)
                                 (for/list ([f (in-list files)]
                                            [i (in-range ERROR-LIMIT)])
                                   (format "    ~a" (path->url f)))
                                 ""))
                         "")
                  empty))))))
  
  ; Send message to IRC
  (send-mail-message "drdr@racket-lang.org"
                     (format "http://drdr.racket-lang.org/~a/" 
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
  ((rebase-path (revision-log-dir rev) (revision-trunk-dir rev))
   pth))

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
           [(? eof-object?)
            #f]
           [(and log (struct status (start end command-line output-log)))
            (define dur (status-duration log))
            (define any-stderr? (ormap stderr? output-log))
            (define changed?
              (if (and (previous-rev)
                       (not (path-random? (trunk-path log-pth))))
                  (with-handlers ([exn:fail? 
                                   ; This #f means that new files are NOT considered changed
                                   (lambda (x) #f)])
                    (define prev-log-pth
                      ((rebase-path (revision-log-dir (current-rev)) 
                                    (revision-log-dir (previous-rev)))
                       log-pth))
                    (log-different? output-log
                                    (status-output-log (read-cache prev-log-pth))))
                  #f))
            (define responsible 
              (or (path-responsible (trunk-path log-pth))
                  (and (regexp-match #rx"/planet/" (path->string* log-pth))
                       "jay")
                  ; XXX maybe mflatt, eli, or tewk
                  (and (regexp-match #rx"/src/" (path->string* log-pth))
                       "jay")
                  "nobody"))
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
                    [#f
                     acc]
                    [(and n (struct rendering (pth-start pth-end pth-dur pth-timeouts pth-unclean-exits pth-stderrs _pth-responsible pth-changed)))
                     (match acc
                       [#f n]
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
                         (scm-commit-author (read-cache (revision-commit-msg (current-rev))))))
                  (or (path-responsible (trunk-path dir-pth))
                      "nobody"))
                 
                 empty)
                (directory-list* dir-pth))))
      #f))

(provide/contract
 [analyze-revision (exact-nonnegative-integer? . -> . void)]
 [analyze-logs (exact-nonnegative-integer? . -> . void)]
 [log-rendering (path-string? . -> . (or/c rendering? false/c))]
 [dir-rendering (path-string? . -> . (or/c rendering? false/c))])
