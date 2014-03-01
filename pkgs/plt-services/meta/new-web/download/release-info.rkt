#lang racket/base

(require racket/system racket/port racket/match racket/runtime-path
         racket/promise)

(define-runtime-path THIS-GIT "../../../../../.git")

(define (warn fmt . xs)
  (eprintf "Warning: ~a\a\n" (apply format fmt xs))
  (flush-output (current-error-port))
  (sleep 1)
  #f)

(define git*
  (lazy
    (define exe (or (find-executable-path "git")
                    (warn "no `git' executable => no release info")))
    (define (try dir) (and dir (directory-exists? dir) dir))
    (define dir (and exe (or (ormap try (list (getenv "GIT_DIR") THIS-GIT))
                             (warn "no git dir found => no release info\n  (~a)"
                                   "set $GIT_DIR to a racket repo .git dir"))))
    (define nowhere (open-output-nowhere))
    (and dir (λ args (define o (open-output-string))
                     (parameterize ([current-directory dir]
                                    [current-output-port o]
                                    [current-error-port nowhere])
                       (and (apply system* exe "--no-pager" args)
                            (get-output-string o)))))))

(provide get-version-tag-info)
(define (get-version-tag-info version)
  (define git (force git*))
  (let/ec return
    (unless git (return #f))
    (define (bad . args) (apply warn args) (return #f))
    (define (try fmt)
      (let* ([tag (format fmt version)]
             [text (and (git "cat-file" "-e" tag)
                        (git "cat-file" "tag" tag))])
        (and text (cons tag text))))
    (let* ([text  (or (try "v~a") (try "old-v~a")
                      (bad "no git info for ~s (missing tag)" version))]
           [tag   (car text)]
           [text  (cdr text)]
           [check (λ (x) (or x (bad "malformed git info for ~s" tag)))]
           [sep   (check (regexp-match-positions #rx"\n\n" text))]
           [meta  (map (λ (h)
                         (let ([m (check (regexp-match-positions #rx" " h))])
                           (list (string->symbol (substring h 0 (caar m)))
                                 (substring h (cdar m)))))
                       (regexp-split #rx"\n" (substring text 0 (caar sep))))]
           [text  (substring text (cdar sep))])
      (match meta
        [`((object ,_) (type "commit") (tag ,_)
           (tagger ,(regexp #rx"^(.* <.*>) ([0-9]+) ([-+]?[0-9][0-9])00$"
                            (list _ tagger date ofs))))
         ;; ignore the time offset (it probably depends on where the tag
         ;; was made)
         (list tagger (seconds->date (string->number date)) text)]
        [_ (check #f)]))))
