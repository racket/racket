#lang scheme/unit

  (require string-constants
           "sig.rkt"
           "../preferences.rkt"
           mred/mred-sig
           racket/path)
  
  (import mred^
          [prefix keymap: framework:keymap^]
          [prefix frame: framework:frame^])
  
  (export (rename framework:finder^
                  [-put-file put-file]
                  [-get-file get-file]))
  
  (define dialog-parent-parameter (make-parameter #f))
  
  (define filter-match?
    (λ (filter name msg)
      (let-values ([(base name dir?) (split-path name)])
        (if (regexp-match-exact? filter (path->bytes name))
            #t
            (begin
              (message-box (string-constant error) msg)
              #f)))))
  
  (define default-filters (make-parameter '(["Any" "*.*"])))
  (define default-extension (make-parameter ""))
  
  ;; dialog wrappers
  
  (define (*put-file style)
    (lambda ([name #f]
             [directory #f]
             [replace? #f]
             [prompt (string-constant select-file)]
             [filter #f]
             [filter-msg (string-constant file-wrong-form)]
             [parent-win (dialog-parent-parameter)])
      (let* ([directory (if (and (not directory) (string? name))
                            (path-only name)
                            directory)]
             [name (or (and (string? name) (file-name-from-path name))
                       name)]
             [f (put-file prompt parent-win directory name
                          (default-extension) style (default-filters)
                          #:dialog-mixin frame:focus-table-mixin)])
        (and f (or (not filter) (filter-match? filter f filter-msg))
             (let* ([f (simple-form-path f)]
                    [dir (path-only f)]
                    [name (file-name-from-path f)])
               (cond
                 [(not (and (path-string? dir) (directory-exists? dir)))
                  (message-box (string-constant error)
                               (string-constant dir-dne))
                  #f]
                 [(or (not name) (equal? name ""))
                  (message-box (string-constant error)
                               (string-constant empty-filename))
                  #f]
                 [else f]))))))
  
  (define op (current-output-port))
  (define (*get-file style)
    (lambda ([directory #f]
             [prompt (string-constant select-file)]
             [filter #f]
             [filter-msg (string-constant file-wrong-form)]
             [parent-win (dialog-parent-parameter)])
      (let ([f (get-file prompt parent-win directory #f
                         (default-extension) style (default-filters)
                         #:dialog-mixin frame:focus-table-mixin)])
        (and f (or (not filter) (filter-match? filter f filter-msg))
             (cond [(directory-exists? f)
                    (message-box (string-constant error)
                                 (string-constant that-is-dir-name))
                    #f]
                   [(not (file-exists? f))
                    (message-box (string-constant error) 
                                 (string-constant file-dne))
                    #f]
                   [else (simple-form-path f)])))))
  
  (define-syntax-rule
    (define/rename id exp)
    (define id (procedure-rename exp 'id)))
  
  ;; external interfaces to file functions
  
  (define/rename std-put-file    (*put-file '()))
  (define/rename std-get-file    (*get-file '()))
  (define/rename common-put-file (*put-file '(common)))
  (define/rename common-get-file (*get-file '(common)))
  
  (define -put-file
    (λ args
      (apply (case (preferences:get 'framework:file-dialogs)
               [(std) std-put-file]
               [(common) common-put-file])
             args)))
  (define -get-file
    (λ args
      (apply (case (preferences:get 'framework:file-dialogs)
               [(std) std-get-file]
               [(common) common-get-file])
             args)))
