#lang racket/base
(require racket/class
         (prefix-in wx: "kernel.rkt")
         (rename-in "wxme/cycle.rkt"
                    [set-editor-get-file! wx:set-editor-get-file!]
                    [set-editor-put-file! wx:set-editor-put-file!])
         "lock.rkt"
         "wx.rkt"
         "cycle.rkt"
         "check.rkt"
         "mrtop.rkt"
         "path-dialog.rkt")

(provide get-file
         get-file-list
         put-file
         get-directory)

(define ((mk-file-selector who put? multi? dir?)
         message parent directory filename extension style filters dialog-mixin)
  ;; Calls from C++ have wrong kind of window:
  (when (is-a? parent wx:window%)
    (set! parent (as-entry (λ () (wx->mred parent)))))
  
  (check-label-string/false who message)
  (check-top-level-parent/false who parent)
  (check-path/false who directory)
  (check-path/false who filename)
  (check-string/false who extension)
  (check-style who #f (cond
                        [dir? '(common enter-packages)]
                        [else '(common packages enter-packages)]) style)
  (unless (and (list? filters)
               (andmap (λ (p)
                         (and (list? p)
                              (= (length p) 2)
                              (string? (car p))
                              (string? (cadr p))))
                       filters))
    (raise-argument-error who "(listof (list/c string? string?))" filters))
  (let* ([std? (memq 'common style)]
         [style (if std? (remq 'common style) style)])
    (if std?
        (send (new (dialog-mixin path-dialog%)
                   [put?      put?]
                   [dir?      dir?]
                   [multi?    multi?]
                   [message   message]
                   [parent    parent]
                   [directory directory]
                   [filename  filename]
                   [filters
                    (cond [(eq? filters default-filters) #t] ; its own defaults
                          [dir? #f]
                          [else filters])])
              run)
        (wx:file-selector
         message directory filename extension
         ;; file types:
         filters
         ;; style:
         (cons (cond [dir?   'dir]
                     [put?   'put]
                     [multi? 'multi]
                     [else   'get])
               style)
         ;; parent:
         (and parent (mred->wx parent))))))

(define default-filters '(("Any" "*.*")))

;; We duplicate the definition for `get-file', `get-file-list', and
;; `put-file' so that they have the right arities and names

(define-syntax define-file-selector
  (syntax-rules ()
    [(_ name put? multi?)
     (define (name [message #f] [parent #f] [directory #f] [filename #f]
                   [extension #f] [style null] [filters default-filters]
                   #:dialog-mixin [dialog-mixin values])
       ((mk-file-selector 'name put? multi? #f)
        message parent directory filename extension style filters dialog-mixin))]))

(define-file-selector get-file      #f #f)
(define-file-selector get-file-list #f #t)
(define-file-selector put-file      #t #f)

(define (get-directory [message #f] [parent #f] [directory #f] [style null] #:dialog-mixin [dialog-mixin values])
  ((mk-file-selector 'get-directory #f #f #t)
   message parent directory #f #f style null dialog-mixin))

(set-get-file! get-file)
(wx:set-editor-get-file! get-file)
(wx:set-editor-put-file! put-file)
