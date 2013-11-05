#lang racket/base
(require racket/class 
         racket/gui/base
         string-constants)

(module find-completions racket/base
  (require racket/contract/base)
  (provide
   (contract-out
    [find-completions (-> string? (listof (list/c string? path?)))]))
  
  (define (ignore? x) (member x '("compiled")))
  
  (define (find-completions string)
    (find-completions/internal string
                               (find-all-collection-dirs)
                               directory-list
                               directory-exists?))
  
  (define (find-completions/internal string collection-dirs dir->content is-dir?)
    (define segments (regexp-split #rx"/" string))
    (define first-candidates
      (cond
        [(null? segments) '()]
        [else
         (define reg (regexp 
                      (string-append "^" 
                                     (regexp-quote (car segments))
                                     (if (null? (cdr segments))
                                         ""
                                         "$"))))
         (filter (λ (line) (regexp-match reg (list-ref line 0)))
                 collection-dirs)]))
    (define unsorted
      (let loop ([segments (cdr segments)]
                 [candidates first-candidates])
        (cond
          [(null? segments) candidates]
          [else
           (define reg (regexp (string-append 
                                "^"
                                (regexp-quote (car segments))
                                (if (null? (cdr segments))
                                    ""
                                    "$"))))
           (define nexts
             (for*/list ([key+candidate (in-list candidates)]
                         [candidate (in-value (list-ref key+candidate 1))]
                         #:when (is-dir? candidate)
                         [ent (in-list (dir->content candidate))]
                         [ent-str (in-value (path->string ent))]
                         #:unless (ignore? ent-str)
                         #:when (regexp-match reg ent-str))
               (list ent-str (build-path candidate ent))))
           (loop (cdr segments) nexts)])))
    (sort unsorted string<=? #:key (λ (x) (path->string (list-ref x 1)))))
  
  ;; -> (listof (list string? path?))
  ;; returns a list of all of the directories that are being treated as collections,
  ;; (together with the names of the collections)
  (define (find-all-collection-dirs)
    ;; link-content : (listof (list (or/c 'root 'static-root string?) path?))
    (define link-content 
      (apply
       append
       (for/list ([link (in-list (current-library-collection-links))])
         (cond
           [link 
            (define-values (base name dir?) (split-path link))
            (if (file-exists? link)
                (for/list ([link-ent (call-with-input-file link read)]
                           #:when (if (= 3 (length link-ent))
                                      (regexp-match (list-ref link-ent 2) (version))
                                      #t))
                  `(,(list-ref link-ent 0)
                    ,(simplify-path (build-path base (list-ref link-ent 1)))))
                '())]
           [else
            (for/list ([clp (in-list (current-library-collection-paths))])
              `(root ,(simplify-path clp)))]))))
    
    (apply
     append
     (for/list ([just-one (in-list link-content)])
       (define-values (what pth) (apply values just-one))
       (cond
         [(string? what)
          (list just-one)]
         [else
          (cond
            [(directory-exists? pth)
             (for/list ([dir (in-list (directory-list pth))]
                        #:when (directory-exists? (build-path pth dir)))
               (list (path->string dir) (build-path pth dir)))]
            [else '()])]))))
  
  (module+ test
    (require rackunit
             racket/list
             racket/contract
             racket/match)
    
    (define/contract find-completions/c
      (-> string? (listof (list/c string? path?)) (-> path? (listof path?)) (-> path? boolean?)
          (listof (list/c string? path?)))
      find-completions/internal)
    
    (define coll-table
      `(("racket" ,(string->path "/plt/pkgs/compatibility-pkgs/compatibility-lib/racket"))
        ("racket" ,(string->path "/plt/pkgs/draw-pkgs/draw-lib/racket"))
        ("racket" ,(string->path "/plt/racket/collects/racket"))
        ("rackunit" ,(string->path "plt/pkgs/gui-pkgs/gui-lib/rackunit"))))
    
    (define (dir-list d)
      (match (path->string d)
        ["/plt/racket/collects/racket"
         (map string->path '("list.rkt" "info.rkt" "include.rkt" "init.rkt" "gui"))]
        ["/plt/racket/collects/racket/gui"
         (map string->path '("dynamic.rkt"))]
        ["/plt/pkgs/draw-pkgs/draw-lib/racket"
         (map string->path '("gui"))]
        ["/plt/pkgs/draw-pkgs/draw-lib/racket/gui"
         (map string->path '("draw.rkt"))]
        [_ '()]))
    
    (define (dir-exists? d)
      (not (regexp-match #rx"rkt$" (path->string d))))
    
    (check-equal?
     (find-completions/c "rack/" coll-table dir-list dir-exists?)
     '())
    
    (check-equal?
     (find-completions/c "rack" coll-table dir-list dir-exists?)
     coll-table)
    
    (check-equal?
     (find-completions/c "racku" coll-table dir-list dir-exists?)
     (list (last coll-table)))
    
    (check-equal?
     (find-completions/c "racket/i" coll-table dir-list dir-exists?)
     (list (list "include.rkt" (string->path "/plt/racket/collects/racket/include.rkt"))
           (list "info.rkt" (string->path "/plt/racket/collects/racket/info.rkt"))
           (list "init.rkt" (string->path "/plt/racket/collects/racket/init.rkt"))))
    
    (check-equal?
     (find-completions/c "racket/" coll-table dir-list dir-exists?)
     (list (list "gui" (string->path "/plt/pkgs/draw-pkgs/draw-lib/racket/gui"))
           (list "gui" (string->path "/plt/racket/collects/racket/gui"))
           (list "include.rkt" (string->path "/plt/racket/collects/racket/include.rkt"))
           (list "info.rkt" (string->path "/plt/racket/collects/racket/info.rkt"))
           (list "init.rkt" (string->path "/plt/racket/collects/racket/init.rkt"))
           (list "list.rkt" (string->path "/plt/racket/collects/racket/list.rkt"))))
    
    (check-equal?
     (find-completions/c "racket/g" coll-table dir-list dir-exists?)
     (list (list "gui" (string->path "/plt/pkgs/draw-pkgs/draw-lib/racket/gui"))
           (list "gui" (string->path "/plt/racket/collects/racket/gui"))))
    
    (check-equal?
     (find-completions/c "racket/gui/d" coll-table dir-list dir-exists?)
     (list (list "draw.rkt" (string->path "/plt/pkgs/draw-pkgs/draw-lib/racket/gui/draw.rkt"))
           (list "dynamic.rkt" (string->path "/plt/racket/collects/racket/gui/dynamic.rkt"))))))

(module+ test (require (submod ".." find-completions test)))

(require (submod "." find-completions)
         racket/contract
         framework)

(provide
 (contract-out
  [get-module-path-from-user
   (->* () (#:init string? #:pref symbol?) (or/c path? #f))]))

(define (get-module-path-from-user #:init [init-value ""] #:pref [pref-sym #f])
  
  (define dlg%
    (class dialog%
      (define/override (on-subwindow-char receiver event)
        (cond
          [(member (send event get-key-code) '(up down))
           (define old-sel (send lb get-selection))
           (define dir (if (equal? (send event get-key-code) 'up)
                           -1
                           1))
           (unless (= 0 (send lb get-number))
             (send lb set-selection 
                   (cond
                     [old-sel 
                      (modulo (+ old-sel 
                                 (if (equal? (send event get-key-code) 'up)
                                     -1
                                     1))
                              (send lb get-number))]
                     [(equal? (send event get-key-code) 'up)
                      (- (send lb get-number) 1)]
                     [else
                      0])))]
          [else (super on-subwindow-char receiver event)]))
      (super-new)))
  
  (define dlg (new dlg% [label ""][width 600][height 600]))
  (define (tf-callback)
    (adjust-lb)
    (update-buttons))
  (define tf (new text-field% [parent dlg] [label #f]
                  [init-value init-value]
                  [callback (λ (tf evt) 
                              (when pref-sym
                                (preferences:set pref-sym (send tf get-value)))
                              (tf-callback))]))
  (define lb (new list-box% 
                  [parent dlg] [choices '()] [label #f]
                  [callback (λ (lb evt) (update-buttons))]))
                  
  (define (adjust-lb)
    (send lb clear)
    (unless (equal? (send tf get-value) "")
      (for ([i (in-list (find-completions (send tf get-value)))]
            [n (in-naturals)])
        (send lb append (path->string (list-ref i 1)))
        ;; data holds a path => open the file
        ;; data holds a string => add that past the last / in 'tf'
        (cond
          [(file-exists? (list-ref i 1))
           (send lb set-data n (list-ref i 1))]
          [else
           (send lb set-data n (list-ref i 0))]))
      (when (= 1 (send lb get-number))
        (send lb set-selection 0))))
  
  (define bp (new horizontal-panel% 
                  [parent dlg]
                  [stretchable-height #f]
                  [alignment '(right center)]))
  
  (define cancelled? #t)
  
  (define (ok button evt) 
    (set! cancelled? #f)
    (send dlg show #f))
  (define (cancel button evt) (send dlg show #f))
  (define (enter-sub button evt)
    (define item-to-act-on (get-item-to-act-on))
    (define mtch (regexp-match #rx"(^.*/)[^/]*$" (send tf get-value)))
    (define prefix 
      (if mtch
          (list-ref mtch 1)
          ""))

    (send tf set-value (string-append prefix
                                      (send lb get-data item-to-act-on)
                                      "/"))
    (adjust-lb)
    (update-buttons))
    
  (define enter-sub-button (new button% 
                                [parent bp]
                                [style '(border)]
                                [label (string-constant enter-subcollection)]
                                [callback enter-sub]))
  
  (define-values (ok-button cancel-button) (gui-utils:ok/cancel-buttons bp ok cancel))
  
  (define (update-buttons)
    (define item-to-act-on (get-item-to-act-on))
    (cond
      [item-to-act-on 
       (define datum (send lb get-data item-to-act-on))
       (cond
         [(path? datum)
          (send ok-button enable #t)
          (send enter-sub-button enable #f)]
         [(string? datum)
          (send ok-button enable #f)
          (send enter-sub-button enable #t)])]
      [else
       (send ok-button enable #f)
       (send enter-sub-button enable #f)]))
  
  (define (get-item-to-act-on)
    (or (send lb get-selection)
        (and (= 1 (send lb get-number))
             0)))
    
  (adjust-lb)
  (update-buttons)
  (send dlg show #t)
  (cond
    [cancelled? #f]
    [else (send lb get-data (get-item-to-act-on))]))
