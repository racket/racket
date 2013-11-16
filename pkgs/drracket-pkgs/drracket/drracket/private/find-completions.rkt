#lang racket/base

(require racket/contract/base
         racket/system
         racket/port)

(define lcl/c
  (listof (or/c #f
                (and/c path? complete-path?)
                (hash/c (or/c (and/c symbol? module-path?) #f)
                        (listof (and/c path? complete-path?))
                        #:flat? #t))))
(define lcp/c
  (listof (and/c path? complete-path?)))

(provide
 (contract-out
  [alternate-racket-clcl/clcp (-> path-string? (values lcl/c lcp/c))]
  [find-completions (->* (string?)
                         (#:alternate-racket (or/c #f 
                                                   path-string?
                                                   (list/c lcl/c lcp/c)))
                         (listof (list/c string? path?)))]))

(define (ignore? x) 
  (or (member x '("compiled"))
      (regexp-match #rx"~$" x)))

(define (find-completions string #:alternate-racket [alternate-racket #f])
  (find-completions/internal string
                             (find-all-collection-dirs alternate-racket)
                             directory-list
                             directory-exists?))

(define (find-completions/internal string collection-dirs dir->content is-dir?)
  (define segments (regexp-split #rx"/" string))
  (define first-candidates
    (cond
      [(null? segments) '()]
      [else
       (define reg (regexp (string-append "^" (regexp-quote (car segments)))))
       (filter (λ (line) (regexp-match reg (list-ref line 0)))
               collection-dirs)]))
  (define unsorted
    (let loop ([segments (cdr segments)]
               [candidates first-candidates])
      (cond
        [(null? segments) candidates]
        [else
         (define reg (regexp (string-append "^" (regexp-quote (car segments)))))
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
(define (find-all-collection-dirs alternate-racket)
  (define-values (library-collection-links library-collection-paths)
    (cond
      [(list? alternate-racket)
       (values (list-ref alternate-racket 0)
               (list-ref alternate-racket 1))]
      [else
       (alternate-racket-clcl/clcp alternate-racket)]))
  ;; link-content : (listof (list (or/c 'root 'static-root string?) path?))
  (define link-content 
    (apply
     append
     (for/list ([link (in-list library-collection-links)])
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
          (for/list ([clp (in-list library-collection-paths)])
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

(define (alternate-racket-clcl/clcp alternate-racket) 
  (define (use-current-racket n)
    (values (current-library-collection-links)
            (current-library-collection-paths)))
  (cond
    [alternate-racket
     (cond
       [(file-exists? alternate-racket)
        (define result-port (open-output-string))
        (define success?
          (parameterize ([current-output-port result-port]
                         [current-error-port (open-output-nowhere)]
                         [current-input-port (open-input-string "")])
            (system* alternate-racket 
                     "-l" "racket/base"
                     "-e" 
                     (format "~s" `(write 
                                    (let loop ([exp
                                                (list (current-library-collection-links)
                                                      (current-library-collection-paths))])
                                      (cond
                                        [(pair? exp) (cons (loop (car exp)) (loop (cdr exp)))]
                                        [(hash? exp) (for/hash ([(k v) (in-hash exp)])
                                                       (values (loop k)
                                                               (loop v)))]
                                        [(path? exp) `#s(pth ,(path->string exp))]
                                        [else exp])))))))
        (cond
          [success?
           (struct pth (p) #:prefab)
           (define (convert-back exp)
             (let loop ([exp exp])
               (cond
                 [(pth? exp) (string->path (pth-p exp))]
                 [(pair? exp) (cons (loop (car exp)) (loop (cdr exp)))]
                 [(hash? exp) (for/hash ([(k v) (in-hash exp)])
                                (values (loop k)
                                        (loop v)))]
                 [else exp])))
           (define ip (open-input-string (get-output-string result-port)))
           (define links/paths 
             (convert-back
              (with-handlers ([exn:fail:read? (λ (x) #f)])
                (read ip))))
           (define okay-values? (list/c lcl/c lcp/c))
           (cond
             [(okay-values? links/paths)
              (values (list-ref links/paths 0)
                      (list-ref links/paths 1))]
             [else (use-current-racket 0)])]
          [else (use-current-racket 1)])]
       [else (use-current-racket 2)])]
    [else (use-current-racket 3)]))

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
      ["plt/pkgs/gui-pkgs/gui-lib/rackunit"
       (map string->path '("something.rkt"))]
      [_ '()]))
  
  (define (dir-exists? d)
    (not (regexp-match #rx"rkt$" (path->string d))))
  
  (check-equal?
   (find-completions/c "rack/" coll-table dir-list dir-exists?)
   `(("gui" ,(string->path "/plt/pkgs/draw-pkgs/draw-lib/racket/gui"))
     ("gui" ,(string->path "/plt/racket/collects/racket/gui"))
     ("include.rkt" ,(string->path "/plt/racket/collects/racket/include.rkt"))
     ("info.rkt" ,(string->path "/plt/racket/collects/racket/info.rkt"))
     ("init.rkt" ,(string->path "/plt/racket/collects/racket/init.rkt"))
     ("list.rkt" ,(string->path "/plt/racket/collects/racket/list.rkt"))
     ("something.rkt" ,(string->path "plt/pkgs/gui-pkgs/gui-lib/rackunit/something.rkt"))))
  
  (check-equal?
   (find-completions/c "racke/" coll-table dir-list dir-exists?)
   `(("gui" ,(string->path "/plt/pkgs/draw-pkgs/draw-lib/racket/gui"))
     ("gui" ,(string->path "/plt/racket/collects/racket/gui"))
     ("include.rkt" ,(string->path "/plt/racket/collects/racket/include.rkt"))
     ("info.rkt" ,(string->path "/plt/racket/collects/racket/info.rkt"))
     ("init.rkt" ,(string->path "/plt/racket/collects/racket/init.rkt"))
     ("list.rkt" ,(string->path "/plt/racket/collects/racket/list.rkt"))))
  
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
         (list "dynamic.rkt" (string->path "/plt/racket/collects/racket/gui/dynamic.rkt")))))
