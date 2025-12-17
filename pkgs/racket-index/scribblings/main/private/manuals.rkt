#lang racket/base

(require scribble/manual
         scribble/struct
         scribble/tag
         setup/getinfo
         setup/main-collects
         setup/dirs
         scheme/list
         scheme/match
         "../config.rkt")

(provide make-start-page)

(define-struct sec (cat label))
(struct doc (category? category priority fam path spec))
(struct category (lang cat str) #:transparent)

(define sections
  (map (lambda (xs) (apply make-sec xs)) manual-sections))

(define (rename-category lang cat)
  (cond
    [(box? cat) (category lang 'other-library (unbox cat))]
    [(string? cat) (category lang 'other-library (string-append lang " " cat))]
    [(memq cat '(language teaching experimental legacy other racket-core omit omit-start)) cat]
    [(eq? cat 'getting-started) (category lang 'getting-started (format "~a Getting Started" lang))]
    [(and (eq? cat 'core) (equal? lang "Racket")) 'racket-core]
    [else
     (define str (string-append lang " " (cadr (or (assq cat manual-sections) '(_ "Other")))))
     (category lang cat str)]))

(define (maybe-rename-category main-language-family fam cat)
  (if (member main-language-family fam)
      (if (box? cat)
          (unbox cat)
          cat)
      (rename-category (car fam) cat)))

(define (racketize-category cat)
  (if (eq? cat 'core)
      'racket-core
      cat))

(define (add-sections mk-sep l)
  (if (null? l)
    null
    (let loop ([l l] [key (if (equal? "" (caddar l)) (caar l) +inf.0)])
      (cond [(null? l) null]
            [(equal? (caar l) key) (cons (cadar l) (loop (cdr l) key))]
            [else (let ([lbl (caddar l)] ; currently always ""
                        [l (cons (cadar l) (loop (cdr l) (caar l)))]
                        [sep? (not (= (truncate (/ key 10))
                                      (truncate (/ (caar l) 10))))])
                    (if sep? (cons (mk-sep lbl) l) l))]))))

(define (get-docs all? tag
                  #:custom-secs [custom-secs (make-hash)])
  (let* ([recs (find-relevant-directory-records (list tag) (if all? 'all-available 'no-user))]
         [infos (map get-info/full (map directory-record-path recs))]
         [docs (append-map
                (lambda (i rec)
                  (define dir (directory-record-path rec))
                  (define s (and i
                                 (i tag)))
                  (if (not s)
                    null
                    (filter-map
                     (lambda (d)
                       (if (and (not all?)
                                (pair? (cdr d))
                                (or (memq 'user-doc (cadr d))
                                    (memq 'user-doc-root (cadr d))))
                         #f
                         (let* ([new-cat (or (and ((length d) . > . 2)
                                                  (let ([cat (caddr d)])
                                                    (and (pair? cat)
                                                         (list? cat)
                                                         cat)))
                                             '(library))]
                                [sub-cat (and (list? new-cat)
                                              ((length new-cat) . > . 1)
                                              (cadr new-cat))]
                                [fam (and (list? new-cat)
                                          ((length new-cat) . > . 2)
                                          (caddr new-cat))])
                           (doc
                            ;; Whether a category is present via `scribblings`
                            ((length d) . > . 2)
                            ;; Category (tenative)
                            (let ([the-cat
                                   (if (pair? new-cat) (car new-cat) 'unknown)])
                              (or (and (string? the-cat)
                                       the-cat)
                                  (and (box? the-cat)
                                       (string? (unbox the-cat))
                                       the-cat)
                                  (and (or (eq? the-cat 'omit) 
                                           (eq? the-cat 'omit-start))
                                       the-cat)
                                  (ormap (lambda (sec)
                                           (and (eq? the-cat (sec-cat sec))
                                                the-cat))
                                         sections)
                                  'library))
                            ;; Priority (tentative)
                            (if (and sub-cat (real? sub-cat)) sub-cat 0)
                            ;; Language family (tentative)
                            (if (and (pair? fam) (list? fam) (andmap string? fam))
                                fam
                                '("Racket"))
                            ;; Path
                            (build-path dir (if (pair? d) (car d) "???"))
                            ;; Spec
                            (let ([spec (directory-record-spec rec)])
                              (list* (car spec)
                                     (if (pair? d) (car d) "UNKNOWN")
                                     (if (eq? 'planet (car spec))
                                         (list (append (cdr spec)
                                                       (list (directory-record-maj rec)
                                                             (list '= (directory-record-min rec)))))
                                         (cdr spec))))))))
                     s)))
                infos
                recs)])
    docs))

(define (maybe-cdr l)
  ;; drop initial separator, if there are any lines at all
  (if (null? l)
      null
      (cdr l)))

(define (make-start-page all?
                         #:main-language-family [main-language-family (get-main-language-family)])
  (let* ([custom-secs (make-hash)]
         [docs/orig (get-docs all? 'scribblings #:custom-secs custom-secs)]
         [plain-line
          (lambda content
            (list (make-flow (list (make-paragraph content)))))]
         [line
          (lambda (spec indent?)
            (plain-line (if indent? (hspace 2) null)
                        (if (element? spec)
                            spec
                            (other-manual spec #:underline? #f))))]
         [cat-order (for/hasheq ([sec (in-list manual-sections)]
                                 [i (in-naturals)])
                      (values (car sec) i))])
    (define (contents renderer part resolve-info)
      ;; Update categories based on cross-reference information
      (define docs
        (for/list ([d (in-list docs/orig)])
          (cond
            [(element? (doc-spec d)) d]
            [else
             (define props* (resolve-get part resolve-info `(doc-properties ,(doc-prefix (doc-spec d) #f "top"))))
             (define props (cond
                             [(hash? props*) props*]
                             [else #hasheq()]))
             (define fam (let ([l (hash-ref props 'language-family #f)])
                           (if (and (pair? l) (list? l) (andmap string? l))
                               l
                               (doc-fam d))))
             (define cat+priority (let* ([ht* (hash-ref props 'category #f)]
                                         [ht (if (hash? ht*) ht* #hasheq())])
                                    (or (hash-ref ht main-language-family #f)
                                        (hash-ref ht 'default #f)
                                        (list (doc-category d) (doc-priority d)))))
             (define cat (maybe-rename-category
                          main-language-family fam
                          (or (and (pair? cat+priority)
                                   (let ([s (car cat+priority)])
                                     (and (or (symbol? s)
                                              (string? s)
                                              (and (box? s) (string? (unbox s))))
                                          s)))
                              'library)))
             (define priority (or (and (pair? cat+priority)
                                       (pair? (cdr cat+priority))
                                       (let ([p (cadr cat+priority)])
                                         (and (real? p)
                                              p)))
                                  0))
             (struct-copy doc d
                          [category (cond
                                      [(or (string? cat) (category? cat))
                                       (hash-ref! custom-secs cat (gensym))]
                                      [else cat])]
                          [priority priority])])))
      (define sections+custom
        (let ([sections
               (append-map (λ (sec)
                             (if (eq? 'library (sec-cat sec))
                                 (append (for/list ([label (in-list
                                                            (sort (filter string? (hash-keys custom-secs))
                                                                  string<?))])
                                           (make-sec (hash-ref custom-secs label)
                                                     label))
                                         (list sec)
                                         (for/list ([label (in-list
                                                            (sort (filter category? (hash-keys custom-secs))
                                                                  (lambda (a b)
                                                                    (cond
                                                                      [(equal? (category-lang a)
                                                                               (category-lang b))
                                                                       (define oa (hash-ref cat-order (category-cat a) 100))
                                                                       (define ob (hash-ref cat-order (category-cat b) 100))
                                                                       (if (eqv? oa ob)
                                                                           (string<=? (category-str a) (category-str b))
                                                                           (< oa ob))]
                                                                      [else
                                                                       (cond
                                                                         [(equal? (category-lang a) "Racket") #t]
                                                                         [(equal? (category-lang b) "Racket") #f]
                                                                         [else (string<=? (category-lang a) (category-lang b))])]))))])
                                           (make-sec (hash-ref custom-secs label)
                                                     label)))
                                 (list sec)))
                           sections)])
          (if (equal? main-language-family "Racket")
              sections
              ;; move 'racket-core, 'language, and 'teaching to just after 'library
              (let loop ([l sections])
                (define (find name)
                  (ormap (lambda (s) (and (eq? (sec-cat s) name) s)) sections))
                (cond
                  [(null? l) null]
                  [(eq? (sec-cat (car l)) 'library)
                   (list* (car l)
                          (find 'racket-core)
                          (find 'language)
                          (find 'teaching)
                          (loop (cdr l)))]
                  [(memq (sec-cat (car l)) '(racket-core teaching language))
                   (loop (cdr l))]
                  [else (cons (car l) (loop (cdr l)))])))))
      (make-table
       #f
       (maybe-cdr
        (append-map
         (lambda (sec)
           (let ([docs (filter (lambda (doc) (eq? (doc-category doc) (sec-cat sec)))
                               docs)])
             (cond [(null? docs)
                    ;; Drop section if it contains no manuals.
                    null]
                   [else
                    (append
                     ;; Spacer
                     (list (plain-line (hspace 1)))
                     ;; Section title, if any:
                     (if (sec-label sec)
                         (list
                          (plain-line (let loop ([s (sec-label sec)])
                                        (match s
                                          [(category lang cat str) str]
                                          [(list 'elem parts ...)
                                           (apply elem (map loop parts))]
                                          [(list 'link text doc-mod-path)
                                           (seclink "top" #:doc doc-mod-path #:underline? #f text)]
                                          [(list 'link text doc-mod-path tag)
                                           (seclink tag #:doc doc-mod-path #:underline? #f text)]
                                          [_ s]))))
                         null)
                     ;; Documents in section:
                     (add-sections
                      (lambda (str)
                        (plain-line
                         (make-element (if (string=? str "") "sepspace" "septitle")
                                       (list 'nbsp str))))
                      (sort (map (lambda (doc)
                                   (list (doc-priority doc)
                                         (line (doc-spec doc)
                                               (and (sec-label sec) #t))
                                         ""))
                                 docs)
                            (lambda (ad bd)
                              (if (= (car ad) (car bd))
                                  (let ([str (lambda (x)
                                               (regexp-replace
                                                #rx"(?:The|A|An) "
                                                (element->string
                                                 (cadr (paragraph-content
                                                        (car (flow-paragraphs
                                                              (caadr x)))))
                                                 renderer part resolve-info)
                                                ""))])
                                    (string-ci<? (str ad) (str bd)))
                                  (> (car ad) (car bd)))))))])))
         sections+custom))))
    (make-delayed-block contents)))
