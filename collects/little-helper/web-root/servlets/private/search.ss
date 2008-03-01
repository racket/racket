; This is a copy of /collects/help/search.ss (just temporary).
; The function search-results-page have been added.

#lang scheme/base

(require setup/xref
         scribble/xref
         scribble/struct
         scribble/manual-struct
         scribble/decode
         scribble/manual
         (prefix-in scheme: scribble/scheme)
         net/sendurl
         net/uri-codec
         mzlib/contract
         setup/dirs)

(provide/contract [send-main-page (-> void?)])
(provide perform-search search-results-page)

(define (send-main-page)
  (let* ([path (build-path (find-user-doc-dir) "index.html")]
         [path (if (file-exists? path)
                 path (build-path (find-doc-dir) "index.html"))])
    (send-url/file path)))

;; Configuration of search results
(define maximum-entries 500)
(define exact-score 1000)
(define prefix-score 200)
(define suffix-score  20)
(define contain-score 10)
(define words-factor 0.9)
(define exported-entry-factor 1.1) ; prefer bindings and modules
(define regexp-score-factor 1.1) ; regexps get higher score
(define nomatch-score -5) ; prefer less irrelevant terms

(define (perform-search terms #:exact? [exact? #f] #:go-if-one? [go-if-one? #t])
  (if (null? terms)
    (send-main-page)
    (let* ([xref (load-collections-xref)]
           [scorer (terms->scorer terms exact?)]
           [entries (xref-index xref)]
           [entries
            (let loop ([es entries] [r '()])
              (if (null? es)
                r
                (loop (cdr es)
                      (let* ([e (car es)] [score (scorer e)])
                        (if (score . > . 0) (cons (cons score e) r) r)))))]
           ;; use to debug weird search results
           ;; [_ (for ([x (sort entries scored-entry<?)])
           ;;      (printf "~a ~s\n" (car x) (entry-words (cdr x))))]
           [entries (map cdr (sort entries scored-entry<?))])
      (if (and go-if-one? (= 1 (length entries)))
        (let*-values ([(tag) (entry-tag (car entries))]
                      [(path tag) (xref-tag->path+anchor xref tag)])
          (send-url/file path #:fragment (and tag (uri-encode tag))))
        (let* ([term->label
                (λ (t) (format "``~a''" (if (regexp? t) (object-name t) t)))]
               [search-title ; note: terms is not null at this point (see above)
                (apply string-append (term->label (car terms))
                       (map (λ (x) (format ", ~a" (term->label x)))
                            (cdr terms)))]
               [search-title (string-append "Search results for " search-title)]
               [contents
                (if (null? entries)
                  (list (make-element "schemeerror" (list "No results found.")))
                  (build-itemization entries))]
               [contents (cons (title search-title) contents)])
          (send-url/contents (xref-render xref (decode contents) #f)
                             #:delete-at (* 60 10)))))))

; Same as perform-search but instead of sending the result to the browser,
; it just returns the result.
; TODO : Get this to return relative urls in the link targets. 
(define (search-results-page terms 
                             #:exact? [exact? #f] 
                             #:go-if-one? [go-if-one? #t] 
                             #:base-url [base-url "file://"])
  (if (null? terms)
    "No Terms" ; todo
    (let* ([xref (load-collections-xref)]
           [scorer (terms->scorer terms exact?)]
           [entries (xref-index xref)]
           [entries
            (let loop ([es entries] [r '()])
              (if (null? es)
                  r
                  (loop (cdr es)
                        (let* ([e (car es)] [score (scorer e)])
                          (if (score . > . 0) (cons (cons score e) r) r)))))]
           ;; use to debug weird search results
           ;; [_ (for ([x (sort entries scored-entry<?)])
           ;;      (printf "~a ~s\n" (car x) (entry-words (cdr x))))]
           [entries (map cdr (sort entries scored-entry<?))])
      (if (and go-if-one? (= 1 (length entries)))
        (let*-values ([(tag) (entry-tag (car entries))]
                      [(path tag) (xref-tag->path+anchor xref tag)])
          (string-append base-url "view.scm/" path (and tag (uri-encode tag))))
        (let* ([term->label
                (λ (t) (format "``~a''" (if (regexp? t) (object-name t) t)))]
               [search-title ; note: terms is not null at this point (see above)
                (apply string-append (term->label (car terms))
                       (map (λ (x) (format ", ~a" (term->label x)))
                            (cdr terms)))]
               [search-title (string-append "Search results for " search-title)]
               [contents
                (if (null? entries)
                  (list (make-element "schemeerror" (list "No results found.")))
                  (build-itemization entries))]
               [contents (cons (title search-title) contents)])
          (xref-render xref (decode contents) #f))))))


;; converts a list of search terms to a scoring function
(define (terms->scorer terms exact? [words? #f])
  ;; turns a string to one that matches word prefixes (eg turn a "reg-qu"
  ;; string to "reg\\w*-qu\\w*"), as with convenient completers like Emacs or
  ;; zsh.
  (define (collect-words strings)
    (apply append (map (lambda (t) (regexp-match* #px"\\w+" t))
                       (filter string? strings))))
  (define scorers
    (map (lambda (term)
           (let* ([rx?     (regexp? term)]
                  [rx      (if rx? (object-name term) (regexp-quote term #f))]
                  ;; note: still works if we're given a regexp with ^/$ anchors
                  [exact   (regexp (format "^~a$" rx))]
                  [prefix  (regexp (format "^~a" rx))]
                  [suffix  (regexp (format "~a$" rx))]
                  [contain (if rx? term (regexp rx))])
             (lambda (str)
               (let* ([match? (lambda (rx sc)
                                (if (and rx (regexp-match? rx str)) sc 0))]
                      [sc (if exact?
                            (match? exact exact-score)
                            (+ (match? exact   exact-score)
                               (match? prefix  prefix-score)
                               (match? suffix  suffix-score)
                               (match? contain contain-score)))]
                      [sc (cond [(sc . <= . 0) nomatch-score]
                                [rx? (* sc regexp-score-factor)]
                                [else sc])])
                 sc))))
         terms))
  (define word-scorer
    (and (not words?) (not exact?) (terms->scorer (collect-words terms) #f #t)))
  (lambda (entry)
    (let* ([words (entry-words entry)]
           [words (if words? (collect-words words) words)]
           [sc (foldl (lambda (word acc)
                        (+ acc (foldl (lambda (sc acc) (+ acc (sc word)))
                                      0 scorers)))
                      0
                      words)])
      (if words?
        sc
        (let ([desc (entry-desc entry)]
              [sc (+ sc (* words-factor (word-scorer entry)))])
          ;; give some bonus for bindings and modules
          (if (or (exported-index-desc? desc) (module-path-index-desc? desc))
            (* sc exported-entry-factor)
            sc))))))

(define (scored-entry<? x y)
  (let ([xsc (car x)] [ysc (car y)])
    (cond [(> xsc ysc) #t]
          [(< xsc ysc) #f]
          [else (let loop ([xs (entry-words (cdr x))]
                           [ys (entry-words (cdr y))])
                  (cond [(null? ys) #f]
                        [(null? xs) #t]
                        [(string-ci=? (car xs) (car ys))
                         (or (loop (cdr xs) (cdr ys))
                             ;; Try string<? so "Foo" still precedes "foo"
                             (string<? (car xs) (car ys)))]
                        [else (string-ci<? (car xs) (car ys))]))])))

;; build-itemization : (nonempty-listof entry) -> (listof <stuff>)
(define (build-itemization entries)
  (define entries*
    (if (<= (length entries) maximum-entries)
      entries
      (let loop ([n maximum-entries] [es entries] [r '()])
        (if (or (null? es) (zero? n))
          (reverse r)
          (loop (sub1 n) (cdr es) (cons (car es) r))))))
  (cons (apply itemize
               (map (λ (entry)
                       (apply item
                              (make-link-element "indexlink"
                                                 (entry-content entry)
                                                 (entry-tag entry))
                              (make-extra-content (entry-desc entry))))
                    entries*))
        (if (eq? entries* entries)
          '()
          (list (make-element "schemeerror"
                              (list (format "Search truncated after ~a hits."
                                            maximum-entries)))))))

(define (make-extra-content desc)
  ;; Use `desc' to provide more details on the link:
  (append
   (if (method-index-desc? desc)
     (list " method of "
           ;; This is bad. We need a more abstract way to take a
           ;; binding name and tag/source to create a Scheme link.
           (make-element
            "schemesymbol"
            (list (make-link-element
                   "schemevaluelink"
                   (list (symbol->string (exported-index-desc-name desc)))
                   (method-index-desc-class-tag desc)))))
     null)
   (if (and (exported-index-desc? desc)
            (not (null? (exported-index-desc-from-libs desc))))
     (cons ", provided from "
           (cdr (apply append
                       (map (lambda (lib) (list ", " (scheme:to-element lib)))
                            (exported-index-desc-from-libs desc)))))
     null)))
