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
(provide perform-search)

(define (send-main-page)
  (let* ([path (build-path (find-user-doc-dir) "index.html")]
         [path (if (file-exists? path)
                 path (build-path (find-doc-dir) "index.html"))])
    (send-url/file path)))

;; Configuration of search results
(define maximum-entries 500)
(define exact-score 1000)
(define exact-word-score 600)
(define words1-score 400)
(define words2-score 100)
(define prefix-score 200)
(define suffix-score  20)
(define contain-score 10)
(define exported-entry-factor 1.1) ; prefer bindings and modules
(define regexp-score-factor 1.1) ; regexps get higher score
(define nomatch-score -1) ; prefer less irrelevant terms

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

;; converts a list of search terms to a scoring function
(define (terms->scorer terms exact?)
  ;; turns a string to one that matches word prefixes (eg turn a "reg-qu"
  ;; string to "reg\\w*-qu\\w*"), as with convenient completers like Emacs or
  ;; zsh.
  (define (words-rx str pat)
    (let* ([strs (reverse (map (lambda (s) (regexp-quote s #f))
                               (regexp-split #px"(?<=\\w)\\b" str)))]
           [strs (cons (car strs)
                       (map (lambda (s) (string-append s pat)) (cdr strs)))])
      (string-append "^" (apply string-append (reverse strs)) "$")))
  (define scorers
    (map (lambda (term)
           (let* ([rx?     (regexp? term)]
                  [rx      (if rx? (object-name term) (regexp-quote term #f))]
                  ;; note: still works if we're given a regexp with ^/$ anchors
                  [exact   (regexp (format "^~a$" rx))]
                  [prefix  (regexp (format "^~a" rx))]
                  [suffix  (regexp (format "~a$" rx))]
                  [exact-word (and (not rx?) (pregexp (format "\\b~a\\b" rx)))]
                  [words1  (and (not rx?) (pregexp (words-rx term "\\w*")))]
                  [words2  (and (not rx?) (pregexp (words-rx term ".*")))]
                  [contain (if rx? term (regexp rx))])
             (lambda (str)
               (let* ([match? (lambda (rx) (and rx (regexp-match? rx str)))]
                      [sc (cond [(match? exact) exact-score]
                                [exact? nomatch-score]
                                [(match? exact-word) exact-word-score]
                                [(match? words1)     words1-score]
                                [(match? words2)     words2-score]
                                [(match? prefix)     prefix-score]
                                [(match? suffix)     suffix-score]
                                [(match? contain)    contain-score]
                                [else nomatch-score])]
                      [sc (if (and rx? (sc . > . 0))
                            (* sc regexp-score-factor)
                            sc)])
                 sc))))
         terms))
  (lambda (entry)
    (let ([sc (foldl (lambda (word acc)
                       (+ acc (foldl (lambda (sc acc) (+ acc (sc word)))
                                     0 scorers)))
                     0
                     (entry-words entry))])
      ;; give some bonus for bindings and modules
      (let ([desc (entry-desc entry)])
        (if (or (exported-index-desc? desc) (module-path-index-desc? desc))
          (* sc exported-entry-factor)
          sc)))))

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
