#lang scheme/base

(require setup/xref
         scribble/xref
         scribble/struct
         scribble/manual-struct
         scribble/decode
         scribble/basic
         scribble/manual
         (prefix-in scheme: scribble/scheme)
         net/sendurl
         mzlib/contract)

(provide/contract
 [generate-search-results (-> (listof string?) void?)])

(define (make-extra-content desc)
  ;; Use `desc' to provide more details on the link:
  (append
   (cond
     [(method-index-desc? desc)
      (list " method of "
            ;; This is bad. We need a more abstract way to take a
            ;; binding name and tag/source to create a Scheme link.
            (make-element
             "schemesymbol"
             (list (make-link-element
                    "schemevaluelink"
                    (list (symbol->string (exported-index-desc-name desc)))
                    (method-index-desc-class-tag desc)))))]
     [else null])
   (cond
     [(and (exported-index-desc? desc)
           (not (null? (exported-index-desc-from-libs desc))))
      (cons ", provided from "
            (cdr (apply append
                        (map (lambda (lib)
                               (list ", "
                                     (scheme:to-element lib)))
                             (exported-index-desc-from-libs desc)))))]
     [else null])))

(define search-results-files
  (reverse
   (let loop ([n 10])
     (cond
       [(zero? n) '()]
       [else
        (cons (build-path (find-system-path 'temp-dir) 
                          (format "search-results-~a.html" n))
              (loop (- n 1)))]))))

(define (next-search-results-file)
  (begin0 (car search-results-files)
          (set! search-results-files 
                (append (cdr search-results-files) 
                        (list (car search-results-files))))))

(define (generate-search-results search-keys)
  (let ([file (next-search-results-file)]
        [search-regexps (map (λ (x) (regexp (regexp-quote x #f))) search-keys)]
        [exact-search-regexps (map (λ (x) (regexp (format "^~a$" (regexp-quote x #f)))) search-keys)]
        [search-key-string
         (cond
           [(null? search-keys) ""]
           [else
            (apply
             string-append
             (car search-keys)
             (map (λ (x) (format ", or ~a" x))
                  (cdr search-keys)))])])
    (let ([x (load-collections-xref)])
      (xref-render
       x
       (decode `(,(title (format "Search results for ~a" search-key-string))
                 ,@(let* ([index (xref-index x)]
                          [len (length index)]
                          [matching-entries (filter (has-match search-regexps) index)]
                          [exact-matches (filter (has-match exact-search-regexps) matching-entries)]
                          [inexact-matches (filter (compose not (has-match exact-search-regexps))  matching-entries)])
                     (append
                      (build-itemization "Exact matches" exact-matches)
                      (build-itemization "Containing matches" inexact-matches)))))
       file)
      (send-url (format "file://~a" (path->string file)))
      (void))))

;; has-match : (listof regexp) -> entry -> boolean
(define ((has-match search-regexps) entry)
  (ormap (λ (str) 
           (ormap 
            (λ (key) (regexp-match key str))
            search-regexps))
         (entry-words entry)))

;; build-itemization : (listof entry) -> (listof <stuff>)
(define (build-itemization title entries)
  (cond
    [(null? entries) '()]
    [else 
     (list 
      (bold title)
      (apply itemize
             (map
              (λ (entry)
                (apply item
                       (make-link-element
                        "indexlink"
                        (entry-content entry)
                        (entry-tag entry))
                       (make-extra-content
                        (entry-desc entry))))
              (limit-length
               500
               (sort
                entries
                (λ (x y) (string-ci<=? (entry->sort-key x) (entry->sort-key y))))))))]))

(define (limit-length n l)
  (cond
    [(zero? n) '()]
    [(null? l) '()]
    [else (cons (car l) (limit-length (- n 1) (cdr l)))]))

(define (entry->sort-key e)
  (let ([words (entry-words e)])
    (apply string-append
           (car words)
           (map (λ (x) (string-append ", " x))
                (cdr words)))))
