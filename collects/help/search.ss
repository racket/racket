#lang scheme/base

(require setup/scribble-index
         scribble/struct
         scribble/manual-struct
         scribble/decode
         scribble/basic
         scribble/manual
         (prefix-in scheme: scribble/scheme)
         browser/external
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
        [search-regexps (map (λ (x) (regexp-quote x #f)) search-keys)]
        [search-key-string
         (cond
           [(null? search-keys) ""]
           [else
            (apply
             string-append
             (car search-keys)
             (map (λ (x) (format ", or ~a" x))
                  (cdr search-keys)))])])
    (let ([x (load-xref)])
      (xref-render
       x
       (decode `(,(title (format "Search results for ~a" search-key-string))
                 ,(let* ([index (xref-index x)]
                         [len (length index)])
                    (apply itemize
                           (map
                            (λ (entry)
                              (apply item
                                     (make-link-element
                                      "indexlink"
                                      (entry-content entry)
                                      (entry-link-key entry))
                                     (make-extra-content
                                      (entry-desc entry))))
                            (filter
                             (λ (entry) 
                               (ormap (λ (str) 
                                        (ormap 
                                         (λ (key) (regexp-match key str))
                                         search-regexps))
                                      (entry-words entry)))
                             index))))))
       file)
      (send-url (format "file://~a" (path->string file)))
      (void))))
