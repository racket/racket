#lang at-exp scheme/base
(require scribble/manual
         scribble/struct
         scribble/decode
         scheme/string)

(provide autobib-style-extras
         define-cite
         make-bib in-bib (rename-out [auto-bib? bib?])
         proceedings-location journal-location book-location 
         techrpt-location dissertation-location
         author-name org-author-name authors editor)

(define (autobib-style-extras)
  (let ([abs (lambda (s)
               (build-path (collection-path "scriblib") s))])
    `((css ,(abs "autobib.css")) (tex ,(abs "autobib.tex")))))

(define-struct auto-bib (author date entry-element key specific))
(define-struct bib-group (ht))

(define-struct (author-element element) (names cite))
         
(define (add-cite group bib-entry which)
  (hash-set! (bib-group-ht group) bib-entry #t)
  (make-delayed-element
   (lambda (renderer part ri)
     (let ([s (resolve-get part ri `(,which ,(auto-bib-key bib-entry)))])
       (list (make-link-element #f 
                                (list s (auto-bib-specific bib-entry))
                                `(autobib ,(auto-bib-key bib-entry))))))
   (lambda () "(???)")
   (lambda () "(???)")))

(define (add-cites group bib-entries)
  (make-element
   #f
   (list 'nbsp
         "("
         (let loop ([keys bib-entries])
           (if (null? (cdr keys))
               (add-cite group (car keys) 'autobib-cite)
               (make-element
                #f
                (list (loop (list (car keys)))
                      "; "
                      (loop (cdr keys))))))
         ")")))

(define (gen-bib tag group)
  (let* ([author<? (lambda (a b)
                     (string<? (author-element-names (auto-bib-author a))
                               (author-element-names (auto-bib-author b))))]
         [bibs (sort (hash-map (bib-group-ht group)
                               (lambda (k v) k))
                     author<?)])
    (make-unnumbered-part
     #f
     `((part ,tag))
     '("Bibliography")
     '()
     null
     (make-flow
      (list
       (make-table
        "SBibliography"
        (map (lambda (k)
               (list
                (make-flow
                 (list
                  (make-paragraph
                   (list
                    (make-collect-element
                     #f
                     (list (make-target-element
                            #f
                            (list (auto-bib-entry-element k))
                            `(autobib ,(auto-bib-key k))))
                     (lambda (ci)
                       (collect-put! ci 
                                     `(autobib-cite ,(auto-bib-key k))
                                     (make-element
                                      #f
                                      (list
                                       (author-element-cite (auto-bib-author k))
                                       " "
                                       (auto-bib-date k))))
                       (collect-put! ci 
                                     `(autobib-inline ,(auto-bib-key k))
                                     (make-element
                                      #f
                                      (list
                                       (author-element-cite (auto-bib-author k))
                                       'nbsp
                                       "("
                                       (auto-bib-date k)
                                       ")")))))))))))
             bibs))))
     null)))

(define-syntax-rule (define-cite ~cite citet generate-bibliography)
  (begin
    (define group (make-bib-group (make-hasheq)))
    (define (~cite bib-entry . bib-entries)
      (add-cites group (cons bib-entry bib-entries)))
    (define (citet bib-entry)
      (add-cite group bib-entry 'autobib-inline))
    (define (generate-bibliography #:tag [tag "doc-bibliography"])
      (gen-bib tag group))))

(define (make-bib #:title title	 	 	 	 
                  #:author [author #f]
                  #:is-book? [is-book? #f]
                  #:location [location #f]
                  #:date [date #f]
                  #:url [url #f])
  (let* ([author (if (author-element? author)
                     author
                     (parse-author author))]
         [elem (make-element
                "bibentry"
                (append
                 (if author `(,@(decode-content (list author)) ". ") null)
                 ;; (if is-book? null '(ldquo))
                 (if is-book?
                     (list (italic title))
                     (decode-content (list title)))
                 (if (regexp-match? #rx"[.!?,]$" (element->string title))
                     null
                     '("."))
                 ;; (if is-book? null '(rdquo))
                 (if location
                     `(" " ,@(decode-content (list location)) ,(if date "," "."))
                     null)
                 (if date `(" " ,@(decode-content (list date)) ".") null)
                 (if url `(" " ,(link url (make-element 'url (list url)))) null)))])
    (make-auto-bib
     author
     date 
     elem
     (element->string elem)
     "")))

(define (in-bib bib where)
  (make-auto-bib
   (auto-bib-author bib)
   (auto-bib-date bib)
   (auto-bib-entry-element bib)
   (auto-bib-key bib)
   (string-append (auto-bib-specific bib) where)))

(define (parse-author a)
  (if (author-element? a)
      a
      (let* ([s (element->string a)]
             [m (regexp-match #rx"^(.*) ([A-Za-z]+)$" s)])
        (make-author-element
         #f
         (list a)
         (if m
             (string-append (caddr m) " " (cadr m))
             s)
         (if m
             (caddr m)
             s)))))
    
(define (proceedings-location
         location
         #:pages [pages #f]
         #:series [series #f]
         #:volume [volume #f])
  (let* ([s @elem{In @italic{@elem{Proc. @|location|}}}]
         [s (if series
                @elem{@|s|, @|series|}
                s)]
         [s (if volume
                @elem{@|s| volume @|volume|}
                s)]
         [s (if pages
                @elem{@|s|, pp. @(car pages)--@(cadr pages)}
                s)])
    s))

(define (journal-location
         location
         #:pages [pages #f]
         #:number [number #f]
         #:volume [volume #f])
  (let* ([s @italic{@|location|}]
         [s (if volume
                @elem{@|s| @|volume|}
                s)]
         [s (if number
                @elem{@|s|(@|number|)}
                s)]
         [s (if pages
                @elem{@|s|, pp. @(car pages)--@(cadr pages)}
                s)])
    s))

(define (book-location
         #:edition [edition #f]
         #:publisher [publisher #f])
  (let* ([s (if edition
                @elem{@(string-titlecase edition) edition}
                #f)]
         [s (if publisher
                (if s
                   @elem{@|s|. @|publisher|}
                   publisher)
                s)])
    (unless s
      (error 'book-location "no arguments"))
    s))

(define (techrpt-location
         #:institution org
         #:number num)
  @elem{@|org|, @|num|})

(define (dissertation-location
         #:institution org
         #:degree [degree "PhD"])
  @elem{@|degree| dissertation, @|org|})

;; ----------------------------------------

(define (author-name first last #:suffix [suffix #f])
  (make-author-element
   #f
   (list
    (format "~a ~a~a" first last (if suffix
                                     (format " ~a" suffix)
                                     "")))
   (format "~a ~a~a" last first (if suffix
                                    (format " ~a" suffix)
                                    ""))
   last))

(define (org-author-name org)
  (make-author-element
   #f
   (list org)
   org
   org))

(define (authors name . names)
  (let ([names (map parse-author (cons name names))])
    (make-author-element
     #f
     (let loop ([names names] [prefix 0])
       (if (null? (cdr names))
           (case prefix
             [(0) (list (car names))]
             [(1) (list " and " (car names))]
             [else (list ", and " (car names))])
           (case prefix
             [(0) (list* (car names)
                         (loop (cdr names) (add1 prefix)))]
             [else (list* ", "
                          (car names)
                          (loop (cdr names) (add1 prefix)))])))
     (string-join (map author-element-names names) " / ")
     (case (length names)
       [(1) (author-element-cite (car names))]
       [(2) (format "~a and ~a" 
                    (author-element-cite (car names))
                    (author-element-cite (cadr names)))]
       [else (format "~a et al." (author-element-cite (car names)))]))))

(define (editor name)
  (let ([name (parse-author name)])
    (make-author-element
     #f
     (append (element-content name)
             '(" (Ed.)"))
     (author-element-names name)
     (author-element-cite name))))
