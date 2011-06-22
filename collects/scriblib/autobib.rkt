#lang at-exp racket/base
(require scribble/manual
         racket/list
         scribble/core
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         scheme/string
         setup/main-collects)

(provide define-cite
         make-bib in-bib (rename-out [auto-bib? bib?])
         proceedings-location journal-location book-location 
         techrpt-location dissertation-location
         author-name org-author-name authors other-authors editor)

(define autobib-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list
     (make-css-addition (abs "autobib.css"))
     (make-tex-addition (abs "autobib.tex")))))

(define bib-table-style (make-style "AutoBibliography" autobib-style-extras)) 
(define bibentry-style (make-style "Autobibentry" autobib-style-extras))
                                    
(define-struct auto-bib (author date entry-element key specific))
(define-struct bib-group (ht))

(define-struct (author-element element) (names cite))
(define-struct (other-author-element author-element) ())
         
(define (add-cite group bib-entry which with-specific?)
  (let ([key (auto-bib-key bib-entry)])
    (hash-set! (bib-group-ht group) key bib-entry)
    (make-delayed-element
     (lambda (renderer part ri)
       (let ([s (resolve-get part ri `(,which ,key))])
         (list (make-link-element #f 
                                  (list (or s "???") 
                                        (if with-specific?
                                            (auto-bib-specific bib-entry)
                                            ""))
                                  `(autobib ,(auto-bib-key bib-entry))))))
     (lambda () "(???)")
     (lambda () "(???)"))))

(define (add-inline-cite group bib-entries)
  (for ([i bib-entries]) 
    (hash-set! (bib-group-ht group) (auto-bib-key i) i))
  (when (and (pair? (cdr bib-entries)) 
             (not (apply equal? (map (compose author-element-names auto-bib-author)  bib-entries))))
    (error 'citet "citet must be used with identical authors, given ~a" 
           (map (compose author-element-names auto-bib-author) bib-entries)))
  (make-element 
   #f
   (list (add-cite group (car bib-entries) 'autobib-author #f)
         'nbsp
         "("
         (let loop ([keys bib-entries])
           (if (null? (cdr keys))
               (add-cite group (car keys) 'autobib-date #t)
               (make-element
                #f
                (list (loop (list (car keys)))
                      "; "
                      (loop (cdr keys))))))
         ")")))

(define (add-cites group bib-entries sort?)
  (define-values (groups keys)
    (for/fold ([h (hash)] [ks null]) ([b (reverse bib-entries)])
      (let ([k (author-element-names (auto-bib-author b))])
        (values (hash-update h k (lambda (cur) (cons b cur)) null)
                (cons k (remove k ks))))))
  (make-element
   #f
   (append 
    (list 'nbsp "(")
    (add-between
     (for/list ([k (if sort? (sort keys string-ci<?) keys)])
       (let ([v (hash-ref groups k)])
         (make-element 
          #f 
          (list* 
           (add-cite group (car v) 'autobib-author #f)
           " "
           (add-between
            (for/list ([b v]) (add-cite group b 'autobib-date #t))
            ", ")))))
     "; ")
   (list ")"))))

(define (extract-bib-key b)
  (author-element-names (auto-bib-author b)))

(define (extract-bib-year b)
  (string->number (auto-bib-date b)))


(define (gen-bib tag group sec-title)
  (let* ([author/date<? 
          (lambda (a b)
            (or
             (string-ci<? (extract-bib-key a) (extract-bib-key b))
             (and (string-ci=? (extract-bib-key a) (extract-bib-key b))
                  (extract-bib-year a) (extract-bib-year b)
                  (< (extract-bib-year a) (extract-bib-year b)))))]
         [bibs (sort (hash-map (bib-group-ht group)
                               (lambda (k v) v))
                     author/date<?)])
    (make-part
     #f
     `((part ,tag))
     (list sec-title)
     (make-style #f '(unnumbered))
     null
     (list
      (make-table
       bib-table-style
       (map (lambda (k)
              (list
               (make-paragraph
                plain
                (list
                 (make-collect-element
                  #f
                  (list (make-target-element
                         #f
                         (list (auto-bib-entry-element k))
                         `(autobib ,(auto-bib-key k))))
                  (lambda (ci)
                    (collect-put! ci 
                                  `(autobib-author ,(auto-bib-key k))
                                  (make-element
                                   #f
                                   (list
                                    (author-element-cite (auto-bib-author k)))))
                    (collect-put! ci 
                                  `(autobib-date ,(auto-bib-key k))
                                  (make-element
                                   #f
                                   (list
                                    (auto-bib-date k))))))))))
            bibs)))
     null)))

(define-syntax-rule (define-cite ~cite citet generate-bibliography)
  (begin
    (define group (make-bib-group (make-hasheq)))
    (define (~cite #:sort? [sort? #t] bib-entry . bib-entries)
      (add-cites group (cons bib-entry bib-entries) sort?))
    (define (citet bib-entry . bib-entries)
      (add-inline-cite group (cons bib-entry bib-entries)))
    (define (generate-bibliography #:tag [tag "doc-bibliography"] #:sec-title [sec-title "Bibliography"])
      (gen-bib tag group sec-title))))

(define (ends-in-punc? e)
  (regexp-match? #rx"[.!?,]$" (content->string e)))

(define (make-bib #:title title	 	 	 	 
                  #:author [author #f]
                  #:is-book? [is-book? #f]
                  #:location [location #f]
                  #:date [date #f]
                  #:url [url #f])
  (let* ([author (cond
                  [(not author) #f]
                  [(author-element? author) author]
                  [else (parse-author author)])]
         [elem (make-element
                bibentry-style
                (append
                 (if author 
                     `(,author
                       ,@(if (ends-in-punc? author)
                             '(" ")
                             '(". ")))
                     null)
                 ;; (if is-book? null '(ldquo))
                 (if is-book?
                     (list (italic title))
                     (decode-content (list title)))
                 (if (ends-in-punc? title)
                     null
                     '("."))
                 ;; (if is-book? null '(rdquo))
                 (if location
                     `(" " ,@(decode-content (list location)) ,(if date "," "."))
                     null)
                 (if date `(" " ,@(decode-content (list (to-string date))) ".") null)
                 (if url `(" " ,(link url (make-element 'url (list url)))) null)))])
    (make-auto-bib
     (or author (org-author-name title))
     (to-string date)
     elem
     (content->string elem)
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
      (let* ([s (content->string a)]
             [m (regexp-match #px"^(.*) (([\\-]|\\p{L})+)$" s)])
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
                @elem{@|s|, @(format "~a" series)}
                s)]
         [s (if volume
                @elem{@|s| volume @(format "~a" volume)}
                s)]
         [s (if pages
                @elem{@|s|, pp. @(to-string (car pages))--@(to-string (cadr pages))}
                s)])
    s))

(define (journal-location
         location
         #:pages [pages #f]
         #:number [number #f]
         #:volume [volume #f])
  (let* ([s @italic{@|location|}]
         [s (if volume
                @elem{@|s| @(to-string volume)}
                s)]
         [s (if number
                @elem{@|s|(@(to-string number))}
                s)]
         [s (if pages
                @elem{@|s|, pp. @(to-string (car pages))--@(to-string (cadr pages))}
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

(define (other-authors)
  (make-other-author-element
   #f
   (list "Alia")
   "al."
   "al."))

(define (authors name . names)
  (let ([names (map parse-author (cons name names))])
    (make-author-element
     #f
     (let loop ([names names] [prefix 0])
       (if (null? (cdr names))
           (case prefix
             [(0) (list (car names))]
             [(1) (if (other-author-element? (car names))
                      (list " et al.")
                      (list " and " (car names)))]
             [else (if (other-author-element? (car names))
                       (list ", et al.")
                       (list ", and " (car names)))])
           (case prefix
             [(0) (list* (car names)
                         (loop (cdr names) (add1 prefix)))]
             [else (list* ", "
                          (car names)
                          (loop (cdr names) (add1 prefix)))])))
     (string-join (map author-element-names names) " / ")
     (case (length names)
       [(1) (author-element-cite (car names))]
       [(2) (if (other-author-element? (cadr names))
                (format "~a et al." (author-element-cite (car names)))
                (format "~a and ~a" 
                        (author-element-cite (car names))
                        (author-element-cite (cadr names))))]
       [else (format "~a et al." (author-element-cite (car names)))]))))

(define (editor name)
  (let ([name (parse-author name)])
    (make-author-element
     #f
     (append (element-content name)
             '(" (Ed.)"))
     (author-element-names name)
     (author-element-cite name))))

(define (to-string v) (format "~a" v))
