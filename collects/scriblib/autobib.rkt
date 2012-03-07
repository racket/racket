#lang at-exp racket/base
(require scribble/manual
         racket/list
         scribble/core
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         (for-syntax syntax/parse
                     racket/base)
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

;; render the use of a citation.
(define (add-cite group bib-entry which with-specific? with-disambiguation?)
  (let ([key (auto-bib-key bib-entry)])
    (hash-set! (bib-group-ht group) key bib-entry)
    (make-delayed-element
     (lambda (renderer part ri)
       ;; (list which key) should be mapped to the bibliography element.
       (define s (resolve-get part ri `(,which ,key)))
       (define disambiguation
         (cond [(and with-disambiguation?
                     (resolve-get part ri `(autobib-disambiguation ,key)))
                =>
                (λ (dis)
                   (case dis
                     [(unambiguous) '()]
                     [else
                      (list
                       (make-link-element #f (list dis)
                                          `(autobib ,(auto-bib-key bib-entry))))]))]
               [else '()]))
       (cons (make-link-element #f
                                (list (or s "???")
                                      (if with-specific?
                                          (auto-bib-specific bib-entry)
                                          ""))
                                `(autobib ,(auto-bib-key bib-entry)))
             disambiguation))
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
   (list (add-cite group (car bib-entries) 'autobib-author #f #f)
         'nbsp
         "("
         (let loop ([keys bib-entries])
           (if (null? (cdr keys))
               (add-cite group (car keys) 'autobib-date #t #t)
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
           (add-cite group (car v) 'autobib-author #f #f)
           " "
           (add-between
            (for/list ([b v]) (add-cite group b 'autobib-date #t #t))
            ", ")))))
     "; ")
   (list ")"))))

(define (extract-bib-key b)
  (author-element-names (auto-bib-author b)))

(define (extract-bib-year b)
  (string->number (auto-bib-date b)))

;; 0 -> a, 1 -> b, etc.
(define (default-disambiguation n)
  (when (>= n 26)
    (error 'default-disambiguation "Citations too ambiguous for default disambiguation scheme."))
  (make-element #f (list (format " ~a" (integer->char (+ 97 n))))))

(define (gen-bib tag group sec-title maybe-disambiguator)
  (define disambiguator (or maybe-disambiguator default-disambiguation))
  (define (author/date<? a b)
    (or (string-ci<? (extract-bib-key a) (extract-bib-key b))
        (and (string-ci=? (extract-bib-key a) (extract-bib-key b))
             (extract-bib-year a)
             (extract-bib-year b)
             (< (extract-bib-year a) (extract-bib-year b)))))
  (define (ambiguous? a b)
    (and (string-ci=? (extract-bib-key a) (extract-bib-key b))
         (extract-bib-year a)
         (extract-bib-year b)
         (= (extract-bib-year a) (extract-bib-year b))))
  (define bibs (sort (hash-values (bib-group-ht group))
                     author/date<?))
  (define disambiguated
    (let ()
      (define (bib->para bib [disambiguation #f])
        (define collect-target
          (list (make-target-element
                  #f
                  (list (auto-bib-entry-element bib))
                  `(autobib ,(auto-bib-key bib)))))
        ;; Communicate to scribble's resolve step.
        (define (collect ci)
          ;; store the author
          (collect-put! ci
                        `(autobib-author ,(auto-bib-key bib)) ;; (list which key)
                        (make-element
                         #f
                         (list (author-element-cite (auto-bib-author bib)))))
          ;; store the date
          (collect-put! ci
                        `(autobib-date ,(auto-bib-key bib)) ;; (list which key)
                        (make-element #f (list (auto-bib-date bib))))
          ;; store how to disambiguate it from other like citations.
          (collect-put! ci
                        `(autobib-disambiguation ,(auto-bib-key bib))
                        (or disambiguation 'unambiguous)))
        (list
         (make-paragraph plain
                         (list (make-collect-element #f collect-target collect)))))
      ;; create the bibliography with disambiguations added.
      (define-values (last num-ambiguous rev-disambiguated*)
        (for/fold ([last #f] [num-ambiguous 0] [rev-disambiguated '()]) ([bib (in-list bibs)])
          (define ambiguous?? (and last (ambiguous? last bib)))
          (define num-ambiguous*
            (cond [ambiguous?? (add1 num-ambiguous)]
                  [else 0]))
          ;; the current entry is ambiguous with the last. Modify the last
          ;; to have the first disambiguation.
          (define rev-disambiguated*
            (cond [(and ambiguous?? (= 0 num-ambiguous))
                   (cons (bib->para last (disambiguator num-ambiguous))
                         (cdr rev-disambiguated))]
                  [else rev-disambiguated]))
          (define para*
            (bib->para bib (and ambiguous?? (disambiguator num-ambiguous*))))
          (values bib num-ambiguous* (cons para* rev-disambiguated*))))
      (reverse rev-disambiguated*)))
  (make-part #f
             `((part ,tag))
             (list sec-title)
             (make-style #f '(unnumbered))
             null
             (list (make-table bib-table-style disambiguated))
             null))

(define-syntax (define-cite stx)
  (syntax-parse stx
    [(_ cite* citet generate-bibliography
        (~optional (~seq #:disambiguate fn)
                   #:defaults ([fn #'#f])))
     (syntax/loc stx
       (begin
         (define group (make-bib-group (make-hasheq)))
         (define (cite* #:sort? [sort? #t] bib-entry . bib-entries)
           (add-cites group (cons bib-entry bib-entries) sort?))
         (define (citet bib-entry . bib-entries)
           (add-inline-cite group (cons bib-entry bib-entries)))
         (define (generate-bibliography #:tag [tag "doc-bibliography"] #:sec-title [sec-title "Bibliography"])
           (gen-bib tag group sec-title fn))))]))

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
         [content
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
           (if url `(" " ,(link url (make-element 'url (list url)))) null))]
         [elem (make-element bibentry-style content)])
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
   ;; "where" is the only specific part of auto-bib elements currently.
   (string-append (auto-bib-specific bib) where)))

(define (parse-author a)
  (cond [(author-element? a) a]
        [else
         (define s (content->string a)) ;; plain text rendering
         (define m (regexp-match #px"^(.*) (([\\-]|\\p{L})+)$" s))
         (define names
           (cond [m (string-append (caddr m) " " (cadr m))]
                 [else s]))
         (define cite
           (cond [m (caddr m)]
                 [else s]))
         (make-author-element #f (list a) names cite)]))

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

(define (authors name . names*)
  (define names (map parse-author (cons name names*)))
  (define slash-names (string-join (map author-element-names names) " / "))
  (define cite
    (case (length names)
      [(1) (author-element-cite (car names))]
      [(2) (if (other-author-element? (cadr names))
               (format "~a et al." (author-element-cite (car names)))
               (format "~a and ~a"
                       (author-element-cite (car names))
                       (author-element-cite (cadr names))))]
      [else (format "~a et al." (author-element-cite (car names)))]))
  (make-author-element
     #f
     (let loop ([names names] [prefix 0])
       (cond [(null? (cdr names))
              (case prefix
                [(0) names]
                [(1) (if (other-author-element? (car names))
                         (list " et al.")
                         (list " and " (car names)))]
                [else (if (other-author-element? (car names))
                          (list ", et al.")
                          (list ", and " (car names)))])]
             [else
              (case prefix
                [(0) (list* (car names)
                            (loop (cdr names) (add1 prefix)))]
                [else (list* ", "
                             (car names)
                             (loop (cdr names) (add1 prefix)))])]))
     slash-names
     cite))

(define (editor name)
  (let ([name (parse-author name)])
    (make-author-element
     #f
     (append (element-content name)
             '(" (Ed.)"))
     (author-element-names name)
     (author-element-cite name))))

(define (to-string v) (format "~a" v))
