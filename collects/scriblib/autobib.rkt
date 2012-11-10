#lang at-exp racket/base
(require scribble/manual
         racket/list
         racket/date
         racket/class
         scribble/core
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         (for-syntax syntax/parse
                     racket/base)
         scheme/string
         setup/main-collects
         racket/contract)

(provide define-cite
         author+date-style number-style
         make-bib in-bib (rename-out [auto-bib? bib?])
         proceedings-location journal-location book-location
         techrpt-location dissertation-location
         author-name org-author-name 
         (contract-out
          [authors (->* (content?) #:rest (listof content?) element?)])
         other-authors
         editor)

(define autobib-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list
     (make-css-addition (abs "autobib.css"))
     (make-tex-addition (abs "autobib.tex")))))

(define bib-single-style (make-style "AutoBibliography" autobib-style-extras))
(define bib-columns-style (make-style #f autobib-style-extras))

(define bibentry-style (make-style "Autobibentry" autobib-style-extras))
(define colbibnumber-style (make-style "Autocolbibnumber" autobib-style-extras))
(define colbibentry-style (make-style "Autocolbibentry" autobib-style-extras))

(define-struct auto-bib (author date title location url is-book? key specific))
(define-struct bib-group (ht))

(define-struct (author-element element) (names cite))
(define-struct (other-author-element author-element) ())

(define (author-element-names* x)
  (and x (author-element-names x)))

;; render the use of a citation.
(define (add-cite group bib-entry which with-specific? disambiguation style)
  (let ([key (auto-bib-key bib-entry)])
    (when disambiguation
      (for ([bib disambiguation])
        (hash-set! (bib-group-ht group) (auto-bib-key bib) bib)))
    (hash-set! (bib-group-ht group) key bib-entry)
    (make-delayed-element
     (lambda (renderer part ri)
       ;; (list which key) should be mapped to the bibliography element.
       (define s (resolve-get part ri `(,which ,key)))
       (make-link-element #f
                          (list (or s "???")
                                (cond [(not (send style disambiguate-date?)) '()]
                                      [disambiguation ;; should be a list of bib-entries with same author/date
                                       (define disambiguation*
                                         (add-between (for/list ([bib (in-list disambiguation)])
                                                        (define key (auto-bib-key bib))
                                                        (define maybe-disambiguation
                                                          (resolve-get part ri `(autobib-disambiguation ,key)))
                                                        (case maybe-disambiguation
                                                          [(#f) #f]
                                                          [(unambiguous) #f]
                                                          [else (make-link-element #f maybe-disambiguation `(autobib ,key))]))
                                                      ","))
                                       (cond [(not (car disambiguation*)) '()] ;; the bib was unambiguous
                                             [else disambiguation*])]
                                      [else '()])
                                (if with-specific?
                                    (auto-bib-specific bib-entry)
                                    ""))
                          `(autobib ,(auto-bib-key bib-entry))))
     (lambda () "(???)")
     (lambda () "(???)"))))

(define (add-date-cites group bib-entries delimiter style sort? maybe-date<? maybe-date=?)
  (define date<? (or maybe-date<? default-date<?))
  (define date=? (or maybe-date=? default-date=?))
  (define sorted-by-date (if sort?
                             (sort bib-entries date<?)
                             bib-entries))
  (define partitioned-by-ambiguity
    (let-values ([(last last-ambiguous-list partition)
                  (for/fold ([last #f]
                             [currently-ambiguous '()]
                             [partition '()])
                      ([bib (reverse sorted-by-date)])
                    (cond [(and (send style collapse-for-date?)
                                last (date=? last bib) 
                                (equal? (auto-bib-specific bib) "")
                                (equal? (auto-bib-specific last) ""))
                           ;; can group
                           (values bib (cons bib currently-ambiguous) partition)]
                          ;; first element.
                          [(not last) (values bib (list bib) partition)]
                          ;; not ambiguous. Start next group.
                          [else (values bib (list bib) (cons currently-ambiguous partition))]))])
      (cons last-ambiguous-list partition)))
  (cond [(null? bib-entries) '()]
        [else
         (add-between
          (for/list ([part (in-list partitioned-by-ambiguity)])
            (add-cite group (car part) 'autobib-date #t part style))
          delimiter)]))

(define all-equal?
  (case-lambda
   [(a) #t]
   [(a b) (equal? a b)]
   [(a . bs) (andmap (lambda (v) (equal? a v)) bs)]))

(define (add-inline-cite group bib-entries style bib-date<? bib-date=?)
  (for ([i bib-entries])
    (hash-set! (bib-group-ht group) (auto-bib-key i) i))
  (when (and (pair? (cdr bib-entries))
             (not (apply all-equal? (map (compose author-element-names* auto-bib-author) bib-entries))))
    (error 'citet "citet must be used with identical authors, given ~a"
           (map (compose author-element-names* auto-bib-author) bib-entries)))
  (make-element
   #f
   (list (add-cite group (car bib-entries) 'autobib-author #f #f style)
         'nbsp
         (send style get-cite-open)
         (add-date-cites group bib-entries 
                         (send style get-group-sep)
                         style #t bib-date<? bib-date=?)
         (send style get-cite-close))))

;; This allows citing multiple sources in one @cite. Groups of citations are separated by semicolons.
(define (add-cites group bib-entries sort? style bib-date<? bib-date=?)
  (define-values (groups keys)
    (for/fold ([h (hash)] [ks null]) ([b (reverse bib-entries)])
      (let ([k (author-element-names* (auto-bib-author b))])
        (values (hash-update h k (lambda (cur) (cons b cur)) null)
                (cons k (remove k ks))))))
  (make-element
   #f
   (append
    (list 'nbsp (send style get-cite-open))
    (add-between
     (for/list ([k (if sort? (sort keys string-ci<?) keys)])
       (let ([v (hash-ref groups k)])
         (make-element
          #f
          (send style
                render-author+dates
                (add-cite group (car v) 'autobib-author #f #f style)
                (add-date-cites group v (send style get-item-sep) style sort? bib-date<? bib-date=?)))))
     (send style get-group-sep))
   (list (send style get-cite-close)))))

(define (extract-bib-author b)
  (or (auto-bib-author b)
      (org-author-name (auto-bib-title b))))

(define (extract-bib-key b)
  (author-element-names (extract-bib-author b)))

;; Defaults only care about the year.
(define (default-render-date-bib date)
  (make-element #f (list (number->string (date-year date)))))
(define (default-render-date-cite date)
  (make-element #f (list (number->string (date-year date)))))
(define (default-date<? b0 b1)
  (and (auto-bib-date b0) (auto-bib-date b1)
       (< (date-year (auto-bib-date b0)) (date-year (auto-bib-date b1)))))
(define (default-date=? b0 b1)
  (and (auto-bib-date b0) (auto-bib-date b1)
       (= (date-year (auto-bib-date b0)) (date-year (auto-bib-date b1)))))

;; 0 -> a, 1 -> b, etc.
(define (default-disambiguation n)
  (when (>= n 26)
    (error 'default-disambiguation "Citations too ambiguous for default disambiguation scheme."))
  (make-element #f (list (format "~a" (integer->char (+ 97 n))))))

(define author+date-style
  (new
   (class object%
     (define/public (bibliography-table-style) bib-single-style)
     (define/public (entry-style) bibentry-style)
     (define/public (disambiguate-date?) #t)
     (define/public (collapse-for-date?) #t)
     (define/public (get-cite-open) "(")
     (define/public (get-cite-close) ")")
     (define/public (get-group-sep) "; ")
     (define/public (get-item-sep) ", ")
     (define/public (render-citation date-cite i) date-cite)
     (define/public (render-author+dates author dates) (list* author " " dates))
     (define/public (bibliography-line i e) (list e))
     (super-new))))

(define number-style
  (new
   (class object%
     (define/public (bibliography-table-style) bib-columns-style)
     (define/public (entry-style) colbibentry-style)
     (define/public (disambiguate-date?) #f)
     (define/public (collapse-for-date?) #f)
     (define/public (get-cite-open) "[")
     (define/public (get-cite-close) "]")
     (define/public (get-group-sep) ", ")
     (define/public (get-item-sep) ", ")
     (define/public (render-citation date-cite i) (number->string i))
     (define/public (render-author+dates author dates) dates)
     (define/public (bibliography-line i e)
       (list (make-paragraph plain
                             (make-element colbibnumber-style (list "[" (number->string i) "]")))
             e))
     (super-new))))

(define (gen-bib tag group sec-title 
                 style maybe-disambiguator 
                 maybe-render-date-bib maybe-render-date-cite 
                 maybe-date<? maybe-date=?)
  (define disambiguator (or maybe-disambiguator default-disambiguation))
  (define date<? (or maybe-date<? default-date<?))
  (define date=? (or maybe-date=? default-date=?))
  (define render-date-bib (or maybe-render-date-bib default-render-date-bib))
  (define render-date-cite (or maybe-render-date-cite default-render-date-cite))
  (define (author/date<? a b)
    ;; Compare author names, then date, then full key
    (or (string-ci<? (extract-bib-key a) (extract-bib-key b))
        (and (string-ci=? (extract-bib-key a) (extract-bib-key b))
             (cond
              [(not (auto-bib-date a))
               (if (auto-bib-date b)
                   #f
                   (string-ci<? (auto-bib-key a) (auto-bib-key b)))]
              [(not (auto-bib-date b)) #t]
              [(date<? a b) #t]
              [(date<? b a) #f]
              [else (string-ci<? (auto-bib-key a) (auto-bib-key b))]))))
  (define (ambiguous? a b)
    (and (string-ci=? (author-element-cite (extract-bib-author a))
                      (author-element-cite (extract-bib-author b)))
         (auto-bib-date a)
         (auto-bib-date b)
         (date=? a b)))
  (define bibs (sort (hash-values (bib-group-ht group))
                     author/date<?))
  (define disambiguated
    (let ()
      (define (bib->para bib disambiguation i)
        (define collect-target
          (list (make-target-element
                  #f
                  (bib->entry bib style disambiguation render-date-bib i)
                  `(autobib ,(auto-bib-key bib)))))
        ;; Communicate to scribble's resolve step.
        (define (collect ci)
          ;; store the author
          (collect-put! ci
                        `(autobib-author ,(auto-bib-key bib)) ;; (list which key)
                        (make-element
                         #f
                         (list (author-element-cite (extract-bib-author bib)))))
          ;; store the date
          (when (auto-bib-date bib)
            (collect-put! ci
                          `(autobib-date ,(auto-bib-key bib)) ;; (list which key)
                          (make-element #f (list 
                                            (send style
                                                  render-citation
                                                  (render-date-cite (auto-bib-date bib))
                                                  i)))))
          ;; store how to disambiguate it from other like citations.
          (collect-put! ci
                        `(autobib-disambiguation ,(auto-bib-key bib))
                        (or disambiguation 'unambiguous)))
        (send style
              bibliography-line
              i
              (make-paragraph plain
                              (list (make-collect-element #f collect-target collect)))))
      ;; create the bibliography with disambiguations added.
      (define-values (last num-ambiguous rev-disambiguated*)
        (for/fold ([last #f] [num-ambiguous 0] [rev-disambiguated '()]) ([bib (in-list bibs)]
                                                                         [i (in-naturals 1)])
          (define ambiguous?? (and (send style disambiguate-date?)
                                   last 
                                   (ambiguous? last bib)))
          (define num-ambiguous*
            (cond [ambiguous?? (add1 num-ambiguous)]
                  [else 0]))
          ;; the current entry is ambiguous with the last. Modify the last
          ;; to have the first disambiguation.
          (define rev-disambiguated*
            (cond [(and ambiguous?? (= 0 num-ambiguous))
                   (cons (bib->para last (disambiguator num-ambiguous) i)
                         (cdr rev-disambiguated))]
                  [else rev-disambiguated]))
          (define para*
            (bib->para bib (and ambiguous?? (disambiguator num-ambiguous*)) i))
          (values bib num-ambiguous* (cons para* rev-disambiguated*))))
      (reverse rev-disambiguated*)))
  (make-part #f
             `((part ,tag))
             (list sec-title)
             (make-style #f '(unnumbered))
             null
             (list (make-table (send style bibliography-table-style) disambiguated))
             null))

(define (bib->entry bib style disambiguation render-date-bib i)
  (define-values (author date title location url is-book?)
    (values (auto-bib-author bib)
            (auto-bib-date bib)
            (auto-bib-title bib)
            (auto-bib-location bib)
            (auto-bib-url bib)
            (auto-bib-is-book? bib)))
  (make-element (send style entry-style)
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
                 (if date `(" "
                            ,@(if disambiguation
                                  `(,@(decode-content (list (render-date-bib date))) ,disambiguation)
                                  (decode-content (list (render-date-bib date))))
                            ".")
                     null)
                 (if url `(" " ,(link url (make-element 'url (list url)))) null))))

(define-syntax (define-cite stx)
  (syntax-parse stx
    [(_ (~var ~cite) citet generate-bibliography
        (~or (~optional (~seq #:style style) #:defaults ([style #'author+date-style]))
             (~optional (~seq #:disambiguate fn) #:defaults ([fn #'#f]))
             (~optional (~seq #:render-date-in-bib render-date-bib) #:defaults ([render-date-bib #'#f]))
             (~optional (~seq #:render-date-in-cite render-date-cite) #:defaults ([render-date-cite #'#f]))
             (~optional (~seq #:date<? date<?) #:defaults ([date<? #'#f]))
             (~optional (~seq #:date=? date=?) #:defaults ([date=? #'#f]))) ...)
     (syntax/loc stx
       (begin
         (define group (make-bib-group (make-hasheq)))
         (define the-style style)
         (define (~cite #:sort? [sort? #t] bib-entry . bib-entries)
           (add-cites group (cons bib-entry bib-entries) sort? the-style date<? date=?))
         (define (citet bib-entry . bib-entries)
           (add-inline-cite group (cons bib-entry bib-entries) the-style date<? date=?))
         (define (generate-bibliography #:tag [tag "doc-bibliography"] #:sec-title [sec-title "Bibliography"])
           (gen-bib tag group sec-title the-style fn render-date-bib render-date-cite date<? date=?))))]))

(define (ends-in-punc? e)
  (regexp-match? #rx"[.!?,]$" (content->string e)))

(define (understand-date inp)
  ;; Currently there is no string->date function.
  ;; Common usage of autobib has assumed that this should be the year.
  (cond [(or (string? inp) (number? inp))
         (define year
           (cond [(string? inp) (string->number inp)]
                 [else inp]))
         (date 0 0 0 1 1 ;; second/minute/hour/day/month
               year
               ;; week-day/year-day/daylight savings time?/timezone offset
               0 0 #f 0)]
        [(date? inp) inp]
        [(not inp) #f] ;; no date is fine too.
        [else (error 'make-bib "Not given a value that represents a date.")]))

;; We delay making the element for the bib-entry because we may need to add
;; disambiguations during gen-bib.
(define (make-bib #:title title
                  #:author [author #f]
                  #:is-book? [is-book? #f]
                  #:location [location #f]
                  #:date [date #f]
                  #:url [url #f])
  (define author*
    (cond [(not author) #f]
          [(author-element? author) author]
          [else (parse-author author)]))
  (define parsed-date (understand-date date))
  (make-auto-bib author* parsed-date title location url is-book?
                 (content->string
                  (make-element #f
                                (append
                                 (if author* (list author*) null)
                                 (list title)
                                 (if location (decode-content (list location)) null)
                                 (if date (decode-content (list (default-render-date-bib parsed-date))) null)
                                 (if url (list (link url (make-element 'url (list url)))) null))))
                 ""))

(define (in-bib bib where)
  (make-auto-bib
   (auto-bib-author bib)
   (auto-bib-date bib)
   (auto-bib-title bib)
   (auto-bib-location bib)
   (auto-bib-url bib)
   (auto-bib-is-book? bib)
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
