#lang racket/unit

(require "sig.rkt"
         mred/mred-sig
         racket/file
         racket/port
         net/url-sig
         (only-in html read-html-as-xml read-html-comments use-html-spec)
         (except-in xml read-comments)
         racket/class
         "bullet.rkt"
         "option-snip.rkt"
         "entity-names.rkt")


(import mred^ url^)
(export html^)
(init-depend mred^)    

;; CACHE
(define NUM-CACHED 10)
(define cached (make-vector 10 'no-image))
(define cached-name (make-vector 10 #f)) ; string or #f
(define cached-use (make-vector 10 0))

(define html-status-handler
  (make-parameter
   void
   (lambda (f)
     (unless (and (procedure? f)
                  (procedure-arity-includes? f 1))
       (raise-type-error 'html-status-handler
                         "procedure of arity 1"
                         f))
     f)))

(define (status . args)
  ((html-status-handler) (apply format args)))

(define status-stack (make-parameter null))

;; load-status : boolean string (union #f url) -> void
(define (load-status push? what url)
  (let ([s (format "Loading ~a ~a..." 
                   what 
                   (if url
                       (trim 150 (url->string url))
                       "unknown url"))])
    (status-stack (cons s (if push? (status-stack) null)))
    (status "~a" s)))

(define (pop-status)
  (status-stack (cdr (status-stack)))
  (status "~a" (car (status-stack))))
(define (trim len s)
  (if ((string-length s) . <= . len)
      s
      (string-append (substring s 0 (- len 4)) " ...")))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Imap maps
;;

(define-struct image-map-rect (href left top right bottom))

(define finger-cursor (make-object cursor% 'arrow))

(define image-map-snip%
  (class image-snip%
    
    (init-field html-text)
    
    (field [key "#key"])
    (define/public (set-key k) (set! key k))
    (define/public (get-key) key)
    
    (field [rects null])
    
    (define/public (set-rects rs) (set! rects rs))
    
    (inherit get-admin)
    
    (define/private (find-rect x y)
      (let loop ([rects rects])
        (cond
         [(null? rects) #f]
         [else
          (let ([rect (car rects)])
            (if (and (<= (image-map-rect-left rect) x (image-map-rect-right rect))
                     (<= (image-map-rect-top rect) y (image-map-rect-bottom rect)))
                rect
                (loop (cdr rects))))])))
    
    ;; add-area : string (listof number) string -> void
    ;; currently only supports rect shapes
    (define/public (add-area shape coords href)
      (when (and (equal? shape "rect")
                 (= 4 (length coords)))
        (let ([x1 (car coords)]
              [y1 (cadr coords)]
              [x2 (caddr coords)]
              [y2 (cadddr coords)])
          (set! rects (cons (make-image-map-rect
                             href
                             (min x1 x2)
                             (min y1 y2)
                             (max x1 x2)
                             (max y1 y2))
                            rects)))))
    
    (define/override (on-event dc x y editor-x editor-y evt)
      (when (send evt button-up?)
        (let* ([snipx (- (send evt get-x) x)]
               [snipy (- (send evt get-y) y)]
               [rect (find-rect snipx snipy)])
          (when rect
            (send html-text post-url (image-map-rect-href rect)))))
      (super on-event dc x y editor-x editor-y evt))
    
    (define/override (adjust-cursor dc x y editor-x editor-y evt)
      (let ([snipx (- (send evt get-x) x)]
            [snipy (- (send evt get-y) y)])
        (if (find-rect snipx snipy)
            finger-cursor
            #f)))

    ;; warning: buggy. This doesn't actually copy the bitmap
    ;; over because there's no get-bitmap method for image-snip%
    ;; at the time of this writing.
    (define/override (copy)
      (let ([cp (new image-map-snip% (html-text html-text))])
        (send cp set-key key)
        (send cp set-rects rects)))
    
    (super-make-object)
    
    (inherit set-flags get-flags)
    (set-flags (cons 'handles-events (get-flags)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hardwired Racket colorization; should come from a .css file
;;

(define (make-racket-color-delta col)
  (let ([d (make-object style-delta%)])
    (send d set-delta-foreground col)
    d))

(define racket-code-delta (make-racket-color-delta "brown"))
(define racket-code-delta/keyword
  (let ([d (make-racket-color-delta (make-object color% #x99 0 0))])
    (send d set-weight-on 'bold)
    d))
(define racket-code-delta/variable (make-racket-color-delta "navy"))
(define racket-code-delta/global (make-racket-color-delta "purple"))
(define racket-code-delta/selfeval (make-racket-color-delta "forest green"))
(define racket-code-delta/comment (make-racket-color-delta "cornflower blue"))
(define navigation-delta (let ([d (make-racket-color-delta "red")])
                           (send d set-style-on 'italic)
                           d))

(define current-style-class (make-parameter null))

(define (lookup-class-delta class)
  (let ([class-path (cons class (current-style-class))])
    (cond
     [(sub-path? class-path '("racket")) racket-code-delta]
     [(sub-path? class-path '("keyword" "racket")) racket-code-delta/keyword]
     [(sub-path? class-path '("variable" "racket")) racket-code-delta/variable]
     [(sub-path? class-path '("global" "racket")) racket-code-delta/global]
     [(or (sub-path? class-path '("selfeval" "racket"))
          (sub-path? class-path '("racketresponse"))) racket-code-delta/selfeval]
     [(sub-path? class-path '("comment" "racket")) racket-code-delta/comment]
     [(sub-path? class-path '("navigation")) navigation-delta]
     [else #f])))

(define (sub-path? a b)
  (cond
   [(null? b) #t]
   [(null? a) #f]
   [else (and (equal? (car a) (car b))
              (sub-path? (cdr a) (cdr b)))]))

(define (with-style-class class thunk)
  (if class
      (parameterize ([current-style-class (cons class (current-style-class))])
        (thunk))
      (thunk)))

(define (lookup-span-class-delta class) (lookup-class-delta class))

(define re:hexcolor 
  (regexp "^#([0-9a-fA-F][0-9a-fA-F])([0-9a-fA-F][0-9a-fA-F])([0-9a-fA-F][0-9a-fA-F])$"))

(define color-string->color
  (lambda (str)
    (let ([m (regexp-match re:hexcolor str)])
      (if m
          (make-object color%
                       (string->number (cadr m) 16)
                       (string->number (caddr m) 16)
                       (string->number (cadddr m) 16))
          (send the-color-database find-color str)))))

(define html-eval-ok (make-parameter #t))
(define html-img-ok (make-parameter #t))

(define (get-bitmap-from-url url)
  (if (html-img-ok)
      (let ([tmp-filename (make-temporary-file "rktguiimg~a")])
        (load-status #t "image" url)
        (call-with-output-file* tmp-filename
                                (lambda (op)
                                  (with-handlers ([exn:fail? 
                                                   (lambda (x)
                                                     (printf "exn.9 ~s\n" (and (exn? x)
                                                                               (exn-message x)))
                                                     (void))])
                                    (call/input-url 
                                     url
                                     get-pure-port
                                     (lambda (ip)
                                       (copy-port ip op)))))
                                #:exists 'truncate)
        (pop-status)
        (let ([bitmap (make-object bitmap% tmp-filename)])
          (with-handlers ([exn:fail?
                           (lambda (x)
                             (message-box "Warning"
                                          (format "Could not delete file ~s\n\n~a"
                                                  tmp-filename
                                                  (if (exn? x)
                                                      (exn-message x)
                                                      x))))])
            (delete-file tmp-filename))
          (if (send bitmap ok?)
              bitmap
              #f)))
      #f))

;; cache-bitmap : string -> (is-a?/c bitmap%)
(define (cache-bitmap url)
  (let ([url-string (url->string url)])
    (let loop ([n 0])
      (cond
       [(= n NUM-CACHED)
        ;; Look for item to uncache
        (vector-set! cached-use 0 (max 0 (sub1 (vector-ref cached-use 0))))
        (let ([m (let loop ([n 1][m (vector-ref cached-use 0)])
                   (if (= n NUM-CACHED)
                       m
                       (begin
                         (vector-set! cached-use n (max 0 (sub1 (vector-ref cached-use n))))
                         (loop (add1 n) (min m (vector-ref cached-use n))))))])
          (let loop ([n 0])
            (if (= (vector-ref cached-use n) m)
                (let ([bitmap (get-bitmap-from-url url)])
                  (cond
                   [bitmap
                    (vector-set! cached n bitmap)
                    (vector-set! cached-name n url-string)
                    (vector-set! cached-use n 5)
                    bitmap]
                   [else #f]))
                (loop (add1 n)))))]
       [(equal? url-string (vector-ref cached-name n))
        (vector-set! cached-use n (min 10 (add1 (vector-ref cached-use n))))
        (vector-ref cached n)]
       [else
        (loop (add1 n))]))))

(define (update-image-maps image-map-snips image-maps)
  (for-each
   (lambda (image-map-snip)
     (let ([image-map-key (send image-map-snip get-key)])
       (let loop ([image-maps image-maps])
         (cond
          [(null? image-maps) (void)]
          [else
           (let* ([image-map (car image-maps)]
                  [name (get-field image-map 'name)])
             (if (and name 
                      (equal? (format "#~a" name)
                              (send image-map-snip get-key)))
                 (find/add-areas image-map-snip image-map)
                 (loop (cdr image-maps))))]))))
   image-map-snips))

(define (find/add-areas image-map-snip image-map)
  (let loop ([sexp image-map])
    (cond
     [(and (pair? sexp)
           (eq? (car sexp) 'area)
           (pair? (cdr sexp)))
      (add-area image-map-snip (cadr sexp))
      (loop (cddr sexp))]
     [(pair? sexp)
      (loop (car sexp))
      (loop (cdr sexp))]
     [else (void)])))

;; add-area : snip (listof (list sym string))[assoc] -> void
;; the second arg type is actually `any', but if it
;; matches the above, it is interprted propoerly;
;; otherwise silently nothing happens.
(define (add-area image-map-snip sexp)
  (let ([shape #f]
        [coords #f]
        [href #f])
    (let loop ([sexp sexp])
      (cond
       [(pair? sexp)
        (let ([fst (car sexp)])
          (when (and (pair? fst)
                     (symbol? (car fst))
                     (pair? (cdr fst))
                     (string? (cadr fst)))
            (case (car fst)
              [(shape) (set! shape (cadr fst))]
              [(coords) (set! coords (cadr fst))]
              [(href) (set! href (cadr fst))]
              [else (void)]))
          (loop (cdr sexp)))]
       [else (void)]))
    (when (and shape coords href)
      (let ([p-coords (parse-coords coords)])
        (when p-coords
          (send image-map-snip add-area shape p-coords href))))))

;; parse-coords : string -> (listof number)
;; separates out a bunch of comma separated numbers in a string
;; into a list of racket numbers
(define (parse-coords str)
  (let loop ([str str])
    (cond
     [(regexp-match #rx"^[ \t\n]*([0-9]+)[ \t\n]*,(.*)$" str)
      =>
      (lambda (m)
        (let ([num (cadr m)]
              [rst (caddr m)])
          (cons (string->number num)
                (loop rst))))]
     [(regexp-match #rx"^[ \t\n]*([0-9]+)[ \t\n]*" str)
      =>
      (lambda (m)
        (list (string->number (cadr m))))]
     [else null])))

(define (make-get-field str)
  (let ([s (apply
            string-append
            (map
             (lambda (c)
               (format "[~a~a]"
                       (char-upcase c)
                       (char-downcase c)))
             (string->list str)))]
        [spc (string #\space #\tab #\newline #\return #\vtab)])
    (let ([re:plain (regexp (format "(^|[~a])~a[~a]*=[~a]*([^~a]*)" spc s spc spc spc))]
          [re:quote (regexp (format "(^|[~a])~a[~a]*=[~a]*\"([^\"]*)\"" spc s spc spc))])
      (lambda (args)
        (let ([m (or (regexp-match re:quote args)
                     (regexp-match re:plain args))])
          (and m (caddr m)))))))

(define (get-field e name)
  (let ([a (assq name (cadr e))])
    (and a (cadr a))))

(define get-racket-arg
  (let ([get-rkt (make-get-field "racket")])
    (lambda (str)
      (let ([v (get-rkt str)])
        (and v (filter-racket v))))))

(define filter-racket 
  (lambda (v)
    (regexp-replace* "[|]" v "\"")))

(define face-list #f)

(define default-font (make-object font% 12 'default))

(define re:quot (regexp "[&][qQ][uU][oO][tT][;]"))
(define re:amp (regexp "[&][aA][mM][pP][;]"))

(define re:empty (regexp (format "^[ ~c]*$" (integer->char 160))))

(define-struct form (action target method [parts #:mutable] [active-select #:mutable]))
(define (protect-chars s)
  (apply string-append 
         (map (lambda (c)
                (if (or (char-alphabetic? c)
                        (char-numeric? c))
                    (string c)
                    (format "%~a"
                            (let ([s (format "0~x" (or (char->integer c) 65))])
                              (substring s (- (string-length s) 2) (string-length s))))))
              (string->list s))))

(define re:true (regexp-quote "true" #f))
(define (true? s) (regexp-match re:true s))

(define verbatim-tags '(listing xmp plaintext))
(define preformatted-tags '(pre))
(define exact-whitespace-tags (append verbatim-tags
                                      preformatted-tags))
(define comment-tags '(script))
(define atomic-tags '(p br hr li dd dt img html meta link input))
(define enum-tags '(ul dl ol menu))

(define space-eating-tags '(title p div center br h1 h2 h3 h4
                                  li dt dd
                                  ul ol dl menu
                                  samp kbd pre blockquote
                                  table tr td))

(define whitespace-string "[ \t\n\r\v\f]+")
(define re:whitespace (regexp whitespace-string))
(define re:starting-whitespace (regexp (format "^~a" whitespace-string)))
(define re:ending-whitespace (regexp (format "~a$" whitespace-string)))
(define re:leading-newline (regexp "^(\r|\n|(\r\n))"))

(define (remove-leading-newline c)
  (cond
   [(string? c)
    (let ([s (regexp-match-positions re:leading-newline c)])
      (cond
       [(and s (= (cdar s) (length s)))
        ;; It's all newline:
        (values "" #t)]
       [s (values (substring c (cdar s) (string-length c)) #t)]
       [else (values c #t)]))]
   [(pair? c)
    (let loop ([b (cddr c)][accum null])
      (if (null? b)
          (values (list* (car c) (cadr c) (reverse accum)) #f)
          (let-values ([(d done?) (remove-leading-newline (car b))])
            (if done?
                (values (list* (car c) (cadr c) (append (reverse accum)
                                                        (list d)
                                                        (cdr b)))
                        #t)
                (loop (cdr b) (cons d accum))))))]
   [else (values c #f)]))

(define (fixup-whitespace c leading-ok?)
  (cond
   [(string? c)
    (let ([s (regexp-match-positions re:starting-whitespace c)]
          [e (regexp-match-positions re:ending-whitespace c)])
      (if (and s e
               (= (caar s) (caar e)))
          ;; It's all whitespace:
          (if leading-ok?
              (values " " #f)
              (values "" #f))
          ;; Normal case:
          (values
           (string-append
            (if (and s leading-ok?) " " "")
            (regexp-replace* re:whitespace
                             (substring c 
                                        (if s
                                            (cdar s)
                                            0)
                                        (if e
                                            (caar e)
                                            (string-length c)))
                             " ")
            (if e " " ""))
           (not e))))]
   [(symbol? c) (values c #t)]
   [(number? c) (values c #t)]
   [(comment? c)
    (let ([code (get-racket-arg (comment-text c))])
      (if code
          (let ([s (with-handlers ([exn:fail?
                                    (lambda (exn)
                                      (format
                                       "<font color=\"red\">Error during &lt;!-- RACKET=... --&gt;: <i>~a</i></font>"
                                       (if (exn? exn)
                                           (exn-message exn)
                                           (format "~s" exn))))])
                     (if (html-eval-ok)
                         (eval (read (open-input-string code)))
                         (error "disabled")))])
            (if (string? s)
                (let ([content (read-html (open-input-string s))])
                  (fixup-whitespace content leading-ok?))
                (values "" leading-ok?)))
          (values "" leading-ok?)))]
   [(p-i? c) (values "" leading-ok?)] ;; processing instruction
   [else (let ([tag (car c)])
           (if (memq tag exact-whitespace-tags)
               (let-values ([(s done?) (remove-leading-newline c)])
                 (values s #f))
               (let-values ([(body leading-ok?)
                             (let loop ([l (cddr c)][leading-ok? 
                                                     (and leading-ok?
                                                          (not (memq tag space-eating-tags)))])
                               (if (null? l)
                                   (values null leading-ok?)
                                   (let*-values ([(f l-ok?)
                                                  (fixup-whitespace (car l) leading-ok?)]
                                                 [(r l-ok?)
                                                  (loop (cdr l) l-ok?)])
                                     (values (cons f r) l-ok?))))])
                 (values
                  (list*
                   tag
                   (cadr c) ; attributes
                   body)
                  (and leading-ok?
                       (not (memq tag space-eating-tags)))))))]))

(define (read-html a-port)
  (let* ([xml (parameterize ([read-html-comments #t]
                             [use-html-spec #f])
                (read-html-as-xml a-port))]
         [xexpr `(html () ,@(map xml->xexpr xml))])
    xexpr))

(define (parse-html a-port)
  (let ([raw (read-html a-port)])
    (let-values ([(v ?) (fixup-whitespace raw #f)])
      v)))

(define html-convert
  (lambda (a-port a-text)
    (let ([content (parse-html a-port)])
      (with-method ([a-text-insert (a-text insert)]
                    [current-pos (a-text last-position)]
                    [delete (a-text delete)]
                    [get-character (a-text get-character)]
                    [change-style (a-text change-style)])
                   (letrec ([image-map-snips null]
                            [image-maps null]
                            
                            [html-basic-style
                             (let ([sl (send a-text get-style-list)])
                               (or (send sl find-named-style "Html Standard")
                                   (send sl find-named-style "Standard")
                                   (send sl find-named-style "Basic")))]
                            
                            ;; inserts 
                            [insert 
                             (λ (what)
                                (let ([pos-before (current-pos)])
                                  (a-text-insert what pos-before)
                                  (let ([pos-after (current-pos)])
                                    (change-style html-basic-style pos-before pos-after))))]
                            
                            [insert-newlines
                             (lambda (num forced-lines para-base)
                               (let ([num (max num forced-lines)])
                                 (unless (zero? num)
                                   (let loop ([pos (current-pos)][num num])
                                     (unless (or (zero? num) (<= pos para-base))
                                       (let ([c (get-character (sub1 pos))])
                                         (if (eq? c #\newline)
                                             (loop (sub1 pos) (sub1 num))
                                             (insert (make-string num #\newline)))))))))]
                            
                            [backover-newlines
                             (lambda (pos base)
                               (if (= pos base)
                                   base
                                   (let ([c (get-character (sub1 pos))])
                                     (if (eq? c #\newline)
                                         (backover-newlines (sub1 pos) base)
                                         pos))))]
                            
                            [whitespaces (string #\space #\tab #\newline #\return)]
                            
                            [delta:fixed (make-object style-delta% 'change-family 'modern)]
                            [delta:default-face (make-object style-delta% 'change-family 'default)]
                            [delta:bold (make-object style-delta% 'change-bold)] 
                            [delta:underline (make-object style-delta% 'change-underline #t)]
                            [delta:italic (make-object style-delta% 'change-italic)]
                            [delta:h1 (let ([d (make-object style-delta% 'change-bold)])
                                        (send d set-size-mult 2.0)
                                        d)]
                            [delta:h2 (let ([d (make-object style-delta% 'change-bold)])
                                        (send d set-size-mult 1.5)
                                        d)]
                            [delta:h3 (let ([d (make-object style-delta% 'change-bold)])
                                        (send d set-size-mult 1.2)
                                        d)]
                            [delta:h4 (make-object style-delta% 'change-bold)]
                            [delta:subscript (let ([d (make-object style-delta%)])
                                               (send d set-alignment-on 'bottom)
                                               (send d set-size-mult 0.8)
                                               d)]
                            [delta:superscript (let ([d (make-object style-delta%)])
                                                 (send d set-alignment-on 'top)
                                                 (send d set-size-mult 0.8)
                                                 d)]
                            [delta:small (let ([d (make-object style-delta%)])
                                           (send d set-size-mult 0.75)
                                           d)]
                            
                            [delta:center (make-object style-delta% 'change-alignment 'center)]
                            [delta:symbol (make-object style-delta% 'change-family 'symbol)]
                            
                            [html-error
                             (lambda args
                               (when #f ; treat them all as ignored warnings
                                 (apply error 'html args)))]
                            
                            [re:transparent #rx"[Tt][Rr][Aa][Nn][Ss][Pp][Aa][Rr][Ee][Nn][Tt]"]
                            
                            [parse-image-source
                             (lambda (s)
                               (let ([src (get-field s 'src)]
                                     [base-url (send a-text get-url)])
                                 (and src
                                      (with-handlers ([exn:fail? (lambda (x) #f)])
                                        (if base-url
                                            (combine-url/relative base-url src)
                                            (string->url src))))))]
                            
                            [unescape 
                             (lambda (s)
                               (apply string-append 
                                      (map (lambda (x)
                                             (if (pcdata? x)
                                                 (pcdata-string x)
                                                 ""))
                                           (read-html-as-xml 
                                            (open-input-string s)))))]
                            [parse-href
                             (let ([href-error
                                    (lambda (s)
                                      (html-error "bad reference in ~s" s))])
                               (lambda (s)
                                 (let* ([url-string
                                         (cond 
                                          [(get-field s 'href)
                                           => (lambda (str)
                                                (if (string=? str "")
                                                    (begin (href-error s)
                                                           #f)
                                                    (unescape str)))]
                                          [else #f])]
                                        [label (get-field s 'name)]
                                        [racket (let ([v (get-field s 'racket)])
                                                  (and v (filter-racket v)))])
                                   (values url-string label racket))))]
                            
                            [parse-font
                             (let ([face-regexp (regexp "([^,]*), *(.*)")])
                               (lambda (args)
                                 (let ([size-string (get-field args 'size)]
                                       [face-string (get-field args 'face)]
                                       [color-string (get-field args 'color)]
                                       [bg-color-string (get-field args 'bgcolor)])
                                   (let ([size
                                          (and size-string
                                               (let* ([n (string->number size-string)])
                                                 (and n
                                                      (integer? n)
                                                      (<= -127 n 127)
                                                      (cond
                                                       [(char=? #\+ (string-ref size-string 0))
                                                        (make-object style-delta% 'change-bigger n)]
                                                       [(negative? n)
                                                        (make-object style-delta% 'change-smaller (- n))]
                                                       [else
                                                        (cond
                                                         [(n . < . 2)
                                                          (make-object style-delta% 'change-smaller (- 2 n))]
                                                         [(n . > . 2)
                                                          (make-object style-delta% 'change-bigger (- n 2))]
                                                         [else #f])]))))]
                                         [face (and face-string
                                                    (or
                                                     (and (string=? face-string "monospace")
                                                          (make-object style-delta% 'change-family 'modern))
                                                     (let ([f (let loop ([f face-string])
                                                                (let ([m (regexp-match face-regexp f)]
                                                                      [try-face (lambda (s)
                                                                                  (unless face-list
                                                                                    (set! face-list (get-face-list)))
                                                                                  (ormap
                                                                                   (lambda (s-norm)
                                                                                     (and (string-ci=? s s-norm)
                                                                                          s-norm))
                                                                                   face-list))])
                                                                  (if m
                                                                      (or (try-face (cadr m))
                                                                          (loop (caddr m)))
                                                                      (try-face f))))])
                                                       (and f
                                                            (let ([d (make-object style-delta%)])
                                                              (send d set-delta-face f))))))]
                                         [color (let ([clr (and color-string (color-string->color color-string))])
                                                  (and clr
                                                       (let ([d (make-object style-delta%)])
                                                         (send d set-delta-foreground clr))))]
                                         [bg-color (let ([bg-clr (and bg-color-string
                                                                      (color-string->color bg-color-string))])
                                                     (and bg-clr
                                                          (let ([d (make-object style-delta%)])
                                                            (send d set-delta-background bg-clr))))])
                                     (let loop ([delta #f][l (list size face color bg-color)])
                                       (cond
                                        [(null? l) delta]
                                        [(not (car l)) (loop delta (cdr l))]
                                        [else (if delta
                                                  (loop (begin
                                                          (send delta collapse (car l))
                                                          delta)
                                                        (cdr l))
                                                  (loop (car l) (cdr l)))]))))))]
                            
                            [make-unsupported
                             (lambda (tag args)
                               (let ([name (get-field args 'name)]
                                     [type (get-field args 'type)])
                                 (if (and (eq? tag 'input) type (string=? type "hidden"))
                                     "" ; hidden input
                                     (format "[~a~a NOT SUPPORTED]"
                                             (if name
                                                 (format "~a " name)
                                                 "")
                                             (case tag
                                               [(select) "POPUP MENU"]
                                               [(textarea) "TEXT AREA"]
                                               [(input) (if type
                                                            (case (string->symbol type)
                                                              [(text) "TEXT FIELD"]
                                                              [else "BUTTON"])
                                                            "BUTTON")])))))]
                            
                            [heading (lambda (delta forced-lines rest para-base)
                                       (insert-newlines 2 forced-lines para-base)
                                       (let-values ([(start-pos) (current-pos)]
                                                    [(r rfl) (rest)]
                                                    [(end-pos) (current-pos)])
                                         (insert-newlines 2 rfl para-base)
                                         (values
                                          (lambda ()
                                            (change-style delta start-pos end-pos)
                                            (r))
                                          rfl)))]
                            
                            [styler (lambda (delta rest [drop-empty? #f])
                                      (let*-values ([(start-pos) (current-pos)]
                                                    [(r rfl) (rest)]
                                                    [(end-pos) (current-pos)])
                                        (if (and drop-empty?
                                                 (regexp-match re:empty (send a-text get-text start-pos end-pos)))
                                            (begin
                                              (delete start-pos end-pos)
                                              (values void rfl))
                                            (values
                                             (lambda ()
                                               (change-style delta start-pos end-pos)
                                               (r))
                                             rfl))))]
                            
                            [maybe-bg-color (lambda (e rest drop-empty?)
                                              (let* ([c (get-field e 'bgcolor)]
                                                     [color (and c (color-string->color c))])
                                                (cond
                                                 [color
                                                  (styler (let ([d (make-object style-delta%)])
                                                            (send d set-delta-background color)
                                                            d)
                                                          rest
                                                          drop-empty?)]
                                                 [drop-empty?
                                                  (let*-values ([(start-pos) (current-pos)]
                                                                [(r rfl) (rest)]
                                                                [(end-pos) (current-pos)])
                                                    (values (if (regexp-match re:empty (send a-text get-text start-pos end-pos))
                                                                void
                                                                r)
                                                            rfl))]
                                                 [else
                                                  (rest)])))]
                            
                            [para-aligner (lambda (alignment delta rest)
                                            (let*-values ([(start-pos) (current-pos)]
                                                          [(r rfl) (rest)]
                                                          [(end-pos) (current-pos)])
                                              (values
                                               (lambda ()
                                                 (let ([last-para (send a-text position-paragraph 
                                                                        (backover-newlines end-pos start-pos))])
                                                   (let loop ([para (send a-text position-paragraph start-pos)])
                                                     (if (eq? alignment 'left-outdent)
                                                         (begin
                                                           (send a-text set-paragraph-alignment para 'left)
                                                           (send a-text set-paragraph-margins para 0 (* 2 (get-bullet-width)) 0))
                                                         (send a-text set-paragraph-alignment para alignment))
                                                     (when delta
                                                       (change-style delta start-pos end-pos))
                                                     (unless (= para last-para)
                                                       (loop (add1 para)))))
                                                 (r))
                                               rfl)))]
                            
                            ;; translate-number : number -> void
                            [translate-number
                             (lambda (e)
                               (cond
                                [(and (not (= e #xFFFF))
                                      (not (= e #xFFFE))
                                      (not (<= #xD800 e #xDFFF))
                                      (send default-font screen-glyph-exists? (integer->char e)))
                                 (insert (integer->char e))
                                 void]
                                [(<= 913 e 969)
                                 (let ([lp (current-pos)])
                                   (insert (integer->char (+ (- e 913) (char->integer #\A))))
                                   (lambda ()
                                     (change-style delta:symbol lp (+ lp 1))))]
                                
                                ;; poor ascii approximations. probably these
                                ;; (and other) characters exist somewhere,
                                ;; but I don't know where.
                                [(= e 160) (insert " ") void]
                                [(= e 338) (insert "OE") void]
                                [(= e 339) (insert "oe") void]
                                [(= e 732) (insert "~") void]
                                [(= e 710) (insert "^") void]
                                [(= e 8242) (insert "'") void]
                                [(= e 8243) (insert "''") void]
                                [(= e 8260) (insert "/") void]
                                [(= e 8722) (insert "-") void]
                                [(= e 8727) (insert "*") void]
                                [(= e 8764) (insert "~") void]
                                [(= e 8804) (insert "<") void]
                                [(= e 8805) (insert ">") void]
                                [(= e 8211) (insert "--") void]
                                [(= e 8212) (insert "---") void]
                                
                                [else (insert (format "&#~a;" e))
                                      void]))]
                            
                            ;; ========================================
                            ;; This is the main formatting function.
                            ;; It consumes:
                            ;;   e : xexpr - the HTML content
                            ;;   para-base : num - a marker for a paragraph start (e.g.,
                            ;;                     the bullet for <li>), though the actual
                            ;;                     paragraph start may be later
                            ;;   enum-depth : num - current depth of enumerations
                            ;; The result is a function of no arguments that finalizes
                            ;;  the region for `e', which normally means applying font changes.
                            ;;  (The changes have to be applied outside-in, so that local
                            ;;  specifications override enclosing ones.)
                            ;; Translate must not modify any existing text, and the
                            ;;  result function must not move any items.
                            [translate
                             (lambda (e para-base enum-depth forced-lines form)
                               (cond
                                [(string? e) 
                                 (let ([lp (current-pos)])
                                   (insert e)
                                   ;; we change the style here directly
                                   ;; since we want this style to only appear
                                   ;; if the style is overridden by the context
                                   (change-style delta:default-face 
                                                 lp
                                                 (+ lp (string-length e)))
                                   (values void
                                           0))]
                                [(symbol? e) 
                                 (let ([a (entity-name->integer e)])
                                   (if a
                                       (values (translate-number a) 0)
                                       (begin
                                         (insert (format "&~a;" e))
                                         (values void 0))))]
                                [(number? e) 
                                 (values (translate-number e) 0)]
                                [(or (comment? e) (p-i? e)) (values void forced-lines)]
                                [else (let* ([tag (car e)]
                                             [rest/base/depth/form/fl
                                              (lambda (para-base enum-depth form forced-lines)
                                                (let* ([p (foldl (lambda (x p) 
                                                                   (let-values ([(f fl) (translate x para-base enum-depth (car p) form)])
                                                                     (cons fl (cons f (cdr p)))))
                                                                 (cons forced-lines null)
                                                                 (cddr e))]
                                                       [l (reverse (cdr p))])
                                                  (values (lambda ()
                                                            (map (lambda (f) (f)) l))
                                                          (car p))))]
                                             [rest/base/depth
                                              (lambda (para-base enum-depth)
                                                (rest/base/depth/form/fl para-base enum-depth form forced-lines))]
                                             [rest/form (lambda (form) (rest/base/depth/form/fl para-base enum-depth form forced-lines))]
                                             [rest/form (lambda (form) (rest/base/depth/form/fl para-base enum-depth form forced-lines))]
                                             [rest (lambda () (rest/base/depth/form/fl para-base enum-depth form forced-lines))]
                                             [rest/fl (lambda (fl) (rest/base/depth/form/fl para-base enum-depth form fl))])
                                        (case tag
                                          [(title)
                                           (let ([pos (current-pos)])
                                             ;; Render content
                                             (rest)
                                             (send a-text set-title (send a-text get-text pos (current-pos)))
                                             (delete pos (current-pos)))
                                           (values void forced-lines)]
                                          [(a)
                                           (let-values ([(url-string label racket) (parse-href e)])
                                             (let* ([style (get-field e 'style)]
                                                    [pos (current-pos)])
                                               (let-values ([(r rfl) (rest)])
                                                 (let ([end-pos (current-pos)])
                                                   (cond
                                                    [url-string
                                                     (send a-text add-link pos end-pos (regexp-replace* re:amp url-string "\\&"))
                                                     ;; might have a label, too:
                                                     (when label
                                                       (send a-text add-tag label pos))
                                                     (values
                                                      (lambda ()
                                                        (when (or (not style)
                                                                  (not (regexp-match re:transparent style)))
                                                          (send a-text make-link-style pos end-pos))
                                                        (r))
                                                      rfl)]
                                                    [label
                                                     (send a-text add-tag label pos)
                                                     (values r rfl)]
                                                    [racket
                                                     (send a-text add-racket-callback pos end-pos racket)
                                                     (values
                                                      (lambda ()
                                                        (when (or (not style)
                                                                  (not (regexp-match re:transparent style)))
                                                          (send a-text make-link-style pos end-pos))
                                                        (r))
                                                      rfl)]
                                                    [else (values r rfl)])))))]
                                          [(style) (values void forced-lines)]
                                          [(h1) (heading delta:h1 forced-lines rest para-base)]
                                          [(h2) (heading delta:h2 forced-lines rest para-base)]
                                          [(h3) (heading delta:h3 forced-lines rest para-base)]
                                          [(h4) (heading delta:h4 forced-lines rest para-base)]
                                          [(b strong) (styler delta:bold rest)]
                                          [(i em var dfn cite) (styler delta:italic rest)]
                                          [(u) (styler delta:underline rest)]
                                          [(sup) (styler delta:superscript rest)]
                                          [(sub) (styler delta:subscript rest)]
                                          [(small) (styler delta:small rest)]
                                          [(font)
                                           (let ([delta (parse-font e)])
                                             (if delta
                                                 (styler delta rest)
                                                 (rest)))]
                                          [(li dd dt)
                                           (insert-newlines 1 forced-lines para-base)
                                           (let ([pos (current-pos)]
                                                 [bullet? (eq? tag 'li)])
                                             (when bullet?
                                               (let ([before (current-pos)])
                                                 (insert (make-object bullet-snip% (sub1 enum-depth)))
                                                 (change-style (send (send a-text get-style-list) find-named-style "Standard")
                                                               before
                                                               (+ before 1))))
                                             (let*-values ([(r rfl) (rest/base/depth (add1 pos) enum-depth)]
                                                           [(end-pos) (current-pos)])
                                               (values
                                                (lambda ()
                                                  (let ([end-para (send a-text position-paragraph 
                                                                        (backover-newlines end-pos pos))]
                                                        [left-margin (* 2 (get-bullet-width) enum-depth)])
                                                    (let loop ([para (send a-text position-paragraph pos)]
                                                               [first? #t])
                                                      (send a-text set-paragraph-margins
                                                            para
                                                            (if first?
                                                                (max 0 (- left-margin
                                                                          (if bullet?
                                                                              (get-bullet-width)
                                                                              0)))
                                                                left-margin)
                                                            left-margin
                                                            0)
                                                      (unless (= para end-para)
                                                        (loop (add1 para) #f))))
                                                  (r))
                                                rfl)))]
                                          [(ul menu ol dl)
                                           (insert-newlines (if (zero? enum-depth) 2 1) forced-lines para-base)
                                           (let-values ([(r rfl)
                                                         (rest/base/depth para-base (add1 enum-depth))])
                                             (insert-newlines (if (zero? enum-depth) 2 1) rfl para-base)
                                             (values r rfl))]
                                          [(p)
                                           (insert-newlines 2 forced-lines para-base)
                                           (let-values ([(r rfl) (rest)])
                                             (insert-newlines 2 rfl para-base)
                                             (values r rfl))]
                                          [(blockquote)
                                           (insert-newlines 2 forced-lines para-base)
                                           (let*-values ([(pos) (current-pos)]
                                                         [(r rfl) (rest/base/depth para-base (add1 enum-depth))]
                                                         [(end-pos) (current-pos)])
                                             (begin0
                                              (values
                                               (lambda ()
                                                 (let ([end-para (send a-text position-paragraph 
                                                                       (backover-newlines end-pos pos))]
                                                       [left-margin (* 2 (get-bullet-width) (add1 enum-depth))])
                                                   (let loop ([para (send a-text position-paragraph pos)])
                                                     (send a-text set-paragraph-margins
                                                           para
                                                           left-margin
                                                           left-margin
                                                           left-margin)
                                                     (unless (= para end-para)
                                                       (loop (add1 para)))))
                                                 (r))
                                               rfl)
                                              (insert-newlines 2 rfl para-base)))]
                                          [(center)
                                           (insert-newlines 2 forced-lines para-base)
                                           (let-values ([(r rfl)
                                                         (para-aligner 'center #f rest)])
                                             (insert-newlines 2 rfl para-base)
                                             (values r rfl))]
                                          [(div)
                                           (insert-newlines 1 forced-lines para-base)
                                           (let* ([align (get-field e 'align)]
                                                  [class (get-field e 'class)]
                                                  [delta (and class (lookup-class-delta class))])
                                             (with-style-class
                                              class
                                              (lambda ()
                                                (let-values ([(r rfl)
                                                              (cond
                                                               [(and (string? align) (string-ci=? align "center"))
                                                                (para-aligner 'center delta rest)]
                                                               [(and (string? align) (string-ci=? align "left"))
                                                                (para-aligner 'left delta rest)]
                                                               [(and (string? align) (string-ci=? align "left-outdent"))
                                                                (para-aligner 'left-outdent delta rest)]
                                                               [(or (and (string? align) (string-ci=? align "right"))
                                                                    (and (string? class) (string-ci=? class "navigation")))
                                                                (para-aligner 'right delta rest)]
                                                               [delta
                                                                (styler delta rest)]
                                                               [else (rest)])])
                                                  (insert-newlines 1 rfl para-base)
                                                  (values r rfl)))))]
                                          [(br)
                                           (insert-newlines 1 (+ 1 forced-lines) para-base)
                                           (rest/fl (+ forced-lines 1))]
                                          [(table)
                                           (insert-newlines 1 forced-lines para-base)
                                           (let-values ([(r rfl)
                                                         (maybe-bg-color e rest #t)])
                                             (insert-newlines 1 rfl para-base)
                                             (values r (max 1 rfl)))]
                                          [(tr)
                                           (insert-newlines 1 forced-lines para-base)
                                           (let-values ([(r rfl)
                                                         (maybe-bg-color e rest #t)])
                                             (insert-newlines 1 rfl para-base)
                                             (values r rfl))]
                                          [(td)
                                           (maybe-bg-color e rest #t)]
                                          [(map) 
                                           (set! image-maps (cons e image-maps))
                                           (rest)]
                                          [(img)
                                           (let* ([url (parse-image-source e)]
                                                  [alt (get-field e 'alt)]
                                                  [bitmap (and url (cache-bitmap url))]
                                                  [usemap (get-field e 'usemap)])
                                             (cond
                                              [(and bitmap usemap)
                                               (let ([pos (current-pos)]
                                                     [image-map-snip (make-object image-map-snip% a-text)])
                                                 (send image-map-snip set-bitmap bitmap)
                                                 (send image-map-snip set-key usemap)
                                                 (insert image-map-snip)
                                                 (set! image-map-snips (cons image-map-snip image-map-snips))
                                                 (change-style delta:center pos (add1 pos)))]
                                              [bitmap
                                               (let ([pos (current-pos)])
                                                 (insert (make-object image-snip% bitmap))
                                                 (change-style delta:center pos (add1 pos)))]
                                              [alt
                                               (insert alt)]
                                              [else
                                               (let ([pos (current-pos)])
                                                 (insert (new image-snip%))
                                                 (change-style delta:center pos (add1 pos)))])
                                             (rest))]
                                          [(form)
                                           (rest/form (make-form (get-field e 'action) (get-field e 'target) (get-field e 'method) null #f))]
                                          [(input select textarea)
                                           (let ([unsupported (make-unsupported tag e)]
                                                 [pos (current-pos)]
                                                 [type (let ([t (get-field e 'type)])
                                                         (and t (string->symbol t)))]
                                                 [send-form (lambda (add-self?)
                                                              (let ([post-string
                                                                     (apply 
                                                                      string-append
                                                                      (map (lambda (v)
                                                                             (if (car v)
                                                                                 (format "~a=~a&"
                                                                                         (car v)
                                                                                         (protect-chars (or ((cdr v))
                                                                                                            "?")))
                                                                                 ""))
                                                                           (form-parts form)))])
                                                                (send a-text post-url
                                                                      (form-action form)
                                                                      (string->bytes/utf-8
                                                                       (if add-self?
                                                                           ;; Add this button
                                                                           (format "~a~a=~a" 
                                                                                   post-string 
                                                                                   (get-field e 'name) 
                                                                                   (protect-chars (get-field e 'value)))
                                                                           ;; remove trailing &
                                                                           (substring post-string 
                                                                                      0 
                                                                                      (max 0 (sub1 (string-length post-string)))))))))])
                                             (let-values ([(name cb get-val)
                                                           (cond
                                                            [(eq? tag 'select)
                                                             (let ([select (make-object option-snip%)])
                                                               (set-form-active-select! form select)
                                                               (insert select)
                                                               (values (get-field e 'name)
                                                                       #f
                                                                       (lambda () 
                                                                         (send select get-value))))]
                                                            [(or (and (eq? tag 'input)
                                                                      (or (not type) (eq? type 'text)))
                                                                 (eq? tag 'textarea))
                                                             (let* ([text (make-object text%)]
                                                                    [snip (make-object editor-snip% text)]
                                                                    [size (get-field e 'size)]
                                                                    [val (get-field e 'value)])
                                                               (let ([km (send text get-keymap)])
                                                                 ((current-text-keymap-initializer) km)
                                                                 (unless (eq? tag 'textarea)
                                                                   (send km add-function "send-form"
                                                                         (lambda (t e)
                                                                           (send-form #f)))
                                                                   (send km map-function "enter" "send-form")))
                                                               (let ([width (* 10 (or (and size (string->number size))
                                                                                      25))])
                                                                 (send text set-min-width width)
                                                                 (send text set-max-width width))
                                                               (when val
                                                                 (send text insert val))
                                                               (insert snip)
                                                               (values
                                                                (get-field e 'name)
                                                                #f
                                                                (lambda () (send text get-text))))]
                                                            [(and (eq? tag 'input)
                                                                  (eq? type 'submit))
                                                             (insert (get-field e 'value))
                                                             (values
                                                              #f ; because we leave out this button when it's not pushed
                                                              (lambda ()
                                                                (send-form #t))
                                                              #f)]
                                                            [(and (eq? tag 'input)
                                                                  (eq? type 'checkbox))
                                                             (let ([cb (make-object checkbox-snip% (true? (or (get-field e 'value) "false")))])
                                                               (insert cb)
                                                               (values
                                                                (get-field e 'name)
                                                                #f
                                                                (lambda () (if (send cb get-value)
                                                                               "true"
                                                                               "false"))))]
                                                            [(and (eq? tag 'input)
                                                                  (eq? type 'hidden))
                                                             (values
                                                              (get-field e 'name)
                                                              #f
                                                              (lambda () (regexp-replace* re:quot (get-field e 'value) "\"")))]
                                                            [else
                                                             (insert unsupported)
                                                             (values #f #f #f)])])
                                               (set-form-parts! form
                                                                (cons (cons name
                                                                            (or get-val
                                                                                (lambda ()
                                                                                  (get-field e 'value))))
                                                                      (form-parts form)))
                                               (let-values ([(r rfl) (rest)]
                                                            [(end-pos) (current-pos)])
                                                 (set-form-active-select! form #f)
                                                 (values
                                                  (lambda ()
                                                    (cond
                                                     [cb
                                                      (send a-text make-link-style pos end-pos)
                                                      (send a-text add-thunk-callback pos end-pos cb)]
                                                     [else
                                                      (change-style delta:default-face pos end-pos)])
                                                    (r))
                                                  rfl))))]
                                          [(option)
                                           (let-values ([(pos) (current-pos)]
                                                        [(r rfl) (rest)]
                                                        [(val) (get-field e 'value)]
                                                        [(selected?) (true? (or (get-field e 'selected) 
                                                                                "false"))]
                                                        [(end-pos) (current-pos)])
                                             (let ([str (send a-text get-text pos end-pos)]
                                                   [select (form-active-select form)])
                                               (delete pos end-pos)
                                               (when select
                                                 (send select add-option str (or val str))
                                                 (when selected?
                                                   (send select set-value val)))
                                               (values r rfl)))]
                                          [(tt code samp kbd pre)
                                           (when (memq tag '(pre))
                                             (insert-newlines 2 forced-lines para-base))
                                           (let-values ([(r rfl)
                                                         (let* ([class (get-field e 'class)]
                                                                [delta (and class (lookup-class-delta class))])
                                                           (with-style-class
                                                            class
                                                            (lambda ()
                                                              (styler (if delta
                                                                          (let ([d (make-object style-delta% 'change-nothing)])
                                                                            (send d copy delta)
                                                                            (send d collapse delta:fixed)
                                                                            d)
                                                                          delta:fixed)
                                                                      rest))))])
                                             (when (memq tag '(pre))
                                               (insert-newlines 2 rfl para-base))
                                             (values r rfl))]
                                          [(span)
                                           (let* ([class (get-field e 'class)]
                                                  [delta (and class (lookup-class-delta class))])
                                             (if delta
                                                 (styler delta rest)
                                                 (rest)))]
                                          [else (rest)]))]))])
                     (load-status #f "page" (send a-text get-url))
                     (let-values ([(f fl) (translate content 0 0 0 (make-form #f #f #f null #f))])
                       (f))
                     (send a-text add-tag "top" 0)
                     (update-image-maps image-map-snips image-maps)
                     (send a-text set-position 0))))))
