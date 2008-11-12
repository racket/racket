
#lang scheme/base

(require "decode.ss"
         "struct.ss"
         "config.ss"
         "manual-struct.ss"
         "decode-struct.ss"
         scheme/list
         scheme/class
         setup/main-collects
         syntax/modresolve
         (for-syntax scheme/base))

(provide title
         section
         subsection
         subsubsection
         subsubsub*section
         include-section)

(define (gen-tag content)
  (regexp-replace* "[^-a-zA-Z0-9_=]" (content->string content) "_"))

(define (prefix->string p)
  (and p (if (string? p) p (module-path-prefix->string p))))

(define (convert-tag tag content)
  (if (list? tag)
    (append-map (lambda (t) (convert-tag t content)) tag)
    `((part ,(or tag (gen-tag content))))))

(define (title #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style #f]
               #:version [version #f] . str)
  (let ([content (decode-content str)])
    (make-title-decl (prefix->string prefix)
                     (convert-tag tag content)
                     version
                     style
                     content)))

(define (section #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style #f]
                 . str)
  (let ([content (decode-content str)])
    (make-part-start 0 (prefix->string prefix)
                     (convert-tag tag content)
                     style
                     content)))

(define (subsection #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style #f]
                    . str)
  (let ([content (decode-content str)])
    (make-part-start 1
                     (prefix->string prefix)
                     (convert-tag tag content)
                     style
                     content)))

(define (subsubsection #:tag [tag #f] #:tag-prefix [prefix #f]
                       #:style [style #f] . str)
  (let ([content (decode-content str)])
    (make-part-start 2
                     (prefix->string prefix)
                     (convert-tag tag content)
                     style
                     content)))

(define (subsubsub*section #:tag [tag #f] . str)
  (let ([content (decode-content str)])
    (make-paragraph (list (make-element 'bold content)))))

(define-syntax (include-section stx)
  (syntax-case stx ()
    [(_ mod)
     (with-syntax ([mod (syntax-local-introduce #'mod)])
       #'(begin
           (require (only-in mod doc))
           doc))]))

;; ----------------------------------------

(provide author
         author+email)
(define (author . auths)
  (make-styled-paragraph 
   (let ([nl (make-element 'newline '("\n"))])
     (case (length auths)
       [(1) auths]
       [(2) (list (car auths) nl "and " (cadr auths))]
       [else (let ([r (reverse auths)])
               (append (add-between (reverse (cdr r))
                                    (make-element #f (list "," nl)))
                       (list "," nl "and " (car r))))]))
   "author"))
(define (author+email name email)
  (make-element #f
                (list
                 name
                 " <" 
                 (regexp-replace* #rx"[.]"
                                  (regexp-replace* #rx"@" email " at ")
                                  " dot ")
                 ">")))

;; ----------------------------------------

(provide intern-taglet
         module-path-index->taglet
         module-path-prefix->string)

(define interned (make-weak-hash))
  
(define (intern-taglet v)
  (let ([v (if (list? v)
               (map intern-taglet v)
               v)])
    (if (or (string? v)
            (bytes? v)
            (list? v))
        (let ([b (hash-ref interned v #f)])
          (if b
              (or (weak-box-value b)
                  ;; just in case the value is GCed before we extract it:
                  (intern-taglet v))
              (begin
                (hash-set! interned v (make-weak-box v))
                v)))
        v)))

(define (do-module-path-index->taglet mod)
  ;; Derive the name from the module path:
  (let ([p (collapse-module-path-index
            mod
            (lambda () (build-path (current-directory) "dummy")))])
    (if (path? p)
        ;; If we got a path back anyway, then it's best to use the resolved
        ;; name; if the current directory has changed since we 
        ;; the path-index was resolved, then p might not be right. Also,
        ;; the resolved path might be a symbol instead of a path.
        (let ([rp (resolved-module-path-name 
                   (module-path-index-resolve mod))])
          (if (path? rp)
              (intern-taglet 
               (path->main-collects-relative rp))
              rp))
        (let ([p (if (and (pair? p)
                          (eq? (car p) 'planet))
                     ;; Normalize planet verion number based on current
                     ;; linking:
                     (let-values ([(path pkg)
                                   (get-planet-module-path/pkg p #f #f)])
                       (list* 'planet
                              (cadr p)
                              (list (car (caddr p))
                                    (cadr (caddr p))
                                    (pkg-maj pkg)
                                    (pkg-min pkg))
                              (cdddr p)))
                     ;; Otherwise the path is fully normalized:
                     p)])
          (intern-taglet p)))))

(define collapsed (make-weak-hasheq))
(define (module-path-index->taglet mod)
  (or (hash-ref collapsed mod #f)
      (let ([v (do-module-path-index->taglet mod)])
        (hash-set! collapsed mod v)
        v)))

(define (module-path-prefix->string p)
  (format "~a" (module-path-index->taglet (module-path-index-join p #f))))

;; ----------------------------------------

(require syntax/modcollapse
         ;; Needed to normalize planet version numbers:
         (only-in planet/resolver get-planet-module-path/pkg)
         (only-in planet/private/data pkg-maj pkg-min))

(provide itemize item item?)

(define (itemize #:style [style #f] . items)
  (let ([items (filter (lambda (v) (not (whitespace? v))) items)])
    (for ([v items])
      (unless (an-item? v)
        (error 'itemize "expected an item, found something else: ~e" v)))
    (let ([flows (map an-item-flow items)])
      (if style
          (make-styled-itemization flows style)
          (make-itemization flows)))))

(define-struct an-item (flow))
(define (item? x) (an-item? x))

(define (item . str)
  (make-an-item (decode-flow str)))

;; ----------------------------------------

(provide hspace
         elem aux-elem
         italic bold
         tt span-class
         subscript superscript)

(define hspace-cache (make-vector 100 #f))

(define (hspace n)
  (if (n . < . (vector-length hspace-cache))
      (or (vector-ref hspace-cache n)
          (let ([h (make-element 'hspace (list (make-string n #\space)))])
            (vector-set! hspace-cache n h)
            h))
      (make-element 'hspace (list (make-string n #\space)))))

(define (elem . str)
  (make-element #f (decode-content str)))

(define (aux-elem . s)
  (make-aux-element #f (decode-content s)))

(define (italic . str)
  (make-element 'italic (decode-content str)))

(define (bold . str)
  (make-element 'bold (decode-content str)))

(define (tt . str)
  (let* ([l (decode-content str)]
         [l (let ([m (and (pair? l)
                          (string? (car l))
                          (regexp-match-positions #rx"^ +" (car l)))])
              (if m
                (list* (hspace (- (cdar m) (caar m)))
                       (substring (car l) (cdar m))
                       (cdr l))
                l))])
    (if (andmap string? l)
      (make-element 'tt l)
      (make-element #f (map (lambda (s)
                              (if (or (string? s) (symbol? s))
                                (make-element 'tt (list s))
                                s))
                            l)))))

(define (span-class classname . str)
  (make-element classname (decode-content str)))

(define (subscript . str)
  (make-element 'subscript (decode-content str)))

(define (superscript . str)
  (make-element 'superscript (decode-content str)))

;; ----------------------------------------

(provide section-index index index* as-index index-section
         get-index-entries index-block)

(define (section-index . elems)
  (make-part-index-decl (map element->string elems) elems))

(define (record-index word-seq element-seq tag content)
  (make-index-element #f
                      (list (make-target-element #f content `(idx ,tag)))
                      `(idx ,tag)
                      word-seq
                      element-seq
                      #f))

(define (index* word-seq content-seq . s)
  (let ([key (make-generated-tag)])
    (record-index (map clean-up-index-string word-seq)
                  content-seq key (decode-content s))))

(define (index word-seq . s)
  (let ([word-seq (if (string? word-seq) (list word-seq) word-seq)])
    (apply index* word-seq word-seq s)))

(define (as-index . s)
  (let ([key (make-generated-tag)]
        [content (decode-content s)])
    (record-index
     (list (clean-up-index-string (content->string content)))
     (if (= 1 (length content)) content (list (make-element #f content)))
     key
     content)))

(define (index-section #:title [title "Index"] #:tag [tag #f])
  (make-unnumbered-part #f
                        `((part ,(or tag "doc-index")))
                        (list title)
                        'index
                        null
                        (make-flow (list (index-block)))
                        null))

;; returns an ordered list of (list tag (text ...) (element ...) index-desc)
(define (get-index-entries sec ri)
  (define (compare-lists xs ys <?)
    (let loop ([xs xs] [ys ys])
      (cond [(and (null? xs) (null? ys)) '=]
            [(null? xs) '<]
            [(null? ys) '>]
            [(<? (car xs) (car ys)) '<]
            [(<? (car ys) (car xs)) '>]
            [else (loop (cdr ys) (cdr xs))])))
  ;; string-ci<? as a major key, and string<? next, so "Foo" precedes "foo"
  ;; (define (string*<? s1 s2)
  ;;   (or (string-ci<? s1 s2)
  ;;       (and (not (string-ci<? s2 s1)) (string<? s1 s2))))
  (define (get-desc entry)
    (let ([desc (cadddr entry)])
      (cond [(exported-index-desc? desc)
             (cons 'libs (map (lambda (l)
                                (format "~s" l))
                              (exported-index-desc-from-libs desc)))]
            [(module-path-index-desc? desc) '(mod)]
            [(part-index-desc? desc) '(part)]
            [(delayed-index-desc? desc) '(delayed)]
            [else '(#f)])))
  ;; parts first, then modules, then bindings, delayed means it's not
  ;; the last round, and #f means no desc
  (define desc-order '(part mod libs delayed #f))
  ;; this defines an imposed ordering for module names
  (define lib-order '(#rx"^scheme(?:/|$)" #rx"^r.rs(?:/|$)" #rx"^lang(?:/|$)"))
  (define (lib<? lib1 lib2)
    (define (lib-level lib)
      (let loop ([i 0] [rxs lib-order])
        (if (or (null? rxs) (regexp-match? (car rxs) lib))
          i (loop (add1 i) (cdr rxs)))))
    (let ([l1 (lib-level lib1)] [l2 (lib-level lib2)])
      (if (= l1 l2) (string<? lib1 lib2) (< l1 l2))))
  (define (compare-desc e1 e2)
    (let* ([d1 (get-desc e1)] [d2 (get-desc e2)]
           [t1 (car d1)]      [t2 (car d2)])
      (cond [(memq t2 (cdr (memq t1 desc-order))) '<]
            [(memq t1 (cdr (memq t2 desc-order))) '>]
            [else (case t1 ; equal to t2
                    [(part) '=] ; will just compare tags
                    [(mod)  '=] ; the text fields are the names of the modules
                    [(libs) (compare-lists (cdr d1) (cdr d2) lib<?)]
                    [(delayed) '>] ; dosn't matter, will run again
                    [(#f) '=])])))
  (define (entry<? e1 e2)
    (let ([text1 (cadr e1)] [text2 (cadr e2)])
      (case (compare-lists text1 text2 string-ci<?)
        [(<) #t] [(>) #f]
        [else (case (compare-desc e1 e2)
                [(<) #t] [(>) #f]
                [else (case (compare-lists text1 text2 string<?)
                        [(<) #t] [(>) #f]
                        [else
                         ;; (error 'get-index-entries
                         ;;        ;; when this happens, revise this code so
                         ;;        ;; ordering will always be deterministic
                         ;;        "internal error -- unordered entries: ~e ~e"
                         ;;        e1 e2)
                         ;; Instead, just compare the tags
                         (string<? (format "~a" (car e1))
                                   (format "~a" (car e2)))])])])))
  (define l null)
  (hash-for-each
   (let ([parent (collected-info-parent (part-collected-info sec ri))])
     (if parent
       (collected-info-info (part-collected-info parent ri))
       (collect-info-ext-ht (resolve-info-ci ri))))
   (lambda (k v)
     (when (and (pair? k) (eq? 'index-entry (car k)))
       (set! l (cons (cons (cadr k) v) l)))))
  (sort l entry<?))

(define (index-block)
  (define alpha (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (define (rows . rows)
    (make-table 'index (map (lambda (row)
                              (list (make-flow (list (make-paragraph row)))))
                            rows)))
  (define contents
    (lambda (renderer sec ri)
      (define l (get-index-entries sec ri))
      (define manual-newlines? (send renderer index-manual-newlines?))
      (define alpha-starts (make-hasheq))
      (define alpha-row
        (let loop ([i l] [alpha alpha])
          (define (add-letter let l)
            (list* (make-element "nonavigation" (list (string let))) " " l))
          (cond [(null? alpha) null]
                [(null? i) (add-letter (car alpha) (loop i (cdr alpha)))]
                [else
                 (let* ([strs (cadr (car i))]
                        [letter (if (or (null? strs) (string=? "" (car strs)))
                                  #f
                                  (char-upcase (string-ref (car strs) 0)))])
                   (cond [(not letter) (loop (cdr i) alpha)]
                         [(char-ci>? letter (car alpha))
                          (add-letter (car alpha) (loop i (cdr alpha)))]
                         [(char-ci=? letter (car alpha))
                          (hash-set! alpha-starts (car i) letter)
                          (list* (make-element
                                  (make-target-url (format "#alpha:~a" letter)
                                                   #f)
                                  (list (string (car alpha))))
                                 " "
                                 (loop (cdr i) (cdr alpha)))]
                         [else (loop (cdr i) alpha)]))])))
      (define body
        (let ([br (if manual-newlines? (make-element 'newline '("\n")) "")])
          (map (lambda (i)
                 (let ([e (make-link-element
                           "indexlink"
                           `(,@(add-between (caddr i) ", ") ,br)
                           (car i))])
                   (cond [(hash-ref alpha-starts i #f)
                          => (lambda (let)
                               (make-element
                                (make-url-anchor
                                 (format "alpha:~a" (char-upcase let)))
                                (list e)))]
                         [else e])))
               l)))
      (if manual-newlines?
        (rows alpha-row '(nbsp) body)
        (apply rows alpha-row '(nbsp) (map list body)))))
  (make-delayed-block contents))

;; ----------------------------------------

(provide table-of-contents
         local-table-of-contents)

(define (table-of-contents)
  (make-delayed-block
   (lambda (renderer part ri)
     (send renderer table-of-contents part ri))))

(define (local-table-of-contents #:style [style #f])
  (make-delayed-block
   (lambda (renderer part ri)
     (send renderer local-table-of-contents part ri style))))
