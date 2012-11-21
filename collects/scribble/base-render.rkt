#lang scheme/base

(require "core.rkt"
         "private/render-utils.rkt"
         mzlib/class
         mzlib/serialize
         scheme/file
         scheme/path
         setup/main-collects
         setup/path-relativize
         file/convertible
         net/url-structs
         "render-struct.rkt")

(provide render%
         render<%>)

(define render<%>
  (interface ()
    traverse
    collect
    resolve
    render
    serialize-info
    deserialize-info
    get-external
    get-undefined
    
    ;; undocumented:
    current-render-mode
    get-substitutions
    render-part
    render-flow
    render-intrapara-block
    render-table
    render-itemization
    render-paragraph
    render-content
    render-nested-flow
    render-block
    render-other
    get-dest-directory))

(define render%
  (class* object% (render<%>)

    (init-field dest-dir
                [refer-to-existing-files #f]
                [root-path #f]
                [prefix-file #f]
                [style-file #f]
                [style-extra-files null]
                [extra-files null]
                [helper-file-prefix #f])

    (define/public (current-render-mode)
      '())

    (define/public (get-dest-directory [create? #f])
      (when (and dest-dir create? (not (directory-exists? dest-dir)))
        (make-directory* dest-dir))
      dest-dir)

    (define/public (get-substitutions) null)

    (define/public (get-suffix) #".txt")

    (define/public (index-manual-newlines?)
      #f)

    (define/public (format-number number sep)
      (if (or (null? number)
              (andmap not number))
        null
        (cons (let ([s (apply
                        string-append
                        (map (lambda (n) (if n (format "~s." n) ""))
                             (reverse number)))])
                (substring s 0 (sub1 (string-length s))))
              sep)))

    (field [report-output?? #f])
    (define/public (report-output?) report-output??)
    (define/public (report-output!) (set! report-output?? #t))

    ;; ----------------------------------------

    (define/public (extract-part-style-files d ri tag stop-at-part? pred extract)
      (let ([ht (make-hash)])
        (let loop ([p d][up? #t][only-up? #f])
          (let ([s (part-style p)])
            (when up?
              (let ([p (collected-info-parent (part-collected-info p ri))])
                (if p
                    (loop p #t #t)
                    null)))
             (extract-style-style-files (part-style p) ht pred extract)
             (unless only-up?
               (extract-content-style-files (part-to-collect p) d ri ht pred extract)
               (extract-content-style-files (part-title-content p) d ri ht pred extract)
               (extract-flow-style-files (part-blocks p) d ri ht pred extract))
             (unless only-up?
               (for-each (lambda (p)
                           (unless (stop-at-part? p)
                             (loop p #f #f)))
                         (part-parts p)))))
        (for/list ([k (in-hash-keys ht)]) (if (or (bytes? k) (url? k))
                                              k 
                                              (main-collects-relative->path k)))))

    (define/private (extract-style-style-files s ht pred extract)
      (for ([v (in-list (style-properties s))])
        (when (pred v)
          (hash-set! ht (extract v) #t))))

    (define/private (extract-flow-style-files blocks d ri ht pred extract)
      (for ([b (in-list blocks)])
        (extract-block-style-files b d ri ht pred extract)))

    (define/private (extract-block-style-files p d ri ht pred extract)
      (cond
        [(table? p)
         (extract-style-style-files (table-style p) ht pred extract)
         (for-each (lambda (blocks)
                     (for-each (lambda (block)
                                 (unless (eq? block 'cont)
                                   (extract-block-style-files block d ri ht pred extract)))
                               blocks))
                   (table-blockss p))]
        [(itemization? p)
         (extract-style-style-files (itemization-style p) ht pred extract)
         (for-each (lambda (blocks)
                     (extract-flow-style-files blocks d ri ht pred extract))
                   (itemization-blockss p))]
        [(nested-flow? p) 
         (extract-style-style-files (nested-flow-style p) ht pred extract)
         (extract-flow-style-files (nested-flow-blocks p) d ri ht pred extract)]
        [(compound-paragraph? p)
         (extract-style-style-files (compound-paragraph-style p) ht pred extract)
         (extract-flow-style-files (compound-paragraph-blocks p) d ri ht pred extract)]
        [(delayed-block? p)
         (let ([v ((delayed-block-resolve p) this d ri)])
           (extract-block-style-files v d ri ht pred extract))]
        [(traverse-block? p)
         (extract-block-style-files (traverse-block-block p ri) d ri ht pred extract)]
        [else
         (extract-style-style-files (paragraph-style p) ht pred extract)
         (extract-content-style-files (paragraph-content p) d ri ht pred extract)]))

    (define/public (string-to-implicit-styles e) null)

    (define/private (extract-content-style-files e d ri ht pred extract)
      (cond
       [(string? e) (let ([ses (string-to-implicit-styles e)])
                      (when (pair? ses)
                        (for ([s (in-list ses)])
                          (extract-style-style-files s ht pred extract))))]
       [(element? e)
        (when (style? (element-style e))
          (extract-style-style-files (element-style e) ht pred extract))
        (extract-content-style-files (element-content e) d ri ht pred extract)]
       [(multiarg-element? e)
        (when (style? (multiarg-element-style e))
          (extract-style-style-files (multiarg-element-style e) ht pred extract))
        (extract-content-style-files (multiarg-element-contents e) d ri ht pred extract)]
       [(list? e)
        (for ([e (in-list e)])
          (extract-content-style-files e d ri ht pred extract))]
       [(delayed-element? e)
        (extract-content-style-files (delayed-element-content e ri) d ri ht pred extract)]
       [(traverse-element? e)
        (extract-content-style-files (traverse-element-content e ri) d ri ht pred extract)]
       [(part-relative-element? e)
        (extract-content-style-files (part-relative-element-content e ri) d ri ht pred extract)]))

    (define/public (extract-version d)
      (or (ormap (lambda (v)
                   (and (document-version? v)
                        (document-version-text v)))
                 (style-properties (part-style d)))
          ""))

    (define/public (extract-date d)
      (ormap (lambda (v)
               (and (document-date? v)
                    (document-date-text v)))
             (style-properties (part-style d))))

    (define/private (extract-pre-paras d sym)
      (let loop ([l (part-blocks d)])
        (cond
         [(null? l) null]
         [else (let ([v (car l)])
                 (cond
                  [(and (paragraph? v)
                        (eq? sym (style-name (paragraph-style v))))
                   (cons v (loop (cdr l)))]
                  [(compound-paragraph? v)
                   (append (loop (compound-paragraph-blocks v))
                           (loop (cdr l)))]
                  [else (loop (cdr l))]))])))

    (define/public (extract-authors d)
      (extract-pre-paras d 'author))

    (define/public (extract-pretitle d)
      (extract-pre-paras d 'pretitle))
    
    ;; ----------------------------------------

    (define root (make-mobile-root root-path))
    
    (define-values (:path->root-relative
                    :root-relative->path)
      (if root-path
          (make-relativize (lambda () root-path)
                           root
                           'path->root-relative
                           'root-relative->path)
          (values #f #f)))

    (define/public (path->root-relative p)
      (if root-path
          (:path->root-relative p)
          p))

    (define/public (root-relative->path p)
      (if (root-relative? p)
          (apply build-path (mobile-root-path (car p))
                 (map bytes->path-element (cdr p)))
          p))

    (define/public (root-relative? p)
      (and (pair? p)
           (mobile-root? (car p))))

    ;; ----------------------------------------

    (define/public (fresh-tag-collect-context? d ci)
      #f)
    (define/public (fresh-tag-resolve-context? d ri)
      #f)
    (define/public (fresh-tag-render-context? d ri)
      #f)

    (define/private (extend-prefix d fresh?)
      (cond
       [fresh? null]
       [(part-tag-prefix d)
        (cons (part-tag-prefix d) (current-tag-prefixes))]
       [else (current-tag-prefixes)]))

    ;; ----------------------------------------
    ;; marshal info

    (define/public (get-serialize-version)
      4)

    (define/public (serialize-infos ri n d)
      (if (= n 1)
          (list (serialize-info ri))
          (map (lambda (ht) (serialize-one-ht ri ht))
               (partition-info (resolve-info-ci ri) n d))))

    (define/private (partition-info all-ci n d)
      ;; partition information in `all-ci' based on `d's:
      (let ([prefix (part-tag-prefix d)]
            [new-hts (for/list ([i (in-range n)])
                       (make-hash))]
            [covered (make-hash)])
        ;; Fill in new-hts from parts:
        (for ([sub-d (in-list (part-parts d))]
              [i (in-naturals)])
          (define ht (list-ref new-hts (min (add1 i) (sub1 n))))
          (define cdi (hash-ref (collect-info-parts all-ci) sub-d #f))
          (define sub-prefix (part-tag-prefix sub-d))
          (when cdi
            (for ([(k v) (in-hash (collected-info-info cdi))])
              (when (cadr k)
                (define sub-k (if sub-prefix
                                  (convert-key sub-prefix k)
                                  k))
                (define full-k (if prefix
                                   (convert-key prefix sub-k)
                                   sub-k))
                (hash-set! ht full-k v)
                (hash-set! covered full-k #t)))))
        ;; Anything not covered in the new-hts must go in the main hts:
        (let ([ht0 (car new-hts)])
          (for ([(k v) (in-hash (collect-info-ht all-ci))])
            (unless (hash-ref covered k #f)
              (hash-set! ht0 k v))))
        ;; Return hts:
        new-hts))

    (define/public (serialize-info ri)
      (serialize-one-ht ri (collect-info-ht (resolve-info-ci ri))))

    (define/public (serialize-one-ht ri ht)
      (parameterize ([current-serialize-resolve-info ri])
        (serialize (cons root ht))))

    (define/public (deserialize-info v ci #:root [root-path #f])
      (let ([root+ht (deserialize v)]
            [in-ht (collect-info-ext-ht ci)])
        (when root-path
          (set-mobile-root-path! (car root+ht) root-path))
        (for ([(k v) (cdr root+ht)])
          (hash-set! in-ht k v))))

    (define/public (get-defined ci)
      (hash-map (collect-info-ht ci) (lambda (k v) k)))

    (define/public (get-defineds ci n d)
      (for/list ([ht (partition-info ci n d)])
        (hash-map ht (lambda (k v) k))))

    (define/public (get-external ri)
      (hash-map (resolve-info-undef ri) (lambda (k v) k)))

    (define/public (get-undefined ri)
      (for/list ([(k v) (in-hash (resolve-info-undef ri))]
                 #:unless (or (eq? v 'found)
                              (and v
                                   ;; v is a search key; see if any key in the set was resolved:
                                   (let ([ht (hash-ref (resolve-info-searches ri) v)])
                                     (for/or ([k2 (in-hash-keys ht)])
                                       (eq? 'found (hash-ref (resolve-info-undef ri) k2 #f)))))))
        k))

    (define/public (transfer-info ci src-ci)
      (let ([in-ht (collect-info-ext-ht ci)])
        (for ([(k v) (collect-info-ext-ht src-ci)])
          (hash-set! in-ht k v)))
      (set-demand-chain-demands! 
       (collect-info-ext-demand ci)
       (cons (collect-info-ext-demand src-ci)
             (demand-chain-demands (collect-info-ext-demand ci)))))

    ;; ----------------------------------------
    ;; document-order traversal

    (define/public (traverse ds fns)
      (let loop ([fp #hasheq()])
        (let ([fp2 (start-traverse ds fns fp)])
          (if (equal? fp fp2)
              fp
              (loop fp2)))))

    (define/public (start-traverse ds fns fp)
      (for/fold ([fp fp]) ([d (in-list ds)])
        (traverse-part d fp)))

    (define/public (traverse-part d fp)
      (let* ([fp (if (part-title-content d)
                     (traverse-content (part-title-content d) fp)
                     fp)]
             [fp (traverse-content (part-to-collect d) fp)]
             [fp (traverse-flow (part-blocks d) fp)])
        (for/fold ([fp fp]) ([p (in-list (part-parts d))])
          (traverse-part p fp))))

    (define/public (traverse-paragraph p fp)
      (traverse-content (paragraph-content p) fp))

    (define/public (traverse-flow p fp)
      (for/fold ([fp fp]) ([p (in-list p)])
        (traverse-block p fp)))

    (define/public (traverse-block p fp)
      (cond [(table? p) (traverse-table p fp)]
            [(itemization? p) (traverse-itemization p fp)]
            [(nested-flow? p) (traverse-nested-flow p fp)]
            [(compound-paragraph? p) (traverse-compound-paragraph p fp)]
            [(delayed-block? p) fp]
            [(traverse-block? p) (traverse-force fp p 
                                                 (traverse-block-traverse p) 
                                                 (lambda (p fp) (traverse-block p fp)))]
            [else (traverse-paragraph p fp)]))

    (define/public (traverse-table i fp)
      (for*/fold ([fp fp]) ([ds (in-list (table-blockss i))]
                            [d (in-list ds)])
        (if (eq? d 'cont) 
            fp
            (traverse-block d fp))))

    (define/public (traverse-itemization i fp)
      (for/fold ([fp fp]) ([d (in-list (itemization-blockss i))])
        (traverse-flow d fp)))

    (define/public (traverse-nested-flow i fp)
      (for/fold ([fp fp]) ([d (in-list (nested-flow-blocks i))])
        (traverse-block d fp)))

    (define/public (traverse-compound-paragraph i fp)
      (for/fold ([fp fp]) ([d (in-list (compound-paragraph-blocks i))])
        (traverse-block d fp)))

    (define/public (traverse-content i fp)
      (cond
       [(traverse-element? i) (traverse-force fp i (traverse-element-traverse i)
                                              (lambda (i fp) (traverse-content i fp)))]
       [(element? i) (traverse-content (element-content i) fp)]
       [(list? i) (for/fold ([fp fp]) ([c (in-list i)])
                    (traverse-content c fp))]
       [(multiarg-element? i)
        (for/fold ([fp fp]) ([c (in-list (multiarg-element-contents i))])
          (traverse-content c fp))]
       [else fp]))

    (define/private (traverse-force fp p proc again)
      (let ([v (hash-ref fp p (lambda () proc))])
        (if (procedure? v)
            (let ([fp fp])
              (let ([v2 (v (lambda (key default)
                             (if (eq? key 'scribble:current-render-mode)
                                 (current-render-mode)
                                 (hash-ref fp key default)))
                           (lambda (key val)
                             (if (eq? key 'scribble:current-render-mode)
                                 (raise-mismatch-error 
                                  'traverse-info-set! 
                                  "cannot set value for built-in key: "
                                  key)
                                 (set! fp (hash-set fp key val)))))])
                (let ([fp (hash-set fp p v2)])
                  (if (procedure? v2)
                      fp
                      (again v2 fp)))))
            fp)))
    
    ;; ----------------------------------------
    ;; global-info collection

    (define/public (collect ds fns fp [demand (lambda (key ci) #f)])
      (let ([ci (make-collect-info fp
                                   (make-hash)
                                   (make-hash)
                                   (make-demand-chain (list demand))
                                   (make-hasheq)
                                   (make-hasheq)
                                   null
                                   (make-hasheq)
                                   null)])
        (start-collect ds fns ci)
        ci))

    (define/public (start-collect ds fns ci)
      (map (lambda (d) (collect-part d #f ci null))
           ds))

    (define/public (collect-part d parent ci number)
      (let ([p-ci (make-collect-info
                   (collect-info-fp ci)
                   (make-hash)
                   (collect-info-ext-ht ci)
                   (collect-info-ext-demand ci)
                   (collect-info-parts ci)
                   (collect-info-tags ci)
                   (if (part-tag-prefix d)
                       (append (collect-info-gen-prefix ci)
                               (list (part-tag-prefix d)))
                       (collect-info-gen-prefix ci))
                   (collect-info-relatives ci)
                   (cons d (collect-info-parents ci)))])
        (hash-set! (collect-info-parts ci)
                   d
                   (make-collected-info number
                                        parent
                                        (collect-info-ht p-ci)))
        (parameterize ([current-tag-prefixes
                        (extend-prefix d (fresh-tag-collect-context? d p-ci))])
          (when (part-title-content d)
            (collect-content (part-title-content d) p-ci))
          (collect-part-tags d p-ci number)
          (collect-content (part-to-collect d) p-ci)
          (collect-flow (part-blocks d) p-ci)
          (let loop ([parts (part-parts d)]
                     [pos 1])
            (unless (null? parts)
              (let ([s (car parts)])
                (collect-part s d p-ci
                              (cons (if (part-style? s 'unnumbered)
                                        #f 
                                        pos)
                                    number))
                (loop (cdr parts)
                      (if (part-style? s 'unnumbered)
                          pos
                          (add1 pos)))))))
        (let ([prefix (part-tag-prefix d)])
          (for ([(k v) (collect-info-ht p-ci)])
            (when (cadr k)
              (collect-put! ci (if prefix
                                   (convert-key prefix k) 
                                   k) 
                            v))))))

    (define/private (convert-key prefix k)
      (case (car k)
        [(part tech cite)
         (let ([rhs (cadr k)])
           (if (or (string? rhs) (pair? rhs))
               (list (car k) (cons prefix (if (pair? rhs) rhs (list rhs))))
               k))]
        [(index-entry)
         (let ([v (convert-key prefix (cadr k))])
           (if (eq? v (cadr k)) k (list 'index-entry v)))]
        [else k]))

    (define/public (collect-part-tags d ci number)
      (for ([t (part-tags d)])
        (let ([t (generate-tag t ci)])
          (collect-put! ci
                        t
                        ;; INFO SHAPE:
                        ;; The HTML renderer defines its info as an
                        ;;  extension of this vector's shape, so that
                        ;;  other renderers can use HTML info.
                        (vector (or (part-title-content d) '("???")) 
                                (add-current-tag-prefix t)
                                number)))))

    (define/public (collect-paragraph p ci)
      (collect-content (paragraph-content p) ci))

    (define/public (collect-flow p ci)
      (for ([p (in-list p)])
        (collect-block p ci)))

    (define/public (collect-block p ci)
      (cond [(table? p) (collect-table p ci)]
            [(itemization? p) (collect-itemization p ci)]
            [(nested-flow? p) (collect-nested-flow p ci)]
            [(compound-paragraph? p) (collect-compound-paragraph p ci)]
            [(delayed-block? p) (void)]
            [(traverse-block? p) (collect-block (traverse-block-block p ci) ci)]
            [else (collect-paragraph p ci)]))

    (define/public (collect-table i ci)
      (for ([d (in-list (apply append (table-blockss i)))])
        (unless (eq? d 'cont) (collect-block d ci))))

    (define/public (collect-itemization i ci)
      (for ([d (in-list (itemization-blockss i))])
        (collect-flow d ci)))

    (define/public (collect-nested-flow i ci)
      (for ([d (in-list (nested-flow-blocks i))])
        (collect-block d ci)))

    (define/public (collect-compound-paragraph i ci)
      (for ([d (in-list (compound-paragraph-blocks i))])
        (collect-block d ci)))

    (define/public (collect-content i ci)
      (if (part-relative-element? i)
        (let ([content (or (hash-ref (collect-info-relatives ci) i #f)
                           (let ([v ((part-relative-element-collect i) ci)])
                             (hash-set! (collect-info-relatives ci) i v)
                             v))])
          (collect-content content ci))
        (begin (when (target-element? i) (collect-target-element i ci))
               (when (index-element? i) (collect-index-element i ci))
               (when (collect-element? i) ((collect-element-collect i) ci))
               (when (element? i)
                 (collect-content (element-content i) ci))
               (when (multiarg-element? i)
                 (collect-content (multiarg-element-contents i) ci))
               (when (list? i)
                 (for ([e (in-list i)]) (collect-content e ci))))))

    (define/public (collect-target-element i ci)
      (let ([t (generate-tag (target-element-tag i) ci)])
        (collect-put! ci t
                      ;; See "INFO SHAPE" above.
                      (vector (element-content i)
                              (add-current-tag-prefix t)))))

    (define/public (collect-index-element i ci)
      (collect-put! ci
                    `(index-entry ,(generate-tag (index-element-tag i) ci))
                    (list (index-element-plain-seq i)
                          (index-element-entry-seq i)
                          (index-element-desc i))))

    ;; ----------------------------------------
    ;; global-info resolution

    (define/public (resolve ds fns ci)
      (let ([ri (make-resolve-info ci (make-hasheq) (make-hash) (make-hash))])
        (start-resolve ds fns ri)
        ri))

    (define/public (start-resolve ds fns ri)
      (map (lambda (d) (resolve-part d ri)) ds))

    (define/public (resolve-part d ri)
      (parameterize ([current-tag-prefixes
                      (extend-prefix d (fresh-tag-resolve-context? d ri))])
        (when (part-title-content d)
          (resolve-content (part-title-content d) d ri))
        (resolve-flow (part-blocks d) d ri)
        (for ([p (part-parts d)])
          (resolve-part p ri))))

    (define/public (resolve-paragraph p d ri)
      (resolve-content (paragraph-content p) d ri))

    (define/public (resolve-flow f d ri)
      (for ([p (in-list f)])
        (resolve-block p d ri)))

    (define/public (resolve-block p d ri)
      (cond
        [(table? p) (resolve-table p d ri)]
        [(itemization? p) (resolve-itemization p d ri)]
        [(nested-flow? p) (resolve-nested-flow p d ri)]
        [(compound-paragraph? p) (resolve-compound-paragraph p d ri)]
        [(delayed-block? p) 
         (let ([v ((delayed-block-resolve p) this d ri)])
           (hash-set! (resolve-info-delays ri) p v)
           (resolve-block v d ri))]
        [(traverse-block? p) (resolve-block (traverse-block-block p ri) d ri)]
        [else (resolve-paragraph p d ri)]))

    (define/public (resolve-table i d ri)
      (for ([f (in-list (apply append (table-blockss i)))])
        (unless (eq? f 'cont) (resolve-block f d ri))))

    (define/public (resolve-itemization i d ri)
      (for ([f (in-list (itemization-blockss i))])
        (resolve-flow f d ri)))
    
    (define/public (resolve-nested-flow i d ri)
      (for ([f (in-list (nested-flow-blocks i))])
        (resolve-block f d ri)))

    (define/public (resolve-compound-paragraph i d ri)
      (for ([f (in-list (compound-paragraph-blocks i))])
        (resolve-block f d ri)))

    (define/public (resolve-content i d ri)
      (cond
        [(part-relative-element? i)
         (resolve-content (part-relative-element-content i ri) d ri)]
        [(delayed-element? i)
         (resolve-content (or (hash-ref (resolve-info-delays ri) i #f)
                              (let ([v ((delayed-element-resolve i) this d ri)])
                                (hash-set! (resolve-info-delays ri) i v)
                                v))
                          d ri)]
        [(traverse-element? i)
         (resolve-content (traverse-element-content i ri) d ri)]
        [(list? i)
         (for ([i (in-list i)])
           (resolve-content i d ri))]
        [(element? i)
         (cond
           [(index-element? i)
            (let ([e (index-element-desc i)])
              (when (delayed-index-desc? e)
                (let ([v ((delayed-index-desc-resolve e) this d ri)])
                  (hash-set! (resolve-info-delays ri) e v))))]
           [(link-element? i)
            (resolve-get d ri (link-element-tag i))])
         (resolve-content (element-content i) d ri)]
        [(multiarg-element? i)
         (resolve-content (multiarg-element-contents i) d ri)]))

    ;; ----------------------------------------
    ;; render methods

    (define/public (auto-extra-files? v) #f)
    (define/public (auto-extra-files-paths v) null)

    (define/public (install-extra-files ds)
      (for ([fn extra-files]) (install-file fn #:private-name? #f))
      (unless prefix-file
        (for ([d (in-list ds)])
          (let ([extras (ormap (lambda (v) (and (auto-extra-files? v) v))
                               (style-properties (part-style d)))])
            (when extras
              (for ([fn (in-list (auto-extra-files-paths extras))])
                (install-file (main-collects-relative->path fn)
                              #:private-name? #f)))))))

    (define/public (render ds fns ri)
      ;; maybe this should happen even if fns is empty or all #f?
      ;; or maybe it should happen for each file rendered (when d is not #f)?
      (unless (andmap not ds) (install-extra-files ds))
      (map (lambda (d fn)
             (define (one) (render-one d ri fn))
             (when (report-output?) (printf " [Output to ~a]\n" fn))
             (if fn
               (with-output-to-file fn #:exists 'truncate/replace one)
               ;; a #f filename means return the contents as a string
               (let ([o (open-output-string)])
                 (parameterize ([current-output-port o])
                   (one)
                   (get-output-string o)))))
           ds
           fns))

    (define/public (render-one d ri fn)
      (render-part d ri))

    (define/public (render-part d ri)
      (parameterize ([current-tag-prefixes
                      (extend-prefix d (fresh-tag-render-context? d ri))])
        (render-part-content d ri)))

    (define/public (render-part-content d ri)
      (list
       (when (part-title-content d)
         (render-content (part-title-content d) d ri))
       (render-flow (part-blocks d) d ri #f)
       (map (lambda (s) (render-part s ri))
            (part-parts d))))

    (define/public (render-paragraph p part ri)
      (render-content (paragraph-content p) part ri))

    (define/public (render-compound-paragraph p part ri starting-item?)
      (apply append (let loop ([l (compound-paragraph-blocks p)]
                               [first? #t])
                      (cond
                       [(null? l) null]
                       [else (cons
                              (render-intrapara-block (car l) part ri first? (null? (cdr l))
                                                      (and first? starting-item?))
                              (loop (cdr l) #f))]))))

    (define/public (render-flow p part ri starting-item?)
      (if (null? p)
          null
          (append
           (render-block (car p)
                         part ri starting-item?)
           (apply append
                  (map (lambda (p)
                         (render-block p part ri #f))
                       (cdr p))))))

    (define/public (render-intrapara-block p part ri first? last? starting-item?)
      (render-block p part ri starting-item?))

    (define/public (render-block p part ri starting-item?)
      (cond
       [(table? p) (if (memq 'aux (style-properties (table-style p)))
                       (render-auxiliary-table p part ri)
                       (render-table p part ri starting-item?))]
       [(itemization? p) (render-itemization p part ri)]
       [(nested-flow? p) (render-nested-flow p part ri starting-item?)]
       [(compound-paragraph? p) (render-compound-paragraph p part ri starting-item?)]
       [(delayed-block? p) 
        (render-block (delayed-block-blocks p ri) part ri starting-item?)]
       [(traverse-block? p) 
        (render-block (traverse-block-block p ri) part ri starting-item?)]
       [else (render-paragraph p part ri)]))

    (define/public (render-auxiliary-table i part ri)
      null)

    (define/public (render-table i part ri starting-item?)
      (map (lambda (d) (if (eq? i 'cont) null (render-block d part ri #f)))
           (apply append (table-blockss i))))

    (define/public (render-itemization i part ri)
      (map (lambda (d) (render-flow d part ri #t))
           (itemization-blockss i)))

    (define/public (render-nested-flow i part ri starting-item?)
      (for/list ([b (in-list (nested-flow-blocks i))]
                 [pos (in-naturals)])
        (render-block b part ri (and starting-item? (zero? pos)))))

    (define/public (render-content i part ri)
      (cond
        [(string? i) (render-other i part ri)] ; short-cut for common case
        [(list? i)
         (apply append (for/list ([i (in-list i)]) (render-content i part ri)))]
        [(and (link-element? i)
              (null? (element-content i)))
         (let ([v (resolve-get part ri (link-element-tag i))])
           (if v
               (render-content (strip-aux (or (vector-ref v 0) "???")) part ri)
               (render-content (list "[missing]") part ri)))]
        [(element? i)
         (when (render-element? i)
           ((render-element-render i) this part ri))
         (render-content (element-content i) part ri)]
        [(multiarg-element? i)
         (render-content (multiarg-element-contents i) part ri)]
        [(delayed-element? i)
         (render-content (delayed-element-content i ri) part ri)]
        [(traverse-element? i)
         (render-content (traverse-element-content i ri) part ri)]
        [(part-relative-element? i)
         (render-content (part-relative-element-content i ri) part ri)]
        [(convertible? i) (list "???")]
        [else (render-other i part ri)]))

    (define/public (render-other i part ri)
      (list i))

    ;; ----------------------------------------

    (define copied-srcs (make-hash))
    (define copied-dests (make-hash))

    (define/public (install-file fn [content #f] #:private-name? [private-name? #t])
      (if (and refer-to-existing-files
               (not content))
          (if (string? fn)
              (string->path fn)
              fn)
          (let ([normalized (normal-case-path (simplify-path (path->complete-path fn)))])
            (or (and (not content)
                     (hash-ref copied-srcs normalized #f))
                (let ([src-dir (path-only fn)]
                      [dest-dir (get-dest-directory #t)]
                      [fn (file-name-from-path fn)])
                  (let ([src-file (build-path (or src-dir (current-directory)) fn)]
                        [dest-file (build-path (or dest-dir (current-directory))
                                               (if (and private-name?
                                                        helper-file-prefix)
                                                   (string-append helper-file-prefix
                                                                  (path-element->string fn))
                                                   fn))]
                        [next-file-name (lambda (dest)
                                          (let-values ([(base name dir?) (split-path dest)])
                                            (build-path
                                             base
                                             (let ([s (path-element->string (path-replace-suffix name #""))])
                                               (let ([n (regexp-match #rx"^(.*)_([0-9]+)$" s)])
                                                 (format "~a_~a~a"
                                                         (if n (cadr n) s)
                                                         (if n (add1 (string->number (caddr n))) 2)
                                                         (let ([ext (filename-extension name)])
                                                           (if ext
                                                               (bytes-append #"." ext)
                                                               ""))))))))])
                    (let-values ([(dest-file normalized-dest-file)
                                  (let loop ([dest-file dest-file])
                                    (let ([normalized-dest-file 
                                           (normal-case-path (simplify-path (path->complete-path dest-file)))]
                                          [check-same
                                           (lambda (src)
                                             (call-with-input-file* 
                                                dest-file
                                                (lambda (dest)
                                                  (or (and (not content)
                                                           (equal? (port-file-identity src)
                                                                   (port-file-identity dest)))
                                                      (let loop ()
                                                        (let ([s (read-bytes 4096 src)]
                                                              [d (read-bytes 4096 dest)])
                                                          (and (equal? s d)
                                                               (or (eof-object? s) (loop)))))))))])
                                      (if (file-exists? dest-file)
                                          (cond
                                           [(or (and content
                                                     (check-same (open-input-bytes content)))
                                                (and (not content)
                                                     (call-with-input-file* src-file check-same)))
                                            ;; same content at that destination
                                            (values dest-file normalized-dest-file)]
                                           [(hash-ref copied-dests normalized-dest-file #f)
                                            ;; need a different file
                                            (loop (next-file-name dest-file))]
                                           [else
                                            ;; replace the file
                                            (delete-file dest-file)
                                            (values dest-file normalized-dest-file)])
                                          ;; new file
                                          (values dest-file normalized-dest-file))))])
                      (unless (file-exists? dest-file)
                        (if content
                            (call-with-output-file*
                             dest-file
                             (lambda (dest) (write-bytes content dest)))
                            (copy-file src-file dest-file)))
                      (hash-set! copied-dests normalized-dest-file #t)
                      (let ([result (path->string (file-name-from-path dest-file))])
                        (unless content
                          (hash-set! copied-srcs normalized result))
                        result))))))))

    ;; ----------------------------------------

    (define/private (do-table-of-contents part ri delta quiet depth)
      (make-table plain (generate-toc part
                                      ri
                                      (+ delta
                                         (length (collected-info-number
                                                  (part-collected-info part ri))))
                                      #t
                                      quiet
                                      depth
                                      null)))

    (define/public (table-of-contents part ri)
      (do-table-of-contents part ri -1 not +inf.0))

    (define/public (local-table-of-contents part ri style)
      (do-table-of-contents part ri -1 not (if (eq? style 'immediate-only) 
                                               1
                                               +inf.0)))

    (define/public (quiet-table-of-contents part ri)
      (do-table-of-contents part ri 1 (lambda (x) #t) +inf.0))

    (define/private (generate-toc part ri base-len skip? quiet depth prefixes)
      (let* ([number (collected-info-number (part-collected-info part ri))]
             [prefixes (if (part-tag-prefix part)
                           (cons (part-tag-prefix part) prefixes)
                           prefixes)]
             [subs
              (if (and (quiet (and (part-style? part 'quiet)
                                   (not (= base-len (sub1 (length number))))))
                       (positive? depth))
                  (apply append (map (lambda (p)
                                       (if (part-style? p 'toc-hidden)
                                           null
                                           (generate-toc p ri base-len #f quiet (sub1 depth) prefixes)))
                                     (part-parts part)))
                  null)])
        (if skip?
            subs
            (let ([l (cons
                      (list (make-paragraph
                             plain
                             (list
                              (make-element
                               'hspace
                               (list (make-string (* 2 (- (length number)
                                                          base-len))
                                                  #\space)))
                              (make-link-element
                               (if (= 1 (length number)) "toptoclink" "toclink")
                               (append
                                (format-number
                                 number
                                 (list (make-element 'hspace '(" "))))
                                (or (part-title-content part) '("???")))
                               (for/fold ([t (car (part-tags part))])
                                   ([prefix (in-list prefixes)])
                                 (convert-key prefix t))))))
                      subs)])
              (if (and (= 1 (length number))
                       (or (not (car number)) ((car number) . > . 1)))
                  (cons (list (make-paragraph
                               plain
                               (list (make-element 'hspace (list "")))))
                        l)
                  l)))))

    ;; ----------------------------------------

    (super-new)))


;; ----------------------------------------

(define-struct demand-chain ([demands #:mutable])
  #:property prop:procedure (lambda (self key ci)
                              (for/or ([demand (in-list (demand-chain-demands self))])
                                (demand key ci))))

