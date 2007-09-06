
(module base-render mzscheme
  (require "struct.ss"
           (lib "class.ss")
           (lib "serialize.ss")
           (lib "file.ss"))

  (provide render%)

  (define render%
    (class object%

      (init-field dest-dir)

      (define/public (get-dest-directory)
        dest-dir)

      (define/public (get-substitutions) null)
      
      (define/public (get-suffix) #".txt")

      (define/public (format-number number sep)
        (if (or (null? number)
                (andmap not number))
            null
            (cons
             (let ([s (apply
                       string-append
                       (map (lambda (n) 
                              (if n
                                  (format "~s." n)
                                  ""))
                            (reverse number)))])
               (substring s 0 (sub1 (string-length s))))
             sep)))

      (define/public (strip-aux content)
        (cond
         [(null? content) null]
         [(aux-element? (car content))
          (strip-aux (cdr content))]
         [else (cons (car content)
                     (strip-aux (cdr content)))]))

      ;; ----------------------------------------
      ;; marshal info

      (define/public (get-serialize-version)
        1)
      
      (define/public (serialize-info ri)
        (parameterize ([current-serialize-resolve-info ri])
          (serialize (collect-info-ht (resolve-info-ci ri)))))

      (define/public (deserialize-info v ci)
        (let ([ht (deserialize v)]
              [in-ht (collect-info-ext-ht ci)])
          (hash-table-for-each ht (lambda (k v)
                                    (hash-table-put! in-ht k v)))))
      (define/public (get-defined ci)
        (hash-table-map (collect-info-ht ci) (lambda (k v) k)))

      (define/public (get-undefined ri)
        (hash-table-map (resolve-info-undef ri) (lambda (k v) k)))
      
      ;; ----------------------------------------
      ;; global-info collection

      (define/public (collect ds fns)
        (let ([ci (make-collect-info (make-hash-table 'equal)
                                     (make-hash-table 'equal)
                                     (make-hash-table)
                                     (make-hash-table)
                                     "")])
          (start-collect ds fns ci)
          ci))

      (define/public (start-collect ds fns ci)
        (map (lambda (d)
               (collect-part d #f ci null))
             ds))

      (define/public (collect-part d parent ci number)
        (let ([p-ci (make-collect-info (make-hash-table 'equal)
                                       (collect-info-ext-ht ci)
                                       (collect-info-parts ci)
                                       (collect-info-tags ci)
                                       (if (part-tag-prefix d)
                                           (string-append (collect-info-gen-prefix ci)
                                                          (part-tag-prefix d)
                                                          ":")
                                           (collect-info-gen-prefix ci)))])
          (when (part-title-content d)
            (collect-content (part-title-content d) p-ci))
          (collect-part-tags d p-ci number)
          (collect-content (part-to-collect d) p-ci)
          (collect-flow (part-flow d) p-ci)
          (let loop ([parts (part-parts d)]
                     [pos 1])
            (unless (null? parts)
              (let ([s (car parts)])
                (collect-part s d p-ci
                              (cons (if (unnumbered-part? s)
                                        #f
                                        pos)
                                    number))
                (loop (cdr parts)
                      (if (unnumbered-part? s) pos (add1 pos))))))
          (hash-table-put! (collect-info-parts ci)
                           d 
                           (make-collected-info
                            number
                            parent
                            (collect-info-ht p-ci)))
          (let ([prefix (part-tag-prefix d)])
            (hash-table-for-each (collect-info-ht p-ci)
                                 (lambda (k v)
                                   (when (cadr k)
                                     (hash-table-put! (collect-info-ht ci)
                                                      (if prefix
                                                          (convert-key prefix k)
                                                          k)
                                                      v)))))))

      (define/private (convert-key prefix k)
        (case (car k)
          [(part tech)
           (if (string? (cadr k))
               (list (car k)
                     (string-append prefix
                                    ":"
                                    (cadr k)))
               k)]
          [(index-entry)
           (let ([v (convert-key prefix (cadr k))])
             (if (eq? v (cadr k))
                 k
                 (list 'index-entry v)))]
          [else k]))

      (define/public (collect-part-tags d ci number)
        (for-each (lambda (t)
                    (hash-table-put! (collect-info-ht ci)
                                     (generate-tag t ci)
                                     (list (or (part-title-content d) '("???"))
                                           number)))
                  (part-tags d)))
      
      (define/public (collect-content c ci)
        (for-each (lambda (i)
                    (collect-element i ci))
                  c))

      (define/public (collect-paragraph p ci)
        (collect-content (paragraph-content p) ci))

      (define/public (collect-flow p ci)
        (for-each (lambda (p)
                    (collect-flow-element p ci))
                  (flow-paragraphs p)))

      (define/public (collect-flow-element p ci)
        (cond
         [(table? p) (collect-table p ci)]
         [(itemization? p) (collect-itemization p ci)]
         [(blockquote? p) (collect-blockquote p ci)]
         [(delayed-flow-element? p) (void)]
         [else (collect-paragraph p ci)]))
        
      (define/public (collect-table i ci)
        (for-each (lambda (d) (when (flow? d)
                                (collect-flow d ci)))
                  (apply append (table-flowss i))))
      
      (define/public (collect-itemization i ci)
        (for-each (lambda (d) (collect-flow d ci))
                  (itemization-flows i)))

      (define/public (collect-blockquote i ci)
        (for-each (lambda (d) (collect-flow-element d ci))
                  (blockquote-paragraphs i)))

      (define/public (collect-element i ci)
        (when (target-element? i)
          (collect-target-element i ci))
        (when (index-element? i)
          (collect-index-element i ci))
        (when (collect-element? i)
          ((collect-element-collect i) ci))
        (when (element? i)
          (for-each (lambda (e)
                      (collect-element e ci))
                    (element-content i))))

      (define/public (collect-target-element i ci)
        (collect-put! ci
                      (generate-tag (target-element-tag i) ci)
                      (list i)))

      (define/public (collect-index-element i ci)
        (collect-put! ci
                      `(index-entry ,(generate-tag (index-element-tag i) ci))
                      (list (index-element-plain-seq i)
                            (index-element-entry-seq i))))

      ;; ----------------------------------------
      ;; global-info resolution

      (define/public (resolve ds fns ci)
        (let ([ri (make-resolve-info ci
                                     (make-hash-table)
                                     (make-hash-table 'equal))])
          (start-resolve ds fns ri)
          ri))

      (define/public (start-resolve ds fns ri)
        (map (lambda (d)
               (resolve-part d ri))
             ds))

      (define/public (resolve-part d ri)
        (when (part-title-content d)
          (resolve-content (part-title-content d) d ri))
        (resolve-flow (part-flow d) d ri)
        (for-each (lambda (p)
                    (resolve-part p ri))
                  (part-parts d)))
      
      (define/public (resolve-content c d ri)
        (for-each (lambda (i)
                    (resolve-element i d ri))
                  c))

      (define/public (resolve-paragraph p d ri)
        (resolve-content (paragraph-content p) d ri))

      (define/public (resolve-flow p d ri)
        (for-each (lambda (p)
                    (resolve-flow-element p d ri))
                  (flow-paragraphs p)))

      (define/public (resolve-flow-element p d ri)
        (cond
         [(table? p) (resolve-table p d ri)]
         [(itemization? p) (resolve-itemization p d ri)]
         [(blockquote? p) (resolve-blockquote p d ri)]
         [(delayed-flow-element? p) 
          (let ([v ((delayed-flow-element-resolve p) this d ri)])
            (hash-table-put! (resolve-info-delays ri) p v)
            (resolve-flow-element v d ri))]
         [else (resolve-paragraph p d ri)]))
        
      (define/public (resolve-table i d ri)
        (for-each (lambda (f) (when (flow? f)
                                (resolve-flow f d ri)))
                  (apply append (table-flowss i))))
      
      (define/public (resolve-itemization i d ri)
        (for-each (lambda (f) (resolve-flow f d ri))
                  (itemization-flows i)))

      (define/public (resolve-blockquote i d ri)
        (for-each (lambda (f) (resolve-flow-element f d ri))
                  (blockquote-paragraphs i)))

      (define/public (resolve-element i d ri)
        (cond
         [(delayed-element? i)
          (resolve-content (or (hash-table-get (resolve-info-delays ri)
                                               i
                                               #f)
                               (let ([v ((delayed-element-resolve i) this d ri)])
                                 (hash-table-put! (resolve-info-delays ri)
                                                  i
                                                  v)
                                 v))
                           d ri)]
         [(element? i)
          (cond
           [(link-element? i)
            (resolve-get d ri (link-element-tag i))])
          (for-each (lambda (e)
                      (resolve-element e d ri))
                    (element-content i))]))

      ;; ----------------------------------------
      ;; render methods

      (define/public (render ds fns ri)
        (map (lambda (d fn)
               (printf " [Output to ~a]\n" fn)
               (with-output-to-file fn
                 (lambda ()
                   (render-one d ri fn))
                 'truncate/replace))
             ds
             fns))
               
      (define/public (render-one d ri fn)
        (render-part d ri))

      (define/public (render-part d ri)
        (list
         (when (part-title-content d)
           (render-content (part-title-content d) d ri))
         (render-flow (part-flow d) d ri)
         (map (lambda (s) (render-part s ri))
              (part-parts d))))
      
      (define/public (render-content c part ri)
        (apply append
               (map (lambda (i)
                      (render-element i part ri))
                    c)))

      (define/public (render-paragraph p part ri)
        (render-content (paragraph-content p) part ri))

      (define/public (render-flow p part ri)
        (apply append
               (map (lambda (p)
                      (render-flow-element p part ri))
                    (flow-paragraphs p))))

      (define/public (render-flow-element p part ri)
        (cond
         [(table? p) (if (auxiliary-table? p)
                         (render-auxiliary-table p part ri)
                         (render-table p part ri))]
         [(itemization? p) (render-itemization p part ri)]
         [(blockquote? p) (render-blockquote p part ri)]
         [(delayed-flow-element? p) 
          (render-flow-element (delayed-flow-element-flow-elements p ri) part ri)]
         [else (render-paragraph p part ri)]))
        
      (define/public (render-auxiliary-table i part ri)
        null)

      (define/public (render-table i part ri)
        (map (lambda (d) (if (flow? i)
                             (render-flow d part ri)
                             null))
             (apply append (table-flowss i))))
      
      (define/public (render-itemization i part ri)
        (map (lambda (d) (render-flow d part ri))
             (itemization-flows i)))
      
      (define/public (render-blockquote i part ri)
        (map (lambda (d) (render-flow-element d part ri))
             (blockquote-paragraphs i)))
      
      (define/public (render-element i part ri)
        (cond
         [(and (link-element? i)
               (null? (element-content i)))
          (let ([v (resolve-get part ri (link-element-tag i))])
            (if v
                (render-content (strip-aux (car v)) part ri)
                (render-content (list "[missing]") part ri)))]
         [(element? i)
          (render-content (element-content i) part ri)]
         [(delayed-element? i)
          (render-content (delayed-element-content i ri) part ri)]
         [else
          (render-other i part ri)]))

      (define/public (render-other i part ri)
        (list i))

      ;; ----------------------------------------

      (define/public (install-file fn)
        (let ([src-dir (path-only fn)]
              [dest-dir (get-dest-directory)]
              [fn (file-name-from-path fn)])
          (let ([src-file (build-path (or src-dir (current-directory))
                                      fn)]
                [dest-file (build-path (or dest-dir (current-directory))
                                       fn)])
            (unless (and (file-exists? dest-file)
                         (call-with-input-file* 
                          src-file
                          (lambda (src)
                            (call-with-input-file* 
                             dest-file
                             (lambda (dest)
                               (or (equal? (port-file-identity src)
                                           (port-file-identity dest))
                                   (let loop ()
                                     (let ([s (read-bytes 4096 src)]
                                           [d (read-bytes 4096 dest)])
                                       (and (equal? s d)
                                            (if (eof-object? s)
                                                #t
                                                (loop)))))))))))
              (when (file-exists? dest-file) (delete-file dest-file))
              (copy-file src-file dest-file))
            (path->string fn))))

      ;; ----------------------------------------

      (define/private (do-table-of-contents part ri delta quiet)
        (make-table #f (generate-toc part
                                     ri
                                     (+ delta
                                        (length (collected-info-number
                                                 (part-collected-info part ri))))
                                     #t
                                     quiet)))

      (define/public (table-of-contents part ri)
        (do-table-of-contents part ri -1 not))

      (define/public (local-table-of-contents part ri)
        (table-of-contents part ri))

      (define/public (quiet-table-of-contents part ri)
        (do-table-of-contents part ri 1 (lambda (x) #t)))

      (define/private (generate-toc part ri base-len skip? quiet)
        (let ([number (collected-info-number (part-collected-info part ri))])
          (let ([subs 
                 (if (quiet (and (part-style? part 'quiet)
                                 (not (= base-len (sub1 (length number))))))
                     (apply
                      append
                      (map (lambda (p) (generate-toc p ri base-len #f quiet)) (part-parts part)))
                     null)])
            (if skip?
                subs
                (let ([l (cons
                          (list (make-flow
                                 (list
                                  (make-paragraph
                                   (list
                                    (make-element 'hspace (list (make-string (* 2 (- (length number) base-len)) #\space)))
                                    (make-link-element (if (= 1 (length number))
                                                           "toptoclink"
                                                           "toclink")
                                                       (append
                                                        (format-number number 
                                                                       (list
                                                                        (make-element 'hspace '(" "))))
                                                        (or (part-title-content part) '("???")))
                                                       (car (part-tags part))))))))
                          subs)])
                  (if (and (= 1 (length number))
                           (or (not (car number))
                               ((car number) . > . 1)))
                      (cons (list (make-flow (list (make-paragraph (list
                                                                    (make-element 'hspace (list "")))))))
                            l)
                      l))))))
      
      ;; ----------------------------------------
      
      (super-new))))
