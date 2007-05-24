
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
             (apply
              string-append
              (map (lambda (n) 
                     (if n
                         (format "~s." n)
                         ""))
                   (reverse number)))
             sep)))

      ;; ----------------------------------------
      ;; global-info collection

      (define/public (save-info fn info)
        (let ([s (serialize info)])
          (with-output-to-file fn
            (lambda ()
              (write s))
            'truncate/replace)))

      (define/public (load-info fn info)
        (let ([ht (deserialize (with-input-from-file fn read))])
          (hash-table-for-each ht (lambda (k v)
                                    (hash-table-put! info k v))))
        info)
      
      (define/public (collect ds fns)
        (let ([ht (make-hash-table 'equal)])
          (map (lambda (d)
                 (collect-part d #f ht null))
               ds)
          ht))

      (define/public (collect-part d parent ht number)
        (let ([p-ht (make-hash-table 'equal)])
          (when (part-title-content d)
            (collect-content (part-title-content d) p-ht))
          (when (part-tag d)
            (collect-part-tag d p-ht))
          (collect-flow (part-flow d) p-ht)
          (let loop ([parts (part-parts d)]
                     [pos 1])
            (unless (null? parts)
              (let ([s (car parts)])
                (collect-part s d p-ht
                              (cons (if (unnumbered-part? s)
                                        #f
                                        pos)
                                    number))
                (loop (cdr parts)
                      (if (unnumbered-part? s) pos (add1 pos))))))
          (set-part-collected-info! d (make-collected-info
                                       number
                                       parent
                                       p-ht))
          (hash-table-for-each p-ht
                               (lambda (k v)
                                 (hash-table-put! ht k v)))))

      (define/public (collect-part-tag d ht)
        (hash-table-put! ht `(part ,(part-tag d)) (part-title-content d)))
      
      (define/public (collect-content c ht)
        (for-each (lambda (i)
                    (collect-element i ht))
                  c))

      (define/public (collect-paragraph p ht)
        (collect-content (paragraph-content p) ht))

      (define/public (collect-flow p ht)
        (for-each (lambda (p)
                    (collect-flow-element p ht))
                  (flow-paragraphs p)))

      (define/public (collect-flow-element p ht)
        (cond
         [(table? p) (collect-table p ht)]
         [(itemization? p) (collect-itemization p ht)]
         [(delayed-flow-element? p) (void)]
         [else (collect-paragraph p ht)]))
        
      (define/public (collect-table i ht)
        (for-each (lambda (d) (collect-flow d ht))
                  (apply append (table-flowss i))))
      
      (define/public (collect-itemization i ht)
        (for-each (lambda (d) (collect-flow d ht))
                  (itemization-flows i)))

      (define/public (collect-element i ht)
        (when (target-element? i)
          (collect-target-element i ht))
        (when (index-element? i)
          (collect-index-element i ht))
        (when (element? i)
          (for-each (lambda (e)
                      (collect-element e ht))
                    (element-content i))))

      (define/public (collect-target-element i ht)
        (hash-table-put! ht (target-element-tag i) i))

      (define/public (collect-index-element i ht)
        (hash-table-put! ht `(index-entry ,(index-element-tag i))
                         (list (index-element-plain-seq i)
                               (index-element-entry-seq i))))

      ;; ----------------------------------------
      ;; render methods

      (define/public (render ds fns ht)
        (map (lambda (d fn)
               (printf " [Output to ~a]\n" fn)
               (with-output-to-file fn
                 (lambda ()
                   (render-one d ht fn))
                 'truncate/replace))

             ds
             fns))
               
      (define/public (render-one d ht fn)
        (render-part d ht))

      (define/public (render-part d ht)
        (list
         (when (part-title-content d)
           (render-content (part-title-content d) d ht))
         (render-flow (part-flow d) d ht)
         (map (lambda (s) (render-part s ht))
              (part-parts d))))
      
      (define/public (render-content c part ht)
        (apply append
               (map (lambda (i)
                      (render-element i part ht))
                    c)))

      (define/public (render-paragraph p part ht)
        (render-content (paragraph-content p) part ht))

      (define/public (render-flow p part ht)
        (apply append
               (map (lambda (p)
                      (render-flow-element p part ht))
                    (flow-paragraphs p))))

      (define/public (render-flow-element p part ht)
        (cond
         [(table? p) (render-table p part ht)]
         [(itemization? p) (render-itemization p part ht)]
         [(delayed-flow-element? p) (render-flow-element
                                     ((delayed-flow-element-render p) this part ht)
                                     part ht)]
         [else (render-paragraph p part ht)]))
        
      (define/public (render-table i part ht)
        (map (lambda (d) (render-flow d part ht)) 
             (apply append (table-flowss i))))
      
      (define/public (render-itemization i part ht)
        (map (lambda (d) (render-flow d part ht))
             (itemization-flows i)))
      
      (define/public (render-element i part ht)
        (cond
         [(and (link-element? i)
               (null? (element-content i)))
          (let ([v (hash-table-get ht (link-element-tag i) #f)])
            (if v
                (render-content v part ht)
                (render-content (list "[missing]") part ht)))]
         [(element? i)
          (render-content (element-content i) part ht)]
         [(delayed-element? i)
          (render-content (force-delayed-element i this part ht) part ht)]
         [else
          (render-other i part ht)]))

      (define/public (render-other i part ht)
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

      (define/public (table-of-contents part ht)
        (make-table #f (cdr (render-toc part))))

      (define/private (render-toc part)
        (let ([number (collected-info-number (part-collected-info part))])
          (let ([l (cons
                    (list (make-flow
                           (list
                            (make-paragraph
                             (list
                              (make-element 'hspace (list (make-string (* 2 (length number)) #\space)))
                              (make-link-element (if (= 1 (length number))
                                                     "toptoclink"
                                                     "toclink")
                                                 (append
                                                  (format-number number 
                                                                 (list
                                                                  (make-element 'hspace '(" "))))
                                                  (part-title-content part))
                                                 `(part ,(part-tag part))))))))
                    (apply
                     append
                     (map (lambda (p) (render-toc p)) (part-parts part))))])
            (if (and (= 1 (length number))
                     (or (not (car number))
                         ((car number) . > . 1)))
                (cons (list (make-flow (list (make-paragraph (list
                                                              (make-element 'hspace (list " ")))))))
                      l)
                l))))

      ;; ----------------------------------------
      
      (super-new))))
