
;; FIXME: Need to disable printing of structs with custom-write property

(module pretty-printer mzscheme
  (require (lib "list.ss")
           (lib "class.ss")
           (lib "pretty.ss")
           (lib "mred.ss" "mred")
           "pretty-range.ss"
           "pretty-helper.ss"
           "interfaces.ss"
           "params.ss")
  (provide syntax-pp%
           (struct range (obj start end)))

  ;; syntax-pp%
  ;; Pretty printer for syntax objects.
  (define syntax-pp%
    (class* object% (syntax-pp<%>)
      (init-field main-stx)
      (init-field typesetter)
      (init-field (primary-partition #f))
      (init-field (columns (current-default-columns)))
      
      (unless (syntax? main-stx)
        (error 'syntax-snip% "got non-syntax object: ~s" main-stx))
      
      (define datum #f)
      (define ht:flat=>stx #f)
      (define ht:stx=>flat #f)
      (define identifier-list null)
      (define -range #f)
      
      (define/public (get-range) -range)
      (define/public (get-identifier-list) identifier-list)
      (define/public (flat=>stx obj)
        (hash-table-get ht:flat=>stx obj))
      (define/public (stx=>flat obj)
        (hash-table-get ht:stx=>flat obj))
      
      (define/public (pretty-print-syntax)
        (define range (new ranges%))
        (define (pp-pre-hook obj port)
          (send range set-start obj (send typesetter get-current-position)))
        (define (pp-post-hook obj port)
          (let ([start (send range get-start obj)]
                [end (send typesetter get-current-position)])
            (when start
              (send range add-range
                    (flat=>stx obj)
                    (cons start end)))))
        (define (pp-size-hook obj display-like? port)
          (cond [(is-a? obj editor-snip%)
                 columns]
                [(syntax-dummy? obj)
                 (let ((ostring (open-output-string)))
                   ((if display-like? display write) (syntax-dummy-val obj) ostring)
                   (string-length (get-output-string ostring)))]
                [else #f]))
        (define (pp-print-hook obj display-like? port)
          (cond [(syntax-dummy? obj)
                 ((if display-like? display write) (syntax-dummy-val obj) port)]
                [(is-a? obj editor-snip%)
                 (write-special obj port)]
                [else 
                 (error 'pretty-print-hook "unexpected special value: ~e" obj)]))
        (define (pp-extend-style-table)
          (let* ([ids identifier-list]
                 [syms (map (lambda (x) (stx=>flat x)) ids)]
                 [like-syms (map syntax-e ids)])
            (pretty-print-extend-style-table (pp-better-style-table)
                                             syms
                                             like-syms)))
        (define (pp-better-style-table)
          (pretty-print-extend-style-table (pretty-print-current-style-table)
                                           (map car extended-style-list)
                                           (map cdr extended-style-list)))
        
        (parameterize 
            ([pretty-print-pre-print-hook pp-pre-hook]
             [pretty-print-post-print-hook pp-post-hook]
             [pretty-print-size-hook pp-size-hook]
             [pretty-print-print-hook pp-print-hook]
             [pretty-print-columns columns]
             [pretty-print-current-style-table (pp-extend-style-table)]
             ;; Printing parameters (mzscheme manual 7.9.1.4)
             [print-unreadable #t]
             [print-graph #f]
             [print-struct #f]
             [print-box #t]
             [print-vector-length #t]
             [print-hash-table #f]
             [print-honu #f])
          (pretty-print datum (send typesetter get-output-port))
          (set! -range range)))

      ;; recompute-tables : -> void
      (define/private (recompute-tables)
        (set!-values (datum ht:flat=>stx ht:stx=>flat)
                     (syntax->datum/tables main-stx primary-partition 12 #f))
        (set! identifier-list 
              (filter identifier? (hash-table-map ht:stx=>flat (lambda (k v) k)))))

      ;; Initialization
      (recompute-tables)
      (super-new)))

  (define extended-style-list
    '((define-values          . define)
      (define-syntaxes        . define-syntax)))
  )
