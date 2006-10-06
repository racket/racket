
(module snipclass mzscheme
  (require (lib "class.ss")
           (lib "unitsig.ss")
           (lib "mred.ss" "mred")
           (lib "match.ss")
           (lib "string.ss")
           (lib "list.ss")
           "interfaces.ss")
  (provide snipclass@)
  
  (define snipclass@
    (unit/sig snipclass^
      (import snip^)
      ;; COPIED AND MODIFIED from mrlib/syntax-browser.ss
      (define syntax-snipclass%
        (class snip-class%
          (define/override (read stream)
            (let ([str (send stream get-bytes)])
              (make-object syntax-snip% 
                (unmarshall-syntax (read-from-string (bytes->string/utf-8 str))))))
          (super-instantiate ())))
      
      (define snip-class (make-object syntax-snipclass%))
      (send snip-class set-version 2)
      (send snip-class set-classname
            (format "~s" '(lib "implementation.ss" "macro-debugger" "syntax-browser")))
      (send (get-the-snip-class-list) add snip-class)
      ))
  
  (define (unmarshall-syntax stx)
    (match stx
      [`(syntax
         (source ,src)
         (source-module ,source-module) ;; marshalling
         (position ,pos)
         (line ,line)
         (column ,col)
         (span ,span)
         (original? ,original?)
         (properties ,@(properties ...))
         (contents ,contents))
        (foldl
         add-properties
         (datum->syntax-object
          #'here ;; ack
          (unmarshall-object contents)
          (list (unmarshall-object src)
                line
                col
                pos
                span))
         properties)]
      [else #'unknown-syntax-object]))
  
  ;; add-properties : syntax any -> syntax
  (define (add-properties prop-spec stx)
    (match prop-spec
      [`(,(and sym (? symbol?))
          ,prop)
        (syntax-property stx sym (unmarshall-object prop))]
      [else stx]))
  
  (define (unmarshall-object obj)
    (let ([unknown (λ () (string->symbol (format "unknown: ~s" obj)))])
      (if (and (pair? obj)
               (symbol? (car obj)))
          (case (car obj)
            [(pair) 
             (if (pair? (cdr obj))
                 (let ([raw-obj (cadr obj)])
                   (if (pair? raw-obj)
                       (cons (unmarshall-object (car raw-obj))
                             (unmarshall-object (cdr raw-obj)))
                       (unknown)))
                 (unknown))]
            [(other) 
             (if (pair? (cdr obj))
                 (cadr obj)
                 (unknown))]
            [(syntax) (unmarshall-syntax obj)]
            [else (unknown)])
          (unknown))))
  )
