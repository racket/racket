(module xml mzscheme
  (require mzlib/class
           xml/xml
           mzlib/list
           "wxme.rkt"
           "editor.rkt"
           "private/readable-editor.rkt")

  (provide reader
           xml-editor%)

  (define xml-editor% (class readable-editor% (super-new)))

  (define reader
    (new (class editor-reader%
           (inherit read-editor-snip)
           (define/override (read-snip text? vers stream)
             (let ([elim-whitespace? (zero? (send stream read-integer "elim-whitespace?"))])
               (read-editor-snip text? vers stream elim-whitespace? xml-editor%)))

           (define/override (generate-special editor src line col pos)
             (parameterize ([permissive-xexprs #t])
               (let* ([port (send editor get-content-port)]
                      [xml (read-xml port)]
                      [xexpr (xml->xexpr (document-element xml))]
                      [clean-xexpr (if (send editor get-data)
                                       (eliminate-whitespace-in-empty-tags xexpr)
                                       xexpr)])
                 (list 'quasiquote clean-xexpr))))

           (super-new))))

  ;; FIXME! Copied from xml-snip-helpers.rkt verbatim
  (define (eliminate-whitespace-in-empty-tags xexpr)
    (cond
     [(and (pair? xexpr)
           (symbol? (car xexpr)))
      (list* (car xexpr)
             (cadr xexpr)
             (map eliminate-whitespace-in-empty-tags
                  (eliminate-whitespace-in-list (cddr xexpr))))]
     [else xexpr]))
  (define (eliminate-whitespace-in-list xexprs)
    (cond
     [(andmap (lambda (x) (or (not (string? x))
                              (string-whitespace? x)))
              xexprs)
      (filter (lambda (x) (not (string? x))) xexprs)]
     [else xexprs]))
  (define (string-whitespace? str)
    (let loop ([i (string-length str)])
      (cond
       [(zero? i) #t]
       [(char-whitespace? (string-ref str (- i 1)))
        (loop (- i 1))]
       [else #f]))))
