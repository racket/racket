#lang racket/base

(require xml/xml
         syntax/readerr
         racket/gui
         "syntax-property.rkt")

(provide xml-read-special
         xml-snip<%>
         scheme-read-special
         scheme-snip<%>)

(define (scheme-read-special snip source line col pos)
  (let ([text (send snip get-editor)]
        [splice? (send snip get-splice?)])
    (when (= 0 (send text last-position))
      (raise-read-error 
       (if splice?
           "read: bad syntax: empty scheme splice box"
           "read: bad syntax: empty scheme box")
       source line col pos 1))
    (let* ([source-name (get-source-name text)]
           [stx (read-syntax source-name
                             (open-input-text-editor text 0 'end values source-name))])
      (when (eof-object? stx)
        (raise-read-error
         (if splice?
             "read: bad syntax: empty scheme splice box"
             "read: bad syntax: empty scheme box")
         source-name 1 1 1 (send text last-position)))
      stx)))

(define (get-source-name text)
  (cond
    [(method-in-interface? 'get-port-name (object-interface text))
     (send text get-port-name)]
    [else
     (send text get-filename)]))

(define (xml-read-special eliminate-whitespace-in-empty-tags? snip source line col pos)
  (let ([editor (send snip get-editor)]
        [old-locked #f])
    (when (= 0 (send editor last-position))
      (raise-read-error "read: bad syntax: empty xml box"
                        source line col pos 1))
    (dynamic-wind
     (lambda () 
       (set! old-locked (send editor is-locked?))
       (send editor lock #t))
     (lambda ()
       (let* ([source-name (get-source-name editor)]
              [port (open-input-text-editor editor 0 'end (xml-snip-filter editor) source-name)]
              [xml (parameterize ([permissive-xexprs #t]) (read-xml port))]
              [xexpr (parameterize ([permissive-xexprs #t]) (xml->xexpr (document-element xml)))]
              [clean-xexpr (if eliminate-whitespace-in-empty-tags?
                               (eliminate-whitespace-in-empty-tags xexpr)
                               xexpr)]
              [expd-xexpr (expand-embedded clean-xexpr)]
              [qq-body (datum->syntax #'here expd-xexpr (list editor #f #f #f #f))])
         (with-syntax ([qq-body qq-body])
           (stepper-syntax-property (syntax (quasiquote qq-body))
                                    'stepper-xml-hint
                                    'from-xml-box))))
     (lambda () (send editor lock old-locked)))))

(define ((xml-snip-filter text) s)
  (cond
    [(is-a? s scheme-snip<%>)
     (let* ([position (send text get-snip-position s)]
            [line (send text position-paragraph position)]
            [col (- position (send text paragraph-start-position line))])
       (make-wrapped s text line col position))]
    [else s]))

(define scheme-snip<%>
  (interface ()
    get-splice?))

(define xml-snip<%>
  (interface ()))

;; eliminate-whitespace-in-empty-tags : xexpr -> xexpr
(define (eliminate-whitespace-in-empty-tags xexpr)
  (cond
    [(and (pair? xexpr)
          (symbol? (car xexpr)))
     (list* (car xexpr)
            (cadr xexpr)
            (map eliminate-whitespace-in-empty-tags
                 (eliminate-whitespace-in-list (cddr xexpr))))]
    [else xexpr]))

;; wrapped = (make-wraped sexp text number number number)
(define-struct wrapped (snip text line col pos))

;; expand-embedded : xexpr -> xexpr
;; constructs a new xexpr that has the embedded snips expanded 
;; and wrapped with unquotes
;; CRUCIAL INVARIANT: an expression must not receive both 'from-xml-box and 'from-scheme/splice-box tags.
(define (expand-embedded _xexpr)
  (let loop ([xexpr _xexpr])
    (cond
      [(pair? xexpr)
       (cons (loop (car xexpr))
             (loop (cdr xexpr)))]
      [(wrapped? xexpr)
       (let* ([snip (wrapped-snip xexpr)]
              [text (wrapped-text xexpr)]
              [pos (wrapped-pos xexpr)]
              [line (wrapped-line xexpr)]
              [col (wrapped-col xexpr)]
              [raw-stxs (list (send snip read-special text line col pos))])
         (with-syntax ([(stxs ...) raw-stxs])
           (if (and (is-a? snip scheme-snip<%>)
                    (send snip get-splice?))
               (with-syntax ([err (syntax/loc 
                                      (car (last-pair raw-stxs))
                                    (error 'scheme-splice-box "expected a list, found: ~e" lst))])
                 #`,@#,(stepper-syntax-property #`(let ([lst (begin stxs ...)])
                                                    (if (list? lst)
                                                        lst
                                                        err))
                                                'stepper-xml-hint
                                                'from-splice-box))
               #`,#,(stepper-syntax-property #`(begin stxs ...) 
                                             'stepper-xml-hint
                                             'from-scheme-box))))]
      [else xexpr])))

;; eliminate-whitespace-in-list (listof xexpr) -> (listof xexpr)
;; if each string in xexprs is a whitespace string, remove all strings
;; otherwise, return input.
(define (eliminate-whitespace-in-list xexprs)
  (cond
    [(andmap (lambda (x) (or (not (string? x))
                             (string-whitespace? x)))
             xexprs)
     (filter (lambda (x) (not (string? x))) xexprs)]
    [else xexprs]))

;; string-whitespace? : string -> boolean
;; return #t if the input string consists entirely of whitespace
(define (string-whitespace? str)
  (let loop ([i (string-length str)])
    (cond
      [(zero? i) #t]
      [(char-whitespace? (string-ref str (- i 1)))
       (loop (- i 1))]
      [else #f])))


;; transformable? : snip -> boolean
;; deteremines if a snip can be expanded here
(define (transformable? snip)
  (or (is-a? snip xml-snip<%>)
      (is-a? snip scheme-snip<%>)))
