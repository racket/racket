
(module xml-snip-helpers mzscheme
  (require (lib "xml.ss" "xml")
           (lib "readerr.ss" "syntax")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss"))
  
  (provide xml-read-special
           xml-snip<%>
           scheme-read-special
           scheme-snip<%>)
  
  (define (scheme-read-special snip file line col pos)
    (let ([text (send snip get-editor)]
          [splice? (send snip get-splice?)])
      (when (= 0 (send text last-position))
        (let-values ([(txt line col pos) (find-position-in-outer snip)])
          (raise-read-error 
           (if splice?
               "read: bad syntax: empty scheme splice box"
               "read: bad syntax: empty scheme box")
           txt line col pos 1)))
      (let ([stx (read-syntax
                  text
                  (open-input-text-editor text 0 (send text last-position)))])
        (when (eof-object? stx)
          (raise-read-error
           (if splice?
               "read: bad syntax: empty scheme splice box"
               "read: bad syntax: empty scheme box")
           text 1 1 1 (send text last-position)))
	stx)))
  
  (define (xml-read-special eliminate-whitespace-in-empty-tags? snip file line col pos)
    (let ([editor (send snip get-editor)]
          [old-locked #f])
      (when (= 0 (send editor last-position))
        (let-values ([(txt line col pos) (find-position-in-outer snip)])
          (raise-read-error "read: bad syntax: empty xml box"
                            txt line col pos 1)))
      (dynamic-wind
       (lambda () 
         (set! old-locked (send editor is-locked?))
         (send editor lock #t))
       (lambda ()
         (let* ([port (open-input-text-editor editor 0 'end (xml-snip-filter editor))]
                [xml (read-xml port)]
                [xexpr (xml->xexpr (document-element xml))]
                [clean-xexpr (if eliminate-whitespace-in-empty-tags?
                                 (eliminate-whitespace-in-empty-tags xexpr)
                                 xexpr)]
                [expd-xexpr (expand-embedded clean-xexpr)]
                [qq-body (datum->syntax-object #'here expd-xexpr (list editor #f #f #f #f))])
           (with-syntax ([qq-body qq-body])
             (syntax-property (syntax (quasiquote qq-body))
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
                   #`,@#,(syntax-property #`(let ([lst (begin stxs ...)])
                                              (if (list? lst)
                                                  lst
                                                  err))
                                          'stepper-xml-hint
                                          'from-splice-box))
                 #`,#,(syntax-property #`(begin stxs ...) 
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
  
  ;; find-in-position-in-outer : 
  ;;    editor-snip -> (values (union #f text%) (union #f number) (union #f number) (union #f number))
  (define (find-position-in-outer editor-snip)
    (let/ec k
      (let ([fail (lambda () (k #f #f #f #f))])
        (let ([admin (send editor-snip get-admin)])
          (unless admin (fail))
          (let ([outer-editor (send admin get-editor)])
            (unless (is-a? outer-editor text%) (fail))
            (let ([pos (send outer-editor get-snip-position editor-snip)])
              (unless pos (fail))
              (let* ([line (send outer-editor position-paragraph pos)]
                     [line-start (send outer-editor paragraph-start-position line)])
                (values outer-editor (+ line 1) (+ (- pos line-start) 1) (+ pos 1))))))))))