#lang scheme/gui

(define (find sym l)
  (cond
   [(null? l) null]
   [(and (pair? (car l))
         (eq? sym (caar l)))
    (car l)]
   [else (find sym (cdr l))]))

(define (add-method s table)
  (let* ([s (if (keyword? (cadr s))
                (cddr s)
                s)]
         [name (caadr s)]
         [args (map cadr (cdadr s))])
    (cons (cons name args)
          table)))

(define (read-methods path kind table)
  (let ([s (call-with-input-file* path (lambda (in)
                                         (parameterize ([read-accept-reader #t])
                                           (read in))))])
    (for/fold ([table table])
        ([s (in-list (find kind s))])
      (if (pair? s)
          (cond
           [(eq? (car s) 'defmethod)
            (add-method s table)]
           [else table])
          table))))

(define editor-methods
  (read-methods (build-path (collection-path "scribblings" "gui")
                            "editor-intf.scrbl")
                'definterface/title
                null))

(define (delete l l2)
  (if (null? l)
      l2
      (delete (cdr l) (filter (lambda (p) (not (eq? (car l) (car p)))) l2))))

(define text-methods
  (list->vector
   (delete
    '(read-header-from-file read-footer-from-file read-from-file
                            end-write-header-footer-to-file)
    (read-methods (build-path (collection-path "scribblings" "gui")
                              "text-class.scrbl")
                  'defclass/title
                  (delete '(do-paste-x-selection do-paste do-copy) editor-methods)))))

;; ----------------------------------------

(define bm-dc
  (let ([bm (make-object bitmap% 10 10)])
    (make-object bitmap-dc% bm)))
(define frame
  (new frame% [label "Test"]))
(define canvas
  (new editor-canvas% [parent frame]))

(define (generate-args contract-expr)
  (if (pair? contract-expr)
      (case (car contract-expr)
        [(or/c one-of/c) (generate-args
                          (list-ref
                           (cdr contract-expr)
                           (random (length (cdr contract-expr)))))]
        [(and/c)
         (cond
          [(equal? contract-expr '(and/c exact? integer?))
           (generate-args 'exact-integer?)]
          [(equal? contract-expr '(and/c real? (not/c negative?)))
           (random-elem '#(0.0 1.0 100.0 1000.0))]
          [else (error "unknown" contract-expr)])]
        [(box/c) `(box ,(generate-args (cadr contract-expr)))]
        [(listof) (case (random 3)
                    [(0) 'null]
                    [(1) (list 'list
                               (generate-args (cadr contract-expr)))]
                    [(2) (list 'list
                               (generate-args (cadr contract-expr))
                               (generate-args (cadr contract-expr)))])]
        [(quote)
         `(quote ,(cadr contract-expr))]
        [(is-a?/c)
         (case (cadr contract-expr)
           [(editor-stream-out%)
            (make-object editor-stream-out% (make-object editor-stream-out-bytes-base%))]
           [(editor-stream-in%)
            (make-object editor-stream-in% (make-object editor-stream-in-bytes-base% #""))]
           [(snip%)
            (let ([s (make-object string-snip%)])
              (send s insert "hi" 2)
              s)]
           [(mouse-event%)
            (make-object mouse-event% 'motion)]
           [(key-event%)
            (make-object key-event%)]
           [(editor-data%) (new editor-data%)]
           [(text%) (new text%)]
           [(pasteboard%) (new pasteboard%)]
           [(cursor%) (make-object cursor% 'arrow)]
           [(style-delta%) (new style-delta%)]
           [(style-list%) (new style-list%)]
           [(style<%>) (send (new style-list%) basic-style)]
           [(editor-canvas%) canvas]
           [(frame% dialog%) frame]
           [(dc<%>) bm-dc]
           [(editor-admin%) (send t get-admin)]
           [(bitmap%) (make-object bitmap% 10 10)]
           [(color%) (new color%)]
           [(keymap%) (new keymap%)]
           [(editor-wordbreak-map%) (new editor-wordbreak-map%)]
           [else (error "unknown" contract-expr)])]
        [(->) void]
        [else (error "unknown" contract-expr)])
      (case contract-expr
        [(any/c) #f]
        [(path?) (string->path "/tmp/foo")]
        [(path-string?) "/tmp/foo"]
        [(input-port?) (open-input-bytes #"")]
        [(output-port?) (open-output-bytes)]
        [(real?)
         (random-elem '#(0.0 1.0 -1.0 100.0 -100.0))]
        [(exact-nonnegative-integer?)
         (random-elem '#(0 1 2 10 100 1000))]
        [(exact-integer?)
         (random-elem '#(0 1 -1 2 10 -10 100 1000))]
        [(string?)
         (random-elem '#("a" "hello" ""))]
        [(#f) #f]
        [(#t) #t]
        [else (error "unknown" contract-expr)])))

(define (random-elem v)
  (vector-ref v (random (vector-length v))))

;; ----------------------------------------

(define t (new text%))

; (send t copy-self)
; (send t begin-write-header-footer-to-file (generate-args '(is-a?/c editor-stream-out%)) "" (box 0))
; is-printing?
; #f for set-keymap
; seqcontract print
; undo error
; get-character
; blink-caret & no admin
; move-position & no admin

(define-namespace-anchor a)

(let ([n (abs (current-milliseconds))])
  (printf "~s\n" n)
  (random-seed n))

(parameterize ([current-namespace (namespace-anchor->namespace a)])
  (let loop ()
    (let ([m (random-elem text-methods)])
      (let ([name (car m)]
            [args (map generate-args (cdr m))])
        (printf "Call ~s\n" (cons name args))
        (eval `(send ,t ,(car m) ,@args))
        (loop)))))

