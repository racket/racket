(module scribble mzscheme
  (require (prefix a: "reader.ss") (lib "kw.ss") (lib "list.ss"))
  (provide (all-from-except mzscheme read read-syntax define lambda)
           (rename a:read read) (rename a:read-syntax read-syntax)
           (rename define/kw define) (rename lambda/kw lambda))

  ;; --------------------------------------------------------------------------
  ;; Utilities

  (define-syntax define*
    (syntax-rules ()
      [(_ (name . args) . body)
       (begin (provide name) (define/kw (name . args) . body))]
      [(_ name val)
       (begin (provide name) (define name val))]))

  (define-syntax define-format-element
    (syntax-rules ()
      ([_ name tag]
       (begin (define (name . args) (cons tag args))
              (provide name)))))

  ;; allows specifying attributes through sub-elements
  (define (subs->keys x keys)
    (let ([syms+keys
           (append (map (lambda (k) (string->symbol (keyword->string k))) keys)
                   keys)]
          [tag (car x)])
      (define (amb-error key)
        (error tag "ambiguous `~a' specification" key))
      (let loop ([xs (cdr x)] [kvs '()] [seen '()])
        (if (not (or (null? xs) (null? (cdr xs)) (not (keyword? (car xs)))))
          (let ([key (car xs)])
            (when (memq key seen) (amb-error key))
            (loop (cddr xs) (list* (cadr xs) key kvs) (cons key seen)))
          (let loop ([xs xs] [body '()] [seen seen])
            (cond [(null? xs)
                   (cons tag (append! (reverse! kvs) (reverse! body)))]
                  [(or (not (pair? (car xs))) (not (memq (caar xs) syms+keys)))
                   (loop (cdr xs) (cons (car xs) body) seen)]
                  [else
                   (let ([key (if (keyword? (caar xs))
                                (caar xs)
                                (string->keyword (symbol->string (caar xs))))])
                     (when (memq key seen) (amb-error (caar xs)))
                     (when (and (pair? (cdar xs)) (keyword? (cadar xs)))
                       (error tag "sub-element for `~s' key as its own keys"
                              (caar xs)))
                     (set! kvs (list* (cdar xs) key kvs))
                     (loop (cdr xs) body (cons key seen)))]))))))

  ;; --------------------------------------------------------------------------
  ;; Formatting values and functions

  (define* (document . body)
    (subs->keys (cons 'document body) '(#:title #:author #:date)))

  (define-format-element b         'bold)
  (define-format-element bf        'bold)
  (define-format-element bold      'bold)
  (define-format-element i         'italic)
  (define-format-element it        'italic)
  (define-format-element italic    'italic)
  (define-format-element u         'underline)
  (define-format-element ul        'underline)
  (define-format-element underline 'underline)
  (define-format-element tt        'tt)

  (define-format-element title     'title)
  (define-format-element author    'author)
  ;; (define-format-element date      'date)

  )
