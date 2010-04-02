#lang scheme
(require web-server/http
         web-server/private/xexpr
         (only-in "lib.ss"
                  formlet/c
                  pure
                  cross))

; Low-level
(define (next-name i)
  (values (format "input_~a" i) (add1 i)))
(define (make-input* render)
  (lambda (i)
    (let-values ([(w i) (next-name i)])
      (define wb (string->bytes/utf-8 w))
      (values (list (render w))
              (lambda (env) 
                (for/list ([b (in-list env)]
                           #:when (bytes=? wb (binding-id b)))
                  b))
              i))))
(define (make-input render)
  (lambda (i)
    (let-values ([(w i) (next-name i)])
      (values (list (render w))
              (lambda (env) (bindings-assq (string->bytes/utf-8 w) env))
              i))))

(define binding:form-required
  (pure 
   (lambda (bf)
     (if (binding:form? bf)
         (binding:form-value bf)
         (error 'formlets "Missing required field")))))

(define (binding:form/default default)
  (pure
   (lambda (bf)
     (if (binding:form? bf)
         (binding:form-value bf)
         default))))

(provide/contract
 [make-input* ((string? . -> . pretty-xexpr/c) . -> . (formlet/c (listof binding?)))]
 [make-input ((string? . -> . pretty-xexpr/c) . -> . (formlet/c (or/c false/c binding?)))]
 #;[binding:form-required (formlet/c (binding? . -> . bytes?))]
 #;[binding:form/default (bytes? . -> . (formlet/c (binding? . -> . bytes?)))])

; HTML Spec
(define (text-or-password 
         #:password? password?
         #:value [value #f]
         #:size [size #f]
         #:max-length [max-length #f]
         #:read-only? [read-only? #f]
         #:attributes [attrs empty])
  (make-input
   (lambda (n)
     (list 'input
           (list* (list 'name n)
                  (list 'type 
                        (if password? "password" "text"))
                  (append
                   (filter list?
                           (list (and value (list 'value (bytes->string/utf-8 value)))
                                 (and size (list 'size (number->string size)))
                                 (and max-length (list 'maxlength (number->string max-length)))
                                 (and read-only? (list 'readonly "true"))))
                   attrs))))))

(define (text-input 
         #:value [value #f]
         #:size [size #f]
         #:max-length [max-length #f]
         #:read-only? [read-only? #f]
         #:attributes [attrs empty])
  (text-or-password 
   #:password? #f
   #:value value
   #:size size
   #:max-length max-length
   #:read-only? read-only?
   #:attributes attrs))

(define (password-input 
         #:value [value #f]
         #:size [size #f]
         #:max-length [max-length #f]
         #:read-only? [read-only? #f]
         #:attributes [attrs empty])
  (text-or-password 
   #:password? #t
   #:value value
   #:size size
   #:max-length max-length
   #:read-only? read-only?
   #:attributes attrs))

(define (checkbox value checked?
                  #:attributes [attrs empty])
  (make-input
   (lambda (n)
     (list 'input
           (list* (list 'name n)
                  (list 'type "checkbox")
                  (list 'value (bytes->string/utf-8 value))
                  (append (if checked? (list (list 'checked "true")) empty)
                          attrs))))))

; XXX radio

; XXX submit

; XXX reset

; XXX file

; XXX hidden

; XXX image

; XXX button

(define (multiselect-input l
                       #:multiple? [multiple? #t]
                       #:selected? [selected? (λ (x) #f)]
                       #:display [display (λ (x) x)])
  (define value->element (make-hasheq))
  (define i 0)
  (define (remember! e)
    (define this-i
      (begin0 i (set! i (add1 i))))
    (hash-set! value->element this-i e))
  (define (recall i)
    (hash-ref value->element i
              (λ () (error 'input-select* "Invalid selection: ~e" i))))
  (for ([e l])
    (remember! e))
  (cross
   (pure
    (lambda (bs)
      (map (compose recall string->number
                    bytes->string/utf-8
                    binding:form-value)
           bs)))
   (make-input*
    (lambda (name)
      `(select (,@(if multiple? '([multiple "true"]) empty)
                [name ,name])
               ,@(for/list ([(vn e) (in-hash value->element)])
                   (define v (number->string vn))
                   `(option ([value ,v]
                             ,@(if (selected? e)
                                   '([selected "true"])
                                   empty))
                            ,(display e))))))))

(define (select-input l 
                      #:selected? [selected? (λ (x) #f)]
                      #:display [display (λ (x) x)])
  (cross
   (pure first)
   (multiselect-input l
                  #:multiple? #f
                  #:selected? selected?
                  #:display display)))

(define (textarea-input)
  (to-string 
   (required 
    (make-input
     (lambda (n)
       (list 'textarea
             (list (list 'name n))
             ""))))))

(provide/contract
 [multiselect-input (->* (sequence?)
                         (#:multiple? boolean?
                          #:selected? (any/c . -> . boolean?)
                          #:display (any/c . -> . pretty-xexpr/c))
                         (formlet/c (listof any/c)))]
 [select-input (->* (sequence?)
                    (#:selected? (any/c . -> . boolean?)
                     #:display (any/c . -> . pretty-xexpr/c))
                    (formlet/c any/c))]
 [textarea-input (-> (formlet/c string?))]
 [text-input (() 
        (#:value (or/c false/c bytes?)
                 #:size (or/c false/c exact-nonnegative-integer?)
                 #:max-length (or/c false/c exact-nonnegative-integer?)
                 #:read-only? boolean?
                 #:attributes (listof (list/c symbol? string?)))
        . ->* . 
        (formlet/c (or/c false/c binding?)))]
 [password-input (() 
            (#:value (or/c false/c bytes?)
                     #:size (or/c false/c exact-nonnegative-integer?)
                     #:max-length (or/c false/c exact-nonnegative-integer?)
                     #:read-only? boolean?
                     #:attributes (listof (list/c symbol? string?)))
            . ->* . 
            (formlet/c (or/c false/c binding?)))]
 [checkbox ((bytes? boolean?)
            (#:attributes (listof (list/c symbol? string?)))
            . ->* .
            (formlet/c (or/c false/c binding?)))])

; High-level
(define (required f)
  (cross binding:form-required f))

(define (default d f)
  (cross (binding:form/default d) f))

(define (to-string f)
  (cross (pure bytes->string/utf-8) f))

(define (to-number f)
  (cross (pure string->number) f))

(define (to-symbol f)
  (cross (pure string->symbol) f))

(define (to-boolean f)
  (cross (pure 
          (lambda (b)
            (bytes=? b #"on")))
         f))

(provide/contract
 [required ((formlet/c (or/c false/c binding?)) . -> . (formlet/c bytes?))]
 [default (bytes? (formlet/c (or/c false/c binding?)) . -> . (formlet/c bytes?))]
 [to-string ((formlet/c bytes?) . -> . (formlet/c string?))]
 [to-number ((formlet/c string?) . -> . (formlet/c number?))]
 [to-symbol ((formlet/c string?) . -> . (formlet/c symbol?))]
 [to-boolean ((formlet/c bytes?) . -> . (formlet/c boolean?))])

; OLD
(define input-string (to-string (required (text-input))))
(define input-int (to-number input-string))
(define input-symbol (to-symbol input-string))

(provide/contract
 [input-string (formlet/c string?)]
 [input-int (formlet/c integer?)]
 [input-symbol (formlet/c symbol?)])
