#lang racket
(require web-server/http
         web-server/private/xexpr
         (only-in "lib.rkt"
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
(define (input
         #:type [type "text"]
         #:value [value #f]
         #:name [name #f]
         #:size [size #f]
         #:max-length [max-length #f]
         #:read-only? [read-only? #f]
         #:attributes [attrs empty])
  (make-input
   (lambda (n)
     (list 'input
           (list* (list 'name n)
                  (list 'type type)
                  (append
                   (filter list?
                           (list (and value (list 'value (bytes->string/utf-8 value)))
                                 (and name (list 'name (bytes->string/utf-8 name)))
                                 (and size (list 'size (number->string size)))
                                 (and max-length (list 'maxlength (number->string max-length)))
                                 (and read-only? (list 'readonly "true"))))
                   attrs))))))

(define (text-input 
         #:value [value #f]
         #:name [name #f]
         #:size [size #f]
         #:max-length [max-length #f]
         #:read-only? [read-only? #f]
         #:attributes [attrs empty])
  (input
   #:type "text"
   #:value value
   #:name name
   #:size size
   #:max-length max-length
   #:read-only? read-only?
   #:attributes attrs))

(define (password-input 
         #:value [value #f]
         #:name [name #f]
         #:size [size #f]
         #:max-length [max-length #f]
         #:read-only? [read-only? #f]
         #:attributes [attrs empty])
  (input
   #:type "password"
   #:value value
   #:name name
   #:size size
   #:max-length max-length
   #:read-only? read-only?
   #:attributes attrs))

(define (checkbox value checked?
                  #:name [name #f]
                  #:attributes [attrs empty])
  (input
   #:type "checkbox"
   #:value value
   #:name name
   #:attributes
   (if checked? (append (list (list 'checked "true")) attrs) attrs)))

(define (radio value checked?
               #:name [name #f]
               #:attributes [attrs empty])
  (input
   #:type "radio"
   #:name name
   #:attributes
   (if checked? (append (list (list 'checked "true")) attrs) attrs)))

(define (submit value
                #:name [name #f]
                #:attributes [attrs empty])
  (input
   #:type "submit"
   #:name name
   #:value value
   #:attributes attrs))

(define (reset value
               #:name [name #f]
               #:attributes [attrs empty])
  (input
   #:type "reset"
   #:name name
   #:value value
   #:attributes attrs))

(define (file-upload #:name [name #f]
              #:attributes [attrs empty])
  (input
   #:type "file"
   #:name name
   #:attributes attrs))

(define (hidden #:name [name #f]
                #:attributes [attrs empty])
  (input
   #:type "hidden"
   #:name name
   #:attributes attrs))

(define (button type name
                #:disabled [disabled #f]
                #:value [value #f]
                #:attributes [attrs empty])
  (make-input
   (λ (n)
     (list 'button
           (list* (list 'type (bytes->string/utf-8 type))
                  (append 
                   (filter list?
                           (list (and disabled (list 'disabled disabled))
                                 (and value (list 'value (bytes->string/utf-8 value)))))
                   attrs))
           (bytes->string/utf-8 name)))))

(define (img alt src
             #:height [height #f]
             #:longdesc [ldesc #f]
             #:usemap [map #f]
             #:width [width #f]
             #:attributes [attrs empty])
  (make-input
   (λ (n)
     (list 'img
           (list* (list 'src (bytes->string/utf-8 src))
                  (list 'alt (bytes->string/utf-8 alt))
                  (append
                   (filter list?
                           (list (and height (list 'height (number->string height)))
                                 (and ldesc (list 'longdesc (bytes->string/utf-8 ldesc)))
                                 (and map (list 'usemap (bytes->string/utf-8 map)))
                                 (and width (list 'width (number->string width)))))
                   attrs))))))

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

(define (textarea-input
         #:rows [rows #f]
         #:cols [cols #f])         
  (make-input
   (lambda (n)
     (list 'textarea
           (list* (list 'name n)
                  (append
                   (filter list?
                           (list (and rows (list 'rows (number->string rows)))
                                 (and cols (list 'cols (number->string cols)))))))                      
           ""))))

(provide/contract
 [checkbox ((bytes? boolean?)
            (#:name (or/c false/c bytes?)
                     #:attributes (listof (list/c symbol? string?)))
            . ->* .
            (formlet/c (or/c false/c binding?)))]
 [radio ((bytes? boolean?)
         (#:name (or/c false/c bytes?)
                  #:attributes (listof (list/c symbol? string?)))
         . ->* .
         (formlet/c (or/c false/c binding?)))]
 [submit ((bytes?)         
         (#:name (or/c false/c bytes?)
                 #:attributes (listof (list/c symbol? string?)))
         . ->* .
         (formlet/c (or/c false/c binding?)))]
 [reset ((bytes?)
         (#:name (or/c false/c bytes?)
                 #:attributes (listof (list/c symbol? string?)))
         . ->* .
         (formlet/c (or/c false/c binding?)))]
 [file-upload (()
               (#:name (or/c false/c bytes?)
                       #:attributes (listof (list/c symbol? string?)))
               . ->* .
               (formlet/c (or/c false/c binding?)))]
 [hidden (()
         (#:name (or/c false/c bytes?)
                 #:attributes (listof (list/c symbol? string?)))
         . ->* .
         (formlet/c (or/c false/c binding?)))]
 [img ((bytes? bytes?)
      (#:height (or/c false/c exact-nonnegative-integer?)
                #:longdesc (or/c false/c bytes?)
                #:usemap (or/c false/c bytes?)
                #:width (or/c false/c exact-nonnegative-integer?)
                #:attributes (listof (list/c symbol? string?)))
      . ->* .
      (formlet/c string?))]
 [button ((bytes? bytes?)
         (#:disabled boolean?
                     #:value (or/c false/c bytes?)
                     #:attributes (listof (list/c symbol? string?)))
         . ->* .
         (formlet/c (or/c false/c binding?)))]
 [multiselect-input ((sequence?)
                     (#:multiple? boolean?
                                  #:selected? (any/c . -> . boolean?)
                                  #:display (any/c . -> . pretty-xexpr/c))
                     . ->* .
                     (formlet/c (listof any/c)))]
 [select-input ((sequence?)
                (#:selected? (any/c . -> . boolean?)
                             #:display (any/c . -> . pretty-xexpr/c))
                . ->* .
                (formlet/c any/c))]
 [textarea-input (()
                  (#:rows number?
                   #:cols number?)
                  . ->* .
                  (formlet/c string?))])

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
