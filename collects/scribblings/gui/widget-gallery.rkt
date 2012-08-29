(module widget scheme/base
  (require racket/string
           "common.rkt")

  (provide showcase-widget)

  (define-syntax showcase-widget
    (syntax-rules ()
      ((_ widget code ...)
       (begin
         (racketlink widget
                     (image #:suffixes
                            (list ".png")
                            (string-append "image/"
                                           (string-trim (symbol->string 'widget)
                                                        "%"
                                                        #:left? #f))))
         (racketblock code ...))))))
