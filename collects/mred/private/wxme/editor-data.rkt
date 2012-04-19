#lang racket/base

(require racket/class
         racket/file file/convertible
         "../syntax.rkt"
         "private.rkt"
         racket/snip/private/snip-flags
         racket/snip/private/private
         racket/snip/private/style
         racket/snip/private/load-one
         (only-in "cycle.rkt"
                  editor-stream-in% editor-stream-out%
                  get-editor-data-class set-get-editor-data-class!)
         "../wx/common/event.rkt"
         racket/draw)

(provide get-the-editor-data-class-list
         editor-data%
         editor-data-class%
         location-editor-data%
         editor-data-class-list<%>
         the-editor-data-class-list ;; parameter
         make-the-editor-data-class-list
         (struct-out editor-data-class-link))

;; ------------------------------------------------------------

(defclass editor-data% object%
  (properties [[(make-or-false editor-data-class%) dataclass] #f]
              [[(make-or-false editor-data%) next] #f])
  (super-new)
  (def/public (write [editor-stream-out% f])
    (error 'write "should have overridden"))

  (define/public (get-s-dataclass) dataclass)
  (define/public (get-s-next) next)
  (define/public (set-s-next v) (set! next v)))


(defclass location-editor-data% editor-data%
  (inherit set-dataclass)
  (init-field x y)
  (super-new)
  (set-dataclass the-location-editor-data-class)

  (define/public (get-x) x)
  (define/public (get-y) y)

  (def/override (write [editor-stream-out% f])
    (send f put x)
    (send f put y)
    #t))

;; ------------------------------------------------------------

(defclass editor-data-class% object%
  (define classname "wxbad")  
  (def/public (set-classname [string? s])
    (set! classname (string->immutable-string s)))
  (def/public (get-classname) classname)

  (properties [[bool? required?] #f])
  (define/public (get-s-required?) required?)

  (def/public (read [editor-stream-in% f]) (void))

  (super-new))

(defclass location-editor-data-class% editor-data-class%
  (inherit set-classname
           set-required?)

  (super-new)

  (set-classname "wxloc")
  (set-required? #t)

  (def/override (read [editor-stream-in% f])
    (let ([x (send f get-inexact)]
          [y (send f get-inexact)])
      (new location-editor-data% [x x][y y]))))

(define the-location-editor-data-class
  (new location-editor-data-class%))

;; ------------------------------------------------------------

(define-struct editor-data-class-link ([c #:mutable] [name #:mutable] map-position))

(defclass editor-data-class-list% object%
  (define ht (make-hash))
  (define pos-ht (make-hash))
  (define rev-pos-ht (make-hash))

  (super-new)

  (add the-location-editor-data-class)

  (def/public (find [string? name])
    (let ([c (hash-ref ht name #f)])
      (or c
          (let ([c (get-editor-data-class name)])
            (when c (add c))
            c))))
  
  (def/public (find-position [editor-data-class% c])
    (hash-ref pos-ht c 0))
  
  (def/public (add [editor-data-class% c])
    (let ([name (send c get-classname)])
      (hash-set! ht name c)
      (let ([n (add1 (hash-count pos-ht))])
        (hash-set! pos-ht c n)
        (hash-set! rev-pos-ht n c))))

  (def/public (number) (hash-count ht))

  (def/public (nth [exact-nonnegative-integer? n]) (hash-ref rev-pos-ht n #f))

  (def/public (write [editor-stream-out% f])
    (let ([n (number)])
      (send f put n)
      (for ([i (in-range 1 (add1 n))])
        (let ([c (nth i)])
          (send f put (string->bytes/utf-8 (send c get-classname)))

          (send f add-dl (make-editor-data-class-link c
                                                      #f
                                                      i)))))
    #t)

  (def/public (read [editor-stream-in% f])
    (let-boxes ([count 0])
        (send f get count)
      (for/and ([i (in-range count)])
        (let ([s (send f get-bytes)])
          (and (send f ok?)
               (send f add-dl (make-editor-data-class-link
                               #f
                               (bytes->string/utf-8 s #\?)
                               (add1 i)))
               #t)))))

  (define/public (find-by-map-position f n)
    (ormap (lambda (s)
             (and (= n (editor-data-class-link-map-position s))
                  (begin
                    (when (editor-data-class-link-name s)
                      (let ([c (find (editor-data-class-link-name s))])
                        (when (not c)
                          ;; unknown class/version
                          (log-error (format "unknown editor data class: ~e" 
                                             (editor-data-class-link-name s))))
                        (set-editor-data-class-link-name! s #f)
                        (set-editor-data-class-link-c! s c)))
                    (editor-data-class-link-c s))))
           (send f get-dl))))

(define editor-data-class-list<%> (class->interface editor-data-class-list%))

(define (make-the-editor-data-class-list)
  (new editor-data-class-list%))

(define the-editor-data-class-list (make-parameter (make-the-editor-data-class-list)))
(define (get-the-editor-data-class-list)
  (the-editor-data-class-list))

(set-get-editor-data-class!
 (lambda (name)
   (load-one name 'editor-data-class editor-data-class%)))
