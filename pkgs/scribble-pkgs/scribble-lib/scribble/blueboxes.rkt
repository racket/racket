#lang racket/base
(require setup/dirs 
         racket/serialize
         racket/contract
         scribble/core)

(provide
 (contract-out
  [fetch-blueboxes-strs (->* (tag?) (#:blueboxes-cache blueboxes-cache?) 
                             (or/c #f (non-empty-listof string?)))]
  [make-blueboxes-cache (-> boolean? blueboxes-cache?)]
  [blueboxes-cache? (-> any/c boolean?)]))

(struct blueboxes-cache (info) #:mutable)
(define (make-blueboxes-cache populate?)
  (blueboxes-cache (and populate? (build-blueboxes-cache))))

(define (fetch-blueboxes-strs tag #:blueboxes-cache [cache (make-blueboxes-cache #f)])
  (define plain-strs (fetch-strs-for-single-tag tag cache))
  (cond
    [(and plain-strs
          (pair? tag)
          (eq? (car tag) 'def))
     (define constructor-strs 
       (fetch-strs-for-single-tag (cons 'construtor (cdr tag)) cache))
     (if constructor-strs
         (append plain-strs 
                 '("") 
                 ;; cdr drops the "white label" line (constructor, presumably)
                 (cdr constructor-strs))
         plain-strs)]
    [else
     plain-strs]))

(define (fetch-strs-for-single-tag tag cache)
  (unless (blueboxes-cache-info cache)
    (set-blueboxes-cache-info! cache (build-blueboxes-cache)))
  (for/or ([ent (in-list (blueboxes-cache-info cache))])
    (define offset+lens (hash-ref (list-ref ent 2) tag #f))
    (cond
      [offset+lens
       (apply
        append
        (for/list ([offset+len (in-list offset+lens)])
          (define fn (list-ref ent 0))
          (define offset (list-ref ent 1))
          (call-with-input-file fn
            (λ (port)
              (port-count-lines! port)
              (file-position port (+ (car offset+len) offset))
              (for/list ([i (in-range (cdr offset+len))])
                (read-line port))))))]
      [else #f])))

;; build-blueboxes-cache : (listof (list file-path int hash[tag -o> (cons int int)]))
(define (build-blueboxes-cache)
  (filter
   values
   (for*/list ([doc-search-dir (in-list (get-doc-search-dirs))]
               [doc-dir-name (in-list (if (directory-exists? doc-search-dir)
                                          (directory-list doc-search-dir)
                                          '()))])
     (define x (build-path doc-search-dir doc-dir-name "blueboxes.rktd"))
     (and (file-exists? x)
          (call-with-input-file x
            (λ (port)
              (port-count-lines! port)
              (define first-line (read-line port))
              (define pos (file-position port))
              (define desed 
                (with-handlers ([exn:fail? (λ (x) 
                                             (log-warning "Failed to deserialize ~a: ~a"
                                                          x
                                                          (exn-message x))
                                             #f)])
                  (deserialize (read port))))
              (and desed
                   (list x
                         (+ (string->number first-line) pos)
                         desed))))))))
