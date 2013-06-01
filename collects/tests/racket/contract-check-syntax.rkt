#lang racket

(require drracket/check-syntax
         rackunit
         (for-syntax setup/path-to-relative)
         setup/path-to-relative)

(define-syntax (identifier stx)
  (syntax-case stx ()
    [(_ x)
     (identifier? #'x)
     #`(let ([p (open-input-string (format "~s" 'x))])
         (port-count-lines! p)
         (set-port-next-location! 
          p
          #,(syntax-line #'x)
          #,(syntax-column #'x)
          #,(syntax-position #'x))
         (read-syntax '#,(and (path? (syntax-source #'x))
                              (path->relative-string/library (syntax-source #'x)))
                      p))]))

(define (source stx)
  (list (and (path? (syntax-source stx))
             (path->relative-string/library (syntax-source stx)))
        (syntax-line stx)
        (syntax-column stx)))

(define collector%
  (class (annotations-mixin object%)
    (super-new)
    (define/override (syncheck:find-source-object stx)
      stx)
    (define/override (syncheck:add-arrow start-source-obj
                                         start-left
                                         start-right
                                         end-source-obj
                                         end-left
                                         end-right
                                         actual?
                                         phase-level)
      (set! arrows 
            (set-add arrows 
                     (list (source start-source-obj)
                           (source end-source-obj)))))
    (define arrows (set))
    (define/public (collected-arrows) arrows)))

(define-namespace-anchor module-anchor)
(define module-namespace 
  (namespace-anchor->namespace module-anchor))

(let ([annotations (new collector%)])
  (define-values (add-syntax done)
    (make-traversal module-namespace #f))
  
  (define x1 (identifier x))
  (define x2 (identifier x))
  (define x3 (identifier x))
  (define y1 (identifier y))
  (define y2 (identifier y))
  (define z1 (identifier z))
  
  (parameterize ([current-annotations annotations]
                 [current-namespace module-namespace])
    (add-syntax
     (expand #`(->i ([#,x1 any/c]
                     [#,y1 (#,x2) any/c]
                     [#,z1 (#,x3 #,y2) any/c])
                    any)))
    (done))
  
  (check-equal? (send annotations collected-arrows)
                (set (list (source x1) (source x2))
                     (list (source x1) (source x3))
                     (list (source y1) (source y2)))))

(let ([annotations (new collector%)])
  (define-values (add-syntax done)
    (make-traversal module-namespace #f))
  
  (define x1 (identifier x))
  (define x2 (identifier x))
  (define x3 (identifier x))
  (define y1 (identifier y))
  (define y2 (identifier y))
  (define z1 (identifier z))
  
  (parameterize ([current-annotations annotations]
                 [current-namespace module-namespace])
    (add-syntax
     (expand #`(->i ([#,x1 any/c])
                    [r (#,x2) #,x3])))
    (done))
  
  (check-equal? (send annotations collected-arrows)
                (set (list (source x1) (source x2))
                     (list (source x1) (source x3)))))
