#reader scribble/reader
#lang scheme/base
(require scribblings/htdp-langs/common
         scribble/decode
         scribble/struct
         scribble/scheme
         scheme/list
         scheme/pretty
         syntax/docprovide)

(provide prim-ops
         prim-op-defns)

(define (maybe-make-table l t)
  (if (paragraph? t)
      (make-paragraph
       (append l (cons " "
                       (paragraph-content t))))
      (make-table
       "prototype"
       (list (list (make-flow (list (make-paragraph l)))
                   (make-flow (list t)))))))


(define (typeset-type type)
  (let-values ([(in out) (make-pipe)])
    (parameterize ([pretty-print-columns 50])
      (pretty-print type out))
    (port-count-lines! in)
    (read-syntax #f in)))

(define (sort-category category)
  (sort
   (cadr category)
   (lambda (x y)
     (string<=? (symbol->string (car x))
                (symbol->string (car y))))))


(define (make-proto func ctx-stx)
  (maybe-make-table
   (list
    (hspace 2)
    (to-element (datum->syntax ctx-stx (car func)))
    (hspace 1)
    ":"
    (hspace 1))
   (to-paragraph
    (typeset-type (cadr func)))))
  
(define (prim-ops lib ctx-stx)
  (let ([ops (map (lambda (cat)
                    (cons (car cat)
                          (list (cdr cat))))
                  (lookup-documentation lib 'procedures))])
    (make-table
     #f
     (apply
      append
      (map (lambda (category)
	     (cons
	      (list (make-flow
		     (list
		      (make-paragraph (list (hspace 1)
					    (bold (car category)))))))
	      (map (lambda (func)
		     (list
		      (make-flow
		       (list
			(make-proto func ctx-stx)))))
		   (sort-category category))))
	   ops)))))


(define (prim-op-defns lib ctx-stx not-in)
  (make-splice
   (let ([ops (map (lambda (cat)
                     (cons (car cat)
                           (list (cdr cat))))
                   (lookup-documentation lib 'procedures))]
         [not-in-ns (map (lambda (not-in-mod)
                           (let ([ns (make-base-namespace)])
                             (parameterize ([current-namespace ns])
                               (namespace-require `(for-label ,not-in-mod)))
                             ns))
                         not-in)])
     (apply
      append
      (map (lambda (category)
             (filter values
                     (map
                      (lambda (func)
                        (let ([id (datum->syntax ctx-stx (car func))])
                          (and (not (ormap
                                     (lambda (ns)
                                       (free-label-identifier=?
                                        id
                                        (parameterize ([current-namespace ns])
                                          (namespace-syntax-introduce (datum->syntax #f (car func))))))
                                     not-in-ns))
                               (let ([desc-strs (cddr func)])
                                 (defthing/proc
                                   (if (pair? (cadr func)) "function" "constant")
                                   id
                                   (to-paragraph (typeset-type (cadr func)))
                                   desc-strs)))))
                      (sort-category category))))
           ops)))))
