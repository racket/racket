#lang scheme/base

(require scribble/manual
         scribble/struct
         scribble/decode
         setup/getinfo
         setup/main-collects
         scheme/list
         "front-toc.ss"
         "../config.ss")

(provide build-contents)

(define-struct sec (cat label))

(define sections
  (map (lambda (xs) (apply make-sec xs)) manual-sections))

(define (in-main-collects? dir)
  (pair? (path->main-collects-relative dir)))

(define (add-sections cat mk-sep l)
  (if (null? l)
    null
    (let loop ([l l] [key (if (equal? "" (caddar l)) (caar l) +inf.0)])
      (cond [(null? l) null]
            [(equal? (caar l) key) (cons (cadar l) (loop (cdr l) key))]
            [else (let ([lbl (caddar l)] ; currently always ""
                        [l (cons (cadar l) (loop (cdr l) (caar l)))]
                        [sep? (not (= (truncate (/ key 10))
                                      (truncate (/ (caar l) 10))))])
                    (if sep? (cons (mk-sep lbl) l) l))]))))

(define (build-contents all?)
  (let* ([dirs (find-relevant-directories '(scribblings))]
         [infos (map get-info/full dirs)]
         [docs (append-map
                (lambda (i dir)
                  (define s (and (or all? (in-main-collects? dir))
                                 (i 'scribblings)))
                  (if (not s)
                    null
                    (filter-map
                     (lambda (d)
                       (if (and (not all?)
                                (pair? (cdr d))
                                (or (memq 'user-doc (cadr d))
                                    (memq 'user-doc-root (cadr d))))
                         #f
                         (let* ([new-cat (if ((length d) . > . 2)
                                           (caddr d)
                                           '(library))]
                                [sub-cat (and (list? new-cat)
                                              ((length new-cat) . > . 1)
                                              (cadr new-cat))])
                           (list
                            ;; Category
                            (let ([the-cat
                                   (if (pair? new-cat) (car new-cat) 'unknown)])
                              (or (and (eq? the-cat 'omit) the-cat)
                                  (ormap (lambda (sec)
                                           (and (eq? the-cat (sec-cat sec))
                                                the-cat))
                                         sections)
                                  'library))
                            ;; Priority
                            (if (and sub-cat (real? sub-cat)) sub-cat 0)
                            ;; Priority label (not used):
                            ""
                            ;; Path
                            (build-path dir (if (pair? d) (car d) "???"))))))
                     s)))
                infos
                dirs)]
         [plain-line
          (lambda content
            (list (make-flow (list (make-paragraph content)))))]
         [line
          (lambda (doc)
            (plain-line (hspace 2) (other-manual doc #:underline? #f)))])
    (make-splice
     (list
      (make-delayed-block
       (lambda (renderer part resolve-info)
         (make-table
          #f
          (cdr
           (append-map
            (lambda (sec)
              (let ([docs (filter (lambda (doc) (eq? (car doc) (sec-cat sec)))
                                  docs)])
                (list*
                 (plain-line (hspace 1))
                 (plain-line (sec-label sec))
                 (add-sections
                  (sec-cat sec)
                  (lambda (str)
                    (plain-line
                     (make-element (if (string=? str "") "sepspace" "septitle")
                                   (list 'nbsp str))))
                  (sort
                   (map (lambda (doc)
                          (list (cadr doc) (line (cadddr doc)) (caddr doc)))
                        docs)
                   (lambda (ad bd)
                     (if (= (car ad) (car bd))
                       (let ([str (lambda (x)
                                    (element->string
                                     (cadr (paragraph-content
                                            (car (flow-paragraphs (caadr x)))))
                                     renderer part resolve-info))])
                         (string-ci<? (str ad) (str bd)))
                       (> (car ad) (car bd)))))))))
            sections)))))
      (front-toc 'start)))))
