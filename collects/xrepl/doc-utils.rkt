#lang racket/base

(require scribble/manual scribble/core scribble/decode
         racket/list racket/sandbox)

(provide (all-from-out scribble/manual)
         RL GUIDE REFERENCE cmd defcmd check-all-documented)

(define RL '(lib "readline/readline.scrbl"))
(define GUIDE '(lib "scribblings/guide/guide.scrbl"))
(define REFERENCE '(lib "scribblings/reference/reference.scrbl"))

(define commands
  (let ([c #f])
    (λ ()
      (unless c
        (define e (call-with-trusted-sandbox-configuration
                   (λ () (make-evaluator 'racket/base))))
        (e '(require xrepl/xrepl))
        (e '(current-namespace (module->namespace 'xrepl/xrepl)))
        (set! c (e '(for/list ([c (in-list commands-list)])
                      (list (car (command-names c))
                            (cdr (command-names c))
                            (command-argline c)
                            (command-blurb c)))))
        (kill-evaluator e))
      c)))
(define documented '())

(define (cmd* name0 . more)
  (define name (if (symbol? name0) name0 (string->symbol name0)))
  (define full-name
    (or (and (assq name (commands)) name)
        (for/or ([c (in-list (commands))]) (and (memq name (cadr c)) (car c)))
        (error 'cmd "unknown command: ~s" name)))
  (define content
    (litchar (let ([s (format ",~a" name)])
               (if (pair? more) (apply string-append s " " more) s))))
  (link-element "plainlink" content `(xrepl ,(format "~a" full-name))))

(define-syntax-rule (cmd name more ...) (cmd* 'name more ...))

(define (cmd-index name)
  (define namestr (format ",~a" name))
  (define tag `(xrepl ,(format "~a" name)))
  (define content (cmd* name))
  (define ielem
    (index-element #f content tag (list namestr) (list content)
                   'xrepl-command))
  (toc-target-element #f (list ielem) tag))

(define (defcmd* name . text)
  (set! documented (cons name documented))
  (define-values [other-names argline blurb]
    (apply values (cond [(assq name (commands)) => cdr]
                        [else (error 'defcmd "unknown command: ~s" name)])))
  (define header
    (list (cmd-index name) (litchar (string-append " " (or argline "")))))
  (define desc
    (list (hspace 2) (make-element 'italic blurb)))
  (define synonyms
    (and (pair? other-names)
         (list (hspace 2)
               "[Synonyms: "
               (add-between (map (λ (n) (litchar (format ",~a" n)))
                                 other-names)
                            " ")
               "]")))
  (splice
   (list* (tabular #:style 'boxed `((,header) (,desc)
                                    ,@(if synonyms `((,synonyms)) `())))
          "\n" "\n" text)))

(define-syntax-rule (defcmd name text ...) (defcmd* 'name text ...))

(define (check-all-documented)
  (unless (= (length documented) (length (remove-duplicates documented)))
    (error 'xrepl-docs "some commands were documented multiple times"))
  (let ([missing (remove* documented (map car (commands)))])
    (when (pair? missing)
      (error 'xrepl-docs "missing command documentation: ~s" missing))))
