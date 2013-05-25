#lang racket/base
(require (for-syntax racket/base)
	 (for-syntax mzlib/etc)
	 (for-syntax mzlib/list)
	 (for-syntax (prefix-in english: "private/english-string-constants.rkt"))
	 (for-syntax (prefix-in spanish: "private/spanish-string-constants.rkt"))
	 (for-syntax (prefix-in german: "private/german-string-constants.rkt"))
	 (for-syntax (prefix-in french: "private/french-string-constants.rkt"))
	 (for-syntax (prefix-in dutch: "private/dutch-string-constants.rkt"))
	 (for-syntax (prefix-in danish: "private/danish-string-constants.rkt"))
	 (for-syntax (prefix-in portuguese: "private/portuguese-string-constants.rkt"))
	 (for-syntax (prefix-in japanese: "private/japanese-string-constants.rkt"))
	 (for-syntax (prefix-in traditional-chinese: "private/traditional-chinese-string-constants.rkt"))
	 (for-syntax (prefix-in simplified-chinese: "private/simplified-chinese-string-constants.rkt"))
	 (for-syntax (prefix-in russian: "private/russian-string-constants.rkt"))
	 (for-syntax (prefix-in ukrainian: "private/ukrainian-string-constants.rkt"))
	 (for-syntax (prefix-in korean: "private/korean-string-constants.rkt")))

(require mzlib/file
         mzlib/etc
         "private/only-once.rkt")

(provide string-constant string-constants this-language all-languages set-language-pref)

;; set-language-pref : symbol -> void
(define (set-language-pref language)
  (put-preferences (list 'plt:human-language) (list language)))

;; table : (listof (list symbol regexp regexp))
;; this table indicates what the default value of the natural language
;; preference is. the first regexp is used under Windows and the second
;; is used on other platforms. All regexps are compared to the result
;; of (system-language+country)
(define table
  '((english             #rx"^en_"        #rx"^English_")
    (spanish             #rx"^es_"        #rx"^Espanol_")
    (german              #rx"^de_"        #rx"^German_")
    (french              #rx"^fr_"        #rx"French_")
    (dutch               #rx"nl_"         #rx"^Netherlands_")
    (danish              #rx"^da_DK"      #rx"^Danish_")
    (portuguese          #rx"^pt_"        #rx"Portuguese_")
    (japanese            #rx"^ja_"        #rx"^Japan_")
    (traditional-chinese #rx"^zh_(HK|TW)" #rx"Chinese_(Hong|Taiwan)")
    (simplified-chinese  #rx"^zh_CN"      #rx"Chinese_China")
    (russian             #rx"^ru_"        #rx"^Russian_")
    (ukrainian           #rx"^uk_"        #rx"^Ukrainian_")
    (korean              #rx"^ko_"        #rx"^Korean_")))

;; default-language : -> symbol
;; uses `table' and system-language+contry to find what language to start with
(define (default-language)
  (let ([slc (system-language+country)])
    (let loop ([table table])
      (if (null? table)
        'english
        (let ([ent (car table)])
          (if (or (regexp-match (cadr ent) slc)
                  (and (cddr ent)
                       (regexp-match (caddr ent) slc)))
            (car ent)
            (loop (cdr table))))))))


;; language : symbol
(define language
  (with-handlers ([exn:fail? (lambda (_) (default-language))])
    (get-preference 'plt:human-language (lambda () (default-language)))))

(define-syntax-set (string-constant string-constants this-language all-languages)
  ;; type sc = (make-sc symbol (listof (list symbol string)) (union #f hash-table[symbol -o> #t]))
  (define-struct sc (language-name constants ht) #:mutable)

  (define available-string-constant-sets
    (list
     (make-sc 'english             english:string-constants             #f)
     (make-sc 'spanish             spanish:string-constants             #f)
     (make-sc 'french              french:string-constants              #f)
     (make-sc 'german              german:string-constants              #f)
     (make-sc 'dutch               dutch:string-constants               #f)
     (make-sc 'danish              danish:string-constants              #f)
     (make-sc 'portuguese          portuguese:string-constants          #f)
     (make-sc 'japanese            japanese:string-constants            #f)
     (make-sc 'traditional-chinese traditional-chinese:string-constants #f)
     (make-sc 'simplified-chinese  simplified-chinese:string-constants  #f)
     (make-sc 'russian             russian:string-constants             #f)
     (make-sc 'ukrainian           ukrainian:string-constants           #f)
     (make-sc 'korean              korean:string-constants              #f)))

  (define first-string-constant-set (car available-string-constant-sets))

  ;; env-var-set? : symbol -> boolean
  ;; returns #t if the user has requested this langage info.
  ;; If the environment variable is set to something that
  ;; isn't well-formed according to `read' you get all output
  ;; If the environment variable is set to a symbol (according to read)
  ;; you get that language. If it is set to a list of symbols
  ;; (again, according to read) you get those languages.
  ;; if it is set to anything else, you get all languages.
  (define (env-var-set? lang)
    (cond [(symbol? specific) (eq? lang specific)]
          [(list? specific) (memq lang specific)]
          [else #t]))

  (define env-var-set
    (or (getenv "PLTSTRINGCONSTANTS")
        (getenv "STRINGCONSTANTS")))

  (define specific
    (and env-var-set
         (with-handlers ([exn:fail:read? (lambda (x) #t)])
           (read (open-input-string env-var-set)))))

  (define the-warning-message #f)
  (define (get-warning-message)
    (unless the-warning-message
      (set! the-warning-message
            (let* (;; type no-warning-cache-key = (cons symbol symbol)
                   ;; warning-table : (listof (list no-warning-cache-key (listof (list sym string))))
                   [warning-table null]
                   [extract-ht
                    (lambda (sc)
                      (unless (sc-ht sc)
                        (let ([ht (make-hash)])
                          (for-each (lambda (ent) (hash-set! ht (car ent) #t))
                                    (sc-constants sc))
                          (set-sc-ht! sc ht)))
                      (sc-ht sc))]
                   [check-one-way
                    (lambda (sc1 sc2)
                      (let ([assoc1 (sc-constants sc1)]
                            [assoc2 (sc-constants sc2)]
                            [ht2 (extract-ht sc2)])
                        (for-each
                         (lambda (pair1)
                           (let* ([constant1 (car pair1)]
                                  [value1 (cadr pair1)]
                                  [pair2 (hash-ref ht2 constant1 (lambda () #f))])
                             (unless pair2
                               (let ([no-warning-cache-key (cons (sc-language-name sc1) (sc-language-name sc2))])
                                 (when (or (env-var-set? (sc-language-name sc1))
                                           (env-var-set? (sc-language-name sc2)))
                                   (cond
                                     [(memf (lambda (ent) (equal? (mcar ent) no-warning-cache-key)) warning-table)
                                      =>
                                      (lambda (x)
                                        (let ([ent (car x)])
                                          (set-mcdr! ent (cons (list constant1 value1) (mcdr ent)))))]
                                     [else
                                      (set! warning-table (cons (mcons no-warning-cache-key
                                                                       (list (list constant1 value1)))
                                                                warning-table))]))))))
                         assoc1)))])

              (for-each (lambda (x)
                          (check-one-way x first-string-constant-set)
                          (check-one-way first-string-constant-set x))
                        (cdr available-string-constant-sets))

              (let ([sp (open-output-string)])
                (for-each
                 (lambda (bad)
                   (let* ([lang-pair (mcar bad)]
                          [constants (mcdr bad)]
                          [lang1-name (car lang-pair)]
                          [lang2-name (cdr lang-pair)])
                     (fprintf sp "WARNING: language ~a had but ~a does not:\n"
                              lang1-name
                              lang2-name)
                     (for-each (lambda (x) (fprintf sp "   ~s\n" x))
                               (sort constants string<=? 
				     #:key (lambda (p)
					     (symbol->string (car p)))
				     #:cache-keys? #t))
                     (newline sp)))
                 warning-table)
                (get-output-string sp)))))
    the-warning-message)

  (define (string-constant/proc stx)
    (syntax-case stx ()
      [(_ name)
       (let ([assoc-table (sc-constants first-string-constant-set)]
             [datum (syntax->datum (syntax name))])
         (unless (symbol? datum)
           (raise-syntax-error #f (format "expected name, got: ~s" datum) stx))
         (let ([default-val (assq datum assoc-table)])
           (unless default-val
             (raise-syntax-error
              #f
              (format "~a is not a known string constant" datum) 
              stx))
           (with-syntax ([(constants ...)
                          (map (lambda (x)
                                 (let ([val (assq datum (sc-constants x))])
                                   (if val
                                     (cadr val)
                                     (cadr default-val))))
                               available-string-constant-sets)]
                         [(languages ...)
                          (map sc-language-name available-string-constant-sets)]
                         [first-constant
                          (cadr (assq datum (sc-constants first-string-constant-set)))])
             (with-syntax ([conditional-for-string
                            (syntax/loc stx
                              (cond [(eq? language 'languages) constants] ...
                                    [else first-constant]))])
               (if env-var-set
                 (with-syntax ([warning-message (get-warning-message)])
                   (syntax/loc stx
                     (begin (maybe-print-message warning-message)
                            conditional-for-string)))
                 (syntax/loc stx conditional-for-string))))))]))

  (define (string-constants/proc stx)
    (syntax-case stx ()
      [(_ name)
       (let ([assoc-table (sc-constants first-string-constant-set)]
             [datum (syntax->datum (syntax name))])
         (unless (symbol? datum)
           (raise-syntax-error #f (format "expected name, got: ~s" datum) stx))
         (let ([default-val (assq datum assoc-table)])
           (unless default-val
             (raise-syntax-error
              #f
              (format "~a is not a known string constant" datum)
              stx))
           (with-syntax ([(constants ...)
                          (map (lambda (x)
                                 (let ([val (assq datum (sc-constants x))])
                                   (if val
                                     (cadr val)
                                     (cadr default-val))))
                               available-string-constant-sets)])
             (syntax (list constants ...)))))]))

  (define (this-language/proc stx)
    (syntax-case stx () [(_) (syntax language)]))

  (define (all-languages/proc stx)
    (syntax-case stx ()
      [(_)
       (with-syntax ([(languages ...)
                      (map sc-language-name available-string-constant-sets)])
         (syntax (list 'languages ...)))])))

