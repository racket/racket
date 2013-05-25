#lang racket/base

(require racket/class racket/match)
(provide autocompletion-cursor<%> autocompletion-cursor%)

(define autocompletion-cursor<%>
  (interface ()
    get-completions  ;      -> (listof string) 
    get-length       ;      -> int
    empty?           ;      -> boolean
    narrow           ; char -> autocompletion-cursor<%>
    widen))          ; char -> autocompletion-cursor<%>
    

;; string -> (values (string -> real) natural)
;; produce a ranking function and a max normal score
;; the ranking function is as follows:
;; w |-> +inf.0 if `prefix' is a prefix of w
;; w |-> 1000 if `prefix' appears in w
;; w |-> n if n parts of `prefix' appear in w as first segments
;; the max normal score is the largest n that the last clause can produce
(define (rank prefix) 
  (define splitters "[-/:_!]")
  (define parts (regexp-split splitters prefix))
  (define re (regexp (string-append "^" (regexp-quote prefix))))
  (values (λ (w) (cond 
                   [(regexp-match re w) +inf.0]
                   ;; it's a very good match prefix appears in the word
                   [(regexp-match (regexp-quote prefix) w) 1000]
                   ;; otherwise, we iterate and check each component of 
                   [else
                    (for/fold ([c 0]) ([r parts])
                      (define rq (regexp-quote r))
                      (cond [(regexp-match (string-append "^" rq) w)
                             (+ 1 c)]
                            [(regexp-match (string-append splitters rq) w)
                             (+ 1 c)]
                            [else c]))]))
          (length parts)))

;; ============================================================
;; autocompletion-cursor<%> implementation

(define autocompletion-cursor%
  (class* object% (autocompletion-cursor<%>)
    
    (init-field word all-words)        
    
    (define-values (rnk max-count) (rank word))
    ;; this determines the fuzziness
    ;; if we set mx to +inf.0, we get just the prefix matches
    ;; if we set mx to 1000, we get just the matches somewhere in the word
    ;; this definition is fuzzier the more parts there are in the word
    (define mx (cond
                 [(<= max-count 2) max-count]
                 [(<= max-count 4) (- max-count 1)]
                 [else (- max-count 2)]))
    
    ;; all the possible completions for `word', in ranked order
    (define all-completions 
      (map car (sort
                ;; we don't use `rnk' as the key to avoid
                ;; constructing a huge list
                (for*/list ([w (in-list all-words)]
                            [r (in-value (rnk w))]
                            #:when (>= r mx))
                  (cons w r))
                (match-lambda** [((cons w r) (cons w* r*))
                                 (or (> r r*)
                                     ;; prefer shorter matches
                                     (< (string-length w) (string-length w*)))]))))
    
    (define all-completions-length (length all-completions))
    
    (define/public (narrow c)
      (new autocompletion-cursor%
           [word (string-append word (list->string (list c)))]
           [all-words all-words]))
    
    (define/public (widen)
      (let ([strlen (string-length word)])
        (cond
          [(< strlen 2) #f]
          [else 
           (new autocompletion-cursor%
                [word (substring word 0 (- (string-length word) 1))]
                [all-words all-words])])))
    
    (define/public (get-completions) all-completions)
    (define/public (get-length) all-completions-length)
    (define/public (empty?) (eq? (get-length) 0))
    
    (super-new)))
