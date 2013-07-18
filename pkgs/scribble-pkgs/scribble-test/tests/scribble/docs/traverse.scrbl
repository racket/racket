#lang scribble/base
@(require scribble/core)

@(traverse-element
  (lambda (get set)
   ;; Not delayed, and so result is "not ready, yet"
   (get 'glossary "not ready, yet")))

@(traverse-element
  (lambda (get set)
   ;; Delayed until second traversal:
   (lambda (get set)
    (get 'glossary "BROKEN"))))

@; Same thing, but with blocks:

@(traverse-block
  (lambda (get set)
    ;; Not delayed:
    (para "The glossary is " 
          (get 'glossary "not ready, yet"))))

@(traverse-block
  (lambda (get set)
    ;; Delayed:
    (lambda (get set)
      (para "The glossary is " 
            (get 'glossary "BROKEN")))))

@(traverse-block
  (lambda (get set)
    ;; Adding a `delayed-block' doesn't delay:
    (traverse-block
      (lambda (get set)
        (para "The glossary is "
              (get 'glossary "not ready, yet"))))))

@(traverse-block
  (lambda (get set)
    (set 'glossary "ready")
    (para "Here is the glossary.")))
