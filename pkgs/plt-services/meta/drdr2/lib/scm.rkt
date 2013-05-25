#lang racket/base
(require racket/list
         racket/contract)

(struct git-push (num author previous-commit commits) #:prefab)
(struct git-commit (hash author date msg) #:prefab)
(struct git-diff git-commit (mfiles) #:prefab)
(struct git-merge git-commit (from to) #:prefab)

(provide/contract
 [struct git-push 
         ([num exact-nonnegative-integer?]
          [author string?]
          [previous-commit string?]
          [commits (listof git-commit?)])]
 [struct git-commit 
         ([hash string?]
          [author string?]
          [date string?]
          [msg (listof string?)])]
 [struct git-diff 
         ([hash string?]
          [author string?]
          [date string?]
          [msg (listof string?)]
          [mfiles (listof string?)])]
 [struct git-merge 
         ([hash string?]
          [author string?]
          [date string?]
          [msg (listof string?)]
          [from string?]
          [to string?])])
  
(define (git-push-start-commit gp)
  (git-commit-hash (last (git-push-commits gp))))
(define (git-push-end-commit gp)
  (git-commit-hash (first (git-push-commits gp))))
(provide/contract
 [git-push-start-commit (git-push? . -> . string?)]
 [git-push-end-commit (git-push? . -> . string?)])
