#lang racket/base
(require racket/match
         setup/link
         racket/file
         racket/set
         racket/list
         racket/format
         "../path.rkt"
         "pkg-db.rkt"
         "collects.rkt"
         "params.rkt"
         "print.rkt"
         "get-info.rkt"
         "trash.rkt")

(provide remove-package
         pkg-remove)

(define (demote-packages quiet? pkg-names)
  (define db (read-pkg-db))
  (for ([pkg-name (in-list pkg-names)])
    (define pi (package-info pkg-name #:db db))
    (unless (pkg-info-auto? pi)
      (unless quiet?
        (printf/flush "Demoting ~a to auto-installed\n" pkg-name))
      (update-pkg-db! pkg-name (update-auto pi #t)))))

(define ((remove-package for-install? quiet? use-trash?) pkg-name)
  (unless quiet?
    (printf/flush "~a ~a\n"
                  (if for-install?
                      "Uninstalling to prepare re-install of"
                      "Removing")
                  pkg-name))
  (define db (read-pkg-db))
  (define pi (package-info pkg-name #:db db))
  (match-define (pkg-info orig-pkg checksum _) pi)
  (define pkg-dir (pkg-directory* pkg-name #:db db))
  (remove-from-pkg-db! pkg-name)
  (define scope (current-pkg-scope))
  (define user? (not (or (eq? scope 'installation)
                         (path? scope))))
  (match orig-pkg
    [`(,(or 'link 'static-link 'clone) ,_ . ,_)
     (links pkg-dir
            #:remove? #t
            #:user? user?
            #:file (scope->links-file scope)
            #:root? (not (sc-pkg-info? pi)))]
    [_
     (links pkg-dir
            #:remove? #t
            #:user? user?
            #:file (scope->links-file scope)
            #:root? (not (sc-pkg-info? pi)))
     (cond
      [(and use-trash?
            (select-trash-dest pkg-name))
       => (lambda (trash-dest)
            (printf/flush "Moving ~a to trash: ~a\n" pkg-name trash-dest)
            (rename-file-or-directory pkg-dir trash-dest))]
      [else
       (delete-directory/files pkg-dir)])]))
      

(define (pkg-remove given-pkgs
                    #:demote? [demote? #f]
                    #:force? [force? #f]
                    #:auto? [auto? #f]
                    #:quiet? [quiet? #f]
                    #:use-trash? [use-trash? #f]
                    #:from-command-line? [from-command-line? #f])
  (define db (read-pkg-db))
  (define all-pkgs
    (hash-keys db))
  (define all-pkgs-set
    (list->set all-pkgs))
  (define metadata-ns (make-metadata-namespace))
  (define in-pkgs (remove-duplicates given-pkgs))
  (define remove-pkgs
    (if auto?
        ;; compute fixpoint:
        (let ([init-drop (set-union
                          (list->set
                           (filter
                            (λ (p) (pkg-info-auto? (hash-ref db p)))
                            all-pkgs))
                          (list->set in-pkgs))])
          (let loop ([drop init-drop]
                     [keep (set-subtract
                            (list->set all-pkgs)
                            init-drop)])
            (define deps
              (list->set
               (append-map (package-dependencies metadata-ns db #t)
                           (set->list keep))))
            (define still-drop (set-subtract drop deps))
            (define delta (set-subtract drop still-drop))
            (if (set-empty? delta)
                (set->list drop)
                (loop still-drop
                      (set-union keep delta)))))
        ;; just given pkgs:
        (if demote?
            null
            in-pkgs)))
  (define setup-collects
    (get-setup-collects remove-pkgs
                        db
                        metadata-ns))
  (unless (or force? demote?)
    ;; Check dependencies on `in-pkgs' (not `pkgs', which has already
    ;; been filtered to remove package with dependencies if `auto?' is
    ;; true).
    (define pkgs-set (list->set in-pkgs))
    (define remaining-pkg-db-set
      (set-subtract all-pkgs-set
                    (if auto?
                        (list->set remove-pkgs)
                        pkgs-set)))
    (define deps-to-be-removed
      (set-intersect
       pkgs-set
       (list->set
        (append-map (package-dependencies metadata-ns db #t)
                    (set->list
                     remaining-pkg-db-set)))))    
    (unless (set-empty? deps-to-be-removed)
      (pkg-error (~a "cannot remove packages that are dependencies of other packages\n"
                     "  dependencies:~a")
                 (format-list 
                  (map
                   (λ (p)
                     (define ds
                       (filter (λ (dp)
                                 (member p ((package-dependencies metadata-ns db #t) dp)))
                               (set->list
                                remaining-pkg-db-set)))
                     (~a p " (required by: " ds ")"))
                   (set->list deps-to-be-removed))))))

  (when demote?
    ;; Demote any package that is not going to be removed:
    (demote-packages
     quiet?
     (set->list (set-subtract (list->set in-pkgs)
                              (list->set remove-pkgs)))))

  (for-each (remove-package #f quiet? use-trash?)
            remove-pkgs)

  (cond
   [(or (null? remove-pkgs) demote?)
    ;; Did nothing, so no setup:
    'skip]
   [else
    ;; setup only collections that still exist:
    (and setup-collects
         (for/list ([c (in-list setup-collects)]
                    #:when (apply collection-path
                                  (if (path-string? c) (list c) c)
                                  #:fail (lambda (s) #f)))
           c))]))

