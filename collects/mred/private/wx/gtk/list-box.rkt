#lang scheme/base
(require ffi/unsafe
	 ffi/unsafe/define
         scheme/class
         (only-in racket/list take drop)
          "../../syntax.rkt"
          "../../lock.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "window.rkt"
         "const.rkt"
         "../common/event.rkt")

(provide list-box%)

;; ----------------------------------------

(define-cstruct _GtkTreeIter ([stamp _int]
                              [user_data _pointer]
                              [user_data2 _pointer]
                              [user_data3 _pointer]))

(define _GtkListStore (_cpointer 'GtkListStore))
(define _GtkCellRenderer (_cpointer 'GtkCellRenderer))
(define _GtkTreeViewColumn _GtkWidget) ; (_cpointer 'GtkTreeViewColumn)

(define-gtk gtk_scrolled_window_new (_fun _pointer _pointer -> _GtkWidget))
(define-gtk gtk_scrolled_window_set_policy (_fun _GtkWidget _int _int -> _void))

(define-gtk gtk_list_store_new (_fun _int _int -> _GtkListStore))
(define-gtk gtk_list_store_clear (_fun _GtkListStore -> _void))
(define-gtk gtk_list_store_append (_fun _GtkListStore _GtkTreeIter-pointer _pointer -> _void))
(define-gtk gtk_list_store_set (_fun _GtkListStore _GtkTreeIter-pointer _int _string _int -> _void))
(define-gtk gtk_tree_view_new_with_model (_fun _GtkListStore -> _GtkWidget))
(define-gtk gtk_tree_view_set_headers_visible (_fun _GtkWidget _gboolean -> _void))
(define-gtk gtk_cell_renderer_text_new (_fun -> _GtkCellRenderer))
(define-gtk gtk_tree_view_column_new_with_attributes (_fun _string _GtkCellRenderer _string _int _pointer -> _GtkTreeViewColumn))
(define-gtk gtk_tree_view_append_column (_fun _GtkWidget _GtkTreeViewColumn -> _void))
(define-gtk gtk_tree_view_get_selection (_fun _GtkWidget -> _GtkWidget))
(define-gtk gtk_list_store_remove (_fun _GtkListStore _GtkTreeIter-pointer -> _gboolean))
(define-gtk gtk_tree_model_get_iter (_fun _GtkListStore _GtkTreeIter-pointer _pointer -> _gboolean))
(define-gtk gtk_tree_view_scroll_to_cell (_fun _GtkWidget _pointer _pointer _gboolean _gfloat _gfloat -> _void))

(define _GList (_cpointer 'List))
(define-glib g_list_foreach (_fun _GList (_fun _pointer -> _void) _pointer -> _void))
(define-glib g_list_free (_fun _GList -> _void))
(define-gtk gtk_tree_selection_get_selected_rows (_fun _GtkWidget _pointer -> (_or-null _GList)))
(define-gtk gtk_tree_selection_path_is_selected (_fun _GtkWidget _pointer -> _gboolean))
(define-gtk gtk_tree_selection_unselect_all (_fun  _GtkWidget -> _void))
(define-gtk gtk_tree_selection_select_path (_fun  _GtkWidget _pointer -> _void))
(define-gtk gtk_tree_selection_unselect_path (_fun  _GtkWidget _pointer -> _void))
(define-gtk gtk_tree_path_new_from_indices (_fun _int _int -> _pointer))
(define-gtk gtk_tree_path_free (_fun _pointer -> _void))
(define-gtk gtk_tree_path_get_indices (_fun _pointer -> _pointer))

(define-gtk gtk_tree_view_get_visible_range (_fun _GtkWidget [sp : (_ptr o _pointer)] [ep : (_ptr o _pointer)]
                                                  -> [ok? : _gboolean]
                                                  -> (values (if ok? sp #f) (if ok? ep #f))))

(define-signal-handler connect-changed "changed"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (send wx queue-changed))))

(defclass list-box% item%
  (init parent cb
        label kind x y w h
        choices style
        font label-font)
  (inherit get-gtk set-auto-size is-window-enabled?)

  (define items choices)
  (define data (map (lambda (c) (box #f)) choices))

  (define store (gtk_list_store_new 1 G_TYPE_STRING))
  (define (reset-content)
    (let ([iter (make-GtkTreeIter 0 #f #f #f)])
      (for ([s (in-list items)])
        (gtk_list_store_append store iter #f)
        (gtk_list_store_set store iter 0 s -1)))
    (maybe-init-select))

  (define/private (maybe-init-select)
    (when (and (= (get-selection) -1)
               (pair? data))
      (set-selection 0)))
    
  (define column
    (let ([renderer (gtk_cell_renderer_text_new)])
      (gtk_tree_view_column_new_with_attributes
       "column"
       renderer
       "text"
       0
       #f)))

  (define gtk (gtk_scrolled_window_new #f #f))
  (gtk_scrolled_window_set_policy gtk GTK_POLICY_NEVER GTK_POLICY_ALWAYS)

  (define client-gtk
    (let* ([client-gtk (gtk_tree_view_new_with_model store)])
      (gtk_tree_view_set_headers_visible client-gtk #f)
      (gtk_tree_view_append_column client-gtk column)
      client-gtk))

  (gtk_container_add gtk client-gtk)
  (gtk_widget_show client-gtk)

  (define selection
    (gtk_tree_view_get_selection client-gtk))

  (super-new [parent parent]
             [gtk gtk]
             [extra-gtks (list client-gtk selection)]
             [callback cb]
             [no-show? (memq 'deleted style)])

  (set-auto-size)

  (connect-changed selection)

  (define/override (get-client-gtk) client-gtk)

  (define callback cb)
  (define ignore-click? #f)
  (define/public (queue-changed)
    (make-will-executor)
    ;; Called from event-handling thread
    (unless ignore-click?
      (queue-window-event
       this
       (lambda ()
         (unless (null? items)
           (callback this (new control-event%
                               [event-type 'list-box]
                               [time-stamp (current-milliseconds)])))))))

  (define/private (get-iter i)
    (let ([iter (make-GtkTreeIter 0 #f #f #f)]
          [p (gtk_tree_path_new_from_indices i -1)])
      (gtk_tree_model_get_iter store iter p)
      (gtk_tree_path_free p)
      iter))

  (def/public-unimplemented get-label-font)

  (define/public (set-string i s)
    (set! items
          (append (take items i)
                  (list s)
                  (drop items (add1 i))))
    (gtk_list_store_set store (get-iter i) 0 s -1))

  (define/public (set-first-visible-item i)
    (let ([p (gtk_tree_path_new_from_indices i -1)])
      (gtk_tree_view_scroll_to_cell client-gtk p #f #t 0.0 0.0)
      (gtk_tree_path_free p)))

  (define/public (set choices)
    (as-entry
     (lambda ()
       (set! ignore-click? #t)
       (clear)
       (set! items choices)
       (set! data (map (lambda (x) (box #f)) choices))
       (reset-content)
       (set! ignore-click? #f))))

  (define/public (get-selections)
    (as-entry
     (lambda ()
       (let ([list (gtk_tree_selection_get_selected_rows selection #f)])
         (if list
             (let ([v null])
               (g_list_foreach list 
                               (lambda (t) 
                                 (set! v (cons (ptr-ref (gtk_tree_path_get_indices t) _int)
                                               v)))
                               #f)
               (g_list_foreach list gtk_tree_path_free #f)
               (g_list_free list)
               (reverse v))
             null)))))
  (define/public (get-selection)
    (let ([l (get-selections)])
      (if (null? l)
          -1
          (car l))))

  (define/private (get-visible-range)
    (as-entry
     (lambda ()
       (let-values ([(sp ep) (gtk_tree_view_get_visible_range client-gtk)])
         (begin0
          (values (if sp (ptr-ref (gtk_tree_path_get_indices sp) _int) 0)
                  (if ep (ptr-ref (gtk_tree_path_get_indices ep) _int) 0))
          (when sp (gtk_tree_path_free sp))
          (when ep (gtk_tree_path_free ep)))))))

  (define/public (get-first-item)
    (let-values ([(start end) (get-visible-range)])
      start))
  (define/public (number-of-visible-items)
    (let-values ([(start end) (get-visible-range)])
      (add1 (- end start))))

  (define/public (number) (length items))

  (define/public (set-data i v) (set-box! (list-ref data i) v))
  (define/public (get-data i) (unbox (list-ref data i)))

  (define/public (selected? i)
    (let ([p (gtk_tree_path_new_from_indices i -1)])
      (begin0
       (gtk_tree_selection_path_is_selected selection p)
       (gtk_tree_path_free p))))

  (define/public (select i [on? #t] [extend? #t])
    (as-entry
     (lambda ()
       (set! ignore-click? #t)
       (let ([p (gtk_tree_path_new_from_indices i -1)])
         (if on?
             (begin
               (unless extend?
                 (gtk_tree_selection_unselect_all selection))
               (gtk_tree_selection_select_path selection p))
             (gtk_tree_selection_unselect_path selection p))
         (gtk_tree_path_free p))
       (set! ignore-click? #f))))

  (define/public (set-selection i)
    (select i #t #f))

  (define/public (delete i)
    (set! items (append (take items i) (drop items (add1 i))))
    (set! data (append (take data i) (drop data (add1 i))))
    (gtk_list_store_remove store (get-iter i))
    (void))

  (define/public (clear)
    (set! items null)
    (set! data null)
    (gtk_list_store_clear store))

  (public [append* append])
  (define (append* s [v #f])
    (as-entry
     (lambda ()
       (set! ignore-click? #t)
       (set! items (append items (list s)))
       (set! data (append data (list (box v))))
       (let ([iter (make-GtkTreeIter 0 #f #f #f)])
         (gtk_list_store_append store iter #f)
         (gtk_list_store_set store iter 0 s -1))
       (maybe-init-select)
       (set! ignore-click? #f))))

  (reset-content))
