#lang racket/base
(require ffi/unsafe
	 ffi/unsafe/define
         racket/class
         (only-in racket/list take drop)
          "../../syntax.rkt"
          "../../lock.rkt"
         "item.rkt"
         "utils.rkt"
         "types.rkt"
         "window.rkt"
         "const.rkt"
         "../common/event.rkt")

(provide 
 (protect-out list-box%))

;; ----------------------------------------

(define-cstruct _GtkTreeIter ([stamp _int]
                              [user_data _pointer]
                              [user_data2 _pointer]
                              [user_data3 _pointer]))

(define _GtkListStore (_cpointer 'GtkListStore))
(define _GtkCellRenderer (_cpointer 'GtkCellRenderer))
(define _GtkTreeViewColumn _GtkWidget) ; (_cpointer 'GtkTreeViewColumn)

(define GTK_SELECTION_SINGLE 1)
(define GTK_SELECTION_MULTIPLE 3)

(define GTK_TREE_VIEW_COLUMN_AUTOSIZE 1)
(define GTK_TREE_VIEW_COLUMN_FIXED 2)

(define-gtk gtk_scrolled_window_new (_fun _pointer _pointer -> _GtkWidget))
(define-gtk gtk_scrolled_window_set_policy (_fun _GtkWidget _int _int -> _void))

(define-gtk gtk_list_store_newv (_fun _int (_list i _long) -> _GtkListStore))
(define-gtk gtk_list_store_clear (_fun _GtkListStore -> _void))
(define-gtk gtk_list_store_append (_fun _GtkListStore _GtkTreeIter-pointer _pointer -> _void))
(define-gtk gtk_list_store_set (_fun _GtkListStore _GtkTreeIter-pointer _int _string _int -> _void))
(define-gtk gtk_tree_view_new_with_model (_fun _GtkListStore -> _GtkWidget))
(define-gtk gtk_tree_view_set_model (_fun _GtkWidget _GtkListStore -> _void))
(define-gtk gtk_tree_view_set_headers_visible (_fun _GtkWidget _gboolean -> _void))
(define-gtk gtk_cell_renderer_text_new (_fun -> _GtkCellRenderer))
(define-gtk gtk_tree_view_column_new_with_attributes (_fun _string _GtkCellRenderer _string _int _pointer -> _GtkTreeViewColumn))
(define-gtk gtk_tree_view_column_set_attributes (_fun _GtkTreeViewColumn _GtkCellRenderer _string _int _pointer -> _void))
(define-gtk gtk_tree_view_column_set_resizable (_fun _GtkTreeViewColumn _gboolean -> _void))
(define-gtk gtk_tree_view_column_set_clickable (_fun _GtkTreeViewColumn _gboolean -> _void))
(define-gtk gtk_tree_view_column_set_reorderable (_fun _GtkTreeViewColumn _gboolean -> _void))
(define-gtk gtk_tree_view_append_column (_fun _GtkWidget _GtkTreeViewColumn -> _void))
(define-gtk gtk_tree_view_remove_column (_fun _GtkWidget _GtkTreeViewColumn -> _void))
(define-gtk gtk_tree_view_get_selection (_fun _GtkWidget -> _GtkWidget))
(define-gtk gtk_tree_selection_set_mode (_fun _GtkWidget _int -> _void))
(define-gtk gtk_list_store_remove (_fun _GtkListStore _GtkTreeIter-pointer -> _gboolean))
(define-gtk gtk_tree_model_get_iter (_fun _GtkListStore _GtkTreeIter-pointer _pointer -> _gboolean))
(define-gtk gtk_tree_view_scroll_to_cell (_fun _GtkWidget _pointer _pointer _gboolean _gfloat _gfloat -> _void))
(define-gtk gtk_tree_view_get_column (_fun _GtkWidget _int -> _GtkTreeViewColumn))
(define-gtk gtk_tree_view_move_column_after (_fun _GtkWidget _GtkTreeViewColumn (_or-null _GtkTreeViewColumn) -> _void))
(define-gtk gtk_tree_view_column_set_title  (_fun _GtkTreeViewColumn _string -> _void))
(define-gtk gtk_tree_view_column_set_sizing (_fun _GtkTreeViewColumn _int -> _void))
(define-gtk gtk_tree_view_column_get_width (_fun _GtkTreeViewColumn -> _int))
(define-gtk gtk_tree_view_column_get_min_width (_fun _GtkTreeViewColumn -> _int))
(define-gtk gtk_tree_view_column_get_max_width (_fun _GtkTreeViewColumn -> _int))
(define-gtk gtk_tree_view_column_set_fixed_width (_fun _GtkTreeViewColumn _int -> _void))
(define-gtk gtk_tree_view_column_set_min_width (_fun _GtkTreeViewColumn _int -> _void))
(define-gtk gtk_tree_view_column_set_max_width (_fun _GtkTreeViewColumn _int -> _void))

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
      (when wx
        (send wx queue-changed)))))

(define-signal-handler connect-clicked "clicked"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx column-clicked gtk)))))

(define-signal-handler connect-activated "row-activated"
  (_fun _GtkWidget _pointer _pointer -> _void)
  (lambda (gtk path column)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx queue-activated)))))

(defclass list-box% item%
  (init parent cb
        label kind x y w h
        choices style
        font label-font
        columns
        column-order)
  (inherit get-gtk set-auto-size is-window-enabled?)

  (define empty-columns (for/list ([l (in-list (cdr columns))])
                          ""))
  (define itemss (for/list ([i (in-list choices)])
                   (cons i empty-columns)))
  
  (define data (map (lambda (c) (box #f)) choices))

  
  (define (make-store count)
    (as-gobject-allocation 
     (gtk_list_store_newv count
                          (for/list ([i (in-range count)])
                            G_TYPE_STRING))))
  (define store (make-store (length columns)))

  (define (reset-content)
    (let ([iter (make-GtkTreeIter 0 #f #f #f)])
      (for ([items (in-list itemss)])
        (gtk_list_store_append store iter #f)
        (for ([item (in-list items)]
              [col (in-naturals 0)])
          (gtk_list_store_set store iter col item -1))))
    (maybe-init-select))

  (define/private (maybe-init-select)
    ;; For consistency with other platforms,
    ;; don't try to select an item initially.
    (when #f
      (when (and (= (get-selection) -1)
                 (pair? data))
        (set-selection 0))))
    
  (define gtk (as-gtk-allocation (gtk_scrolled_window_new #f #f)))
  (gtk_scrolled_window_set_policy gtk GTK_POLICY_AUTOMATIC GTK_POLICY_ALWAYS)

  (define headers? (memq 'column-headers style))
  (define click-headers? (and headers?
                              (memq 'clickable-headers style)))
  (define reorder-headers? (and headers?
                                (memq 'reorderable-headers style)))

  (define renderer (gtk_cell_renderer_text_new))

  (define/private (make-column label col)
    (let* ([column
            (gtk_tree_view_column_new_with_attributes
             label
             renderer
             "text"
             col
             #f)])
      (when headers?
        (gtk_tree_view_column_set_resizable column #t)
        (gtk_tree_view_column_set_min_width column 1)
        (when click-headers?
          (gtk_tree_view_column_set_clickable column #t))
        (when reorder-headers?
          (gtk_tree_view_column_set_reorderable column #t)))
      column))

  (define-values (client-gtk column-gtks)
    (atomically
     (let* ([client-gtk (gtk_tree_view_new_with_model store)]
            [columns (for/list ([label (in-list columns)]
                                [col (in-naturals)])
                       (make-column label col))])
       (gobject-unref store)
       (unless headers?
         (gtk_tree_view_set_headers_visible client-gtk #f))
       (for ([column (in-list columns)])
         (gtk_tree_view_append_column client-gtk column))
       (values client-gtk columns))))

  (when column-order
    (set-column-order column-order))
  (define/public (set-column-order column-order)
    (let loop ([prev #f] [l column-order])
      (unless (null? l)
        (let ([column-gtk (list-ref column-gtks (car l))])
          (gtk_tree_view_move_column_after client-gtk column-gtk prev)
          (loop column-gtk (cdr l))))))

  (gtk_container_add gtk client-gtk)
  (gtk_widget_show client-gtk)

  (define selection
    (gtk_tree_view_get_selection client-gtk))

  (gtk_tree_selection_set_mode selection (if (or (eq? kind 'extended)
                                                 (eq? kind 'multiple))
                                             GTK_SELECTION_MULTIPLE
                                             GTK_SELECTION_SINGLE))

  (super-new [parent parent]
             [gtk gtk]
             [extra-gtks (list* client-gtk selection
                                (if (memq 'clickable-headers style)
                                    column-gtks
                                    null))]
             [callback cb]
             [font font]
             [no-show? (memq 'deleted style)])

  (set-auto-size 32) ; 32 is extra width

  (connect-changed selection)
  (connect-activated client-gtk)
  (for ([column (in-list column-gtks)])
    (column-finish column))

  (define/private (column-finish column)
    (connect-clicked column)
    (let ([w (gtk_tree_view_column_get_width column)])
      (gtk_tree_view_column_set_sizing column GTK_TREE_VIEW_COLUMN_FIXED)
      (gtk_tree_view_column_set_fixed_width column (max 50 w))))

  (define/override (get-client-gtk) client-gtk)

  (define callback cb)
  (define ignore-click? #f)
  (define/private (do-queue-changed type)
    ;; Called from event-handling thread
    (unless ignore-click?
      (queue-window-event
       this
       (lambda ()
         (unless (null? itemss)
           (callback this (new control-event%
                               [event-type type]
                               [time-stamp (current-milliseconds)])))))))

  (define/public (queue-changed)
    (do-queue-changed 'list-box))

  (define/public (queue-activated)
    (do-queue-changed 'list-box-dclick))

  (define/private (column->pos col)
    (let loop ([l column-gtks]
               [pos 0])
      (cond
       [(null? l) #f]
       [(ptr-equal? (car l) col) pos]
       [else (loop (cdr l) (add1 pos))])))

  (define/public (column-clicked col)
    (let ([pos (column->pos col)])
      (when pos
        (queue-window-event
         this
         (lambda ()
           (callback this (new column-control-event%
                               [event-type 'list-box-column]
                               [column pos]
                               [time-stamp (current-milliseconds)])))))))

  (define/public (get-column-order)
    (for/list ([i (in-range (length column-gtks))])
      (column->pos (gtk_tree_view_get_column client-gtk i))))

  (define/private (get-iter i)
    (atomically
     (let ([iter (make-GtkTreeIter 0 #f #f #f)]
           [p (gtk_tree_path_new_from_indices i -1)])
       (gtk_tree_model_get_iter store iter p)
       (gtk_tree_path_free p)
       iter)))

  (def/public-unimplemented get-label-font)

  (define/private (replace-nth items i s)
    (append (take items i)
            (list s)
            (drop items (add1 i))))

  (define/public (set-string i s [col 0])
    (set! itemss
          (replace-nth itemss
                       i
                       (replace-nth (list-ref itemss i)
                                    col
                                    s)))
    (gtk_list_store_set store (get-iter i) col s -1))

  (define/public (set-column-label i s)
    (gtk_tree_view_column_set_title (list-ref column-gtks i) s))

  (define/public (set-column-size i w mn mx)
    (let ([col (list-ref column-gtks i)])
      (gtk_tree_view_column_set_min_width col mn)
      (gtk_tree_view_column_set_max_width col mx)
      (gtk_tree_view_column_set_fixed_width col w)))

  (define/public (get-column-size i)
    (let ([col (list-ref column-gtks i)])
      (values
       (gtk_tree_view_column_get_width col)
       (max (gtk_tree_view_column_get_min_width col) 0)
       (let ([v (gtk_tree_view_column_get_max_width col)])
         (if (negative? v)
             10000
             v)))))

  (define/public (set-first-visible-item i)
    (atomically
     (let ([p (gtk_tree_path_new_from_indices i -1)])
       (gtk_tree_view_scroll_to_cell client-gtk p #f #t 0.0 0.0)
       (gtk_tree_path_free p))))

  (define/public (set choices . more-choices)
    (atomically
     (set! ignore-click? #t)
     (clear)
     (set! itemss (apply map 
                         (lambda (i . rest)
                           (cons i rest))
                         choices
                         more-choices))
     (set! data (map (lambda (x) (box #f)) choices))
     (reset-content)
     (set! ignore-click? #f)))

  (define/public (get-selections)
    (atomically
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
           null))))
  (define/public (get-selection)
    (let ([l (get-selections)])
      (if (null? l)
          -1
          (car l))))

  (define/private (get-visible-range)
    (atomically
     (let-values ([(sp ep) (gtk_tree_view_get_visible_range client-gtk)])
       (begin0
        (values (if sp (ptr-ref (gtk_tree_path_get_indices sp) _int) 0)
                (if ep (ptr-ref (gtk_tree_path_get_indices ep) _int) 0))
        (when sp (gtk_tree_path_free sp))
        (when ep (gtk_tree_path_free ep))))))

  (define/public (get-first-item)
    (let-values ([(start end) (get-visible-range)])
      start))
  (define/public (number-of-visible-items)
    (let-values ([(start end) (get-visible-range)])
      (add1 (- end start))))

  (define/public (number) (length itemss))

  (define/public (set-data i v) (set-box! (list-ref data i) v))
  (define/public (get-data i) (unbox (list-ref data i)))

  (define/public (selected? i)
    (atomically
     (let ([p (gtk_tree_path_new_from_indices i -1)])
       (begin0
        (gtk_tree_selection_path_is_selected selection p)
        (gtk_tree_path_free p)))))

  (define/public (select i [on? #t] [extend? #t])
    (atomically
     (set! ignore-click? #t)
     (let ([p (gtk_tree_path_new_from_indices i -1)])
       (if on?
           (begin
             (unless extend?
               (gtk_tree_selection_unselect_all selection))
             (gtk_tree_selection_select_path selection p))
           (gtk_tree_selection_unselect_path selection p))
       (gtk_tree_path_free p))
     (set! ignore-click? #f)))

  (define/public (set-selection i)
    (select i #t #f))

  (define/public (delete i)
    (set! itemss (append (take itemss i) (drop itemss (add1 i))))
    (set! data (append (take data i) (drop data (add1 i))))
    (gtk_list_store_remove store (get-iter i))
    (void))

  (define/public (clear)
    (set! itemss null)
    (set! data null)
    (gtk_list_store_clear store))

  (public [append* append])
  (define (append* s [v #f])
    (atomically
     (set! ignore-click? #t)
     (set! itemss (append itemss 
                          (list (cons s empty-columns))))
     (set! data (append data (list (box v))))
     (let ([iter (make-GtkTreeIter 0 #f #f #f)])
       (gtk_list_store_append store iter #f)
       (gtk_list_store_set store iter 0 s -1))
     (maybe-init-select)
     (set! ignore-click? #f)))

  (define/public (append-column label)
    (let ([col (add1 (length empty-columns))])
      (set! store (make-store (add1 col)))
      (set! empty-columns (cons "" empty-columns))
      (set! itemss
            (for/list ([items (in-list itemss)])
              (append items (list ""))))
      (gtk_tree_view_set_model client-gtk store)
      (let ([renderer (gtk_cell_renderer_text_new)])
        (gtk_tree_view_column_new_with_attributes
         label
         renderer
         "text"
         col
         #f))
      (let ([column-gtk (make-column label col)])
        (g_object_set_data column-gtk "wx" (g_object_get_data client-gtk "wx"))
        (set! column-gtks (append column-gtks (list column-gtk)))
        (gtk_tree_view_append_column client-gtk column-gtk)
        (reset-content)
        (column-finish column-gtk))))

  (define/public (delete-column i)
    (define (remove-nth l i)
      (cond
       [(zero? i) (cdr l)]
       [else (cons (car l) (remove-nth (cdr l) (sub1 i)))]))
    (set! empty-columns (cdr empty-columns))
    (set! itemss
          (for/list ([items (in-list itemss)])
            (remove-nth items i)))
    (let ([old (list-ref column-gtks i)])
      (set! column-gtks (remove-nth column-gtks i))
      (gtk_tree_view_remove_column client-gtk old))
    (for ([column-gtk (in-list column-gtks)]
          [pos (in-naturals)])
      (when (pos . >= . i)
        (gtk_tree_view_column_set_attributes column-gtk
                                             renderer
                                             "text"
                                             pos
                                             #f)))
    (gtk_list_store_clear store)
    (reset-content))

  (atomically (reset-content)))
