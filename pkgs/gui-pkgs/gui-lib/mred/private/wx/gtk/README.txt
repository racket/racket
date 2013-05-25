
Allocation rules:

 * Use `as-gtk-allocation' when creating a Gtk widget that is the main
   container for a given window<%> object. When the resulting
   reference becomes unreachable, the widget will be released with
   gtk_widget_destroy() through a finalizer.

 * Use `atomically' to create and attach a sub-widget within the main
   widget.  Don't use gtk_widget_destroy(); the containing widget will
   destroy the enclosing widget.

 * For temporary objects, use `atomically' to wrap both the allocation
   and release.

Every call to a function whose name contains "new" needs to be in one
of those cases.
