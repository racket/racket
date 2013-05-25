#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Menu}

@definterface[menu:can-restore<%> (selectable-menu-item<%>)]{
  Classes created with this mixin remember their keybindings so the keybindings
  can be removed and then restored.

  @defmethod*[(((restore-keybinding) void?))]{
    Sets the keyboard shortcut to the setting it had when the class was
    created.
  }
}

@defmixin[menu:can-restore-mixin (selectable-menu-item<%>) (menu:can-restore<%>)]{
}

@definterface[menu:can-restore-underscore<%> (labelled-menu-item<%>)]{
  These menus can save and restore the underscores (indicated via the
  @litchar{&} characters in the original labels) in their labels.

  If the preference @racket['framework:menu-bindings] is @racket[#f], calls
  @method[menu:can-restore-underscore<%> erase-underscores] during
  initialization.

  @defmethod*[(((erase-underscores) void?))]{
    Erases the underscores in the label of this menu, but remembers them so
    they can be restores with @method[menu:can-restore-underscore<%>
    restore-underscores].
  }

  @defmethod*[(((restore-underscores) void?))]{
    Restores underscores in the menu's label to their original state.
  }
}

@defmixin[menu:can-restore-underscore-mixin (labelled-menu-item<%>) (menu:can-restore-underscore<%>)]{
}

@defclass[menu:can-restore-menu-item% (menu:can-restore-mixin menu-item%) ()]{}
@defclass[menu:can-restore-checkable-menu-item% (menu:can-restore-mixin checkable-menu-item%) ()]{}
@defclass[menu:can-restore-underscore-menu% (menu:can-restore-underscore-mixin menu%) ()]{}

@(include-previously-extracted "main-extracts.rkt" #rx"^menu:")
