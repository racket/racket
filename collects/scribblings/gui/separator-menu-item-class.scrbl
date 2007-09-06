#reader(lib "docreader.ss" "scribble")
@require["common.ss"]

@defclass/title[separator-menu-item% object% (menu-item<%>)]{

A separator is an unselectable line in a menu. Its parent must be a
 @scheme[menu%] or @scheme[popup-menu%].


@defconstructor[([parent (or/c (is-a?/c menu%) (is-a?/c popup-menu%))])]{

Creates a new separator in the menu.

}}

