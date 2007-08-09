#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[separator-menu-item% object% (menu-item<%>)]{

A separator is an unselectable line in a menu. Its parent must be a
 @scheme[menu%] or @scheme[popup-menu%].




@defconstructor[[parent @scheme[menu%] or @scheme[popup-menu%] object]]{

Creates a new separator in the menu.



}}

