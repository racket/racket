/*								-*- C++ -*-
 *
 * Purpose: simple menu class
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2010 PLT Scheme Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

#ifndef Menu_h
#define Menu_h

#ifdef __GNUG__
#pragma interface
#endif

#if defined(Have_Xt_Types)
class wxMenu_Widgets : public gc {
public:
    Widget shell;
    Widget menu;
};
#else
class wxMenu_Widgets;
#endif

class wxCommandEvent;
class wxColour;
class wxFont;

typedef void (*wxFunction)(wxObject*, wxCommandEvent*);

typedef void *wxMenuItem;

class wxMenu : public wxObject {
public:
    wxMenu(char *title=NULL, wxFunction func=NULL, wxFont *_font = NULL);
    ~wxMenu(void);

    // popup menu (used by wxWindow);
#   ifdef Have_Xt_Types
    Bool  PopupMenu(Widget in_w, int root_x, int root_y, Bool for_choice = FALSE, int top_extra = 0);
#   endif

    int Number(void);

    // add items to menu
    void  Append(long id, char *label, char *help=NULL, Bool checkable=FALSE);
    void  Append(long id, char *label, wxMenu *submenu, char *help=NULL);
    void  AppendSeparator(void);
    Bool  DeleteItem(long id, int pos);
    Bool  Delete(long id);
    Bool  DeleteByPosition(int pos);
    // modify items
    void  Break(void) {}; // not supported
    void  Check(long id, Bool flag);
    Bool  Checked(long id);
    void  Enable(long id, Bool flag);
    char  *GetHelpString(long id);
    char  *GetLabel(long id);
    char  *GetTitle(void);
    void  SetHelpString(long id, char *help);
    void  SetLabel(long id, char *label);
    void  SetTitle(char *title);
    wxFont *GetFont() { return font; }
    // miscellaneous
    void  *GetClientData(void) { return client_data; }
    void  SetClientData(void *data) { client_data = data; }
    // search for item by label
    int   FindItem(char *label, int strip = 1);

    void Stop(void);
    void Unpop(void);

    void SetWidth(int w);

private:
    // allow callback and menubar access to private data
    friend class wxMenuBar;

    wxMenuItem  *FindItemForId(long id, wxMenu **menu=NULL); // search for internal data
#   ifdef Have_Xt_Types
    static void EventCallback(Widget, XtPointer, XtPointer);
#   endif

    // necessary for popup menus
    wxMenu_Widgets* X;
    wxFunction callback;
    // font & colours
    wxFont*   font;
    // internal representation
    wxMenuItem* title;		// title, if specified with constructor
    wxMenuItem* top;		// first menu item
    wxMenuItem* last;		// last menu item for wxMenu::Append
    wxMenuItem* topdummy;
    wxMenuItem **owner; /* MATTHEW: Pointer to pointer to top */

#ifdef MZ_PRECISE_GC
    wxChildList *children;
#endif

    int requested_width;

    void *client_data;
    void **saferefs; /* cons-like chain of saferefs */
};

extern void wxInitPopupMgr(void);
extern void wxUnpopMenu(void);

#ifdef MZ_PRECISE_GC
extern char *copystring_xt(const char *s);
# define MALLOC_MENU_ITEM()      (menu_item *)XtMalloc(sizeof(menu_item))
# define FREE_MENU_ITEM(i)       XtFree((char *)i)
# define MAKE_MENU_STRING(s)     copystring_xt(s)
# define FREE_MENU_STRING(s)     XtFree((char *)s)
# define EXTRACT_TOP_MENU(item)  ((wxMenu*)GET_SAFEREF(item->user_data))
# define BUNDLE_TOP_MENU(menu)   GC_malloc_immobile_box(GC_malloc_weak_box(gcOBJ_TO_PTR(menu), NULL, 0))
# define FREE_TOP_POINTER(p)     GC_free_immobile_box((void **)p)
#else
# define MALLOC_MENU_ITEM()      (new WXGC_PTRS menu_item)
# define FREE_MENU_ITEM(i)       /* nothing */
# define MAKE_MENU_STRING(s)     s
# define FREE_MENU_STRING(s)     /* nothing */
# define EXTRACT_TOP_MENU(item)  ((wxMenu*)(item->user_data))
# define BUNDLE_TOP_MENU(menu)   ((void*)menu)
# define FREE_TOP_POINTER(p)     /* nothing */
#endif

#endif // Menu_h
