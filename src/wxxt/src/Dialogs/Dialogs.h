/*								-*- C++ -*-
 *
 * Purpose: common dialogs
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2005 PLT Scheme, Inc.
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef wxDialogs_h
#define wxDialogs_h

#ifdef __GNUG__
#pragma interface
#endif

class wxWindow;

int wxMessageBox(
    char *message, char *caption = "Message", long style = wxOK|wxCENTRE,
    wxWindow *parent = NULL, int x = -1, int y = -1);

char *wxFileSelector(char *message, char *default_path,
		     char *default_filename, char *default_extension,
		     char *WXUNUSED(wildcard), int flags, wxWindow *parent, int, int);

char *wxLoadFileSelector(char *WXUNUSED(what), char *extension = NULL, char *default_name = NULL,
			 wxWindow *parent = NULL);
char *wxSaveFileSelector(char *WXUNUSED(what), char *extension = NULL, char *default_name = NULL,
			 wxWindow *parent = NULL);

#endif // wxDialogs_h
