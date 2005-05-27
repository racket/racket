/*								-*- C++ -*-
 *
 * Purpose: file load and save dialogs
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

#include "wx.h"

extern char *wxsFileDialog(char *message, char *default_path, 
			   char *default_filename, char *default_extension, 
			   int is_put, wxWindow *parent);

//-----------------------------------------------------------------------------
// wxFileSelector
//-----------------------------------------------------------------------------
char *wxFileSelector(char *message, char *default_path,
		     char *default_filename, char *default_extension,
		     char *WXUNUSED(wildcard), int flags, wxWindow *parent, int, int)
{
  return wxsFileDialog(message, default_path, default_filename, default_extension, 
		       (flags & wxSAVE), parent);
}

char *wxLoadFileSelector(char *WXUNUSED(what), char *extension, char *default_name,
			 wxWindow *parent)
{
    char wild[60];
    if (*extension == '.')  ++extension;
    sprintf(wild, "*.%s", extension);
    return wxFileSelector("Load file", NULL, default_name,
			  (char*)extension, wild, 0, parent, 0, 0);
}

char *wxSaveFileSelector(char *WXUNUSED(what), char *extension, char *default_name,
			 wxWindow *parent)
{
    char wild[60];
    if (*extension == '.')  ++extension;
    sprintf(wild, "*.%s", extension);
    return wxFileSelector("Save file", NULL, default_name,
			  (char*)extension, wild, wxSAVE, parent, 0, 0);
}
