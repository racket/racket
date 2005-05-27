 /*								-*- C++ -*-
 * $Id: FontDirectory.h,v 1.8 2005/01/01 14:57:11 eli Exp $
 *
 * Purpose: wxWindows font name handling
 *
 * Authors: Markus Holzem, Julian Smart, and Matthew Flatt
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

#ifndef FontDirectory_h
#define FontDirectory_h

#ifdef __GNUG__
# ifndef wx_mac
#  pragma interface
# endif
#endif

class wxHashTable;

class wxFontNameDirectory : public wxObject
{
  wxHashTable *table;
  int nextFontId;
 public:
  wxFontNameDirectory(void);
  ~wxFontNameDirectory();
  char *GetScreenName(int fontid, int weight, int style);
  char *GetPostScriptName(int fontid, int weight, int style);
  void SetScreenName(int fontid, int weight, int style, char *s);
  void SetPostScriptName(int fontid, int weight, int style, char *s);

  void Initialize(int fontid, int family, const char *name);
  int GetNewFontId(void);
  
  int FindOrCreateFontId(const char *name, int family);

  int GetFontId(const char *name, int family);
  char *GetFontName(int fontid);
  int GetFamily(int fontid);
};

extern wxFontNameDirectory *wxTheFontNameDirectory;

void wxInitializeFontNameDirectory(void);

#endif /* FontDirectory_h */
