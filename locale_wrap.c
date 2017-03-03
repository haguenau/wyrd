/*  Wyrd -- a curses-based front-end for Remind
 *  Copyright (C) 2005, 2006, 2007 Paul Pelzl
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License, Version 2,
 *  as published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Please send bug reports, patches, etc. to Paul Pelzl at 
 *  <pelzlpj@eecs.umich.edu>.
 */


/* OCaml binding for setlocale(), which is required to kick ncurses
 * into rendering non-ASCII chars. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <locale.h>

value ml_setlocale(value category, value setting)
{
   CAMLparam2(category, setting);
   CAMLlocal1(result);

   result = caml_copy_string(setlocale(Int_val(category), String_val(setting)));
   CAMLreturn(result);
}


