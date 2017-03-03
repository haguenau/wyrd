(*  Wyrd -- a curses-based front-end for Remind
 *  Copyright (C) 2005, 2006, 2007, 2008, 2010, 2011-2013 Paul Pelzl
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
 *  Bug reports can be entered at http://bugs.launchpad.net/wyrd .
 *  For anything else, feel free to contact Paul Pelzl at <pelzlpj@gmail.com>.
 *)

open Interface;;
open Curses;;


(* Load run configuration file *)
Rcfile.process_rcfile None;;


(* Parse command-line options *)
let parse_anonymous_opt anon = Rcfile.reminders_file := anon in
let usage =
   "Usage: wyrd [OPTIONS] [FILE]\n" ^
   "Open a front-end to remind(1) using FILE as the reminders file.\n\nOPTIONS:"
in
let show_version () =
   print_endline ("Wyrd v" ^ Version.version);
   print_endline "Copyright (C) 2005, 2006, 2007 Paul Pelzl";
   print_endline "";
   print_endline "Wyrd comes with ABSOLUTELY NO WARRANTY.  This is Free Software,";
   print_endline "and you are welcome to redistribute it under certain conditions;";
   print_endline "see the source code for details.";
   print_endline "";
   exit 0;
in
let event_desc_opt = ref None in
let do_quick_event event_desc =
   event_desc_opt := Some event_desc
in
let parse_definition = [
   ("--version", Arg.Unit show_version, " Display version information and exit");
   ("-a", Arg.String do_quick_event, " Add given event to reminders file and exit");
   ("--add", Arg.String do_quick_event, " Add given event to reminders file and exit");
] in
Arg.parse (Arg.align parse_definition) parse_anonymous_opt usage;

(* After parsing all arguments, handle quick reminders.  We have to
 * do it in this order so that the filename (anonymous option) gets
 * set prior to creating the new event. *)
begin match !event_desc_opt with
| Some event_desc ->
   begin try
      let _ = Interface_main.append_quick_event event_desc !Rcfile.reminders_file in
      exit 0
   with Time_lang.Event_parse_error s ->
      Printf.fprintf stderr "Error: %s\n" s;
      exit 1
   end
| None ->
   ()
end;;


let initialize_screen () =
   if Curses_config.wide_ncurses then
      (* ncursesw doesn't render non-ASCII without the setlocale() call *)
      let _ = Locale.setlocale Locale.LC_ALL "" in ()
   else
      ();
   let std = initscr () in
   begin try
      assert (start_color ());
      assert (use_default_colors ());
      Rcfile.validate_colors ()
   with _ ->
      endwin ();
      failwith "Your terminal emulator does not support color."
   end;
   assert (keypad std true);
   assert (cbreak ());
   assert (halfdelay 100);
   assert (noecho ());
   begin try
      assert (curs_set 0)
   with _ ->
      ()
   end;
   Interface_main.create_windows std;;

let iface = Interface.make (initialize_screen ());;


try
   Interface_main.run iface
with error ->
   endwin ();
   Printf.fprintf stderr "Caught error at toplevel:\n%s\n" (Printexc.to_string error);
   if Printexc.backtrace_status () then
      Printf.fprintf stderr "\nBacktrace:\n\n%s\n" (Printexc.get_backtrace ())
   else
      ();;


(* arch-tag: DO_NOT_CHANGE_eeac13df-e93f-4359-8b70-44fefc40e225 *)
