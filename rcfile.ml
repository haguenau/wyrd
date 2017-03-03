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

(* rcfile.ml
 * This file includes everything associated with processing the wyrdrc file.
 * In particular, this includes a number of hashtables used to store the
 * bindings of curses keypresses to calendar operations.
 * Adapted from rcfile code in Orpie, a curses RPN calculator. *)

open Genlex
open Curses


module PairSet = Set.Make (
   struct
      type t      = int * int
      let compare = Pervasives.compare
   end
)

exception Config_failure of string
let config_failwith s = raise (Config_failure s)

type command_t = | ScrollUp | ScrollDown | NextDay | PrevDay 
                 | NextWeek | PrevWeek | NextMonth | PrevMonth
                 | Home | Zoom | Edit | EditAny | NewTimed | NewUntimed 
                 | NewTimedDialog | NewUntimedDialog | SwitchWindow 
                 | SearchNext | BeginSearch | Quit | ViewReminders
                 | ScrollDescUp | ScrollDescDown | Refresh
                 | ViewAllReminders | ViewWeek | ViewMonth
                 | NextReminder | ViewKeybindings | CopyReminder
                 | PasteReminder | PasteReminderDialog 
                 | CutReminder | Goto | QuickEvent
                 | NewGenReminder of int | NewGenReminderDialog of int

type entry_operation_t = | EntryComplete | EntryBackspace | EntryExit

type operation_t = CommandOp of command_t | EntryOp of entry_operation_t

type colorable_object_t = | Help | Timed_default | Timed_current | Untimed_reminder
                          | Timed_date | Selection_info | Description | Status 
                          | Calendar_labels | Calendar_level1 | Calendar_level2
                          | Calendar_level3 | Calendar_today | Left_divider | Right_divider 
                          | Timed_reminder1 | Timed_reminder2 | Timed_reminder3
                          | Timed_reminder4


(* These hashtables store conversions between curses keys and the operations
 * they are associated with. *)
let table_key_command = Hashtbl.create 20
let table_command_key = Hashtbl.create 20

let table_key_entry = Hashtbl.create 20
let table_entry_key = Hashtbl.create 20

let table_commandstr_command = Hashtbl.create 30
let table_command_commandstr = Hashtbl.create 30

(* Default Remind command *)
let remind_command = ref "remind"
(* Default reminders file *)
let reminders_file = ref "$HOME/.reminders"
(* Default editing command strings *)
let edit_old_command = ref "vim +%n %f"
let edit_new_command = ref "vim -c '$' %f"
let edit_any_command = ref "vim %f"
(* Default new reminder templates *)
let timed_template   = ref "REM %M %d %y AT %h:%m DURATION 1:00 MSG "
let untimed_template = ref "REM %M %d %y MSG "
let template0        = ref None
let template1        = ref None
let template2        = ref None
let template3        = ref None
let template4        = ref None
let template5        = ref None
let template6        = ref None
let template7        = ref None
let template8        = ref None
let template9        = ref None
(* algorithm to use for counting busy-ness *)
let busy_algorithm = ref 1
(* number of minutes to assume an untimed reminder requires *)
let untimed_duration = ref 60.
(* Default thresholds for calendar colorization *)
let busy_level1 = ref 2
let busy_level2 = ref 4
let busy_level3 = ref 6
let busy_level4 = ref 8
(* First day of the week? *)
let week_starts_monday = ref false
(* 12/24 hour time selection *)
let schedule_12_hour    = ref false
let selection_12_hour   = ref true
let status_12_hour      = ref true
let description_12_hour = ref true
(* Center the schedule on the cursor? *)
let center_cursor = ref false
(* "jump to" date syntax is big-endian? *)
let goto_big_endian = ref true
(* "quick add" date syntax uses US conventions? *)
let quick_date_US = ref true
(* print week numbers? *)
let number_weeks = ref false
(* home is "sticky"? *)
let home_sticky = ref true
(* width of calendar/untimed reminder windows *)
let untimed_window_width = ref 40
(* trigger advance warning of reminders? *)
let advance_warning = ref false
(* render untimed reminders in bold? *)
let untimed_bold = ref true

(* List of included rc files *)
let included_rcfiles : (string list) ref = ref []

(* Temporary hash tables for sorting out the color palette *)
let color_table         = Hashtbl.create 20
let inverse_color_table = Hashtbl.create 20
(* Final hash table that maps from object to color_pair index *)
let object_palette      = Hashtbl.create 20


(* Turn colors on and off *)
let color_on win obj =
   try
      let color_index = Hashtbl.find object_palette obj in
      wattron win (A.color_pair color_index)
   with Not_found ->
      ()

let color_off win obj =
   try
      let color_index = Hashtbl.find object_palette obj in
      wattroff win (A.color_pair color_index)
   with Not_found ->
      ()


let command_of_key key =
   Hashtbl.find table_key_command key
let key_of_command command =
   Hashtbl.find table_command_key command

let entry_of_key key =
   Hashtbl.find table_key_entry key
let key_of_entry entry =
   Hashtbl.find table_entry_key entry


let decode_single_key_string key_string =
   let decode_alias str =
      match str with
      |"<esc>"       -> 27
      |"<tab>"       -> 9
      |"<enter>"     -> Key.enter
      |"<return>"    -> 10
      |"<insert>"    -> Key.ic
      |"<delete>"    -> Key.dc
      |"<home>"      -> Key.home
      |"<end>"       -> Key.end_
      |"<pageup>"    -> Key.ppage
      |"<pagedown>"  -> Key.npage
      |"<space>"     -> 32
      |"<backspace>" -> Key.backspace
      |"<left>"      -> Key.left
      |"<right>"     -> Key.right
      |"<up>"        -> Key.up
      |"<down>"      -> Key.down
      |"<f1>"        -> (Key.f 1)
      |"<f2>"        -> (Key.f 2)
      |"<f3>"        -> (Key.f 3)
      |"<f4>"        -> (Key.f 4)
      |"<f5>"        -> (Key.f 5)
      |"<f6>"        -> (Key.f 6)
      |"<f7>"        -> (Key.f 7)
      |"<f8>"        -> (Key.f 8)
      |"<f9>"        -> (Key.f 9)
      |"<f10>"       -> (Key.f 10)
      |"<f11>"       -> (Key.f 11)
      |"<f12>"       -> (Key.f 12)
      |_             -> 
         if String.length key_string = 1 then
            int_of_char str.[0]
         else
            config_failwith ("Unrecognized key \"" ^ str ^ "\"")
   in
   (* This regexp is used to extract the ctrl and meta characters from a string
    * representing a keypress.
    * It matches \\M\\C or \\C\\M or \\C or \\M (or no such characters) followed
    * by an arbitrary string. *)
   (* Note: is there a way to use raw strings here?  Getting tired of those
    * backslashes...*)
   let cm_re = Str.regexp
   "^\\(\\(\\\\M\\\\C\\|\\\\C\\\\M\\)\\|\\(\\\\M\\)\\|\\(\\\\C\\)\\)?\\(<.+>\\|.\\)"
   in
   if Str.string_match cm_re key_string 0 then
      let has_meta_ctrl =
         try let _ = Str.matched_group 2 key_string in true
         with Not_found -> false
      and has_meta =
         try let _  = Str.matched_group 3 key_string in true
         with Not_found -> false
      and has_ctrl =
         try let _ = Str.matched_group 4 key_string in true
         with Not_found -> false
      and main_key = Str.matched_group 5 key_string in
      if has_meta_ctrl then
         if String.length main_key = 1 then
            let uc_main_key = String.uppercase main_key in
            let mc_chtype = ((int_of_char uc_main_key.[0]) + 64) in
            let mc_str = "M-C-" ^ uc_main_key in
            (mc_chtype, mc_str)
         else
            config_failwith ("Cannot apply \\\\M\\\\C to key \"" ^ main_key ^ "\";\n" ^
                       "octal notation might let you accomplish this.")
      else if has_meta then
         if String.length main_key = 1 then
            let m_chtype = ((int_of_char main_key.[0]) + 128) in
            let m_str = "M-" ^ main_key in
            (m_chtype, m_str)
         else
            config_failwith ("Cannot apply \\\\M to key \"" ^ main_key ^ "\";\n" ^
                       "octal notation might let you accomplish this.")
      else if has_ctrl then
         if String.length main_key = 1 then
            let uc_main_key = String.uppercase main_key in
            let c_chtype = ((int_of_char uc_main_key.[0]) - 64) in
            let c_str = "C-" ^ uc_main_key in
            (c_chtype, c_str)
         else
            config_failwith ("Cannot apply \\\\C to key \"" ^ main_key ^ "\";\n" ^
                       "octal notation might let you accomplish this.")
      else 
         let octal_regex = Str.regexp "^0o" in
         try
            let _ = Str.search_forward octal_regex key_string 0 in
            ((int_of_string key_string), ("\\" ^ Str.string_after key_string
            2))
         with
            _ -> ((decode_alias main_key), main_key)
   else
      config_failwith ("Unable to match binding string with standard regular expression.")



(* Register a key binding.  This adds hash table entries for translation
 * between curses chtypes and commands (in both directions). *)
let register_binding_internal k k_string op =
   match op with
   |CommandOp x ->
      Hashtbl.add table_key_command k x;
      Hashtbl.add table_command_key x k_string
   |EntryOp x ->
      Hashtbl.add table_key_entry k x;
      Hashtbl.add table_entry_key x k_string


(* convenience routine for previous *)
let register_binding key_string op =
   (* given a string that represents a character, find the associated
    * curses chtype *)
   let k, string_rep = decode_single_key_string key_string in
   register_binding_internal k string_rep op


(* unregister a binding *)
let unregister_binding key_string =
   let k, _ = decode_single_key_string key_string in
   begin try
      let op = Hashtbl.find table_key_command k in
      Hashtbl.remove table_key_command k;
      Hashtbl.remove table_command_key op
   with Not_found -> 
      ()
   end;
   begin try
      let op = Hashtbl.find table_key_entry k in
      Hashtbl.remove table_key_entry k;
      Hashtbl.remove table_entry_key op
   with Not_found -> 
      ()
   end


let commands_list = [
   ("scroll_up"               , CommandOp ScrollUp);
   ("scroll_down"             , CommandOp ScrollDown);
   ("next_day"                , CommandOp NextDay);
   ("previous_day"            , CommandOp PrevDay);
   ("next_week"               , CommandOp NextWeek);
   ("previous_week"           , CommandOp PrevWeek);
   ("next_month"              , CommandOp NextMonth);
   ("previous_month"          , CommandOp PrevMonth);
   ("home"                    , CommandOp Home);
   ("goto"                    , CommandOp Goto);
   ("zoom"                    , CommandOp Zoom);
   ("edit"                    , CommandOp Edit);
   ("edit_any"                , CommandOp EditAny);
   ("copy"                    , CommandOp CopyReminder);
   ("cut"                     , CommandOp CutReminder);
   ("paste"                   , CommandOp PasteReminder);
   ("paste_dialog"            , CommandOp PasteReminderDialog);
   ("scroll_description_up"   , CommandOp ScrollDescUp);
   ("scroll_description_down" , CommandOp ScrollDescDown);
   ("quick_add"               , CommandOp QuickEvent);
   ("new_timed"               , CommandOp NewTimed);
   ("new_timed_dialog"        , CommandOp NewTimedDialog);
   ("new_untimed"             , CommandOp NewUntimed);
   ("new_untimed_dialog"      , CommandOp NewUntimedDialog);
   ("new_template0"           , CommandOp (NewGenReminder 0));
   ("new_template0_dialog"    , CommandOp (NewGenReminderDialog 0));
   ("new_template1"           , CommandOp (NewGenReminder 1));
   ("new_template1_dialog"    , CommandOp (NewGenReminderDialog 1));
   ("new_template2"           , CommandOp (NewGenReminder 2));
   ("new_template2_dialog"    , CommandOp (NewGenReminderDialog 2));
   ("new_template3"           , CommandOp (NewGenReminder 3));
   ("new_template3_dialog"    , CommandOp (NewGenReminderDialog 3));
   ("new_template4"           , CommandOp (NewGenReminder 4));
   ("new_template4_dialog"    , CommandOp (NewGenReminderDialog 4));
   ("new_template5"           , CommandOp (NewGenReminder 5));
   ("new_template5_dialog"    , CommandOp (NewGenReminderDialog 5));
   ("new_template6"           , CommandOp (NewGenReminder 6));
   ("new_template6_dialog"    , CommandOp (NewGenReminderDialog 6));
   ("new_template7"           , CommandOp (NewGenReminder 7));
   ("new_template7_dialog"    , CommandOp (NewGenReminderDialog 7));
   ("new_template8"           , CommandOp (NewGenReminder 8));
   ("new_template8_dialog"    , CommandOp (NewGenReminderDialog 8));
   ("new_template9"           , CommandOp (NewGenReminder 9));
   ("new_template9_dialog"    , CommandOp (NewGenReminderDialog 9));
   ("switch_window"           , CommandOp SwitchWindow);
   ("search_next"             , CommandOp SearchNext);
   ("begin_search"            , CommandOp BeginSearch);
   ("next_reminder"           , CommandOp NextReminder);
   ("view_remind"             , CommandOp ViewReminders);
   ("view_remind_all"         , CommandOp ViewAllReminders);
   ("view_week"               , CommandOp ViewWeek);
   ("view_month"              , CommandOp ViewMonth);
   ("refresh"                 , CommandOp Refresh );
   ("help"                    , CommandOp ViewKeybindings);
   ("entry_complete"          , EntryOp EntryComplete);
   ("entry_backspace"         , EntryOp EntryBackspace);
   ("entry_cancel"            , EntryOp EntryExit);
   ("quit"                    , CommandOp Quit)
] in
let create_translation ((commandstr, operation) : string * operation_t) =
   Hashtbl.add table_commandstr_command commandstr operation;
   Hashtbl.add table_command_commandstr operation commandstr
in
List.iter create_translation commands_list


(* translate a command string to the command type it represents *)
let operation_of_string command_str =
   Hashtbl.find table_commandstr_command command_str

(* translate a command to a string representation *)
let string_of_operation op =
   Hashtbl.find table_command_commandstr op


(* Parse a line from a configuration file.  This operates on a stream
 * corresponding to a non-empty line from the file.  It will match commands
 * of the form
 *    bind key command
 * where 'key' is either a quoted string containing a key specifier or an octal
 * key representation of the form \xxx (unquoted), and multiple_keys is a quoted
 * string containing a number of keypresses to simulate.
 *)
let parse_line line_stream = 
   (* Convenience function for 'set' keyword *)
   let parse_set variable_str variable coersion error =
      begin match line_stream with parser
      | [< 'Ident "=" >] ->
         begin match line_stream with parser
         | [< 'String ss >] ->
            begin try
               variable := coersion ss
            with _ ->
               config_failwith (error ^ "\"set " ^ variable_str ^ " = \"")
            end
         | [< >] ->
            config_failwith (error ^ "\"set " ^ variable_str ^ " = \"")
         end
      | [< >] ->
         config_failwith ("Expected \"=\" after \"set " ^ variable_str ^ "\"")
      end
   in
   (* Convenience function for 'color' keyword *)
   let parse_color obj_str obj =
      let foreground = 
         begin match line_stream with parser
         | [< 'Ident "black" >]   -> Color.black
         | [< 'Ident "red" >]     -> Color.red
         | [< 'Ident "green" >]   -> Color.green
         | [< 'Ident "yellow" >]  -> Color.yellow
         | [< 'Ident "blue" >]    -> Color.blue
         | [< 'Ident "magenta" >] -> Color.magenta
         | [< 'Ident "cyan" >]    -> Color.cyan
         | [< 'Ident "white" >]   -> Color.white
         | [< 'Ident "default" >] -> ~- 1
         | [< >] ->
            config_failwith ("Expected a foreground color after \"set " ^ obj_str)
         end
      in
      let background = 
         begin match line_stream with parser
         | [< 'Ident "black" >]   -> Color.black
         | [< 'Ident "red" >]     -> Color.red
         | [< 'Ident "green" >]   -> Color.green
         | [< 'Ident "yellow" >]  -> Color.yellow
         | [< 'Ident "blue" >]    -> Color.blue
         | [< 'Ident "magenta" >] -> Color.magenta
         | [< 'Ident "cyan" >]    -> Color.cyan
         | [< 'Ident "white" >]   -> Color.white
         | [< 'Ident "default" >] -> ~- 1
         | [< >] ->
            config_failwith ("Expected a background color after \"set " ^ obj_str)
         end
      in
      Hashtbl.replace color_table obj (foreground, background)
   in
   (* Parsing begins here *)
   match line_stream with parser
   | [< 'Kwd "include" >] ->
      begin match line_stream with parser
      | [< 'String include_file >] ->
         included_rcfiles := include_file :: !included_rcfiles
      | [< >] ->
         config_failwith ("Expected a filename string after \"include\"")
      end
   | [< 'Kwd "bind" >] -> 
      let bind_key key = 
         begin match line_stream with parser
         | [< 'Ident command_str >] ->
            begin try
               let command = operation_of_string command_str in
               register_binding key command
            with Not_found ->
               config_failwith ("Unrecognized command name \"" ^ command_str ^ "\"")
            end
         | [< >] ->
            config_failwith ("Expected a command name after \"bind \"" ^ key ^ "\"")
         end
      in
      begin match line_stream with parser
      | [< 'String k >] -> 
         bind_key k
      | [< 'Ident "\\" >] ->
         begin match line_stream with parser
         | [< 'Int octal_int >] ->
            begin
               try
                  let octal_digits = "0o" ^ (string_of_int octal_int) in
                  bind_key octal_digits 
               with 
                  (Failure "int_of_string") -> config_failwith "Expected octal digits after \"\\\""
            end
         | [< >]  ->
            config_failwith "Expected octal digits after \"\\\""
         end
      | [< >] ->
         config_failwith "Expected a key string after keyword \"bind\""
      end
   | [< 'Kwd "unbind" >] ->
      begin match line_stream with parser
      | [< 'String k >] ->
         unregister_binding k
      | [< >] ->
         config_failwith ("Expected a key string after keyword \"unbind\"")
      end
   | [< 'Kwd "set" >] ->
      begin match line_stream with parser
      | [< 'Ident "remind_command" >] ->
         parse_set "remind_command" remind_command (fun x -> x) "Expected a command string after "
      | [< 'Ident "reminders_file" >] ->
         parse_set "reminders_file" reminders_file (fun x -> x) "Expected a filename string after "
      | [< 'Ident "edit_old_command" >] ->
         parse_set "edit_old_command" edit_old_command (fun x -> x) "Expected a command string after "
      | [< 'Ident "edit_new_command" >] ->
         parse_set "edit_new_command" edit_new_command (fun x -> x) "Expected a command string after "
      | [< 'Ident "edit_any_command" >] ->
         parse_set "edit_any_command" edit_any_command (fun x -> x) "Expected a command string after "
      | [< 'Ident "timed_template" >] ->
         parse_set "timed_template" timed_template (fun x -> x) "Expected a template string after "
      | [< 'Ident "untimed_template" >] ->
         parse_set "untimed_template" untimed_template (fun x -> x) "Expected a template string after "
      | [< 'Ident "template0" >] ->
         parse_set "template0" template0 (fun x -> Some x) "Expected a template string after "
      | [< 'Ident "template1" >] ->
         parse_set "template1" template1 (fun x -> Some x) "Expected a template string after "
      | [< 'Ident "template2" >] ->
         parse_set "template2" template2 (fun x -> Some x) "Expected a template string after "
      | [< 'Ident "template3" >] ->
         parse_set "template3" template3 (fun x -> Some x) "Expected a template string after "
      | [< 'Ident "template4" >] ->
         parse_set "template4" template4 (fun x -> Some x) "Expected a template string after "
      | [< 'Ident "template5" >] ->
         parse_set "template5" template5 (fun x -> Some x) "Expected a template string after "
      | [< 'Ident "template6" >] ->
         parse_set "template6" template6 (fun x -> Some x) "Expected a template string after "
      | [< 'Ident "template7" >] ->
         parse_set "template7" template7 (fun x -> Some x) "Expected a template string after "
      | [< 'Ident "template8" >] ->
         parse_set "template8" template8 (fun x -> Some x) "Expected a template string after "
      | [< 'Ident "template9" >] ->
         parse_set "template9" template9 (fun x -> Some x) "Expected a template string after "
      | [< 'Ident "busy_algorithm" >] ->
         parse_set "busy_algorithm" busy_algorithm int_of_string "Expected an integral string after "
      | [< 'Ident "untimed_duration" >] ->
         parse_set "untimed_duration" untimed_duration float_of_string "Expected a float string after "
      | [< 'Ident "busy_level1" >] ->
         parse_set "busy_level1" busy_level1 int_of_string "Expected an integral string after "
      | [< 'Ident "busy_level2" >] ->
         parse_set "busy_level2" busy_level2 int_of_string "Expected an integral string after "
      | [< 'Ident "busy_level3" >] ->
         parse_set "busy_level3" busy_level3 int_of_string "Expected an integral string after "
      | [< 'Ident "busy_level4" >] ->
         parse_set "busy_level4" busy_level4 int_of_string "Expected an integral string after "
      | [< 'Ident "week_starts_monday" >] ->
         parse_set "week_starts_monday" week_starts_monday bool_of_string "Expected a boolean string after "
      | [< 'Ident "schedule_12_hour" >] ->
         parse_set "schedule_12_hour" schedule_12_hour bool_of_string "Expected a boolean string after "
      | [< 'Ident "selection_12_hour" >] ->
         parse_set "selection_12_hour" selection_12_hour bool_of_string "Expected a boolean string after "
      | [< 'Ident "status_12_hour" >] ->
         parse_set "status_12_hour" status_12_hour bool_of_string "Expected a boolean string after "
      | [< 'Ident "description_12_hour" >] ->
         parse_set "description_12_hour" description_12_hour bool_of_string "Expected a boolean string after "
      | [< 'Ident "center_cursor" >] ->
         parse_set "center_cursor" center_cursor bool_of_string "Expected a boolean string after "
      | [< 'Ident "goto_big_endian" >] ->
         parse_set "goto_big_endian" goto_big_endian bool_of_string "Expected a boolean string after "
      | [< 'Ident "quick_date_US" >] ->
         parse_set "quick_date_US" quick_date_US bool_of_string "Expected a boolean string after "
      | [< 'Ident "number_weeks" >] ->
         parse_set "number_weeks" number_weeks bool_of_string "Expected a boolean string after "
      | [< 'Ident "home_sticky" >] ->
         parse_set "home_sticky" home_sticky bool_of_string "Expected a boolean string after "
      | [< 'Ident "untimed_window_width" >] ->
         parse_set "untimed_window_width" untimed_window_width int_of_string "Expected an integral string after "
      | [< 'Ident "advance_warning" >] ->
         parse_set "advance_warning" advance_warning bool_of_string "Expected a boolean string after "
      | [< 'Ident "untimed_bold" >] ->
         parse_set "untimed_bold" untimed_bold bool_of_string "Expected a boolean string after "
      | [< >] ->
         config_failwith ("Unmatched variable name after \"set\"")
      end
   | [< 'Kwd "color" >] ->
      begin match line_stream with parser
      | [< 'Ident "help" >]             -> parse_color "help" Help
      | [< 'Ident "timed_default" >]    -> parse_color "timed_default" Timed_default
      | [< 'Ident "timed_current" >]    -> parse_color "timed_current" Timed_current
      | [< 'Ident "timed_reminder" >]   -> config_failwith 
             ("\"timed_reminder\" has been deprecated.  Please use \"timed_reminder1\".")
      | [< 'Ident "untimed_reminder" >] -> parse_color "untimed_reminder" Untimed_reminder
      | [< 'Ident "timed_date" >]       -> parse_color "timed_date" Timed_date
      | [< 'Ident "selection_info" >]   -> parse_color "selection_info" Selection_info
      | [< 'Ident "description" >]      -> parse_color "description" Description
      | [< 'Ident "status" >]           -> parse_color "status" Status
      | [< 'Ident "calendar_labels" >]  -> parse_color "calendar_labels" Calendar_labels
      | [< 'Ident "calendar_level1" >]  -> parse_color "calendar_level1" Calendar_level1
      | [< 'Ident "calendar_level2" >]  -> parse_color "calendar_level2" Calendar_level2
      | [< 'Ident "calendar_level3" >]  -> parse_color "calendar_level3" Calendar_level3
      | [< 'Ident "calendar_selection" >] -> 
         begin
         Printf.fprintf stderr "Warning: colorable object \"calendar_selection\" has been ";
         Printf.fprintf stderr "deprecated.\nPlease remove this reference from the wyrdrc file.\n"
         end
      | [< 'Ident "calendar_today" >]   -> parse_color "calendar_today" Calendar_today
      | [< 'Ident "left_divider" >]     -> parse_color "left_divider" Left_divider
      | [< 'Ident "right_divider" >]    -> parse_color "right_divider" Right_divider
      | [< 'Ident "timed_reminder1" >]  -> parse_color "timed_reminder1" Timed_reminder1
      | [< 'Ident "timed_reminder2" >]  -> parse_color "timed_reminder2" Timed_reminder2
      | [< 'Ident "timed_reminder3" >]  -> parse_color "timed_reminder3" Timed_reminder3
      | [< 'Ident "timed_reminder4" >]  -> parse_color "timed_reminder4" Timed_reminder4
      end
   | [< 'Kwd "#" >] ->
      ()
   | [< >] ->
      config_failwith "Expected a keyword at start of line";;


(* try opening the rc file, first looking at $HOME/.wyrdrc, 
 * then looking at $PREFIX/etc/wyrdrc *)
let open_rcfile rcfile_op =
   match rcfile_op with
   |None ->
      let home_rcfile =
         let homedir = Sys.getenv "HOME" in
         homedir ^ "/.wyrdrc"
      in
      let rcfile_fullpath = 
         (* expand out any occurrences of ${prefix} that autoconf
          * decides to insert *)
         let prefix_regex = Str.regexp "\\${prefix}" in
         let expanded_sysconfdir = Str.global_replace prefix_regex 
         Install.prefix Install.sysconfdir in
         Utility.join_path expanded_sysconfdir "wyrdrc"
      in
      begin try (open_in home_rcfile, home_rcfile)
      with Sys_error error_str ->
         begin try (open_in rcfile_fullpath, rcfile_fullpath)
         with Sys_error error_str -> failwith 
            ("Could not open configuration file \"" ^ home_rcfile ^ "\" or \"" ^ 
            rcfile_fullpath ^ "\" .")
         end
      end
   |Some file ->
      try (Utility.expand_open_in_ascii file, file)
      with Sys_error error_str -> config_failwith
      ("Could not open configuration file \"" ^ file ^ "\".")


(* validate the color table to make sure that the number of defined colors is legal *)
let validate_colors () =
   (* form a Set of all color pairs, and an inverse mapping from color pair to objects *)
   let initial_palette =
      let process_entry key pair colors = 
         Hashtbl.add inverse_color_table pair key;
         PairSet.add pair colors
      in
      Hashtbl.fold process_entry color_table PairSet.empty
   in
   (* start killing off color pairs as necessary to fit within color_pairs limit *)
   let rec reduce_colors palette =
      if PairSet.cardinal palette > pred (color_pairs ()) then begin
         (* find and remove the color pair with fewest objects assigned *)
         let min_objects = ref 100000
         and best_pair   = ref (-1, -1) in
         let find_best pair =
            let obj_list = Hashtbl.find_all inverse_color_table pair in
            if List.length obj_list < !min_objects then begin
               min_objects := List.length obj_list;
               best_pair   := pair
            end else
               ()
         in
         PairSet.iter find_best palette;
         (* the color pair needs to be removed from two hashtables and the palette set *)
         let obj_list = Hashtbl.find_all inverse_color_table !best_pair in
         List.iter (Hashtbl.remove color_table) obj_list;
         Hashtbl.remove inverse_color_table !best_pair;
         reduce_colors (PairSet.remove !best_pair palette)
      end else
         palette
   in
   let register_color_pair pair n =
      assert (init_pair n (fst pair) (snd pair));
      let f obj = Hashtbl.add object_palette obj n in
      List.iter f (Hashtbl.find_all inverse_color_table pair);
      succ n
   in
   let _ = PairSet.fold register_color_pair (reduce_colors initial_palette) 1 in ()
   


let rec process_rcfile rcfile_op =
   let line_lexer line = 
      make_lexer 
         ["include"; "bind"; "unbind"; "set"; "color"; "#"] 
      (Stream.of_string line)
   in
   let empty_regexp = Str.regexp "^[\t ]*$" in
   let config_stream, rcfile_filename = open_rcfile rcfile_op in
   let line_num = ref 0 in
   try
      while true do
         line_num := succ !line_num;
         let line_string = input_line config_stream in
         (* Printf.fprintf stderr "read line %2d: %s\n" !line_num line_string;
         flush stderr; *)
         if Str.string_match empty_regexp line_string 0 then
            (* do nothing on an empty line *)
            ()
         else
            try
               let line_stream = line_lexer line_string in
               parse_line line_stream;
               (* process any included rcfiles as they are encountered *)
               begin match !included_rcfiles with
               |[] -> ()
               |head :: tail -> 
                  included_rcfiles := tail;
                  process_rcfile (Some head)
               end
            with
               |Config_failure s ->
                  (let error_str = Printf.sprintf "Syntax error on line %d of \"%s\": %s"
                  !line_num rcfile_filename s in
                  failwith error_str)
               |Stream.Failure ->
                  failwith (Printf.sprintf "Syntax error on line %d of \"%s\"" 
                  !line_num rcfile_filename)

      done
   with End_of_file ->
      begin
         close_in config_stream
      end




(* arch-tag: DO_NOT_CHANGE_614115ed-7d1d-4834-bda4-e6cf93ac3fcd *)
