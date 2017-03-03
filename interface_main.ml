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

(* interface_main.ml
 * This file has the bulk of the implementation of the curses-based interface,
 * including the main program loop that grabs keypresses and processes them.
 * The screen rendering code is found in interface_draw.ml . *)

open Curses
open Printf
open Interface
open Interface_draw


exception Interrupt_exception
exception Template_undefined of string

type reminder_type_t = Timed | Untimed | General of int


(*******************************************************************)
(* RESIZE HANDLER                                                  *)
(*******************************************************************)


(* create the (new) windows corresponding to the different areas of the screen *)
(* note: to get around curses issues with addch on the last character of the
 * last row, all windows are created one line too long.  The last lines are
 * never used. (Is this the best way to do things?) *)
let create_windows screen =
   let height, width  = get_size () in
   let cal_height     = 10
   and cal_width      = if !Rcfile.untimed_window_width < 34 then 34
                        else if !Rcfile.untimed_window_width > width - 40 then width - 40
                        else !Rcfile.untimed_window_width
                        in
   let msg_height     = 6 in
   let err_height     = 1 in
   let timed_height   = height - 1 - msg_height - err_height
   and timed_width    = width - cal_width
   and untimed_height = height - 1 - msg_height - err_height - cal_height
   and untimed_width  = cal_width in
   if height >= 23 then 
      if width >= 80 then {
         stdscr       = screen;
         lines        = height;
         cols         = width;
         help_win     = newwin 2 width 0 0;
         hw_cols      = width;
         timed_win    = newwin (succ timed_height) timed_width 1 0;
         tw_lines     = timed_height;
         tw_cols      = timed_width;
         calendar_win = newwin (succ cal_height) cal_width 1 timed_width;
         cw_lines     = cal_height;
         cw_cols      = cal_width;
         untimed_win  = newwin (succ untimed_height) untimed_width (1 + cal_height)
                        timed_width;
         uw_lines     = untimed_height;
         uw_cols      = untimed_width;
         msg_win      = newwin (succ msg_height) width (1 + timed_height) 0;
         mw_lines     = msg_height;
         mw_cols      = width;
         err_win      = newwin err_height width (pred height) 0;
         ew_lines     = err_height;
         ew_cols      = pred width       (* don't print in last line of the last column *)
      }
      else
         (endwin ();
         failwith "Wyrd requires at least an 80 column window.")
   else
      (endwin (); 
      failwith "Wyrd requires at least a 23 line window.")


(* wresize all the child windows via wresize and mvwin *)
let resize_subwins iface =
   let height, width  = get_size () in
   let cal_height     = 10
   and cal_width      = if !Rcfile.untimed_window_width < 34 then 34
                        else if !Rcfile.untimed_window_width > width - 40 then width - 40
                        else 40
                        in
   let msg_height     = 6 in
   let err_height     = 1 in
   let timed_height   = height - 1 - msg_height - err_height
   and timed_width    = width - cal_width
   and untimed_height = height - 1 - msg_height - err_height - cal_height
   and untimed_width  = cal_width in
   let new_scr = 
      if height >= 23 then 
         if width >= 80 then {iface.scr with
            lines        = height;
            cols         = width;
            hw_cols      = width;
            tw_lines     = timed_height;
            tw_cols      = timed_width;
            cw_lines     = cal_height;
            cw_cols      = cal_width;
            uw_lines     = untimed_height;
            uw_cols      = untimed_width;
            mw_lines     = msg_height;
            mw_cols      = width;
            ew_lines     = err_height;
            ew_cols      = pred width
         }
         else
            (endwin ();
            failwith "Wyrd requires at least an 80 column window.")
      else
         (endwin (); 
         failwith "Wyrd requires at least a 23 line window.")
   in
   assert (wresize new_scr.help_win 2 width);
   assert (wresize new_scr.timed_win (succ timed_height) timed_width);
   assert (wresize new_scr.calendar_win (succ cal_height) cal_width);
   assert (mvwin   new_scr.calendar_win 1 timed_width);
   assert (wresize new_scr.untimed_win (succ untimed_height) untimed_width);
   assert (mvwin   new_scr.untimed_win (1 + cal_height) timed_width);
   assert (wresize new_scr.msg_win (succ msg_height) width);
   assert (mvwin   new_scr.msg_win (1 + timed_height) 0);
   assert (wresize new_scr.err_win err_height width);
   assert (mvwin   new_scr.err_win (pred height) 0);
   new_scr


(* Drop curses mode, invoke a function, and restore curses mode. *)
let drop_curses_across (f : 'a -> 'b)  (x : 'a) : 'b =
   def_prog_mode ();
   endwin ();
   let result = f x in
   reset_prog_mode ();
   let _ = curs_set 0 in
   result


(* Invoke an external process as Unix.system, but loop on EINTR while waiting for process
 * completion.  This is a workaround for the fact that SIGWINCH may be received while curses mode is
 * deactivated when we launch an external editor. *)
let system_retry_eintr (command : string) : Unix.process_status =
  match Unix.fork () with
  | 0 ->
      begin try
         Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; command |]
      with _ ->
         exit 127
      end
  | pid -> 
      let rec loop_eintr () =
         try
            snd (Unix.waitpid [] pid)
         with Unix.Unix_error (Unix.EINTR, _, _) ->
            loop_eintr ()
      in
      loop_eintr ()

     

let drop_curses_system (command : string) : Unix.process_status =
   drop_curses_across system_retry_eintr command


(* Parse a natural language event description and append
 * the result to the specified reminders file. *)
let append_quick_event event_spec remfile =
   let (rem_spec, description) = 
      Time_lang.parse_natural_language_event event_spec
   in
   let remline =
      begin match rem_spec with
      | Time_lang.Timed (start, finish) ->
         let date_s = 
            Printf.sprintf "REM %s %d %d " (Utility.string_of_tm_mon start.Unix.tm_mon)
            start.Unix.tm_mday (start.Unix.tm_year + 1900)
         in
         let start_s = 
            Printf.sprintf "AT %.2d:%.2d " start.Unix.tm_hour start.Unix.tm_min
         in
         let duration_s =
            if start <> finish then
               let (start_ts, _)  = Unix.mktime start
               and (finish_ts, _) = Unix.mktime finish in
               let dur = int_of_float ((finish_ts -. start_ts) /. 60.0) in
               Printf.sprintf "DURATION %d:%.2d " (dur / 60) (dur mod 60)
            else
               ""
         in
         date_s ^ start_s ^ duration_s ^ "MSG " ^ description ^ "\n"
      | Time_lang.Untimed timespec ->
         let date_s = 
            Printf.sprintf "REM %s %d %d " (Utility.string_of_tm_mon timespec.Unix.tm_mon)
            timespec.Unix.tm_mday (timespec.Unix.tm_year + 1900)
         in
         date_s ^ "MSG " ^ description ^ "\n"
      end
   in
   (* append the remline to the reminders file *)
   let remfile_channel = 
      open_out_gen [Open_append; Open_creat; Open_text] 416 (Utility.expand_file remfile)
   in
   output_string remfile_channel remline;
   close_out remfile_channel;
   (rem_spec, remline)


(* refresh the screen *)
let handle_refresh (iface : interface_state_t) reminders =
   let _ = wtouchln iface.scr.help_win 0 1 true in
   let _ = wtouchln iface.scr.timed_win 0 iface.scr.tw_lines true in
   let _ = wtouchln iface.scr.calendar_win 0 iface.scr.cw_lines true in
   let _ = wtouchln iface.scr.untimed_win 0 iface.scr.uw_lines true in
   let _ = wtouchln iface.scr.msg_win 0 iface.scr.mw_lines true in
   draw_help iface;
   let new_iface = draw_timed iface reminders.Remind.all_timed in
   draw_date_strip new_iface;
   draw_calendar new_iface reminders;
   let new_iface = draw_untimed new_iface reminders.Remind.curr_untimed in
   if String.length reminders.Remind.remind_error > 0 then
      let _ = beep () in ()
   else
      ();
   draw_error new_iface reminders.Remind.remind_error false;
   (new_iface, reminders)
   

(* handle a curses resize *)
let handle_resize (iface : interface_state_t) reminders =
   endwin ();
   begin match iface.resize_failed_win with
   | None -> ()
   | Some w -> let _ = delwin w in ()
   end;
   begin try
      let new_scr = resize_subwins iface in
      let resized_iface = {
         iface with scr               = new_scr;
                    top_untimed       = 0;
                    left_selection    = if !Rcfile.center_cursor then (iface.scr.tw_lines / 2) - 1 else 1;
                    right_selection   = 1;
                    timed_lineinfo    = Array.make new_scr.tw_lines [];
                    resize_failed_win = None
      } in
      begin try
         assert (curs_set 0)
      with _ ->
         ()
      end;
      clear ();
      assert (refresh ());
      handle_refresh resized_iface reminders
   with Failure s ->
      (* Create a new full-screen window where we can
       * paint an error message. *)
      let w = newwin 0 0 0 0 in
      erase ();
      let _ = mvwaddstr w 0 0 s in
      let _ = wnoutrefresh w in
      let _ = doupdate () in
      ({iface with resize_failed_win = Some w}, reminders)
   end
      
   

(* Any time a new item is selected, the reminders
 * record needs updating and the screen need redrawing *)
let handle_selection_change iface reminders track_home_setting =
   let new_reminders = Remind.update_reminders reminders 
   (timestamp_of_line iface iface.left_selection) in
   let new_iface = draw_untimed iface new_reminders.Remind.curr_untimed in
   let new_iface = draw_timed new_iface new_reminders.Remind.all_timed in
   draw_date_strip new_iface;
   draw_calendar new_iface new_reminders;
   draw_error new_iface "" false;
   ({new_iface with track_home = track_home_setting}, new_reminders)


(* Same as previous, but without calling draw_timed () or
 * draw_date_strip ().  Optimization for the scrolling case. *)
let handle_selection_change_scroll iface reminders track_home_setting =
   let new_reminders = Remind.update_reminders reminders 
   (timestamp_of_line iface iface.left_selection) in
   draw_calendar iface new_reminders;
   let new_iface = draw_untimed iface new_reminders.Remind.curr_untimed in
   draw_error new_iface "" false;
   ({new_iface with track_home = track_home_setting}, new_reminders)


(* handle a "scroll down" event when the timed window is focused
 * and the center_cursor option is turned on *)
let handle_scrolldown_timed_center (iface : interface_state_t) reminders =
   let second_timestamp = timestamp_of_line iface 1 in
   let iface2 = {
      iface with top_timestamp = second_timestamp
   } in
   let (new_iface, new_reminders) =
      handle_selection_change_scroll iface2 reminders false
   in
   (* use a curses scroll operation to shift up the timed window *)
   assert (wscrl new_iface.scr.timed_win 1);
   (* adjust lineinfo array to compensate for scrolling *)
   Array.blit iface.timed_lineinfo 1 iface.timed_lineinfo 0
      (pred (Array.length iface.timed_lineinfo));
   (* do a two-line update to recenter the cursor *)
   let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed
      (iface.left_selection - 1) 2 in
   (* draw in the new line at the bottom of the screen *)
   let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed
      (iface.scr.tw_lines - 1) 1 in
   draw_date_strip new_iface;
   (new_iface, new_reminders)



(* handle a "scroll down" event when the timed window is focused
 * and the center_cursor option is turned off *)
let handle_scrolldown_timed_nocenter (iface : interface_state_t) reminders =
   if iface.left_selection < pred iface.scr.tw_lines then begin
      (* case 1: only the cursor moves *)
      let iface2 = {
         iface with left_selection = succ iface.left_selection
      } in
      let (new_iface, new_reminders) = 
         handle_selection_change_scroll iface2 reminders false
      in
      (* make a two-line update to erase and redraw the cursor *)
      let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed 
         iface.left_selection 2 in
      (new_iface, new_reminders)
   end else begin
      (* case 2: the entire timed window scrolls *)
      let second_timestamp = timestamp_of_line iface 1 in
      let iface2 = {
         iface with top_timestamp = second_timestamp
      } in
      let (new_iface, new_reminders) = 
         handle_selection_change_scroll iface2 reminders false
      in
      (* use a curses scroll operation to shift up the timed window *)
      assert (wscrl new_iface.scr.timed_win 1);
      (* adjust lineinfo array to compensate for scrolling *)
      Array.blit iface.timed_lineinfo 1 iface.timed_lineinfo 0
         (pred (Array.length iface.timed_lineinfo));
      (* do a two-line update to erase the cursor and draw in the
       * new line at the bottom of the screen *)
      let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed
         (iface.scr.tw_lines - 2) 2 in
      draw_date_strip new_iface;
      (new_iface, new_reminders)
   end



(* handle a "scroll down" event when the timed window is focused *)
let handle_scrolldown_timed (iface : interface_state_t) reminders =
   (* watch out for y2k38 *)
   let selected_day = Unix.localtime (iface.top_timestamp +.
   (float_of_int (succ iface.left_selection)) *. (time_inc iface)) in
   if (selected_day.Unix.tm_year + 1900) > 2037 then begin
      draw_error iface "requested year is out of range." false;
      assert (doupdate ());
      (iface, reminders)
   end else
      let iface = {
         iface with top_untimed = 0;
                    top_desc = 0;
                    right_selection = 1
      } in
      if !Rcfile.center_cursor then
         handle_scrolldown_timed_center iface reminders
      else
         handle_scrolldown_timed_nocenter iface reminders



(* handle a "scroll down" event when the untimed window is focused *)
let handle_scrolldown_untimed (iface : interface_state_t) reminders =
   if iface.right_selection < pred iface.scr.uw_lines then begin
      if iface.right_selection < iface.len_untimed - 
      iface.top_untimed then begin
         let new_iface = {
            iface with right_selection = succ iface.right_selection
         } in
         handle_selection_change new_iface reminders false
      end else
         (iface, reminders)
   end else begin
      if iface.right_selection < iface.len_untimed - 
      iface.top_untimed then begin
         let new_iface = {
            iface with top_untimed = succ iface.top_untimed
         } in
         handle_selection_change new_iface reminders false
      end else
         (iface, reminders)
   end



(* handle a "scroll up" event when the timed window is focused
 * and the center_cursor option is turned on *)
let handle_scrollup_timed_center (iface : interface_state_t) reminders =
   let prev_timestamp = timestamp_of_line iface (-1) in
   let iface2 = {
      iface with top_timestamp = prev_timestamp
   } in
   let (new_iface, new_reminders) = 
      handle_selection_change_scroll iface2 reminders false
   in
   (* use a curses scroll operation to shift up the timed window *)
   assert (wscrl new_iface.scr.timed_win (-1));
   (* adjust lineinfo array to compensate for scrolling *)
   Array.blit iface.timed_lineinfo 0 iface.timed_lineinfo 1
      (pred (Array.length iface.timed_lineinfo));
   (* do a two-line update to recenter the cursor *)
   let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed 
      iface.left_selection 2 in
   (* draw in the new top line of the schedule *)
   let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed 0 1 in
   draw_date_strip new_iface;
   (new_iface, new_reminders)



(* handle a "scroll up" event when the timed window is focused
 * and the center_cursor option is turned off *)
let handle_scrollup_timed_nocenter (iface : interface_state_t) reminders =
   if iface.left_selection > 0 then begin
      (* case 1: only the cursor moves *)
      let iface2 = {
         iface with left_selection = pred iface.left_selection
      } in
      let (new_iface, new_reminders) = 
         handle_selection_change_scroll iface2 reminders false
      in
      (* make a two-line update to erase and redraw the cursor *)
      let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed 
         (pred iface.left_selection) 2 in
      (new_iface, new_reminders)
   end else begin
      (* case 2: the entire timed window scrolls *)
      let prev_timestamp = timestamp_of_line iface (-1) in
      let iface2 = {
         iface with top_timestamp = prev_timestamp
      } in
      let (new_iface, new_reminders) = 
         handle_selection_change_scroll iface2 reminders false
      in
      (* use a curses scroll operation to shift up the timed window *)
      assert (wscrl new_iface.scr.timed_win (-1));
      (* adjust lineinfo array to compensate for scrolling *)
      Array.blit iface.timed_lineinfo 0 iface.timed_lineinfo 1
         (pred (Array.length iface.timed_lineinfo));
      (* do a two-line update to erase the cursor and draw in the
       * new line at the bottom of the screen *)
      let new_iface = draw_timed_try_window new_iface new_reminders.Remind.all_timed 0 2 in
      draw_date_strip new_iface;
      (new_iface, new_reminders)
   end



(* handle a "scroll up" event when the timed window is focused *)
let handle_scrollup_timed (iface : interface_state_t) reminders =
   (* watch out for scrolling out of Remind date range *)
   let selected_day = Unix.localtime iface.top_timestamp in
   if (selected_day.Unix.tm_year + 1900) < 1991 then begin
      draw_error iface "requested year is out of range." false;
      assert (doupdate ());
      (iface, reminders)
   end else
      let iface = {
         iface with top_untimed = 0;
                    top_desc = 0;
                    right_selection = 1
      } in
      if !Rcfile.center_cursor then
         handle_scrollup_timed_center iface reminders
      else
         handle_scrollup_timed_nocenter iface reminders


(* handle a "scroll up" event when the untimed window is focused *)
let handle_scrollup_untimed (iface : interface_state_t) reminders =
   if iface.right_selection > 1 then
      let new_iface = {
         iface with right_selection = pred iface.right_selection
      } in
      handle_selection_change new_iface reminders new_iface.track_home
   else if iface.top_untimed > 0 then
      let new_iface = {
         iface with top_untimed = pred iface.top_untimed
      } in
      handle_selection_change new_iface reminders new_iface.track_home
   else
      (iface, reminders)


(* handle a jump to a different day *)
let handle_jump (iface : interface_state_t) reminders jump_func =
   (* new timestamp for top line of screen *)
   let temp    = Unix.localtime iface.top_timestamp in
   let next_tm = jump_func temp in
   let (next_ts, normalized_tm) = Unix.mktime next_tm in
   (* new timestamp for bottom line of screen *)
   let temp2   = Unix.localtime (iface.top_timestamp +.
   (float_of_int (succ iface.left_selection)) *. (time_inc iface)) in
   let next_tm2 = jump_func temp2 in
   let (_, normalized_tm2) = Unix.mktime next_tm2 in
   (* check that the date range is OK for both Remind and *nix *)
   if (normalized_tm.Unix.tm_year + 1900) < 1991 || 
   (normalized_tm2.Unix.tm_year + 1900) > 2037 then begin
      draw_error iface "requested year is out of range." false;
      assert (doupdate ());
      (iface, reminders)
   end else
      let new_iface = {
         iface with top_timestamp = next_ts;
                    top_desc = 0
      } in
      handle_selection_change new_iface reminders false


(* handle switching window focus *)
let handle_switch_focus (iface : interface_state_t) reminders =
   let new_iface = 
      match iface.selected_side with
      |Left  -> {iface with selected_side = Right}
      |Right -> {iface with selected_side = Left}
   in
   handle_selection_change new_iface reminders new_iface.track_home


(* handle switching to the current timeslot *)
let handle_home (iface : interface_state_t) reminders =
   let curr_time = Unix.localtime ((Unix.time ()) -. (time_inc iface)) in
   let (rounded_time, _) = Unix.mktime (round_time iface.zoom_level curr_time) in
   let new_iface = {
      iface with top_timestamp = 
                    if !Rcfile.center_cursor then
                       rounded_time -. (time_inc iface) *. (float_of_int ((iface.scr.tw_lines / 2) - 2))
                    else
                       rounded_time -. (time_inc iface) *. 1.;
                 top_desc        = 0;
                 selected_side   = Left;
                 left_selection  = if !Rcfile.center_cursor then (iface.scr.tw_lines / 2) - 1 else 2;
                 right_selection = 1 
   } in
   handle_selection_change new_iface reminders true


(* handle a zoom keypress *)
let handle_zoom (iface : interface_state_t) reminders =
   let new_iface = 
      let curr_ts = timestamp_of_line iface iface.left_selection in
      let curr_tm = Unix.localtime curr_ts in
      let hour_tm = {
         curr_tm with Unix.tm_sec = 0;
                      Unix.tm_min = 0
      } in
      let (hour_ts, _) = Unix.mktime hour_tm in
      match iface.zoom_level with
      |Hour ->
         let new_top = curr_ts -. (60.0 *. 30.0 *. 
                       (float_of_int iface.left_selection)) in {
            iface with top_timestamp = new_top;
                       top_desc      = 0;
                       zoom_level    = HalfHour
         }
      |HalfHour ->
         let new_top = curr_ts -. (60.0 *. 15.0 *. 
                       (float_of_int iface.left_selection)) in {
            iface with top_timestamp = new_top;
                       top_desc      = 0;
                       zoom_level    = QuarterHour
         }
      |QuarterHour ->
         let new_top = hour_ts -. (60.0 *. 60.0 *. 
                       (float_of_int iface.left_selection)) in {
            iface with top_timestamp = new_top;
                       top_desc      = 0;
                       zoom_level    = Hour
         }
   in
   if new_iface.track_home then
      handle_home new_iface reminders
   else 
      let new_iface = draw_timed new_iface reminders.Remind.all_timed in
      draw_date_strip new_iface;
      (new_iface, reminders)



(* handle creation of a new timed or untimed reminder *)
let handle_new_reminder (iface : interface_state_t) reminders rem_type 
       remfile =
   let ts = timestamp_of_line iface iface.left_selection in
   let tm = Unix.localtime ts in
   (* take care of the substitution characters in remline templates *)
   let substitute template =
      let rec substitute_aux subst_list s =
         match subst_list with
         |[] ->
            s
         |(regex, replacement) :: tail ->
            let new_s = Str.global_replace regex replacement s in
            substitute_aux tail new_s
      in
      substitute_aux [
         (Str.regexp "%monname%", Utility.string_of_tm_mon tm.Unix.tm_mon);
         (Str.regexp "%mon%", string_of_int (succ tm.Unix.tm_mon));
         (Str.regexp "%mday%", string_of_int tm.Unix.tm_mday);
         (Str.regexp "%year%", string_of_int (tm.Unix.tm_year + 1900));
         (Str.regexp "%hour%", Printf.sprintf "%.2d" tm.Unix.tm_hour);
         (Str.regexp "%min%", Printf.sprintf "%.2d" tm.Unix.tm_min);
         (Str.regexp "%wdayname%", Utility.string_of_tm_wday tm.Unix.tm_wday);
         (Str.regexp "%wday%", string_of_int tm.Unix.tm_wday)
      ] template
   in
   try
      let remline = 
         match rem_type with
         | Timed   -> substitute !Rcfile.timed_template
         | Untimed -> substitute !Rcfile.untimed_template
         | General x ->
            let template_status =
               match x with
               | 0 -> (!Rcfile.template0, "template0")
               | 1 -> (!Rcfile.template1, "template1")
               | 2 -> (!Rcfile.template2, "template2")
               | 3 -> (!Rcfile.template3, "template3")
               | 4 -> (!Rcfile.template4, "template4")
               | 5 -> (!Rcfile.template5, "template5")
               | 6 -> (!Rcfile.template6, "template6")
               | 7 -> (!Rcfile.template7, "template7")
               | 8 -> (!Rcfile.template8, "template8")
               | 9 -> (!Rcfile.template9, "template9")
               | _ -> failwith ("unexpected template number " ^ (string_of_int x) ^
                                " in handle_new_reminder")
            in
            begin match template_status with
            | (None, template_str) -> raise (Template_undefined template_str)
            | (Some t, _)          -> substitute t
            end
      in
      (* append the remline to the reminders file *)
      let remfile_channel = 
         open_out_gen [Open_append; Open_creat; Open_text] 416 remfile
      in
      output_string remfile_channel remline;
      close_out remfile_channel;
      (* open the reminders file in an editor *)
      let filename_sub = Str.regexp "%file%" in
      let command = 
         Str.global_replace filename_sub remfile
            !Rcfile.edit_new_command
      in
      let editor_process_status = drop_curses_system command in
      let r = Remind.create_three_month iface.top_timestamp in
      (* if the untimed list has been altered, change the focus to
       * the first element of the list *)
      let new_iface =
         if List.length r.Remind.curr_untimed <> 
            List.length reminders.Remind.curr_untimed then {
            iface with top_untimed = 0;
                       top_desc = 0;
                       right_selection = 1
            }
         else
            iface
      in
      begin match editor_process_status with
      | Unix.WEXITED return_code ->
         if return_code <> 0 then begin
            let (new_iface, r)  = handle_refresh new_iface r in
            let _ = beep () in
            draw_error new_iface 
               "Error when launching editor; configure a different editor in ~/.wyrdrc ." false;
            assert (doupdate ());
            (new_iface, r)
         end else
            handle_refresh new_iface r
      | _ -> 
         let (new_iface, r) = handle_refresh new_iface r in
         draw_error new_iface
            "Editor process was interrupted." false;
         assert (doupdate ());
         (new_iface, r)
      end
   with Template_undefined template_str ->
      draw_error iface (template_str ^ " is undefined.") false;
      assert (doupdate ());
      (iface, reminders)


(* Search forward for the next occurrence of the current search regex.  
 *
 * First find the date of the occurrence, by matching on the output of 'remind
 * -n'.  For that date, recompute timed and untimed reminder lists.  Filter the
 * timed and untimed reminder lists to get only the entries falling on the
 * occurrence date.  Search through the filtered timed list first, and locate
 * the first entry that matches the regex (if any).  Then search through the 
 * filtered untimed list, and locate the first entry that matches the regex (if any).
 * If only one of the lists has a match, return that one; if both lists have a match,
 * return the one with the earlier timestamp.
 * Finally, reconfigure the iface record to highlight the matched entry.
 *
 * If the user has advance_warning enabled, the algorithm becomes more complicated.
 * After finding the correct search date, we have to locate reminders that are
 * present when advance warnings are *suppressed*.  This is accomplished by
 * searching through the reminders list with advanced warning suppressed, then
 * searching for an *identical string* within the reminders list with advanced
 * warning enabled.
 *
 * The saved search regex can be ignored by providing Some regex as the
 * override_regex parameter. 
 *
 * FIXME: This function is about five times too large. *)
let handle_find_next (iface : interface_state_t) reminders override_regex =
   let search_regex =
      match override_regex with
      |None       -> iface.search_regex
      |Some regex -> regex
   in
   try
      (* Note: selected_ts is the timestamp of the next timeslot, minus one second.
       *       This ensures that searching begins immediately after the current
       *       selection. *)
      let selected_ts     = (timestamp_of_line iface (succ iface.left_selection)) -. 1.0 in
      let occurrence_time = Remind.find_next search_regex selected_ts in
      (* Reminders with advance warning included *)
      let no_advw_reminders= 
         if !Rcfile.advance_warning then
            Remind.create_three_month ~suppress_advwarn:true occurrence_time
         else
            Remind.update_reminders reminders occurrence_time
      in
      (* Reminders with advance warning suppressed *)
      let advw_reminders = 
         if !Rcfile.advance_warning then
            Remind.update_reminders reminders occurrence_time
         else
            no_advw_reminders
      in
      let occurrence_tm = Unix.localtime occurrence_time in
      let temp1 = {
         occurrence_tm with Unix.tm_sec  = 0;
                            Unix.tm_min  = 0;
                            Unix.tm_hour = 0
      } in
      let temp2 = {
         occurrence_tm with Unix.tm_sec  = 0;
                            Unix.tm_min  = 0;
                            Unix.tm_hour = 0;
                            Unix.tm_mday = succ occurrence_tm.Unix.tm_mday 
      } in
      let (day_start_ts, _) = Unix.mktime temp1 in
      let (day_end_ts, _)   = Unix.mktime temp2 in
      (* filter functions to determine reminders falling on the occurrence day *)
      let is_current_untimed rem =
         rem.Remind.ur_start >= day_start_ts && rem.Remind.ur_start < day_end_ts
      in
      let is_current_timed rem =
         rem.Remind.tr_start >= day_start_ts && rem.Remind.tr_start < day_end_ts
      in
      (* test the untimed reminders list, with advanced warnings enabled,
       * for entries that match the specified string *)
      let rec check_untimed_advw match_string untimed n timed_match =
         match untimed with
         |[] ->
            begin match timed_match with
            |None ->
               let _ = beep () in
               draw_error iface "search expression not found." false;
               assert (doupdate ());
               (iface, reminders)
            |Some (timed_match_ts, timed_match_iface) ->
               handle_selection_change timed_match_iface advw_reminders false
            end
         |urem :: tail ->
            begin try
               if urem.Remind.ur_start > selected_ts && urem.Remind.ur_msg = match_string then
                  let tm = Unix.localtime urem.Remind.ur_start in
                  let (rounded_time, _) = Unix.mktime (round_time iface.zoom_level tm) in
                  let new_iface =
                     (* take care of highlighting the correct untimed reminder *)
                     if n >= iface.scr.uw_lines then 
                        if !Rcfile.center_cursor then {
                           iface with top_timestamp   = rounded_time -. (time_inc iface) *. 
                                                        (float_of_int (iface.scr.tw_lines / 2 - 1));
                                      top_untimed     = n - iface.scr.uw_lines + 1;
                                      top_desc        = 0;
                                      left_selection  = (iface.scr.tw_lines / 2) - 1;
                                      right_selection = pred iface.scr.uw_lines;
                                      selected_side   = Right
                        } else {
                           iface with top_timestamp   = rounded_time;
                                      top_untimed     = n - iface.scr.uw_lines + 1;
                                      top_desc        = 0;
                                      left_selection  = 0;
                                      right_selection = pred iface.scr.uw_lines;
                                      selected_side   = Right
                        }
                     else 
                        if !Rcfile.center_cursor then {
                           iface with top_timestamp   = rounded_time -. (time_inc iface) *. 
                                                        (float_of_int (iface.scr.tw_lines / 2 - 1));
                                      top_untimed     = 0;
                                      top_desc        = 0;
                                      left_selection  = (iface.scr.tw_lines / 2) - 1 ;
                                      right_selection = n;
                                      selected_side   = Right
                        } else {
                           iface with top_timestamp   = rounded_time;
                                      top_untimed     = 0;
                                      top_desc        = 0;
                                      left_selection  = 0;
                                      right_selection = n;
                                      selected_side   = Right
                        }
                  in
                  (* choose between the timed reminder match and the untimed reminder match *)
                  begin match timed_match with
                  |None ->
                     handle_selection_change new_iface advw_reminders false
                  |Some (timed_match_ts, timed_match_iface) ->
                     if timed_match_ts < urem.Remind.ur_start then
                        handle_selection_change timed_match_iface advw_reminders false
                     else
                        handle_selection_change new_iface advw_reminders false
                  end
               else
                  raise Not_found
            with Not_found ->
               check_untimed_advw match_string tail (succ n) timed_match
            end
      in
      (* test the untimed reminders list, with advanced warnings suppressed,
       * for entries that match the regex *)
      let rec check_untimed_no_advw untimed n timed_match =
         match untimed with
         |[] ->
            begin match timed_match with
            |None ->
               let _ = beep () in
               draw_error iface "search expression not found." false;
               assert (doupdate ());
               (iface, reminders)
            |Some (timed_match_ts, timed_match_iface) ->
               handle_selection_change timed_match_iface advw_reminders false
            end
         |urem :: tail ->
            begin try
               if urem.Remind.ur_start > selected_ts then
                  (* If we find a match, then look for an identical string in
                   * the list of untimed reminders with advance warning enabled *)
                  let _ = Str.search_forward search_regex urem.Remind.ur_msg 0 in
                  let today_untimed_advw = 
                     List.filter is_current_untimed advw_reminders.Remind.curr_untimed
                  in
                  check_untimed_advw urem.Remind.ur_msg today_untimed_advw 1 timed_match
               else
                  raise Not_found
            with Not_found ->
               check_untimed_no_advw tail (succ n) timed_match
            end
      in
      (* test the timed reminders list for entries that match the regex *)
      let rec check_timed timed =
         match timed with
         |[] ->
            let today_untimed_no_advw = 
               List.filter is_current_untimed no_advw_reminders.Remind.curr_untimed
            in
            check_untimed_no_advw today_untimed_no_advw 1 None
         |trem :: tail ->
            begin try
               if trem.Remind.tr_start > selected_ts then
                  let _ = Str.search_forward search_regex trem.Remind.tr_msg 0 in
                  let tm = Unix.localtime trem.Remind.tr_start in
                  let (rounded_time, _) = Unix.mktime (round_time iface.zoom_level tm) in
                  let new_iface = 
                     if !Rcfile.center_cursor then {
                        iface with top_timestamp   = rounded_time -. (time_inc iface) *. 
                                                     (float_of_int (iface.scr.tw_lines / 2 - 1));
                                   top_desc        = 0;
                                   left_selection  = (iface.scr.tw_lines / 2) - 1;
                                   right_selection = 1;
                                   selected_side   = Left
                        } 
                     else {
                        iface with top_timestamp   = rounded_time -. (time_inc iface) *. 2.;
                                   top_desc        = 0;
                                   left_selection  = 2;
                                   right_selection = 1;
                                   selected_side   = Left
                     } 
                  in
                  let today_untimed_no_advw = 
                     List.filter is_current_untimed no_advw_reminders.Remind.curr_untimed
                  in
                  check_untimed_no_advw today_untimed_no_advw 1 (Some (trem.Remind.tr_start, new_iface))
               else
                  raise Not_found
            with Not_found ->
               check_timed tail
            end
      in
      let merged_rem = Remind.merge_timed no_advw_reminders.Remind.curr_timed in
      check_timed (List.filter is_current_timed merged_rem)
   with Remind.Occurrence_not_found ->
      let _ = beep () in
      draw_error iface "search expression not found." false;
      (iface, reminders)


(* Begin entry of a search string *)
let handle_begin_search (iface : interface_state_t) reminders =
   let new_iface = {
      iface with entry_mode = Extended ExtendedSearch
   } in
   draw_error iface "search expression: " true;
   (new_iface, reminders)


(* Find the next reminder after the current selection.
 * This algorithm is a dirty hack, but actually seems to work:
 * 1) If an untimed reminder is selected, and if it is not the last
 *    untimed reminder in the list, then highlight the next untimed reminder.
 * 2) Otherwise, run find_next with a regexp that matches anything. *)
let handle_next_reminder (iface : interface_state_t) reminders =
   if iface.selected_side = Right && 
   iface.right_selection < iface.len_untimed - iface.top_untimed then 
      if !Rcfile.advance_warning then begin
         (* Compute the list of untimed reminders currently displayed to the user *)
         let displayed_reminders = 
            Remind.get_untimed_reminders_for_day reminders.Remind.curr_untimed 
            (timestamp_of_line iface iface.left_selection)
         in
         (* Compute the same list, with advance warnings suppressed *)
         let displayed_reminders_no_advw =
            let reminders_no_advw = 
               Remind.create_three_month ~suppress_advwarn:true 
               (timestamp_of_line iface iface.left_selection)
            in
            Remind.get_untimed_reminders_for_day reminders_no_advw.Remind.curr_untimed
            (timestamp_of_line iface iface.left_selection)
         in
         (* Index of currently selected reminder within displayed_reminders *)
         let selection_index = iface.right_selection + iface.top_untimed in
         (* Simultaneously iterate through the two lists until we reach
          * the currently selected element.  Theoretically displayed_reminders is a 
          * superset of displayed_reminders_no_advw with ordering preserved, so we 
          * should be able to match identical elements as we go. *)
         let rec iter_match superset subset n prev_iface prev_rem =
            match superset with
            | [] ->
               (* Some sort of search failure.  Just skip over today's untimed
                * reminders. *)
               handle_find_next iface reminders (Some (Str.regexp ""))
            | superset_head :: superset_tail ->
               begin match subset with
               | [] ->
                  (* Ran out of reminders in the list with advance warning
                   * suppressed.  Therefore we can skip over untimed reminders. *)
                  handle_find_next iface reminders (Some (Str.regexp ""))
               | subset_head :: subset_tail ->
                  if n > selection_index then
                     let (next_iface, next_rem) = 
                        handle_scrolldown_untimed prev_iface prev_rem
                     in
                     if superset_head = subset_head then
                        (* This should be the correct element.  End here. *)
                        (next_iface, next_rem)
                     else
                        (* Keep searching forward through superset list
                         * to try to locate this element. *)
                        iter_match superset_tail subset (succ n) next_iface next_rem
                  else
                     if superset_head = subset_head then begin
                        (* Iterate both... *)
                        iter_match superset_tail subset_tail (succ n) prev_iface prev_rem
                     end else begin
                        (* Iterate superset only *)
                        iter_match superset_tail subset (succ n) prev_iface prev_rem
                     end
               end
         in
         iter_match displayed_reminders displayed_reminders_no_advw 1 iface reminders
      end else
         (* Easy case.  If the user has not selected advance warning, we can just
          * scroll down the untimed reminders window. *)
         handle_scrolldown_untimed iface reminders
   else
      handle_find_next iface reminders (Some (Str.regexp ""))


(* View the reminders for the selected date using 'less'.
 * If 'trigger_all' is true, then all nonexpired reminders are
 * triggered. *)
let handle_view_reminders (iface : interface_state_t) reminders trigger_all =
   let ts = timestamp_of_line iface iface.left_selection in
   let tm = Unix.localtime ts in
   let rem_date_str = (Utility.string_of_tm_mon tm.Unix.tm_mon) ^ " " ^ 
                      (string_of_int tm.Unix.tm_mday) ^ " " ^
                      (string_of_int (tm.Unix.tm_year + 1900)) in
   let partial_command =
      if trigger_all then
         !Rcfile.remind_command ^ " -q -g -t "
      else
         !Rcfile.remind_command ^ " -q -g "
   in
   let command =
      partial_command ^ !Rcfile.reminders_file ^ " " ^ rem_date_str ^ " | less -c"
   in
   let _ = drop_curses_system command in
   handle_refresh iface reminders



(* View Remind's formatted calendar output for the selected date.  If
 * 'week_only' is true, then create a week calendar, otherwise a month
 * calendar. *)
let handle_view_calendar (iface : interface_state_t) reminders week_only =
   let ts = timestamp_of_line iface iface.left_selection in
   let tm = Unix.localtime ts in
   let rem_date_str = (Utility.string_of_tm_mon tm.Unix.tm_mon) ^ " " ^ 
                      (string_of_int tm.Unix.tm_mday) ^ " " ^
                      (string_of_int (tm.Unix.tm_year + 1900)) in
   let partial_command = 
      if week_only then
         Printf.sprintf "%s -c+1 -w%d " !Rcfile.remind_command iface.scr.cols
      else
         Printf.sprintf "%s -c -w%d " !Rcfile.remind_command iface.scr.cols
   in
   let time_option = if !Rcfile.description_12_hour then "-b0 " else "-b1 " in
   let weekday_option = if !Rcfile.week_starts_monday then "-m " else "" in
   let command = 
      partial_command ^ time_option ^ weekday_option ^ 
      !Rcfile.reminders_file ^ " " ^ rem_date_str ^ " | less -c" 
   in
   let _ = drop_curses_system command in
   handle_refresh iface reminders


let handle_view_keybindings (iface : interface_state_t) reminders =
   let bindings = ref [] in
   let find_binding commandstr operation =
      match operation with
      |Rcfile.CommandOp command ->
         begin try
            let key_str = Hashtbl.find Rcfile.table_command_key command in
            bindings := (Printf.sprintf "%-30s%-20s\n" commandstr key_str) :: !bindings
         with Not_found ->
            bindings := (Printf.sprintf "%-30s\n" commandstr) :: !bindings
         end
      |Rcfile.EntryOp entry ->
         begin try
            let key_str = Hashtbl.find Rcfile.table_entry_key entry in
            bindings := (Printf.sprintf "%-30s%-20s\n" commandstr key_str) :: !bindings
         with Not_found ->
            bindings := (Printf.sprintf "%-30s\n" commandstr) :: !bindings
         end
   in
   Hashtbl.iter find_binding Rcfile.table_commandstr_command;
   let sorted_list = List.fast_sort Pervasives.compare !bindings in
   def_prog_mode ();
   endwin ();
   let out_channel = Unix.open_process_out "less" in
   List.iter (output_string out_channel) sorted_list;
   flush out_channel;
   let _ = Unix.close_process_out out_channel in
   reset_prog_mode ();
   begin try
      assert (curs_set 0)
   with _ ->
      ()
   end;
   handle_refresh iface reminders


(* Handle scrolling down during selection dialog loop *)
let handle_selection_dialog_scrolldown (elements : string list) 
       (selection : int) (top : int) =
   if selection < pred (List.length elements) then
      let lines, cols = get_size () in
      let new_selection = succ selection in
      let new_top =
         if new_selection - top >= pred lines then 
            succ top 
         else 
            top
      in
      (new_selection, new_top)
   else
      (selection, top)


(* Handle scrolling up during selection dialog loop *)
let handle_selection_dialog_scrollup (elements : string list) 
       (selection : int) (top : int) =
   if selection > 0 then
      let new_selection = pred selection in
      let new_top =
         if new_selection < top then 
            pred top 
         else 
            top
      in
      (new_selection, new_top)
   else
      (selection, top)


(* Begin a selection dialog loop *)
let do_selection_dialog (iface : interface_state_t) (title : string) 
       (elements : string list) =
   let init_selection = 0
   and init_top       = 0 in
   let rec selection_loop (selection, top) =
      draw_selection_dialog iface title elements selection top;
      let key = wgetch iface.scr.help_win in
      (* trap the wgetch timeout *)
      if key <> ~-1 then
         try
            match Rcfile.command_of_key key with
            |Rcfile.ScrollDown ->
               selection_loop (handle_selection_dialog_scrolldown elements selection top)
            |Rcfile.ScrollUp ->
               selection_loop (handle_selection_dialog_scrollup elements selection top)
            |Rcfile.Edit ->
               List.nth elements selection
            |_ ->
               let _ = beep () in
               selection_loop (selection, top)
         with Not_found ->
            let _ = beep () in
            selection_loop (selection, top)
      else
         selection_loop (selection, top)
   in
   selection_loop (init_selection, init_top)


(* handle editing the selected reminder *)
let handle_edit (iface : interface_state_t) reminders =
   let adjust_s len s =
      let pad = s ^ (String.make len ' ') in
      Utility.utf8_string_before pad len
   in
   let fl =
      match iface.selected_side with
      |Left  -> 
         begin match iface.timed_lineinfo.(iface.left_selection) with
         | [] -> 
            None
         | tline :: [] -> 
            Some (tline.tl_filename, tline.tl_linenum, tline.tl_msg)
         | rem_list -> 
              let sorted_rem_list = List.fast_sort sort_lineinfo rem_list in
              let get_msg tline = (adjust_s 16 tline.tl_timestr) ^ tline.tl_msg in
              let msg_list = List.rev_map get_msg sorted_rem_list in
              let selected_msg =
                 do_selection_dialog iface "Choose a reminder to edit" msg_list
              in
              let test_msg_match tline = 
                 ((adjust_s 16 tline.tl_timestr) ^ tline.tl_msg) = selected_msg in
              let tline = (List.find test_msg_match sorted_rem_list) in
              Some (tline.tl_filename, tline.tl_linenum, tline.tl_msg)
         end
      |Right ->
         begin match iface.untimed_lineinfo.(iface.right_selection) with
         | Some uline -> Some (uline.ul_filename, uline.ul_linenum, uline.ul_msg)
         | None -> None
         end
   in
   begin match fl with
   |None ->
      let (main_remfile, _) = Remind.get_all_remfiles () in
      if iface.selected_side = Left then
         handle_new_reminder iface reminders Timed main_remfile
      else
         handle_new_reminder iface reminders Untimed main_remfile
   |Some (filename, line_num, msg) -> 
      let filename_sub = Str.regexp "%file%" in
      let lineno_sub   = Str.regexp "%line%" in
      let command_partial = 
         Str.global_replace filename_sub filename !Rcfile.edit_old_command
      in
      let command = Str.global_replace lineno_sub line_num command_partial in
      let _ = drop_curses_system command in
      let r = Remind.create_three_month iface.top_timestamp in
      (* if the untimed list has been altered, change the focus
       * to the first element *)
      let new_iface =
         if List.length r.Remind.curr_untimed <> 
            List.length reminders.Remind.curr_untimed then {
            iface with top_untimed = 0;
                       top_desc = 0;
                       right_selection = 1
            }
         else
            iface
      in
      handle_refresh new_iface r
   end



(* handle free editing of the reminders file *)
let handle_edit_any (iface : interface_state_t) reminders remfile =
   let filename_sub = Str.regexp "%file%" in
   let command = 
      Str.global_replace filename_sub remfile !Rcfile.edit_any_command
   in
   let _ = drop_curses_system command in
   let r = Remind.create_three_month iface.top_timestamp in
   (* if the untimed list has been altered, change the focus
    * to the first element *)
   let new_iface =
      if List.length r.Remind.curr_untimed <> 
         List.length reminders.Remind.curr_untimed then {
         iface with top_untimed = 0;
                    top_desc = 0;
                    right_selection = 1
         }
      else
         iface
   in
   handle_refresh new_iface r



(* handle scrolling the description window up *)
let handle_scroll_desc_up iface reminders =
   let new_iface = {iface with top_desc = succ iface.top_desc} in
   handle_refresh new_iface reminders


(* handle scrolling the description window down *)
let handle_scroll_desc_down iface reminders =
   let top = max 0 (pred iface.top_desc) in
   let new_iface = {iface with top_desc = top} in
   handle_refresh new_iface reminders


(* auxiliary function for the following *)
let handle_copy_reminder_aux iface reminders copy_only =
   let adjust_s len s =
      let pad = s ^ (String.make len ' ') in
      Utility.utf8_string_before pad len
   in
   let fl =
      match iface.selected_side with
      |Left  -> 
         begin match iface.timed_lineinfo.(iface.left_selection) with
         | [] -> 
            None
         | tline :: [] -> 
            Some (tline.tl_filename, tline.tl_linenum)
         | rem_list -> 
              let sorted_rem_list = List.fast_sort sort_lineinfo rem_list in
              let get_msg tline = (adjust_s 16 tline.tl_timestr) ^ tline.tl_msg in
              let msg_list = List.rev_map get_msg sorted_rem_list in
              let selected_msg =
                 let dialog_msg =
                    if copy_only then
                       "Choose a reminder to copy"
                    else
                       "Choose a reminder to cut"
                 in
                 do_selection_dialog iface dialog_msg msg_list
              in
              let _ = handle_refresh iface reminders in
              let test_msg_match tline = 
                 ((adjust_s 16 tline.tl_timestr) ^ tline.tl_msg) = selected_msg in
              let tline = (List.find test_msg_match sorted_rem_list) in
              Some (tline.tl_filename, tline.tl_linenum)
         end
      |Right ->
         begin match iface.untimed_lineinfo.(iface.right_selection) with
         |Some uline -> Some (uline.ul_filename, uline.ul_linenum)
         |None -> None
         end
   in
   begin match fl with
   |None ->
      let _ = beep () in
      draw_error iface "no reminder is selected." false;
      assert (doupdate ());
      (iface, fl)
   |Some (filename, line_num_str) ->
      let in_channel = open_in filename in
      let cont_regex = Str.regexp ".*\\(\\\\\\)$" in
      begin try
         (* grab a copy of the specified line *)
         let line = ref "" in
         for curr_line = 1 to int_of_string line_num_str do
            (* concatenate lines with trailing backslashes *)
            if Str.string_match cont_regex !line 0 then
               let bs_loc = Str.group_beginning 1 in
               line := (Str.string_before !line bs_loc) ^ (input_line in_channel)
            else
               line := input_line in_channel
         done;
         close_in in_channel;
         (* break the REM line up into chunks corresponding to the different fields *)
         let rem_kw_regex = 
            Str.regexp ("\\(REM\\|PRIORITY\\|SKIP\\|BEFORE\\|AFTER\\|" ^
            "OMIT\\|AT\\|SCHED\\|WARN\\|UNTIL\\|SCANFROM\\|DURATION\\|TAG\\|" ^
            "MSG\\|MSF\\|RUN\\|CAL\\|SATISFY\\|SPECIAL\\|PS\\|PSFILE\\)")
         in
         let rem_chunks = Str.full_split rem_kw_regex !line in
         (* merge the chunks back together, but replace the REM and AT clauses
          * with special markers *)
         let rec merge_chunks chunks s found_rem =
            match chunks with
            | [] ->
               if found_rem then s else "REMREPLACE " ^ s
            | (Str.Delim "REM") :: (Str.Text datespec) :: tail ->
               merge_chunks tail (s ^ "REMREPLACE") true
            | (Str.Delim "REM") :: tail ->
               merge_chunks tail (s ^ "REMREPLACE") true
            | (Str.Delim "AT")  :: (Str.Text timespec) :: tail ->
               merge_chunks tail (s ^ "ATREPLACE") found_rem
            | (Str.Delim "AT")  :: tail ->
               merge_chunks tail (s ^ "ATREPLACE") found_rem
            | (Str.Delim x)     :: tail ->
               merge_chunks tail (s ^ x) found_rem
            | (Str.Text x)      :: tail ->
               merge_chunks tail (s ^ x) found_rem
         in
         let marked_remline = merge_chunks rem_chunks "" false in
         let new_iface = {iface with rem_buffer = marked_remline} in
         if copy_only then begin
            draw_error iface "copied reminder to clipboard." false;
            assert (doupdate ());
         end else
            ();
         (new_iface, fl)
      with
      |End_of_file ->
         close_in in_channel;
         let _ = beep () in
         draw_error iface "error while copying reminder." false;
         assert (doupdate ());
         (iface, fl)
      |_ ->
         let _ = beep () in
         draw_error iface "unable to parse selected reminder." false;
         assert (doupdate ());
         (iface, fl)
      end
   end


(* handle copying a reminder string to the buffer *)
let handle_copy_reminder iface reminders copy_only =
   let new_iface, _ = handle_copy_reminder_aux iface reminders copy_only in
   (new_iface, reminders)


(* handle pasting a reminder into a new location *)
let handle_paste_reminder (iface : interface_state_t) reminders remfile =
   let ts = timestamp_of_line iface iface.left_selection in
   let tm = Unix.localtime ts in
   let rem_datespec = 
      "REM " ^ Utility.string_of_tm_mon tm.Unix.tm_mon ^ " " ^
      string_of_int tm.Unix.tm_mday ^ " " ^
      string_of_int (tm.Unix.tm_year + 1900) ^ " "
   and at_timespec =
      "AT " ^ Printf.sprintf "%.2d" tm.Unix.tm_hour ^ ":" ^
      Printf.sprintf "%.2d" tm.Unix.tm_min ^ " "
   in
   (* replace REMREPLACE and ATREPLACE keywords by sensible REM and
    * AT clauses corresponding to the selected date and time *)
   let rem_regex = Str.regexp "REMREPLACE" in
   let at_regex  = Str.regexp "ATREPLACE" in
   let temp        = Str.replace_first rem_regex rem_datespec iface.rem_buffer in
   let new_remline = Str.replace_first at_regex at_timespec temp in
   (* paste into the reminders file *)
   let remfile_channel = 
      open_out_gen [Open_append; Open_creat; Open_text] 416 remfile
   in
   output_string remfile_channel new_remline;
   close_out remfile_channel;
   (* open reminders file in editor *)
   let filename_sub = Str.regexp "%file%" in
   let command = 
      Str.global_replace filename_sub remfile
         !Rcfile.edit_new_command
   in
   let _ = drop_curses_system command in
   let r = Remind.create_three_month iface.top_timestamp in
   (* if the untimed list has been altered, change the focus to
    * the first element of the list *)
   let new_iface =
      if List.length r.Remind.curr_untimed <> 
         List.length reminders.Remind.curr_untimed then {
         iface with top_untimed = 0;
                    top_desc = 0;
                    right_selection = 1
         }
      else
         iface
   in
   handle_refresh new_iface r


(* handle cutting a reminder and dropping it in the clipboard *)
let handle_cut_reminder iface reminders =
   let (iface, fl) = handle_copy_reminder_aux iface reminders false in
   begin match fl with
   |None ->
      (iface, reminders)
   |Some (filename, line_num_str) ->
      let in_channel = open_in filename in
      let cont_regex = Str.regexp ".*\\(\\\\\\)$" in
      (* load in the file, but skip over the selected reminder *)
      let rec process_in_lines lines continuations line_num =
         try
            let line = input_line in_channel in
            if line_num = int_of_string line_num_str then
               (* throw out any continued lines *)
               process_in_lines lines [] (succ line_num)
            else if Str.string_match cont_regex line 0 then
               (* if there's a backslash, buffer the line in continuations list *)
               process_in_lines lines (line :: continuations) (succ line_num)
            else
               (* if there's no backslash, go ahead and dump the continuations list
                * into the list of lines to write *)
               process_in_lines (line :: (continuations @ lines)) [] (succ line_num)
         with End_of_file ->
            close_in in_channel;
            List.rev lines
      in
      let remaining_lines = process_in_lines [] [] 1 in
      let out_channel = open_out filename in
      (* write out the new file *)
      let rec write_lines lines =
         begin match lines with
         | [] ->
            close_out out_channel
         | line :: tail ->
            output_string out_channel (line ^ "\n");
            write_lines tail
         end
      in
      write_lines remaining_lines;
      let r = Remind.create_three_month iface.top_timestamp in
      (* if the untimed list has been altered, change the focus to
       * the first element of the list *)
      let new_iface =
         if List.length r.Remind.curr_untimed <> 
            List.length reminders.Remind.curr_untimed then {
            iface with top_untimed = 0;
                       top_desc = 0;
                       right_selection = 1
            }
         else
            iface
      in
      let final_iface, _ = handle_refresh new_iface r in
      draw_error final_iface "cut reminder to clipboard." false;
      assert (doupdate ());
      (final_iface, r)
   end


(* handle jumping to a specified date *)
let handle_goto (iface : interface_state_t) reminders =
   let tm = Unix.localtime (Unix.time () -. (time_inc iface)) in
   let len = String.length iface.extended_input in
   if not (List.mem len [2; 4; 8]) then
      failwith "length must be 2, 4, or 8 characters."
   else begin
      let (year_s, month_s, day_s) = 
         if !Rcfile.goto_big_endian then
            if len = 8 then (
               String.sub iface.extended_input 0 4,
               String.sub iface.extended_input 4 2,
               String.sub iface.extended_input 6 2
            )
            else if len = 4 then (
               string_of_int (tm.Unix.tm_year + 1900),
               String.sub iface.extended_input 0 2,
               String.sub iface.extended_input 2 2
            )
            else (
               string_of_int (tm.Unix.tm_year + 1900),
               string_of_int (tm.Unix.tm_mon + 1),
               iface.extended_input
            )
         else
            if len = 8 then (
               String.sub iface.extended_input 4 4,
               String.sub iface.extended_input 2 2,
               String.sub iface.extended_input 0 2
            )
            else if len = 4 then (
               string_of_int (tm.Unix.tm_year + 1900),
               String.sub iface.extended_input 2 2,
               String.sub iface.extended_input 0 2
            )
            else (
               string_of_int (tm.Unix.tm_year + 1900),
               string_of_int (tm.Unix.tm_mon + 1),
               iface.extended_input
            )
      in
      let year  = (int_of_string year_s) - 1900 
      and month = (int_of_string month_s) - 1
      and day   = int_of_string day_s in
      let jump_time = {
         tm with Unix.tm_year = year;
                 Unix.tm_mon  = month;
                 Unix.tm_mday = day
      } in
      let (rounded_time, rt) =
         try
            Unix.mktime (round_time iface.zoom_level jump_time)
         with _ ->
            failwith "requested date is out of range."
      in
      if (rt.Unix.tm_year + 1900) < 1991 || (rt.Unix.tm_year + 1900) > 2037 then
         failwith "requested year is out of range."
      else if rt.Unix.tm_mon <> jump_time.Unix.tm_mon then
         failwith "requested month is out of range."
      else if rt.Unix.tm_mday <> jump_time.Unix.tm_mday then
         failwith "requested day of the month is out of range."
      else
         ();
      let new_iface = {
         iface with top_timestamp = 
                       if !Rcfile.center_cursor then
                          rounded_time -. (time_inc iface) *. (float_of_int ((iface.scr.tw_lines / 2) - 2))
                       else
                          rounded_time -. (time_inc iface) *. 1.;
                    top_desc         = 0;
                    selected_side    = Left;
                    left_selection   = if !Rcfile.center_cursor then (iface.scr.tw_lines / 2) - 1 else 2;
                    right_selection  = 1;
                    entry_mode       = Normal;
                    extended_input   = ""
      } in
      handle_selection_change new_iface reminders false
   end
         

(* Begin entry of a date/time to navigate to *)
let handle_begin_goto (iface : interface_state_t) reminders =
   let new_iface = {
      iface with entry_mode = Extended ExtendedGoto
   } in
   if !Rcfile.goto_big_endian then
      draw_error iface "go to date [[YYYY]MM]DD: " true
   else
      draw_error iface "go to date DD[MM[YYYY]]: " true;
   (new_iface, reminders)


(* Handle creation of a quick event *)
let handle_quick_event (iface : interface_state_t) reminders remfile =
   try
      let (rem_spec, remline) = append_quick_event iface.extended_input remfile in
      (* navigate to the new reminder *)
      let (is_timed, start) =
         match rem_spec with
         | Time_lang.Timed (x, _) -> (true, x)
         | Time_lang.Untimed x    -> (false, x)
      in
      let rounded_time =
         let rt_shift = 
            if is_timed then
               let (x, _) = Unix.mktime (round_time iface.zoom_level start) in x
            else
               let (x, _) = Unix.mktime (round_time iface.zoom_level {start with Unix.tm_hour = 8}) in x
         in
         rt_shift -. (time_inc iface)
      in
      let r = Remind.create_three_month iface.top_timestamp in
      let new_iface = {
         iface with top_timestamp = 
                       if !Rcfile.center_cursor then
                          rounded_time -. (time_inc iface) *. (float_of_int ((iface.scr.tw_lines / 2) - 2))
                       else
                          rounded_time -. (time_inc iface) *. 1.;
                    top_desc        = 0;
                    selected_side   = if is_timed then Left else Right;
                    left_selection  = if !Rcfile.center_cursor then (iface.scr.tw_lines / 2) - 1 else 2;
                    right_selection = 1;
                    entry_mode      = Normal;
                    extended_input  = ""
      } in
      handle_selection_change new_iface r false
   with Time_lang.Event_parse_error s ->
      failwith s



(* Begin entry of a quick event *) 
let handle_begin_quick_event (iface : interface_state_t) reminders =
   let new_iface = {
      iface with entry_mode = Extended ExtendedQuick
   } in
   draw_error iface "event description: " true;
   (new_iface, reminders)



(* handle keyboard input for a hotkey, and update the display appropriately *)
let handle_keypress_normal key (iface : interface_state_t) reminders =
   try
      match Rcfile.command_of_key key with
      |Rcfile.ScrollDown ->
         begin match iface.selected_side with
         |Left  -> handle_scrolldown_timed iface reminders
         |Right -> handle_scrolldown_untimed iface reminders
         end
      |Rcfile.ScrollUp ->
         begin match iface.selected_side with
         |Left  -> handle_scrollup_timed iface reminders
         |Right -> handle_scrollup_untimed iface reminders
         end
      |Rcfile.NextDay ->
         let jump_func day = {day with Unix.tm_mday = succ day.Unix.tm_mday} in
         handle_jump iface reminders jump_func
      |Rcfile.PrevDay ->
         let jump_func day = {day with Unix.tm_mday = pred day.Unix.tm_mday} in
         handle_jump iface reminders jump_func
      |Rcfile.NextWeek ->
         let jump_func day = {day with Unix.tm_mday = day.Unix.tm_mday + 7} in
         handle_jump iface reminders jump_func
      |Rcfile.PrevWeek ->
         let jump_func day = {day with Unix.tm_mday = day.Unix.tm_mday - 7} in
         handle_jump iface reminders jump_func
      |Rcfile.NextMonth ->
         let jump_func day = {day with Unix.tm_mon = succ day.Unix.tm_mon} in
         handle_jump iface reminders jump_func
      |Rcfile.PrevMonth ->
         let jump_func day = {day with Unix.tm_mon = pred day.Unix.tm_mon} in
         handle_jump iface reminders jump_func
      |Rcfile.Zoom ->
         handle_zoom iface reminders
      |Rcfile.SwitchWindow ->
         handle_switch_focus iface reminders
      |Rcfile.Home ->
         handle_home iface reminders
      |Rcfile.Goto ->
         handle_begin_goto iface reminders
      |Rcfile.Edit ->
         handle_edit iface reminders
      |Rcfile.EditAny ->
         let (_, all_remfiles) = Remind.get_all_remfiles () in
         let selected_remfile = 
            (* if there's only one remfile, jump right in, otherwise
             * pop up a selection dialog *)
            if List.length all_remfiles > 1 then
               do_selection_dialog iface "Choose a reminders file to edit"
                  all_remfiles
            else
               List.hd all_remfiles
         in
         handle_edit_any iface reminders selected_remfile
      |Rcfile.CopyReminder ->
         handle_copy_reminder iface reminders true
      |Rcfile.CutReminder ->
         handle_cut_reminder iface reminders
      |Rcfile.PasteReminder ->
         if iface.rem_buffer = "" then begin
            let _ = beep () in
            draw_error iface "clipboard is empty." false;
            assert (doupdate ());
            (iface, reminders)
         end else begin
            let (main_remfile, _) = Remind.get_all_remfiles () in
            handle_paste_reminder iface reminders main_remfile
         end
      |Rcfile.PasteReminderDialog ->
         if iface.rem_buffer = "" then begin
            let _ = beep () in
            draw_error iface "clipboard is empty." false;
            assert (doupdate ());
            (iface, reminders)
         end else begin
            let (_, all_remfiles) = Remind.get_all_remfiles () in
            let selected_remfile = 
               (* if there's only one remfile, jump right in, otherwise
                * pop up a selection dialog *)
               if List.length all_remfiles > 1 then
                  do_selection_dialog iface "Choose a reminders file to paste into"
                     all_remfiles
               else
                  List.hd all_remfiles
            in
            handle_paste_reminder iface reminders selected_remfile
         end
      |Rcfile.ScrollDescUp ->
         handle_scroll_desc_up iface reminders
      |Rcfile.ScrollDescDown ->
         handle_scroll_desc_down iface reminders
      |Rcfile.QuickEvent ->
         handle_begin_quick_event iface reminders
      |Rcfile.NewTimed ->
         let (main_remfile, _) = Remind.get_all_remfiles () in
         handle_new_reminder iface reminders Timed main_remfile
      |Rcfile.NewTimedDialog ->
         let remfile = 
            let (_, all_remfiles) = Remind.get_all_remfiles () in
            do_selection_dialog iface "Choose a reminders file" all_remfiles
         in
         handle_new_reminder iface reminders Timed remfile
      |Rcfile.NewUntimed ->
         let (main_remfile, _) = Remind.get_all_remfiles () in
         handle_new_reminder iface reminders Untimed main_remfile
      |Rcfile.NewUntimedDialog ->
         let remfile = 
            let (_, all_remfiles) = Remind.get_all_remfiles () in
            do_selection_dialog iface "Choose a reminders file" all_remfiles
         in
         handle_new_reminder iface reminders Untimed remfile
      |Rcfile.NewGenReminder x ->
         let (main_remfile, _) = Remind.get_all_remfiles () in
         handle_new_reminder iface reminders (General x) main_remfile
      |Rcfile.NewGenReminderDialog x ->
         let remfile = 
            let (_, all_remfiles) = Remind.get_all_remfiles () in
            do_selection_dialog iface "Choose a reminders file" all_remfiles
         in
         handle_new_reminder iface reminders (General x) remfile
      |Rcfile.SearchNext ->
         handle_find_next iface reminders None
      |Rcfile.BeginSearch ->
         handle_begin_search iface reminders
      |Rcfile.NextReminder ->
         handle_next_reminder iface reminders
      |Rcfile.ViewReminders ->
         handle_view_reminders iface reminders false
      |Rcfile.ViewAllReminders ->
         handle_view_reminders iface reminders true
      |Rcfile.ViewWeek ->
         handle_view_calendar iface reminders true
      |Rcfile.ViewMonth ->
         handle_view_calendar iface reminders false
      |Rcfile.ViewKeybindings ->
         handle_view_keybindings iface reminders
      |Rcfile.Refresh ->
         (* NOTE: I'm not sure why the endwin call is necessary here,
          * but I'm having problems getting a true full-screen refresh
          * without it. *)
         def_prog_mode ();
         endwin ();
         reset_prog_mode ();
         begin try
            assert (curs_set 0)
         with _ ->
            ()
         end;
         let i = draw_msg iface in
         let reminders = Remind.create_three_month i.top_timestamp in
         handle_refresh i reminders
      |Rcfile.Quit ->
         let new_iface = {iface with run_wyrd = false} in
         (new_iface, reminders)
   with Not_found ->
      let _ = beep () in
      draw_error iface "key is not bound." false;
      assert (doupdate ());
      (iface, reminders)



(* handle keyboard input and update the display appropriately *)
let handle_keypress key (iface : interface_state_t) reminders =
   match iface.entry_mode with
   |Normal ->
      handle_keypress_normal key (iface : interface_state_t) reminders
   |Extended ext_mode ->
      begin try
         begin match Rcfile.entry_of_key key with
         |Rcfile.EntryComplete ->
            begin match ext_mode with
            |ExtendedSearch ->
               begin try
                  let new_iface = {
                     iface with search_regex = Str.regexp_case_fold iface.extended_input;
                                extended_input = "";
                                entry_mode = Normal
                  } in
                  handle_find_next new_iface reminders None
               with Failure err ->
                  let new_iface = {
                     iface with extended_input = "";
                                entry_mode = Normal
                  } in
                  let _ = beep () in
                  draw_error new_iface ("syntax error in search string: " ^ err) false;
                  assert (doupdate ());
                  (new_iface, reminders)
               end
            |ExtendedGoto ->
               begin try
                  handle_goto iface reminders
               with Failure err ->
                  let new_iface = {
                     iface with extended_input = "";
                                entry_mode = Normal
                  } in
                  let _ = beep () in
                  draw_error new_iface ("syntax error in date specifier: " ^ err) false;
                  assert (doupdate ());
                  (new_iface, reminders)
               end
            |ExtendedQuick ->
               begin try
                  let (main_remfile, _) = Remind.get_all_remfiles () in
                  handle_quick_event iface reminders main_remfile
               with Failure err ->
                  let new_iface = {
                     iface with extended_input = "";
                                entry_mode = Normal
                  } in
                  let _ = beep () in
                  draw_error new_iface ("syntax error in event description: " ^ err) false;
                  assert (doupdate ());
                  (new_iface, reminders)
               end
            end
         |Rcfile.EntryBackspace ->
            let len = Utility.utf8_len iface.extended_input in
            if len > 0 then begin
               let new_iface = {
                  iface with extended_input =
                             Utility.utf8_string_before iface.extended_input (pred len)
               } in
               let prompt =
                  match ext_mode with
                  |ExtendedSearch -> 
                     "search expression: "
                  |ExtendedGoto ->
                     if !Rcfile.goto_big_endian then
                        "go to date [[YYYY]MM]DD: "
                     else
                        "go to date DD[MM[YYYY]]: "
                  |ExtendedQuick ->
                     "event description: "
               in
               draw_error iface (prompt ^ new_iface.extended_input) true;
               (new_iface, reminders)
            end else
               let _ = beep () in
               (iface, reminders)
         |Rcfile.EntryExit ->
            let new_iface = {
               iface with extended_input = "";
                          entry_mode = Normal
            } in
            let msg =
               match ext_mode with
               |ExtendedSearch -> "search cancelled."
               |ExtendedGoto   -> "date entry cancelled."
               |ExtendedQuick  -> "quick event cancelled."
            in
            draw_error new_iface msg false;
            (new_iface, reminders)
         end
      with Not_found ->
         begin match ext_mode with
         |ExtendedGoto ->
            begin try
               (* only digits are accepted for goto dates *)
               if key >= 48 && key <= 57 && (String.length iface.extended_input < 8) then begin
                  let c = char_of_int key in
                  let new_iface = {
                     iface with extended_input = iface.extended_input ^ (String.make 1 c)
                  } in
                  if !Rcfile.goto_big_endian then
                     draw_error iface ("go to date [[YYYY]MM]DD: " ^ new_iface.extended_input) true
                  else
                     draw_error iface ("go to date DD[MM[YYYY]]: " ^ new_iface.extended_input) true;
                  (new_iface, reminders)
               end else
                  failwith "date characters must be digits"
            with Failure _ ->
               let _ = beep () in
               (iface, reminders)
            end
         |_ ->
            begin try
               (* Only printable characters are accepted for search strings and quick events.  "Printable" 
                * is defined as either "printable ASCII" or "non-ASCII UTF-8".  Hopefully this definition
                * will satisfy ncurses. *)
               if (Curses_config.wide_ncurses && key >= 0x80) || (key >= 32 && key <= 126) then begin
                  let c = char_of_int key in
                  let new_iface = {
                     iface with extended_input = iface.extended_input ^ (String.make 1 c)
                  } in
                  let prompt =
                     match ext_mode with
                     |ExtendedSearch -> "search expression: "
                     |ExtendedQuick  -> "event description: "
                     |_ -> failwith "logical error in generating extended entry string"
                  in
                  draw_error new_iface (prompt ^ new_iface.extended_input) true;
                  (new_iface, reminders)
               end else
                  let err_msg =
                     match ext_mode with
                     |ExtendedSearch -> "cannot search for unprintable characters"
                     |ExtendedQuick  -> "events cannot contain unprintable characters"
                     |ExtendedGoto   -> "logical error in generating extended entry string"
                  in
                  failwith err_msg
            with Failure _ | Invalid_argument "char_of_int" ->
               let _ = beep () in
               (iface, reminders)
            end
         end
   end



let rec do_main_loop (iface : interface_state_t) reminders last_update =
   if iface.run_wyrd then begin
      if String.length reminders.Remind.remind_error > 0 then
         draw_error iface ("Error in reminders file: \"" ^
         reminders.Remind.remind_error ^ "\"") false
      else
         ();
      let (iface, wgetch_target) =
        match iface.resize_failed_win with
        | None ->
            (* refresh the msg window (which contains a clock) every wgetch timeout cycle *)
            let iface = draw_msg iface in
            assert (doupdate ());
            (iface, iface.scr.help_win)
        | Some w ->
            (iface, w)
      in
      let key = wgetch wgetch_target in
      let new_iface, new_reminders = 
         (* key = -1 is ncurses wgetch timeout error *)
         if key <> ~- 1 then
            if key = Key.resize then
               handle_resize iface reminders
            else 
               begin match iface.resize_failed_win with
               | None   -> handle_keypress key iface reminders
               | Some w -> let _ = beep () in (iface, reminders)
               end
         else if iface.track_home then
            (* cursor tracks the current time if requested *)
            handle_home iface reminders
         else
            (iface, reminders)
      in
      let remfile_mtimes = Remind.get_remfile_mtimes () in
      let curr_time = Unix.time () in
      (* if stat() indicates that remfiles were updated, or if five minutes has passed,
       * then poll remind(1) and update the display *)
      if (remfile_mtimes <> new_iface.remfile_mtimes || curr_time -. last_update > 300.0) &&
            iface.resize_failed_win = None then begin
         let r = Remind.create_three_month new_iface.top_timestamp in
         (* if the untimed list has been altered, change the focus to
          * the timed window *)
         let new_iface =
            if List.length r.Remind.curr_untimed <> 
                  List.length reminders.Remind.curr_untimed then {
               new_iface with selected_side = Left;
                              top_untimed = 0;
                              right_selection = 1;
                              remfile_mtimes = remfile_mtimes
               }
            else
               {new_iface with remfile_mtimes = remfile_mtimes}
         in
         let (iface2, reminders2) = handle_refresh new_iface r in
         do_main_loop iface2 reminders2 curr_time
      end else
         do_main_loop new_iface new_reminders last_update
   end else
      endwin () (* exit main loop *)


(* initialize the interface and begin the main loop *)
let run (iface : interface_state_t) =
   let set_bkgd win obj =
      try
         let color_index = Hashtbl.find Rcfile.object_palette obj in
         wbkgd win ((A.color_pair color_index) lor (int_of_char ' '))
      with Not_found ->
         ()
   in
   (* set up the proper background colors for all the windows *)
   set_bkgd iface.scr.help_win Rcfile.Help;
   set_bkgd iface.scr.timed_win Rcfile.Timed_default;
   set_bkgd iface.scr.calendar_win Rcfile.Calendar_labels;
   set_bkgd iface.scr.untimed_win Rcfile.Untimed_reminder;
   set_bkgd iface.scr.msg_win Rcfile.Description; 
   scrollok iface.scr.timed_win true;
   let reminders = Remind.create_three_month (iface.top_timestamp) in
   let iface = {iface with remfile_mtimes = Remind.get_remfile_mtimes ()} in
   assert (keypad iface.scr.help_win true);
   draw_help iface;
   draw_date_strip iface;
   let new_iface = draw_timed iface reminders.Remind.all_timed in
   draw_calendar new_iface reminders;
   let new_iface = draw_untimed new_iface reminders.Remind.curr_untimed in
   let new_iface = draw_msg new_iface in
   draw_error new_iface "" false;
   assert (doupdate ());
   do_main_loop new_iface reminders (Unix.time ())
        




(* arch-tag: DO_NOT_CHANGE_b4519dd2-7e94-4cbf-931a-bb5f97445cbf *)
