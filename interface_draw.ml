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


(* interface_draw.ml
 * All drawing operations are found here.
 *)

open Interface
open Curses
open Remind
open Utility


(* sort timed lineinfo entries by starting timestamp *)
let sort_lineinfo line1 line2 =
   ~- (Pervasives.compare line1.tl_start line2.tl_start)


(* Word-wrap a string--split the string at whitespace boundaries
 * to form a list of strings, each of which has length less than
 * 'len'. *)
let word_wrap (s : string) (len : int) =
   let ws = Str.regexp "[\t ]+" in
   let split_words = Str.split ws s in
   let rec process_words words lines =
      match words with
      |[] ->
         List.rev lines
      |word :: remaining_words ->
         let word_len = utf8_len word in
         begin match lines with
         |[] ->
            process_words words [""]
         |line :: remaining_lines ->
            let line_len = utf8_len line in
            if word_len + line_len + 1 <= len then
               if line_len = 0 then
                  process_words remaining_words (word :: remaining_lines)
               else
                  process_words remaining_words ((line ^ " " ^ word) :: remaining_lines)
            else if word_len <= len then
               process_words remaining_words (word :: line :: remaining_lines)
            else
               (* No choice but to break the word apart *)
               let front = utf8_string_before word len
               and back  = utf8_string_after  word len in
               process_words (back :: remaining_words) 
                  (front :: line :: remaining_lines)
         end
   in
   process_words split_words []


(* Generate a 12-hour clock representation of a time record *)
let twelve_hour_string tm =
   if tm.Unix.tm_hour >= 12 then 
      let hour = tm.Unix.tm_hour - 12 in
      if hour = 0 then
         Printf.sprintf "12:%.2dpm" tm.Unix.tm_min
      else
         Printf.sprintf "%d:%.2dpm" hour tm.Unix.tm_min
   else
      if tm.Unix.tm_hour = 0 then
         Printf.sprintf "12:%.2dam" tm.Unix.tm_min
      else
         Printf.sprintf "%d:%.2dam" tm.Unix.tm_hour tm.Unix.tm_min


(* Generate a 12-hour clock representation of a time record, with whitespace padding *)
let twelve_hour_string_pad tm =
   if tm.Unix.tm_hour >= 12 then 
      let hour = tm.Unix.tm_hour - 12 in
      if hour = 0 then
         Printf.sprintf "12:%.2dpm" tm.Unix.tm_min
      else
         Printf.sprintf "%2d:%.2dpm" hour tm.Unix.tm_min
   else
      if tm.Unix.tm_hour = 0 then
         Printf.sprintf "12:%.2dam" tm.Unix.tm_min
      else
         Printf.sprintf "%2d:%.2dam" tm.Unix.tm_hour tm.Unix.tm_min


(* Generate a 24-hour clock representation of a time record *)
let twentyfour_hour_string tm = Printf.sprintf "%.2d:%.2d" tm.Unix.tm_hour tm.Unix.tm_min

                                   
(* Draw a string in a specified window and location, using exactly 'len'
 * characters and truncating with ellipses if necessary. *)
let trunc_mvwaddstr win line col len s =
   let fixed_s =
      let s_len = utf8_len s in
      if s_len <= len then
         let pad = String.make (len - s_len) ' ' in
         s ^ pad
      else if len >= 3 then
         (utf8_string_before s (len - 3)) ^ "..."
      else if len >= 0 then
         utf8_string_before s len
      else
         ""
   in
   assert (mvwaddstr win line col fixed_s)


(* Draw the one-line help window at the top of the screen *)
let draw_help (iface : interface_state_t) =
   Rcfile.color_on iface.scr.help_win Rcfile.Help;
   wattron iface.scr.help_win (A.bold lor A.underline);
   let rec build_help_line operations s =
      match operations with
      |[] -> 
         s
      |(op, op_string) :: tail ->
         try
            let key_string = Rcfile.key_of_command op in
            build_help_line tail (s ^ key_string ^ ":" ^ op_string ^ "   ")
         with Not_found ->
            build_help_line tail s
   in
   let help_string = 
      build_help_line [(Rcfile.ViewKeybindings, "help"); (Rcfile.NewTimed, "new timed"); 
                       (Rcfile.NewUntimed, "new untimed"); (Rcfile.Edit, "edit"); 
                       (Rcfile.Home, "home"); (Rcfile.Zoom, "zoom"); (Rcfile.BeginSearch, "search"); 
                       (Rcfile.Quit, "quit")] ""
   in
   trunc_mvwaddstr iface.scr.help_win 0 0 iface.scr.hw_cols help_string;
   assert (wnoutrefresh iface.scr.help_win)


(* Draw the vertical date strip at the left of the timed window.
 * Note: non-trivial.
 * The first date stamp is a special case; it is drawn at the top
 * of the screen and truncated at the beginning to fit before the
 * first date change.  The remaining date stamps are all drawn immediately
 * after any date changes, and truncated at the end to fit in the
 * window.
 * Step 1: determine the line numbers on which the dates change
 * Step 2: create a string to represent the vertical strip
 * Step 3: draw each of the characters of the string onto the window *)
let draw_date_strip (iface : interface_state_t) =
   (* draw the vertical line to the right of the date string *)
   let acs = get_acs_codes () in
   Rcfile.color_on iface.scr.timed_win Rcfile.Left_divider;
   wattron iface.scr.timed_win A.bold;
   mvwvline iface.scr.timed_win 0 1 acs.Acs.vline iface.scr.tw_lines;
   Rcfile.color_off iface.scr.timed_win Rcfile.Left_divider;
   (* determine the line numbers and timestamps of any date changes within the
    * timed window *)
   let rec check_timestamp date_changes timestamp line =
      if line >= iface.scr.tw_lines then
         date_changes
      else
         let timestamp_tm = Unix.localtime timestamp in
         let next_timestamp = timestamp +. (time_inc iface) in
         let temp = {
            timestamp_tm with
              Unix.tm_sec = 0;
              Unix.tm_min = ~- (time_inc_min iface);
              Unix.tm_hour = 0
         } in
         let (_, before_midnight) = Unix.mktime temp in
         if timestamp_tm.Unix.tm_min = before_midnight.Unix.tm_min && 
            timestamp_tm.Unix.tm_hour = before_midnight.Unix.tm_hour then
            check_timestamp ((line, timestamp) :: date_changes) next_timestamp (succ line)
         else
            check_timestamp date_changes next_timestamp (succ line)
   in
   let date_changes = List.rev (check_timestamp [] iface.top_timestamp 0) in
   (* generate a string to represent the vertical strip *)
   let date_chars = 
      if List.length date_changes > 0 then begin
         (* special case for the top date string, which is always at the
          * top of the screen *)
         let (line, timestamp) = List.hd date_changes in
         let tm = Unix.localtime timestamp in
         let top_date_str = 
            if line >= 7 then
               (* the date will fit completely *)
               (Printf.sprintf " %s %.2d" (string_of_tm_mon tm.Unix.tm_mon) 
                   tm.Unix.tm_mday) ^ (String.make (line - 7) ' ')
            else
               (* there's not enough room for the date, so truncate it *)
               Str.last_chars
               (Printf.sprintf " %s %.2d" (string_of_tm_mon tm.Unix.tm_mon) 
                   tm.Unix.tm_mday) line
         in
         (* all other dates are just rendered at the top of their respective windows *)
         let rec add_date date_str changes =
            match changes with
            | [] -> date_str
            | (line, timestamp) :: tail -> 
               let tm = Unix.localtime timestamp in
               let temp = {
                  tm with Unix.tm_mday = succ tm.Unix.tm_mday
               } in
               let (_, next_day) = Unix.mktime temp in
               let s_len = 
                  if List.length tail > 0 then
                     let (next_line, _) = List.hd tail in
                     next_line - line
                  else
                     iface.scr.tw_lines - line
               in
               let temp_s = 
                  (Printf.sprintf "-%s %.2d" (string_of_tm_mon next_day.Unix.tm_mon) 
                      next_day.Unix.tm_mday) ^ (String.make 100 ' ')
               in
               add_date (date_str ^ (Str.string_before temp_s s_len)) tail
         in
         add_date top_date_str date_changes
      end else
         (* if there are no date changes (e.g. for small window) then just grab the proper
          * date from the top_timestamp *)
         let tm = Unix.localtime iface.top_timestamp in
         (Printf.sprintf " %s %.2d" (string_of_tm_mon tm.Unix.tm_mon) 
             tm.Unix.tm_mday) ^ (String.make (iface.scr.tw_lines - 6) ' ') 
   in
   (* draw the date string vertically, one character at a time *)
   for i = 0 to pred iface.scr.tw_lines do
      if date_chars.[i] = '-' then begin
         wattron iface.scr.timed_win A.underline;
         Rcfile.color_on iface.scr.timed_win Rcfile.Timed_date;
         assert (mvwaddch iface.scr.timed_win i 0 acs.Acs.hline);
         Rcfile.color_off iface.scr.timed_win Rcfile.Timed_date;
         Rcfile.color_on iface.scr.timed_win Rcfile.Left_divider;
         assert (mvwaddch iface.scr.timed_win i 1 acs.Acs.rtee);
         Rcfile.color_off iface.scr.timed_win Rcfile.Left_divider;
         wattroff iface.scr.timed_win A.underline;
      end else begin
         Rcfile.color_on iface.scr.timed_win Rcfile.Timed_date;
         assert (mvwaddch iface.scr.timed_win i 0 (int_of_char date_chars.[i]));
         Rcfile.color_off iface.scr.timed_win Rcfile.Timed_date;
      end
   done;
   wattroff iface.scr.timed_win (A.bold lor A.underline);
   assert (wnoutrefresh iface.scr.timed_win)


(* Draw a portion of the timed schedule.  The algorithm iterates across all
 * reminders in a three month period, and draws them in one by one.  An array
 * is used to keep track of which lines have not yet been drawn, so that these
 * can be filled in later.
 *
 * This routine also updates iface.timed_lineinfo. *)
let draw_timed_window iface reminders top lines =
   let round_down x =
      if x >= 0.0 then int_of_float x
      else pred (int_of_float x)
   in
   let indent_colors = [| 
      Rcfile.Timed_reminder1;
      Rcfile.Timed_reminder2;
      Rcfile.Timed_reminder3;
      Rcfile.Timed_reminder4
   |] in
   let acs = get_acs_codes () in
   let blank = String.make iface.scr.tw_cols ' ' in
   let top_timestamp = timestamp_of_line iface top in
   let top_tm = Unix.localtime top_timestamp in
   let temp = {
      top_tm with
        Unix.tm_sec = 0;
        Unix.tm_min = ~- (time_inc_min iface);
        Unix.tm_hour = 0
   } in
   let (_, before_midnight) = Unix.mktime temp in
   let string_of_tm =
      if !Rcfile.schedule_12_hour then
         twelve_hour_string_pad
      else
         twentyfour_hour_string
   in
   (* this ensures that if there are multiple reminder descriptions
    * displayed simultaneously, the dashes between start and end times
    * will line up properly *)
   let (desc_string_of_tm1, desc_string_of_tm2) =
      if !Rcfile.description_12_hour then
         (twelve_hour_string_pad, twelve_hour_string)
      else
         (twentyfour_hour_string, twentyfour_hour_string)
   in
   Rcfile.color_on iface.scr.timed_win Rcfile.Timed_default;
   (* draw in the blank timeslots *)
   for i = top to pred (top + lines) do
      iface.timed_lineinfo.(i) <- [];
      if i = iface.left_selection && iface.selected_side = Left then
         wattron iface.scr.timed_win A.reverse
      else
         wattroff iface.scr.timed_win A.reverse;
      let ts = timestamp_of_line iface i in
      let tm = Unix.localtime ts in
      let ts_str = string_of_tm tm in
      let curr_ts = Unix.time () in
      (* the current time is highlighted *)
      if curr_ts >= ts && curr_ts < (timestamp_of_line iface (succ i)) then begin 
         Rcfile.color_on iface.scr.timed_win Rcfile.Timed_current;
         wattron iface.scr.timed_win A.bold
      end else
         ();
      if tm.Unix.tm_hour = before_midnight.Unix.tm_hour &&
         tm.Unix.tm_min  = before_midnight.Unix.tm_min then
         wattron iface.scr.timed_win A.underline
      else
         wattroff iface.scr.timed_win A.underline;
      assert (mvwaddstr iface.scr.timed_win i 2 ts_str);
      Rcfile.color_off iface.scr.timed_win Rcfile.Timed_current;
      wattroff iface.scr.timed_win A.bold;
      Rcfile.color_on iface.scr.timed_win Rcfile.Timed_default;
      let s = Str.string_before blank (iface.scr.tw_cols - 7) in
      assert (mvwaddstr iface.scr.timed_win i 7 s)
   done;
   Rcfile.color_off iface.scr.timed_win Rcfile.Timed_default;
   wattroff iface.scr.timed_win (A.reverse lor A.underline);
   (* draw in the timed reminders *)
   let rec process_reminders rem_list indent =
      Rcfile.color_on iface.scr.timed_win indent_colors.(indent);
      wattron iface.scr.timed_win A.bold;
      begin match rem_list with
      |[] ->
         ()
      |rem :: tail ->
         let rem_top_line =
            round_down ((rem.tr_start -. iface.top_timestamp) /. (time_inc iface))
         in
         let get_time_str () =
            if rem.tr_end > rem.tr_start then
               (desc_string_of_tm1 (Unix.localtime rem.tr_start)) ^ "-" ^
               (desc_string_of_tm2 (Unix.localtime rem.tr_end)) ^ " "
            else
               (desc_string_of_tm1 (Unix.localtime rem.tr_start)) ^ " "
         in
         (* draw the top line of a reminder *)
         let clock_pad = if !Rcfile.schedule_12_hour then 10 else 8 in
         if rem_top_line >= top && rem_top_line < top + lines then begin
            let time_str = get_time_str () in
            let ts = timestamp_of_line iface rem_top_line in
            let tm = Unix.localtime ts in
            if rem_top_line = iface.left_selection && iface.selected_side = Left then
               wattron iface.scr.timed_win A.reverse
            else
               wattroff iface.scr.timed_win A.reverse;
            if tm.Unix.tm_hour = before_midnight.Unix.tm_hour &&
               tm.Unix.tm_min  = before_midnight.Unix.tm_min then
               wattron iface.scr.timed_win A.underline
            else
               wattroff iface.scr.timed_win A.underline;
            let curr_lineinfo = {
               tl_filename = rem.tr_filename;
               tl_linenum  = rem.tr_linenum;
               tl_timestr  = time_str;
               tl_msg      = rem.tr_msg;
               tl_start    = rem.tr_start
            } in
            iface.timed_lineinfo.(rem_top_line) <- 
               (curr_lineinfo :: iface.timed_lineinfo.(rem_top_line));
            trunc_mvwaddstr iface.scr.timed_win rem_top_line (clock_pad + (9 * indent)) 
               (iface.scr.tw_cols - clock_pad - (9 * indent)) ("  " ^ rem.tr_msg);
            assert (mvwaddch iface.scr.timed_win rem_top_line 
               (clock_pad + (9 * indent)) acs.Acs.vline)
         end else
            ();
         (* draw any remaining lines of this reminder, as determined by the duration *)
         let count = ref 1 in
         while 
            ((timestamp_of_line iface (rem_top_line + !count)) < rem.tr_end) &&
            (rem_top_line + !count < top + lines)
         do
            if rem_top_line + !count >= top then begin
               let time_str = get_time_str () in
               let ts = timestamp_of_line iface (rem_top_line + !count) in
               let tm = Unix.localtime ts in
               if rem_top_line + !count = iface.left_selection && 
                  iface.selected_side = Left then
                  wattron iface.scr.timed_win A.reverse
               else
                  wattroff iface.scr.timed_win A.reverse;
               if tm.Unix.tm_hour = before_midnight.Unix.tm_hour &&
                  tm.Unix.tm_min  = before_midnight.Unix.tm_min then
                  wattron iface.scr.timed_win A.underline
               else
                  wattroff iface.scr.timed_win A.underline;
               let curr_lineinfo = {
                  tl_filename = rem.tr_filename;
                  tl_linenum  = rem.tr_linenum;
                  tl_timestr  = time_str;
                  tl_msg      = rem.tr_msg;
                  tl_start    = rem.tr_start
               } in
               iface.timed_lineinfo.(rem_top_line + !count) <- 
                  (curr_lineinfo :: iface.timed_lineinfo.(rem_top_line + !count));
               trunc_mvwaddstr iface.scr.timed_win (rem_top_line + !count) 
                  (clock_pad + (9 * indent)) (iface.scr.tw_cols - clock_pad - (9 * indent)) " ";
               assert (mvwaddch iface.scr.timed_win (rem_top_line + !count) 
                  (clock_pad + (9 * indent)) acs.Acs.vline)
            end else
               ();
            count := succ !count
         done;
         (* The reminders list is sorted chronologically, so once we hit a reminder
          * that falls after the end of the calendar window we can stop. *)
         if rem_top_line < top + lines then
            process_reminders tail indent
         else
            ()
      end;
      Rcfile.color_off iface.scr.timed_win indent_colors.(indent)
   in
   (* reminders are rendered in order of indentation level, so the reminders with
    * higher indentation overlap those with lower indentation *)
   for indent = 0 to pred (Array.length reminders) do
      process_reminders reminders.(indent) indent
   done;
   wattroff iface.scr.timed_win (A.bold lor A.reverse);
   assert (wnoutrefresh iface.scr.timed_win)


(* Draw the entire timed reminders window *)
let draw_timed iface reminders = 
   draw_timed_window iface reminders 0 iface.scr.tw_lines;
   {iface with last_timed_refresh = Unix.time ()}


(* Draw just a portion of the timed window when possible.  If
 * iface.last_timed_refresh indicates that the display is stale,
 * then do a whole-window update.  This is to make sure that the
 * current timeslot gets highlighted properly, and old highlights
 * get erased. *)
let draw_timed_try_window iface reminders top lines =
   let curr_tm  = Unix.localtime (Unix.time ()) in
   let timeslot = round_time iface.zoom_level curr_tm in
   let (ts, _)  = Unix.mktime timeslot in
   if iface.last_timed_refresh < ts then
      draw_timed iface reminders
   else begin
      draw_timed_window iface reminders top lines;
      iface
   end


(* render a calendar for the given reminders record *)
let draw_calendar (iface : interface_state_t) 
       (reminders : three_month_rem_t) : unit =
   let sel_tm   = Unix.localtime (timestamp_of_line iface iface.left_selection)
   and today_tm = Unix.localtime (Unix.time()) in
   let cal = reminders.curr_cal in
   let acs = get_acs_codes () in
   Rcfile.color_on iface.scr.calendar_win Rcfile.Right_divider;
   wattron iface.scr.calendar_win A.bold;
   mvwvline iface.scr.calendar_win 0 0 acs.Acs.vline iface.scr.cw_lines;
   Rcfile.color_off iface.scr.calendar_win Rcfile.Right_divider;
   let hspacer = (iface.scr.cw_cols - 20) / 2 in
   let vspacer = (iface.scr.cw_lines - 8) / 2 in
   assert (wmove iface.scr.calendar_win vspacer hspacer);
   wclrtoeol iface.scr.calendar_win;
   Rcfile.color_on iface.scr.calendar_win Rcfile.Calendar_labels;
   wattron iface.scr.calendar_win A.bold;
   assert (waddstr iface.scr.calendar_win cal.Cal.title);
   wattroff iface.scr.calendar_win A.bold;
   assert (wmove iface.scr.calendar_win (vspacer + 1) hspacer);
   wclrtoeol iface.scr.calendar_win;
   assert (waddstr iface.scr.calendar_win cal.Cal.weekdays);
   Rcfile.color_off iface.scr.calendar_win Rcfile.Calendar_labels;
   (* draw the day numbers *)
   let ws = Str.regexp " " in
   let rec draw_week weeks line =
      match weeks with
      | [] ->
         assert (wmove iface.scr.calendar_win line hspacer);
         wclrtoeol iface.scr.calendar_win
      | week :: tail ->
         let split_week = Str.full_split ws week in
         assert (wmove iface.scr.calendar_win line hspacer);
         wclrtoeol iface.scr.calendar_win;
         let rec draw_el elements =
            match elements with
            | [] ->
               ()
            | el :: days ->
               begin match el with
               |Str.Delim s -> 
                  assert (waddstr iface.scr.calendar_win s)
               |Str.Text d ->
                  let day = pred (int_of_string d) in
                  if succ day = sel_tm.Unix.tm_mday then begin
                     (* highlight selected day in reverse video *)
                     wattron iface.scr.calendar_win A.reverse;
                     (* assert (waddstr iface.scr.calendar_win d); *)
                  end else
                     ();
                  if today_tm.Unix.tm_year = sel_tm.Unix.tm_year &&
                     today_tm.Unix.tm_mon = sel_tm.Unix.tm_mon &&
                     succ day = today_tm.Unix.tm_mday then begin
                     (* highlight today's date *)
                     Rcfile.color_on iface.scr.calendar_win Rcfile.Calendar_today;
                     wattron iface.scr.calendar_win A.bold;
                     assert (waddstr iface.scr.calendar_win d);
                     Rcfile.color_off iface.scr.calendar_win Rcfile.Calendar_today;
                     wattroff iface.scr.calendar_win A.bold
                  end else if reminders.curr_counts.(day) <= !Rcfile.busy_level1 then begin
                     Rcfile.color_on iface.scr.calendar_win Rcfile.Calendar_level1;
                     assert (waddstr iface.scr.calendar_win d);
                     Rcfile.color_off iface.scr.calendar_win Rcfile.Calendar_level1
                  end else if reminders.curr_counts.(day) <= !Rcfile.busy_level2 then begin
                     Rcfile.color_on iface.scr.calendar_win Rcfile.Calendar_level2;
                     assert (waddstr iface.scr.calendar_win d);
                     Rcfile.color_off iface.scr.calendar_win Rcfile.Calendar_level2
                  end else if reminders.curr_counts.(day) <= !Rcfile.busy_level3 then begin
                     Rcfile.color_on iface.scr.calendar_win Rcfile.Calendar_level2;
                     wattron iface.scr.calendar_win A.bold;
                     assert (waddstr iface.scr.calendar_win d);
                     Rcfile.color_off iface.scr.calendar_win Rcfile.Calendar_level2;
                     wattroff iface.scr.calendar_win A.bold
                  end else if reminders.curr_counts.(day) <= !Rcfile.busy_level4 then begin
                     Rcfile.color_on iface.scr.calendar_win Rcfile.Calendar_level3;
                     assert (waddstr iface.scr.calendar_win d);
                     Rcfile.color_off iface.scr.calendar_win Rcfile.Calendar_level3
                  end else begin
                     Rcfile.color_on iface.scr.calendar_win Rcfile.Calendar_level3;
                     wattron iface.scr.calendar_win A.bold;
                     assert (waddstr iface.scr.calendar_win d);
                     Rcfile.color_off iface.scr.calendar_win Rcfile.Calendar_level3;
                     wattroff iface.scr.calendar_win A.bold
                  end;
                  wattroff iface.scr.calendar_win A.reverse
               end;
               draw_el days
         in
         draw_el split_week;
         draw_week tail (succ line)
   in
   draw_week cal.Cal.days (vspacer + 2);
   (* draw the week numbers *)
   if !Rcfile.number_weeks then begin
      let weeknum_col = hspacer + (utf8_len cal.Cal.weekdays) + 2 in
      Rcfile.color_on iface.scr.calendar_win Rcfile.Calendar_labels;
      assert (wmove iface.scr.calendar_win (vspacer + 1) weeknum_col);
      assert (waddch iface.scr.calendar_win acs.Acs.vline);
      assert (waddstr iface.scr.calendar_win "Wk#");
      Rcfile.color_off iface.scr.calendar_win Rcfile.Calendar_labels;
      let rec print_weeknum weeknums line =
         match weeknums with
         | [] -> 
            ()
         | weeknum :: tail ->
            assert (wmove iface.scr.calendar_win (vspacer + line) weeknum_col);
            assert (waddch iface.scr.calendar_win acs.Acs.vline);
            assert (waddstr iface.scr.calendar_win weeknum);
            print_weeknum tail (succ line);
      in
      print_weeknum cal.Cal.weeknums 2;
   end else
      ();
   assert (wnoutrefresh iface.scr.calendar_win)



(* Draw the untimed reminders window *)
let draw_untimed (iface : interface_state_t) reminders =
   let lineinfo = Array.make iface.scr.uw_lines None in
   let blank = String.make iface.scr.uw_cols ' ' in
   let curr_ts = timestamp_of_line iface iface.left_selection in
   let today_reminders = Remind.get_untimed_reminders_for_day reminders curr_ts in
   werase iface.scr.untimed_win;
   let acs = get_acs_codes () in
   Rcfile.color_on iface.scr.untimed_win Rcfile.Right_divider;
   wattron iface.scr.untimed_win A.bold;
   assert (mvwaddch iface.scr.untimed_win 0 0 acs.Acs.ltee);
   mvwhline iface.scr.untimed_win 0 1 acs.Acs.hline (pred iface.scr.uw_cols);
   mvwvline iface.scr.untimed_win 1 0 acs.Acs.vline (pred iface.scr.uw_lines);
   Rcfile.color_off iface.scr.untimed_win Rcfile.Right_divider;
   if not !Rcfile.untimed_bold then
      wattroff iface.scr.untimed_win A.bold
   else
      ();
   Rcfile.color_on iface.scr.untimed_win Rcfile.Untimed_reminder;
   (* make sure the cursor doesn't unexpectedly disappear *)
   let iface =
      if iface.selected_side = Right && 
      iface.right_selection > List.length today_reminders then
         if List.length today_reminders = 0 then {
            iface with right_selection = 1;
         } else {
            iface with right_selection = List.length today_reminders
         }
      else
         iface
   in
   let rec render_lines rem_list n line =
      match rem_list with
      |[] ->
         if n = 0 && iface.selected_side = Right then begin
            wattron iface.scr.untimed_win A.reverse;
            trunc_mvwaddstr iface.scr.untimed_win line 2 (iface.scr.uw_cols - 3)  blank;
            wattroff iface.scr.untimed_win A.reverse
         end else
            ()
      |rem :: tail ->
         if line < iface.scr.uw_lines then
            if n >= iface.top_untimed then begin
               if line = iface.right_selection && iface.selected_side = Right then
                  wattron iface.scr.untimed_win A.reverse
               else
                  wattroff iface.scr.untimed_win A.reverse;
               trunc_mvwaddstr iface.scr.untimed_win line 2 (iface.scr.uw_cols - 3) 
                  ("* " ^ rem.ur_msg);
               let curr_lineinfo = {
                  ul_filename = rem.ur_filename;
                  ul_linenum  = rem.ur_linenum;
                  ul_msg      = rem.ur_msg
               } in
               lineinfo.(line) <- Some curr_lineinfo;
               render_lines tail (succ n) (succ line)
            end else
               render_lines tail (succ n) line
         else
            ()
   in
   render_lines today_reminders 0 1;
   Rcfile.color_off iface.scr.untimed_win Rcfile.Untimed_reminder;
   wattroff iface.scr.untimed_win (A.bold lor A.reverse);
   (* if there's not enough window space to display all untimed reminders, display
    * arrows to indicate scrolling ability *)
   if iface.top_untimed > 0 then begin
      assert (mvwaddch iface.scr.untimed_win 1 (pred iface.scr.uw_cols) (int_of_char '^'));
      assert (mvwaddch iface.scr.untimed_win 2 (pred iface.scr.uw_cols) (int_of_char '^'))
   end else
      ();
   if List.length today_reminders > pred iface.scr.uw_lines + iface.top_untimed then begin
      assert (mvwaddch iface.scr.untimed_win (pred iface.scr.uw_lines) 
             (pred iface.scr.uw_cols) (int_of_char 'v'));
      assert (mvwaddch iface.scr.untimed_win (iface.scr.uw_lines - 2) 
             (pred iface.scr.uw_cols) (int_of_char 'v'))
   end else
      ();
   assert (wnoutrefresh iface.scr.untimed_win);
   {iface with untimed_lineinfo = lineinfo;
               len_untimed      = List.length today_reminders}


(* Draw the message window *)
let draw_msg iface =
   let sel_tm = 
      Unix.localtime (timestamp_of_line iface iface.left_selection)
   in
   let day_s = 
      Printf.sprintf "%s, %s %.2d" (full_string_of_tm_wday sel_tm.Unix.tm_wday)
         (full_string_of_tm_mon sel_tm.Unix.tm_mon) sel_tm.Unix.tm_mday
   in
   let day_time_s =
      match iface.selected_side with
      |Left ->
         day_s ^ " at " ^ 
         if !Rcfile.selection_12_hour then
            twelve_hour_string sel_tm
         else
            twentyfour_hour_string sel_tm
      |Right ->
         day_s
   in
   for i = 0 to pred iface.scr.mw_lines do
      assert (wmove iface.scr.msg_win i 0);
      wclrtoeol iface.scr.msg_win
   done;
   (* draw the date stamp *)
   Rcfile.color_on iface.scr.msg_win Rcfile.Selection_info;
   wattron iface.scr.msg_win (A.bold lor A.underline);
   trunc_mvwaddstr iface.scr.msg_win 0 0 iface.scr.mw_cols day_time_s;
   Rcfile.color_off iface.scr.msg_win Rcfile.Selection_info;
   wattroff iface.scr.msg_win (A.bold lor A.underline);
   (* draw the current date *)
   let curr_tm = Unix.localtime (Unix.time ()) in
   Rcfile.color_on iface.scr.msg_win Rcfile.Status;
   wattron iface.scr.msg_win A.bold;
   let curr_tm_str = 
      if !Rcfile.status_12_hour then
         twelve_hour_string curr_tm
      else
         twentyfour_hour_string curr_tm
   in
   let s = 
      Printf.sprintf "Wyrd v%s          Currently: %s, %s %.2d at %s"
         Version.version (full_string_of_tm_wday curr_tm.Unix.tm_wday)
         (full_string_of_tm_mon curr_tm.Unix.tm_mon)
         curr_tm.Unix.tm_mday curr_tm_str
   in
   trunc_mvwaddstr iface.scr.msg_win (pred iface.scr.mw_lines) 0
      iface.scr.mw_cols s;
   Rcfile.color_off iface.scr.msg_win Rcfile.Status;
   wattroff iface.scr.msg_win A.bold;
   (* draw the full MSG string, word wrapping as necessary *)
   let pad = String.make 16 ' ' in
   let rec render_desc times descriptions output =
      let rec render_line lines temp_output =
         match lines with
         |[] ->
            temp_output
         |line :: lines_tail ->
            render_line lines_tail ((pad ^ line) :: temp_output)
      in
      match descriptions with
      |[] ->
         List.rev output
      |desc :: desc_tail ->
         let time_str = Str.string_before ((List.hd times) ^ pad) 16 in
         let first_line = time_str ^ (List.hd desc) in
         render_desc (List.tl times) desc_tail 
            ((render_line (List.tl desc) []) @ first_line :: output)
   in
   let (times, descriptions) =
      match iface.selected_side with
      |Left ->
         begin match iface.timed_lineinfo.(iface.left_selection) with
         |[] -> 
            ([""], [["(no reminder selected)"]])
         |rem_list -> 
              let sorted_rem_list = List.fast_sort sort_lineinfo rem_list in
              let get_times tline = tline.tl_timestr in
              let get_lines tline = 
                 word_wrap tline.tl_msg (iface.scr.mw_cols - 24)
              in
              (List.rev_map get_times sorted_rem_list, List.rev_map get_lines sorted_rem_list)
         end
      |Right ->
         begin match iface.untimed_lineinfo.(iface.right_selection) with
         |None       -> ([""], [["(no reminder selected)"]])
         |Some uline -> ([""], [word_wrap uline.ul_msg (iface.scr.mw_cols - 24)])
         end
   in
   let desc_lines = render_desc times descriptions [] in
   (* draw the pre-rendered lines to the screen *)
   Rcfile.color_on iface.scr.msg_win Rcfile.Description;
   let rec draw_desc_lines lines start line_num =
      match lines with
      |[] ->
         ()
      |line :: tail ->
         if start > 0 then
            draw_desc_lines tail (pred start) line_num
         else if line_num < pred iface.scr.mw_lines then begin
            assert (mvwaddstr iface.scr.msg_win line_num 5 line);
            draw_desc_lines tail start (succ line_num)
         end else
            ()
   in
   let adjusted_top =
      let max_top = max ((List.length desc_lines) - iface.scr.mw_lines + 2) 0 in
      min iface.top_desc max_top
   in
   draw_desc_lines desc_lines adjusted_top 1;
   if adjusted_top > 0 then begin
      assert (mvwaddch iface.scr.msg_win 1 (iface.scr.mw_cols - 1) (int_of_char '^'));
      assert (mvwaddch iface.scr.msg_win 2 (iface.scr.mw_cols - 1) (int_of_char '^'))
   end else
      ();
   if adjusted_top < List.length desc_lines - iface.scr.mw_lines + 2 then begin
      assert (mvwaddch iface.scr.msg_win (iface.scr.mw_lines - 2) 
         (iface.scr.mw_cols - 1) (int_of_char 'v'));
      assert (mvwaddch iface.scr.msg_win (iface.scr.mw_lines - 3) 
         (iface.scr.mw_cols - 1) (int_of_char 'v'))
   end else
      ();
   Rcfile.color_off iface.scr.msg_win Rcfile.Description;
   assert (wnoutrefresh iface.scr.msg_win);
   {iface with top_desc = adjusted_top}


(* Draw a message in the error window.  If draw_cursor = true, then
 * add a blinking cursor at the end of the message. *)
let draw_error iface err draw_cursor =
   werase iface.scr.err_win;
   trunc_mvwaddstr iface.scr.err_win 0 0 iface.scr.ew_cols err;
   let len = utf8_len err in
   if draw_cursor && len <= pred iface.scr.ew_cols then begin
      wattron iface.scr.err_win A.blink;
      assert (mvwaddch iface.scr.err_win 0 len (int_of_char '_'));
      wattroff iface.scr.err_win A.blink
   end else
      ();
   assert (wnoutrefresh iface.scr.err_win)


(* Draw a selection dialog. *)
let draw_selection_dialog (iface : interface_state_t) (title : string)
      (elements : string list) (selection : int) (top : int) =
   erase ();
   (* draw the title *)
   Rcfile.color_on iface.scr.stdscr Rcfile.Help;
   attron A.bold;
   trunc_mvwaddstr iface.scr.stdscr 0 0 iface.scr.cols title;
   Rcfile.color_off iface.scr.stdscr Rcfile.Help;
   attroff A.bold;
   (* draw the list elements *)
   Rcfile.color_on iface.scr.stdscr Rcfile.Untimed_reminder;
   let rec draw_element el_list line count =
      match el_list with
      | [] ->
         ()
      | el :: tail ->
         if count >= top && line < iface.scr.lines then begin
            if count = selection then begin
               attron A.reverse;
               trunc_mvwaddstr iface.scr.stdscr line 2 (iface.scr.cols - 4) el;
               attroff A.reverse;
            end else
               trunc_mvwaddstr iface.scr.stdscr line 2 (iface.scr.cols - 4) el;
            draw_element tail (succ line) (succ count) 
         end else
            draw_element tail line (succ count) 
   in
   draw_element elements 1 0;
   (* if there's not enough window space to display all reminder files, display
    * arrows to indicate scrolling ability *)
   if top > 0 then begin
      assert (mvaddch 1 (iface.scr.cols - 2) (int_of_char '^'));
      assert (mvaddch 2 (iface.scr.cols - 2) (int_of_char '^'))
   end else
      ();
   if List.length elements > pred iface.scr.lines + top then begin
      assert (mvaddch (pred iface.scr.lines) (iface.scr.cols - 2) (int_of_char 'v'));
      assert (mvaddch (iface.scr.lines - 2) (iface.scr.cols - 2) (int_of_char 'v'))
   end else
      ();
   Rcfile.color_off iface.scr.stdscr Rcfile.Untimed_reminder;
   assert (refresh ())




(* arch-tag: DO_NOT_CHANGE_9ff0fd0c-6eb1-410f-8fcf-6dfcf94b346a *)
