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

(* remind.ml
 * functions for interfacing with 'remind(1)' *)


exception Occurrence_not_found

open Utility


(* Define a Set that takes pairs of integers as keys. *)
module IntPair =
   struct
      type t = int * int
      let compare i j = Pervasives.compare i j
   end
module IPSet = Set.Make (IntPair)


(* A record for an individual timed reminder, as read from
 * the output of 'remind -s'. *)
type timed_rem_t = {
   tr_start      : float;
   tr_end        : float;
   tr_msg        : string;
   tr_filename   : string;
   tr_linenum    : string;
   tr_has_weight : bool
}


(* A record for an individual untimed reminder, as read from
 * the output of 'remind -s'. *)
type untimed_rem_t = {
   ur_start      : float;
   ur_msg        : string;
   ur_filename   : string;
   ur_linenum    : string;
   ur_has_weight : bool
}


(* Storage for a three-month window of reminders and
 * the calendar for the current month.
 * Makes it possible to handle edge effects of moving
 * from month to month without constantly calling 
 * rem(1) and regenerating calendar layouts. 
 *
 * Timed reminders are stored in an array of lists,
 * with each element of the array representing a different
 * indentation level on the timetable.  Untimed reminders
 * have no indentation, so they are simply stored in
 * lists. *)
type three_month_rem_t = {
   curr_timestamp : float;
   prev_timed     : timed_rem_t list array;
   curr_timed     : timed_rem_t list array;
   next_timed     : timed_rem_t list array;
   all_timed      : timed_rem_t list array;
   prev_untimed   : untimed_rem_t list;
   curr_untimed   : untimed_rem_t list;
   next_untimed   : untimed_rem_t list;
   all_untimed    : untimed_rem_t list;
   curr_counts    : int array;
   curr_cal       : Cal.t;
   remind_error   : string
}


(* Get the starting timestamp and time record for a given month *)
let month_start_of_tm tm =
   let month_start_tm = {empty_tm with
      Unix.tm_mon   = tm.Unix.tm_mon;
      Unix.tm_year  = tm.Unix.tm_year;
   } in
   Unix.mktime month_start_tm



(* Process information about a timed reminder, create a new reminder
 * record for it, and store it in the timed reminders array at the
 * proper indentation level.
 *
 * The indentation levels are determined by maintaining an array that indicates
 * which levels have been used for each hourly timeslot.  As each reminder is
 * processed, we look into the array to find the smallest indentation level
 * available.  Complexity is approximately
 * (number of reminders) * (average reminder duration).  Since indentation is
 * determined a month at a time, there may be some minor discrepancies at
 * month borders. *)
let process_timed tm duration_s month_start_ts indentations partial_trem 
timed =
   let (f_rem_ts, _) = Unix.mktime tm in
   let duration =
      if duration_s = "*" then 0.0
      else float_of_string duration_s
   in
   (* compute the indentation level *)
   (* top_index and bottom_index provide the range of row indices into
    * array indentations that are covered by this reminder *)
   let top_index = 
      try int_of_float ((f_rem_ts -. month_start_ts) /. 3600.0)
      with _ -> 0
   in
   let bottom_index = 
      try
         let real_bottom_index =
            let shift = 
               if duration > 0.0 then
                  (f_rem_ts +. (duration *. 60.0) -. month_start_ts) /. 3600.0
               else
                  (f_rem_ts +. (duration +. 60.0) -. month_start_ts) /. 3600.0
            in
            (* catch the edge effects when reminders end on hour boundaries *)
            if shift = float_of_int (int_of_float shift) then
               pred (int_of_float shift)
            else
               int_of_float shift
         in
         (* the bottom index could flow off of this month, in which case
          * we truncate and hope everything works out *)
         if real_bottom_index > pred (Array.length indentations) then
            pred (Array.length indentations)
         else
            real_bottom_index
      with _ -> top_index
   in
   (* locate the smallest free indentation level *)
   let rec find_indent level =
      if level < Array.length indentations.(0) then begin
         let collision = ref false in
         for i = top_index to bottom_index do
            if indentations.(i).(level) then
               collision := true
            else
               ()
         done;
         if !collision then
            find_indent (succ level)
         else begin
            for i = top_index to bottom_index do
               indentations.(i).(level) <- true
            done;
            level
         end
      end else
         (* give up and default to maximum indentation *)
         pred (Array.length indentations.(0))
   in
   let indent = find_indent 0 in 
   let trem = {partial_trem with
      tr_start      = f_rem_ts;
      tr_end        = f_rem_ts +. (duration *. 60.);
   } in
   timed.(indent) <- trem :: timed.(indent)



(* Obtain two lists of reminders for the month of the timestamp argument.
 * The first list is for timed reminders, the second is for untimed. 
 * The timed reminders list also provides the indentation levels that
 * draw_timed should use to render each reminder. 
 *
 * The indentation levels are determined by maintaining an array that indicates
 * which levels have been used for each hourly timeslot.  As each reminder is
 * processed, we look into the array to find the smallest indentation level
 * available.  Complexity is approximately
 * (number of reminders) * (average reminder duration).  Since indentation is
 * determined a month at a time, there may be some minor discrepancies at
 * month borders.
 *
 * The optional argument 'suppress_advwarn', if true, will override
 * the "advance_warning" rcfile option. *)
let month_reminders ?(suppress_advwarn=false) timestamp =
   let comment_regex = Str.regexp "^#.*fileinfo \\([^ ]+\\) \\(.*\\)$" in
   let rem_regex = Str.regexp "\\([^ ]+\\) [^ ]+ \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\(.*\\)$" in
   let nodisplay_regex = Str.regexp_case_fold ".*nodisplay" in
   let noweight_regex = Str.regexp_case_fold ".*noweight" in
   let tm = Unix.localtime timestamp in
   let rem_date_str = (string_of_tm_mon tm.Unix.tm_mon) ^ " " ^ 
                      (string_of_int tm.Unix.tm_mday) ^ " " ^
                      (string_of_int (tm.Unix.tm_year + 1900)) in
   let remind_s_flag = 
      if suppress_advwarn then " -s"
      else if !Rcfile.advance_warning then " -sa"
      else " -s" in
   let full_remind_command =
      !Rcfile.remind_command ^ remind_s_flag ^ " -l -g -b2 " ^ 
      !Rcfile.reminders_file ^ " " ^ rem_date_str
   in
   let (out_lines, err_lines) = Utility.read_all_shell_command_output full_remind_command in
   (* check for Remind errors *)
   let remind_err = if List.length err_lines > 0 then List.hd err_lines else "" in
   let num_indentations = 4 in
   let indentations = Array.make_matrix (24 * 31) num_indentations false in
   let (month_start_ts, _) = month_start_of_tm tm in
   let timed = Array.make num_indentations [] in
   let rec build_lists lines untimed =
      begin match lines with
      | [] ->
         for i = 0 to pred (Array.length timed) do
            timed.(i) <- List.rev timed.(i)
         done;
         (remind_err, timed, List.rev untimed)
      | comment_line :: rem_line :: lines_tail ->
         begin try
            if Str.string_match comment_regex comment_line 0 then begin
               let line_num_s = Str.matched_group 1 comment_line
               and filename   = Str.matched_group 2 comment_line in
               if Str.string_match rem_regex rem_line 0 then begin
                  let date_s     = Str.matched_group 1 rem_line
                  and tag        = Str.matched_group 2 rem_line
                  and duration_s = Str.matched_group 3 rem_line
                  and min_s      = Str.matched_group 4 rem_line
                  and msg        = Str.matched_group 5 rem_line in
                  (* further subdivide the date string *)
                  let date_arr = Array.of_list (Str.split (Str.regexp "[/-]") date_s) in
                  let year     = int_of_string date_arr.(0)
                  and month    = int_of_string date_arr.(1)
                  and day      = int_of_string date_arr.(2) in
                  let temp = {empty_tm with
                     Unix.tm_mday  = day;
                     Unix.tm_mon   = pred month;
                     Unix.tm_year  = year - 1900;
                  } in
                  (* check whether this reminder is tagged 'nodisplay' *)
                  if (Str.string_match nodisplay_regex tag 0) then
                     (* skip this reminder due to a 'nodisplay' tag *)
                     build_lists lines_tail untimed
                  else begin
                     let has_weight = not (Str.string_match noweight_regex tag 0) in
                     if min_s = "*" then
                        (* if minutes are not provided, this must be an untimed reminder *)
                        let (f_rem_ts, _) = Unix.mktime temp in
                        let urem = {
                           ur_start      = f_rem_ts;
                           ur_msg        = msg;
                           ur_filename   = filename;
                           ur_linenum    = line_num_s;
                           ur_has_weight = has_weight
                        } in
                        build_lists lines_tail (urem :: untimed)
                     else begin
                        (* if minutes are provided, this must be a timed reminder *)
                        let temp_with_min = {temp with Unix.tm_min = int_of_string min_s} in
                        let partial_trem = {
                           tr_start      = 0.0;  (* still needs to be filled in *)
                           tr_end        = 0.0;  (* still needs to be filled in *)
                           tr_msg        = msg;
                           tr_filename   = filename;
                           tr_linenum    = line_num_s;
                           tr_has_weight = has_weight
                        } in
                        process_timed temp_with_min duration_s month_start_ts indentations
                        partial_trem timed;
                        build_lists lines_tail untimed
                     end
                  end
               end else
                  (* if there was no rem_regex match, continue with next line *)
                  build_lists (rem_line :: lines_tail) untimed
            end else
               (* if there was no comment_regex match, continue with next line *)
               build_lists (rem_line :: lines_tail) untimed
         with _ ->
            (* if there's an error in regexp matching or string coersion,
             * just drop that reminder and go to the next line *)
            build_lists (rem_line :: lines_tail) untimed
         end
      | comment_line :: [] ->
         (* this line doesn't conform to the spec, so throw it out *)
         build_lists [] untimed
      end
   in
   build_lists out_lines []


(* generate a count of how many reminders fall on any given day of
 * the month *)
let count_reminders month_start_tm timed untimed =
   let rem_counts = Array.make 31 0 in
   let count_rems start has_weight =
      let tm = Unix.localtime start in
      if has_weight && tm.Unix.tm_year = month_start_tm.Unix.tm_year &&
      tm.Unix.tm_mon = month_start_tm.Unix.tm_mon then
         let day = pred tm.Unix.tm_mday in
         rem_counts.(day) <- succ rem_counts.(day)
      else
         ()
   in
   let count_timed rem =
      count_rems rem.tr_start rem.tr_has_weight
   in
   let count_untimed rem =
      count_rems rem.ur_start rem.ur_has_weight
   in
   Array.iter (List.iter count_timed) timed;
   List.iter count_untimed untimed;
   rem_counts


(* generate a count of how many hours of reminders one has on
 * any given day of the month.  Note: some minor errors can
 * occur during DST, but this is too small to worry about. *)
let count_busy_hours month_start_tm timed untimed =
   let hour_counts = Array.make 31 0.0 in
   let last_day = 
      let temp = {month_start_tm with Unix.tm_mday = 32} in
      let (_, nextmonth) = Unix.mktime temp in
      32 - nextmonth.Unix.tm_mday
   in
   let count_hours start stop has_weight =
      if has_weight then
         for day = 1 to last_day do
            let day_tm = {month_start_tm with Unix.tm_mday = day} in
            let (day_ts, _) = Unix.mktime day_tm in
            if day_ts >= start then
               if stop > day_ts +. 86400. then
                  hour_counts.(pred day) <- hour_counts.(pred day) +. 24.
               else if stop > day_ts then
                  hour_counts.(pred day) <- hour_counts.(pred day) +.
                  ((stop -. day_ts) /. 3600.)
               else
                  ()
            else if day_ts +. 86400. > start then
               if stop > day_ts +. 86400. then
                  hour_counts.(pred day) <- hour_counts.(pred day) +.
                  24. -. ((start -. day_ts) /. 3600.)
               else
                  hour_counts.(pred day) <- hour_counts.(pred day) +.
                  ((stop -. start) /. 3600.)
            else
               ()
         done
      else
         ()
   in
   let count_timed rem =
      count_hours rem.tr_start rem.tr_end rem.tr_has_weight
   in
   let count_untimed rem =
      let stop = rem.ur_start +. (!Rcfile.untimed_duration *. 60.) in
      count_hours rem.ur_start stop rem.ur_has_weight
   in
   Array.iter (List.iter count_timed) timed;
   List.iter count_untimed untimed;
   Array.map int_of_float hour_counts


(* determine the busy-ness level for each day in the month *)
let count_busy month_tm timed untimed =
   let month_start_tm = {
      month_tm with Unix.tm_sec  = 0;
                    Unix.tm_min  = 0;
                    Unix.tm_hour = 0;
                    Unix.tm_mday = 1
   } in
   match !Rcfile.busy_algorithm with
   |1 -> count_reminders month_start_tm timed untimed
   |2 -> count_busy_hours month_start_tm timed untimed
   |_ -> Rcfile.config_failwith "busy_algorithm must be either 1 or 2"


(* comparison functions for sorting reminders chronologically *)
let cmp_timed rem_a rem_b =
   compare rem_a.tr_start rem_b.tr_start

let cmp_untimed rem_a rem_b =
   compare rem_a.ur_start rem_b.ur_start


(* take an array of timed reminder lists and merge them into
 * a single list sorted by starting timestamp *)
let merge_timed timed =
   let all_rem = ref [] in
   for i = 0 to pred (Array.length timed) do
      all_rem := List.rev_append timed.(i) !all_rem
   done;
   List.fast_sort cmp_timed !all_rem


(* same thing as List.append (or @), but tail-recursive *)
let safe_append a b = List.rev_append (List.rev a) b


(* initialize a new three-month reminder record *)
let create_three_month ?(suppress_advwarn=false) timestamp =
   let month_start_tm = {
      Unix.localtime timestamp with Unix.tm_sec  = 0;
                                    Unix.tm_min  = 0;
                                    Unix.tm_hour = 0;
                                    Unix.tm_mday = 1
   } in
   let (curr_ts, _) = Unix.mktime month_start_tm in
   let temp_prev = {
      month_start_tm with Unix.tm_mon = pred month_start_tm.Unix.tm_mon
   } in
   let temp_next = {
      month_start_tm with Unix.tm_mon = succ month_start_tm.Unix.tm_mon
   } in
   let (prev_ts, _) = Unix.mktime temp_prev
   and (next_ts, _) = Unix.mktime temp_next in
   let (pre, pt, pu) = month_reminders ~suppress_advwarn:suppress_advwarn prev_ts in
   let (cre, ct, cu) = month_reminders ~suppress_advwarn:suppress_advwarn curr_ts in
   let (nre, nt, nu) = month_reminders ~suppress_advwarn:suppress_advwarn next_ts in 
   let at = Array.make (Array.length pt) [] in
   for i = 0 to pred (Array.length at) do
      at.(i) <- safe_append pt.(i) (safe_append ct.(i) nt.(i))
   done;
   let err_str = 
      if String.length pre > 0 then
         pre
      else if String.length cre > 0 then
         cre
      else
         nre
   in
   let au = safe_append pu (safe_append cu nu) in {
      curr_timestamp = curr_ts;
      prev_timed     = pt;
      curr_timed     = ct;
      next_timed     = nt;
      all_timed      = at;
      prev_untimed   = pu;
      curr_untimed   = cu;
      next_untimed   = nu;
      all_untimed    = au;
      curr_counts    = count_busy month_start_tm at au;
      curr_cal       = Cal.make curr_ts !Rcfile.week_starts_monday;
      remind_error   = err_str   
   }



(* Update a three-month reminders record for the next month *)
let next_month reminders =
   let curr_tm = Unix.localtime reminders.curr_timestamp in
   let temp1 = {
      curr_tm with Unix.tm_mon = succ curr_tm.Unix.tm_mon
   } in
   let temp2 = {
      curr_tm with Unix.tm_mon = curr_tm.Unix.tm_mon + 2
   } in
   let (new_curr_timestamp, temp1) = Unix.mktime temp1 in
   let (next_timestamp, temp2)     = Unix.mktime temp2 in
   let (re, t, u) = month_reminders next_timestamp in 
   let at = Array.make (Array.length t) [] in
   for i = 0 to pred (Array.length t) do
      at.(i) <- safe_append reminders.curr_timed.(i) 
                (safe_append reminders.next_timed.(i) t.(i))
   done;
   let au = safe_append reminders.curr_untimed (safe_append reminders.next_untimed u) in {
      curr_timestamp = new_curr_timestamp;
      prev_timed     = reminders.curr_timed;
      curr_timed     = reminders.next_timed;
      next_timed     = t;
      all_timed      = at;
      prev_untimed   = reminders.curr_untimed;
      curr_untimed   = reminders.next_untimed;
      next_untimed   = u;
      all_untimed    = au;
      curr_counts    = count_busy temp1 at au;
      curr_cal       = Cal.make new_curr_timestamp !Rcfile.week_starts_monday;
      remind_error   = re
   }


(* Update a three-month reminders record for the previous month *)
let prev_month reminders =
   let curr_tm = Unix.localtime reminders.curr_timestamp in
   let temp1 = {
      curr_tm with Unix.tm_mon = pred curr_tm.Unix.tm_mon
   } in
   let temp2 = {
      curr_tm with Unix.tm_mon = curr_tm.Unix.tm_mon - 2
   } in
   let (new_curr_timestamp, temp1) = Unix.mktime temp1 in
   let (prev_timestamp, temp2)     = Unix.mktime temp2 in
   let (re, t, u) = month_reminders prev_timestamp in
   let at = Array.make (Array.length t) [] in
   for i = 0 to pred (Array.length t) do
      at.(i) <- safe_append t.(i) 
                (safe_append reminders.prev_timed.(i) reminders.curr_timed.(i))
   done;
   let au = safe_append u (safe_append reminders.prev_untimed reminders.curr_untimed) in {
      curr_timestamp = new_curr_timestamp;
      prev_timed     = t;
      curr_timed     = reminders.prev_timed;
      next_timed     = reminders.curr_timed;
      all_timed      = at;
      prev_untimed   = u;
      curr_untimed   = reminders.prev_untimed;
      next_untimed   = reminders.curr_untimed;
      all_untimed    = au;
      curr_counts    = count_busy temp1 at au;
      curr_cal       = Cal.make new_curr_timestamp !Rcfile.week_starts_monday;
      remind_error   = re
   }


(* Return a new reminders record centered on the current timestamp,
 * doing as little work as possible. *)
let update_reminders rem timestamp =
   let tm     = Unix.localtime timestamp
   and rem_tm = Unix.localtime rem.curr_timestamp in
   if tm.Unix.tm_year = rem_tm.Unix.tm_year &&
      tm.Unix.tm_mon  = rem_tm.Unix.tm_mon then
      rem
   else
      let temp1 = {
         rem_tm with Unix.tm_mon = pred rem_tm.Unix.tm_mon
      } in
      let temp2 = {
         rem_tm with Unix.tm_mon = succ rem_tm.Unix.tm_mon
      } in
      let (_, prev_tm) = Unix.mktime temp1 in
      let (_, next_tm) = Unix.mktime temp2 in
      if tm.Unix.tm_year = prev_tm.Unix.tm_year &&
         tm.Unix.tm_mon  = prev_tm.Unix.tm_mon then
         prev_month rem
      else if tm.Unix.tm_year = next_tm.Unix.tm_year &&
         tm.Unix.tm_mon = next_tm.Unix.tm_mon then
         next_month rem
      else
         create_three_month timestamp



(* Look at all 'next' reminders after the given timestamp.  Search
 * through the list for the first occurrence of the search regexp, and return
 * a timestamp for that date.
 * This calls 'remind -n' twice--once for the current day, once for the next day.
 * The second call is necessary because reminders falling on the current day
 * but before the current timestamp will effectively suppress later recurrences
 * of that reminder. 
 *
 * We also have to make a separate check that the matched reminder is not tagged
 * with 'nodisplay'; since these reminders don't show up on the display, Wyrd
 * should not be able to match them. *)
let find_next msg_regex timestamp =
   let rem_regex = 
      Str.regexp "^\\([^ ]+\\)[/-]\\([^ ]+\\)[/-]\\([^ ]+\\) [^ ]+ \\([^ ]+\\) [^ ]+ \\([^ ]+\\) \\([^ ]+.*\\)$"
   in
   let nodisplay_regex = Str.regexp_case_fold ".*nodisplay" in
   let tm1 = Unix.localtime timestamp in
   let temp = {tm1 with Unix.tm_mday = succ tm1.Unix.tm_mday} in     (* add 24 hours *)
   let (_, tm2) = Unix.mktime temp in
   let rem_date_str1 = (string_of_tm_mon tm1.Unix.tm_mon) ^ " " ^ 
                       (string_of_int tm1.Unix.tm_mday) ^ " " ^
                       (string_of_int (tm1.Unix.tm_year + 1900)) in
   let rem_date_str2 = (string_of_tm_mon tm2.Unix.tm_mon) ^ " " ^ 
                       (string_of_int tm2.Unix.tm_mday) ^ " " ^
                       (string_of_int (tm2.Unix.tm_year + 1900)) in
   let remind_output_for_month date_str =
      let remind_month_command = 
         !Rcfile.remind_command ^ " -n -s -b1 " ^ !Rcfile.reminders_file ^ " " ^ date_str
      in
      let (out_lines, _) = Utility.read_all_shell_command_output remind_month_command in
      out_lines
   in
   (* concatenate the outputs from two consecutive months of Remind data, then sort
    * the lines by leading datestamp *)
   let two_month_output = List.rev_append 
      (remind_output_for_month rem_date_str1) (remind_output_for_month rem_date_str2)
   in
   let out_lines = List.fast_sort compare two_month_output in
   let rec check_messages lines =
      begin match lines with
      | [] ->
         raise Occurrence_not_found
      | line :: lines_tail ->
         begin try
            if Str.string_match rem_regex line 0 then begin
               (* go here if this line is a timed reminder *)
               let year  = int_of_string (Str.matched_group 1 line)
               and month = int_of_string (Str.matched_group 2 line)
               and day   = int_of_string (Str.matched_group 3 line)
               and tag   = Str.matched_group 4 line
               and min_s = Str.matched_group 5 line
               and msg   = Str.matched_group 6 line in
               let temp = {empty_tm with
                  Unix.tm_min   = if min_s = "*" then 0 else (int_of_string min_s);
                  Unix.tm_mday  = day;
                  Unix.tm_mon   = pred month;
                  Unix.tm_year  = year - 1900;
               } in
               let (ts, _) = Unix.mktime temp in
               if ts > timestamp then
                  begin try
                     let _ = Str.search_forward msg_regex msg 0 in
                     (* only return the match if this value is not tagged 'nodisplay' *)
                     if not (Str.string_match nodisplay_regex tag 0) then
                        ts
                     else
                        check_messages lines_tail
                  with Not_found ->
                     check_messages lines_tail
                  end
               else begin
                  check_messages lines_tail
               end
            end else
               (* if there was no regexp match, continue with next line *)
               check_messages lines_tail
         with
         | Failure s ->
           (* if there's an error in string coersion, just drop that reminder and go
            * to the next line *)
           check_messages lines_tail
         end
      end
   in
   check_messages out_lines


(* Get a list of file or directory names INCLUDEd in a specific reminders file *)
let parse_include_directives remfile =
   let filedir = Filename.dirname remfile in
   try
      let remfile_channel = open_in remfile in
      (* match "include <filename>" *)
      let include_regex = Str.regexp_case_fold "^[ \t]*include[ \t]+\\([^ \t]+.*\\)$" in
      (* match "[filedir()]" so we can do a Remind-like directory substitution *)
      let filedir_regex = Str.regexp_case_fold "\\[[ \t]*filedir[ \t]*([ \t]*)[ \t]*\\]" in
      let rec build_filelist files =
         try
            let line = input_line remfile_channel in
            if Str.string_match include_regex line 0 then
               let include_expr = Utility.strip (Str.matched_group 1 line) in
               let new_file = Str.global_replace filedir_regex filedir include_expr in
               build_filelist (new_file :: files)
            else
               build_filelist files
         with End_of_file ->
            close_in remfile_channel;
            files
      in
      build_filelist []
   with Sys_error _ ->
      (* File does not exist *)
      []


(* Given a directory name and an open handle to it, locate all *.rem files contained therein.
 * Closes the directory handle on exit. *)
let find_remdir_scripts dirname remdir_handle =
   let safe_get_filename () =
      try
         Some (Unix.readdir remdir_handle)
      with End_of_file ->
         None
   in
   let rec listdir filelist =
      match safe_get_filename () with
      | Some filename ->
         listdir ((Filename.concat dirname filename) :: filelist)
      | None ->
         Unix.closedir remdir_handle;
         filelist
   in
   let is_rem_script filename = Filename.check_suffix filename ".rem" in
   List.filter is_rem_script (listdir [])


(* Recursively compute a subtree of reminder files that Remind would execute
 * as a result of a single INCLUDE directive.  <pending_include_directives> is a list of 
 * files and directories specified via a number of Remind INCLUDE statements.
 * <known_include_directives> is a Set of include directives which have already been
 * processed.  <known_included_filenames> is a list of filenames which are already
 * known to be included; this is effectively a subset of the <known_include_directives>.
 *
 * We test that a file or directory has been visited by statting it and testing for
 * containment of the (dev, inode) pair in the Set <known_include_directives>.  This should
 * do the right thing in case of symlinks, relative paths, etc. *)
let rec get_included_filenames known_included_filenames known_include_directives pending_include_directives =
   match pending_include_directives with
   | [] ->
      List.fast_sort compare known_included_filenames
   | include_directive :: remaining_include_directives ->
      begin try
         (* The (dev, inode) pair uniquely identifies a file or directory. *)
         let directive_id =
            let status = Unix.stat include_directive in
            (status.Unix.st_dev, status.Unix.st_ino)
         in
         if IPSet.mem directive_id known_include_directives then
            (* This include directive has already been processed... skip it *)
            get_included_filenames known_included_filenames known_include_directives 
               remaining_include_directives
         else
            begin try
               (* This is a new include directive.  Start by assuming it's a directory.  If that works,
                * treat each of the .rem scripts inside this directory as a new INCLUDE directive. *)
               let remdir_handle = Unix.opendir include_directive in
               let new_include_directives = find_remdir_scripts include_directive remdir_handle in
               get_included_filenames known_included_filenames
                  (IPSet.add directive_id known_include_directives)
                  (List.rev_append new_include_directives remaining_include_directives)
            with Unix.Unix_error _ ->
               (* This include directive could not be opened as a directory.
                * Treat it as a regular file: parse it to locate additional
                * INCLUDE lines, and add the additional includes to the pending list. *)
               let new_include_directives = parse_include_directives include_directive in
               get_included_filenames (include_directive :: known_included_filenames) 
                  (IPSet.add directive_id known_include_directives) 
                  (List.rev_append new_include_directives remaining_include_directives)
            end
      with Unix.Unix_error (Unix.ENOENT, _, _) ->
         (* include_directive cannot be found on disk... skip it *)
         get_included_filenames known_included_filenames known_include_directives 
            remaining_include_directives
      end
   

(* Get a tuple of the form (primary remfile, all remfiles).  If !Rcfile.reminders_file
 * is a single file, then "primary_remfile" is that file, and "all_remfiles" is
 * formed by parsing the INCLUDE statements.  If !Rcfile.reminders_file is
 * a directory, then "primary remfile" is the first file in the directory with
 * extension ".rem", and "all remfiles" is all files with extension ".rem"
 * combined with all INCLUDEd files. *)
let get_all_remfiles () =   
   (* Test whether a filename represents an existing directory. *)
   let is_existing_dir fn =
      try 
         let status = Unix.stat fn in
         status.Unix.st_kind = Unix.S_DIR
      with Unix.Unix_error _ ->
         false
   in
   let toplevel_include_directive = Utility.expand_file !Rcfile.reminders_file in
   if is_existing_dir toplevel_include_directive then
      try
         let toplevel_scripts = find_remdir_scripts toplevel_include_directive
            (Unix.opendir toplevel_include_directive)
         in
         if toplevel_scripts = [] then
            (* User selected an empty reminders directory.  Make up a reasonable default filename. *)
            let default_filename = Filename.concat toplevel_include_directive "reminders.rem" in
            (default_filename, [default_filename])
         else
            (* Nonempty reminders directory.  Choose first entry as primary file, and parse
             * through all files to get the INCLUDE structure. *)
            let sorted_scripts = List.fast_sort compare toplevel_scripts in
            (List.hd sorted_scripts,
               get_included_filenames [] IPSet.empty sorted_scripts)
      with Unix.Unix_error _ ->
         (* Can't open the directory... *)
         failwith (Printf.sprintf "Can't open reminders directory \"%s\"" 
            toplevel_include_directive)
   else
      (* toplevel include directive is not a directory.  Treat it as a regular file,
       * which need not exist. *)
      let all_includes = get_included_filenames [] IPSet.empty [toplevel_include_directive] in
      if all_includes = [] then
         (* Special case: if the toplevel include does not exist, pretend it does exist *)
         (toplevel_include_directive, [toplevel_include_directive])
      else
         (toplevel_include_directive, all_includes)



(* Filter a list of untimed reminders, returning a list of those
 * reminders falling on the same day as the timestamp. *)
let get_untimed_reminders_for_day untimed_reminders timestamp =
   let curr_tm = Unix.localtime timestamp in
   let temp1 = {
      curr_tm with Unix.tm_sec  = 0;
                   Unix.tm_min  = 0;
                   Unix.tm_hour = 0
   } in
   let temp2 = {
      curr_tm with Unix.tm_sec  = 0;
                   Unix.tm_min  = 0;
                   Unix.tm_hour = 0;
                   Unix.tm_mday = succ curr_tm.Unix.tm_mday 
   } in
   let (day_start_ts, _) = Unix.mktime temp1 in
   let (day_end_ts, _)   = Unix.mktime temp2 in
   let is_current rem =
      rem.ur_start >= day_start_ts && rem.ur_start < day_end_ts
   in
   List.filter is_current untimed_reminders


(* Forms a list of all remfiles in use, and stats them all to
 * get the mtimes. *)
let get_remfile_mtimes () =
   let safe_mtime filename =
      try
         let st = Unix.stat filename in
         st.Unix.st_mtime
      with Unix.Unix_error _ ->
         0.0
   in
   let (_, all_remfiles) = get_all_remfiles () in
   List.fold_left
      (fun acc remfile -> Interface.SMap.add remfile (safe_mtime remfile) acc)
      Interface.SMap.empty
      all_remfiles


(* arch-tag: DO_NOT_CHANGE_6bb48a1c-2b0c-4254-ba3a-ee9b48007169 *)
