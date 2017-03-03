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

(* time_lang.ml
 *
 * This module implements a regexp-based parser for natural-language descriptions of
 * events.  It recognizes most reasonable permutations of ([DATE] [TIME] DESCRIPTION),
 * including:
 *
 *    "do some stuff tomorrow at 3"
 *    "wednesday meeting with Bob 1:30pm-3:00"
 *    "leave for airport noon next Fri"
 *    "6/14 6pm dinner with Tom"
 *
 * Preference is generally for US time and date conventions; would be quite difficult to
 * internationalize this module. *)


exception Event_parse_error of string
let parse_failwith s = raise (Event_parse_error s)
let parse_fail_default () = parse_failwith "unrecognized format for event."


type event_t = Timed of (Unix.tm * Unix.tm) | Untimed of Unix.tm


(* DEFINE TIME AND DATE REGEXES *)
(* Note: careful when changing these regexps.  If the grouping tree changes,
 * then the fields may be misnumbered in code below. *)

(* Note: ocaml _really_ needs a raw string syntax to make regexps look more sane... *)
let weekdays = 
   "\\(\\(on[ \t]+\\)?\\(next[ \t]+\\)?" ^
   "\\(\\(sunday\\|sun\\)\\|\\(monday\\|mon\\)\\|\\(tuesday\\|tue\\|tues\\)\\|" ^
   "\\(wednesday\\|wed\\)\\|\\(thursday\\|thu\\|thur\\|thurs\\)\\|" ^
   "\\(friday\\|fri\\)\\|\\(saturday\\|sat\\)\\)\\)"
(* typical US-style dates, like 6/1 or 6/1/2006 *)
let numeric_slash_date = "\\(\\([0-9]+\\)/\\([0-9]+\\)\\(/\\([0-9][0-9][0-9][0-9]\\)\\)?\\)"
(* ISO standard dates, like 2006-06-01 *)
let iso_date = "\\(\\([0-9][0-9][0-9][0-9]\\)-\\([0-9]+\\)-\\([0-9]+\\)\\)"
let numeric_date = "\\(\\(on[ \t]+\\)?\\(" ^ numeric_slash_date ^ "\\|" ^ iso_date ^ "\\)\\)"
let short_date =
   "\\(\\(today\\)\\|\\(tomorrow\\)\\|" ^
   "\\(in[ \t]+\\([0-9]+\\)[ \t]+day\\(s\\|s' time\\)?\\)\\)"
let date_spec = 
   "\\(" ^ weekdays ^ "\\|" ^ numeric_date ^ "\\|" ^ short_date ^ "\\)"

(* 5, or 5:30, or 5pm, or 5:30 pm *)
let time_spec = 
   "\\(\\(\\([0-9][0-9]?\\)\\(:\\([0-9][0-9]\\)\\)?[ \t]*\\(am\\|pm\\)?\\)\\|" ^
   "\\(noon\\|midnight\\)\\)"
(* either match a single time spec, or match a pair formatted as a range:
 * 5-7 or 5:30pm-7 or 5 until 7 or 5:30 to 7pm ... *)
let time_range_spec = 
   "\\(\\(at[ \t]+\\)?" ^ time_spec ^ "\\([ \t]*\\(-\\|to\\|\\until\\)[ \t]*" ^
   time_spec ^ "\\)?\\)"

let date_start_regex = Str.regexp_case_fold ("[ \t,.]*" ^ date_spec ^ "[ \t,.]+")
let time_start_regex = Str.regexp_case_fold ("[ \t,.]*" ^ time_range_spec ^ "[ \t,.]+")
let date_end_regex   = Str.regexp_case_fold ("[ \t,.]+" ^ date_spec ^ "[ \t,.]*$")
let time_end_regex   = Str.regexp_case_fold ("[ \t,.]+" ^ time_range_spec ^ "[ \t,.]*$")



(* given a current time and an hour/minute/merid specifier,
 * search forward from current time and provide the next time
 * value that satisfies the spec. *)
(* 'future' is a boolean variable that determines whether the time
 * is required to be in the future or not (i.e. the present is acceptable) *)
let next_matching_time future curr_tm hour min merid =
   let (curr, _) = Unix.mktime curr_tm in
   let get_tm hour_shift day_shift =
      let tm = {
            curr_tm with Unix.tm_hour = hour + hour_shift;
                         Unix.tm_min  = min;
                         Unix.tm_sec  = 0;
                         Unix.tm_mday = curr_tm.Unix.tm_mday + day_shift
      } in
      Unix.mktime tm
   in
   if merid = "am" then
      (* if "am" is specified... *)
      (* try using the hour and minute as given *)
      let (start1, start1_tm) = get_tm 0 0 in
      if start1 > curr || (start1 = curr && not future) then
         start1_tm
      else
         (* if that fails, just use the same time tomorrow *)
         let (start2, start2_tm) = get_tm 0 1 in
         start2_tm
   else if merid = "pm" then
      (* if "pm" is specified... *)
      (* try using the hour and minute as given (but shifted to afternoon) *)
      let (start1, start1_tm) = get_tm 12 0 in
      if start1 > curr || (start1 = curr && not future) then
         start1_tm
      else
         (* if that fails, use the same time tomorrow *)
         let (start2, start2_tm) = get_tm 12 1 in
         start2_tm
   else
      (* if neither "am" nor "pm" are specified... *)
      (* first use the hour and minute as given *)
      let (start1, start1_tm) = get_tm 0 0 in
      if start1 > curr || (start1 = curr && not future) then
         start1_tm
      else
         (* if that fails, look in the afternoon *)
         let (start2, start2_tm) = get_tm 12 0 in
         if start2 > curr || (start2 = curr && not future) then
            start2_tm
         else
            (* if that fails, shift to tomorrow morning *)
            let (start3, start3_tm) = get_tm 0 1 in
            start3_tm


(* Find the next timespec following curr_tm that falls on 'wday'.  'next_week'
 * is a boolean variable that is true if the event should be interpreted
 * as occurring next week. *)
let find_next_wday curr_tm wday next_week =
   (* start scanning forward from the current timespec, unless the 'next'
    * specifier is given; in that case, start from the beginning of
    * next week *)
   let scan_from =
      if next_week then
         let week_diff =
            if !Rcfile.week_starts_monday then
               7 - ((curr_tm.Unix.tm_wday + 6) mod 7)
            else
               7 - curr_tm.Unix.tm_wday
         in
         let temp = {
            curr_tm with Unix.tm_mday = curr_tm.Unix.tm_mday + week_diff;
                         Unix.tm_hour = 0;
                         Unix.tm_min = 0;
                         Unix.tm_sec = 0
         } in
         let (_, norm) = Unix.mktime temp in
         norm
      else
         curr_tm
   in
   let diff = 
      if wday > scan_from.Unix.tm_wday then
         wday - scan_from.Unix.tm_wday
      else if wday = scan_from.Unix.tm_wday && next_week then
         0
      else
         wday - scan_from.Unix.tm_wday + 7
   in
   let temp = {
      scan_from with Unix.tm_mday = scan_from.Unix.tm_mday + diff;
                     Unix.tm_hour = 0;
                     Unix.tm_min  = 0;
                     Unix.tm_sec  = 0
   } in
   let (_, norm) = Unix.mktime temp in
   norm


(* Find the next timespec following curr_tm that satisfies the
 * month and day provided. *)
let find_next_mon_mday curr_tm mon mday =
   if mon > curr_tm.Unix.tm_mon then
      let temp = {
         curr_tm with Unix.tm_mon = mon;
                      Unix.tm_mday = mday;
                      Unix.tm_hour = 0;
                      Unix.tm_min = 0;
                      Unix.tm_sec = 0
      } in
      let (_, norm) = Unix.mktime temp in
      norm
   else if mon = curr_tm.Unix.tm_mon && mday > curr_tm.Unix.tm_mday then
      let temp = {
         curr_tm with Unix.tm_mday = mday;
                      Unix.tm_hour = 0;
                      Unix.tm_min = 0;
                      Unix.tm_sec = 0
      } in
      let (_, norm) = Unix.mktime temp in
      norm
   else
      let temp = {
         curr_tm with Unix.tm_year = succ curr_tm.Unix.tm_year;
                      Unix.tm_mon = mon;
                      Unix.tm_mday = mday;
                      Unix.tm_hour = 0;
                      Unix.tm_min = 0;
                      Unix.tm_sec = 0
      } in
      let (_, norm) = Unix.mktime temp in
      norm
      
      
(* Parse a natural language date string. *)
let parse_natural_language_date date_str =
   let current = Unix.localtime (Unix.time ()) in
   let (_, current_tm) = Unix.mktime current in
   let date_regex = Str.regexp date_spec in
   if Str.string_match date_regex date_str 0 then begin
(*
      for i = 1 to 30 do
         try
            Printf.printf "%2d: \"%s\"\n" i (Str.matched_group i date_str); flush stdout;
         with Not_found -> ()
      done;
*)
      let get_field num = Str.matched_group num date_str in

      let handle_weekday () =
         let has_next =
            try
               let _ = get_field 4 in true
            with Not_found -> false
         in
         try
            (* some variant of "sunday" *)
            let _ = get_field 6 in
            find_next_wday current_tm 0 has_next
         with Not_found ->
         try
            (* some variant of "monday" *)
            let _ = get_field 7 in
            find_next_wday current_tm 1 has_next
         with Not_found ->
         try
            (* some variant of "tuesday" *)
            let _ = get_field 8 in
            find_next_wday current_tm 2 has_next
         with Not_found ->
         try
            (* some variant of "wednesday" *)
            let _ = get_field 9 in
            find_next_wday current_tm 3 has_next
         with Not_found ->
         try
            (* some variant of "thursday" *)
            let _ = get_field 10 in
            find_next_wday current_tm 4 has_next
         with Not_found ->
         try
            (* some variant of "friday" *)
            let _ = get_field 11 in
            find_next_wday current_tm 5 has_next
         with Not_found ->
         try
            (* some variant of "saturday" *)
            let _ = get_field 12 in
            find_next_wday current_tm 6 has_next
         with Not_found ->
            parse_failwith "please submit a bug report for \"unreachable case 3\"."
      in

      let handle_numeric_slash () = 
         try
            (* US-style numeric date specified with slashes *)
            let first  = int_of_string (get_field 17) in
            let second = int_of_string (get_field 18) in
            let (month, monthday) =
               if !Rcfile.quick_date_US then
                  (* assume US conventions (month first, then day of month) *)
                  (first, second)
               else
                  (* assume non-US conventions (day of month first, then month) *)
                  (second, first)
            in
            if month >= 1 && month <= 12 && monthday >= 1 && monthday <= 31 then
               begin try
                  let third = int_of_string (get_field 20) in
                  if third >= 1991 && third <= 2037 then
                     let temp = {
                        current_tm with Unix.tm_year = third - 1900;
                                        Unix.tm_mon = pred month;
                                        Unix.tm_mday = monthday;
                                        Unix.tm_hour = 0;
                                        Unix.tm_min = 0;
                                        Unix.tm_sec = 0
                     } in
                     let (_, norm) = Unix.mktime temp in
                     norm
                  else
                     parse_fail_default ()
               with Not_found ->
                  find_next_mon_mday current_tm (pred month) monthday
               end
            else
               parse_fail_default ()
         with Not_found ->
            parse_failwith "please submit a bug report for \"unreachable case 4\"."
      in

      let handle_iso () =
         try
            (* iso numeric date specified with dashes *)
            let first  = int_of_string (get_field 22) in
            let second = int_of_string (get_field 23) in
            let third  = int_of_string (get_field 24) in
            if first >= 1991 && first <= 2037 && 
            second >= 1 && second <= 12 &&
            third >= 1 && third <= 31 then
               let temp = {
                  current_tm with Unix.tm_year = first - 1900;
                                  Unix.tm_mon = pred second;
                                  Unix.tm_mday = third;
                                  Unix.tm_hour = 0;
                                  Unix.tm_min = 0;
                                  Unix.tm_sec = 0
               } in
               let (_, norm) = Unix.mktime temp in
               norm
            else
               parse_fail_default ()
         with Not_found ->
            parse_failwith "please submit a bug report for \"unreachable case 5\"."
      in

      let handle_short () =
         try
            (* "today" *)
            let _ = get_field 26 in 
            let temp = {
               current_tm with Unix.tm_hour = 0;
                               Unix.tm_min = 0;
                               Unix.tm_sec = 0
            } in
            let (_, norm) = Unix.mktime temp in
            norm
         with Not_found ->
         try
            (* "tomorrow" *)
            let _ = get_field 27 in 
            let temp = {
               current_tm with Unix.tm_mday = succ current_tm.Unix.tm_mday;
                               Unix.tm_hour = 0;
                               Unix.tm_min = 0;
                               Unix.tm_sec = 0
            } in
            let (_, norm) = Unix.mktime temp in
            norm
         with Not_found ->
         try
            (* "in N days" *)
            let num = int_of_string (get_field 29) in
            let temp = {
               current_tm with Unix.tm_mday = current_tm.Unix.tm_mday + num;
                               Unix.tm_hour = 0;
                               Unix.tm_min = 0;
                               Unix.tm_sec = 0
            } in
            let (_, norm) = Unix.mktime temp in
            norm
         with Not_found ->
            parse_failwith "please submit a bug report for \"unreachable case 6\"."
      in

      let date_tm =
         try
            (* a weekday specifier *)
            let _ = get_field 5 in
            handle_weekday ()
         with Not_found ->
         try
            (* a numeric date specifier with slashes *)
            let _ = get_field 16 in
            handle_numeric_slash ()
         with Not_found ->
         try
            (* a numeric date specifier with dashes *)
            let _ = get_field 21 in
            handle_iso ()
         with Not_found -> 
         try
            (* a shortcut date *)
            let _ = get_field 25 in
            handle_short ()
         with Not_found ->
            parse_failwith "please submit a bug report for \"unreachable case 2\"."
      in
      date_tm
   end else
      parse_failwith "please submit a bug report for \"unreachable case 1\"."



(* Parse a natural language time string. *)
(* 'future' is a boolean variable that determines whether the time
 * is required to be in the future or not (i.e. the present is acceptable) *)
let parse_natural_language_time future current_tm time_str =
   let time_regex = Str.regexp time_range_spec in
   if Str.string_match time_regex time_str 0 then begin
(*
      for i = 1 to 30 do
         try
            Printf.printf "%2d: \"%s\"\n" i (Str.matched_group i time_str); flush stdout;
         with
         | Not_found -> ()
      done;
*)
      let get_field num =
         try Str.matched_group num time_str with Not_found -> ""
      in
      let start_hour_s     = get_field 5
      and start_minute_s   = get_field 7
      and start_meridien_s = get_field 8
      and end_hour_s       = get_field 14
      and end_minute_s     = get_field 16
      and end_meridien_s   = get_field 17
      and start_abbrev     = get_field 9
      and end_abbrev       = get_field 18 in

      (* OK, given a timestamp as context, try to choose the most sensible
       * interpretation of the time spec.  Search forward from current time for
       * the first start time that matches, then continue searching forward
       * for the first end time that matches. *)
      let (start_hour, start_meridien) =
         if start_abbrev = "noon" then
            (0, "pm")
         else if start_abbrev = "midnight" then
            (0, "am")
         else if start_hour_s = "" then
            parse_fail_default ()
         else
            let temp = int_of_string start_hour_s in
            if temp = 0 then
               (0, "am")
            else if temp >= 13 && temp <= 23 then
               (temp - 12, "pm") 
            else if temp >= 1 && temp <= 11 then
               (temp, start_meridien_s)
            else if temp = 12 then
               (0, start_meridien_s) 
            else
               parse_fail_default ()
      in
      let start_minute =
         if start_minute_s = "" then
            0
         else
            let temp = int_of_string start_minute_s in
            if temp >= 0 && temp <= 59 then
               temp
            else
               parse_fail_default ()
      in

      let start_tm = 
         next_matching_time future current_tm start_hour start_minute start_meridien
      in
      let end_tm =
         if end_hour_s = "" && end_abbrev = "" then
            start_tm
         else
            let (end_hour, end_meridien) =
               if end_abbrev = "noon" then
                  (0, "pm")
               else if end_abbrev = "midnight" then
                  (0, "am")
               else
                  let temp = int_of_string end_hour_s in
                  if temp = 0 then
                     (0, "am")
                  else if temp >= 13 && temp <= 23 then
                     (temp - 12, "pm") 
                  else if temp >= 1 && temp <= 11 then
                     (temp, end_meridien_s)
                  else if temp = 12 then
                     (0, end_meridien_s) 
                  else
                     parse_fail_default ()
            in
            let end_minute =
               if end_minute_s = "" then
                  0
               else
                  let temp = int_of_string end_minute_s in
                  if temp >= 0 && temp <= 59 then
                     temp
                  else
                     parse_fail_default ()
            in
            next_matching_time true start_tm end_hour end_minute end_meridien
      in
      (start_tm, end_tm)


   end else
      parse_failwith "please submit a bug report for \"unreachable case 10\"."


(* at the beginning of the string, searches for date, then time,
 * then date again, then time again, as necessary *)
let rec find_date_time_begin date time event_remainder attempts =
   let find_element_gen element_start_regex element event_s =
      match element with
      | None ->
         if Str.string_match element_start_regex event_s 0 then begin
            let new_element = String.lowercase (Str.matched_group 1 event_s) in
            let pos = Str.match_end () in
            let new_event_s = Str.string_after event_s pos in
            ((Some new_element), new_event_s)
         end else
            (element, event_s)
      | Some _ ->
         (element, event_s)
   in
   let find_date = find_element_gen date_start_regex in
   let find_time = find_element_gen time_start_regex in

   if attempts >= 2 then
      (date, time, event_remainder)
   else
      let (new_date, event_remainder2) = find_date date event_remainder in
      let (new_time, event_remainder3) = find_time time event_remainder2 in
      find_date_time_begin new_date new_time event_remainder3 (succ attempts)


(* at the end of the string, searches for date, then time,
 * then date again, then time again, as necessary *)
let rec find_date_time_end date time event_remainder attempts =
   let find_element_gen element_end_regex element event_s =
      match element with
      | None ->
         begin try
            let pos = Str.search_forward element_end_regex event_s 0 in
            let new_element = String.lowercase (Str.matched_group 1 event_s) in
            let new_event_s = Str.string_before event_s pos in
            ((Some new_element), new_event_s)
         with Not_found ->
            (element, event_s)
         end
      | Some _ ->
         (element, event_s)
   in
   let find_date = find_element_gen date_end_regex in
   let find_time = find_element_gen time_end_regex in

   if attempts >= 2 then
      (date, time, event_remainder)
   else
      let (new_date, event_remainder2) = find_date date event_remainder in
      let (new_time, event_remainder3) = find_time time event_remainder2 in
      find_date_time_end new_date new_time event_remainder3 (succ attempts)


(* Primary function of this module.  Parses a natural language definition for an event,
 * which might look something like "Wednesday meeting with Bob at 9:30pm." *)
let parse_natural_language_event event_str =
   let (date, time, description) =
      (* search at the start of the string first, then at the end *)
      let (date_begin, time_begin, remainder) = 
         find_date_time_begin None None event_str 0
      in
      find_date_time_end date_begin time_begin remainder 0
   in
   let curr_tm = Unix.localtime (Unix.time ()) in
   let rem_spec =
      match date with
      | None ->
         begin match time with
         | None ->
            parse_fail_default ()
         | Some time_str ->
            Timed (parse_natural_language_time true curr_tm time_str)
         end
      | Some date_str ->
         let date_tm = parse_natural_language_date date_str in
         begin match time with
         | None ->
            Untimed date_tm
         | Some time_str ->
            Timed (parse_natural_language_time false date_tm time_str)
         end
   in
   (rem_spec, description)



(* arch-tag: DO_NOT_CHANGE_a43ce66f-688e-42dd-8c2b-83b55c124a5a *)
