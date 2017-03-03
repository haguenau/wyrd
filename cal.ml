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

(* cal.ml
 * Because cal(1) cannot be relied upon to be uniform across various operating
 * systems (sigh), it seemed best to provide a generic calendar layout
 * algorithm. *)


open Utility

type t = {
   title    : string;
   weekdays : string;
   days     : string list;
   weeknums : string list
}


(* compute the ISO-8601 week number for the given day *)
let weeknum_of_tm day =
   let (_, normalized_day) = Unix.mktime day in
   (* iso weeks start on Monday *)
   let iso_wday = (normalized_day.Unix.tm_wday + 6) mod 7 in
   (* find Thursday of the same week *)
   let thurs = {normalized_day with
      Unix.tm_mday = normalized_day.Unix.tm_mday + 3 - iso_wday
   } in
   let (_, normalized_thurs) = Unix.mktime thurs in
   (* which Thursday of the year is it? *)
   (normalized_thurs.Unix.tm_yday / 7) + 1



(* Create a Cal.t data structure for the desired timestamp.  If
 * start_monday = true then the first day of the week will be
 * Monday. *)
let make timestamp start_monday =
   let tm = Unix.localtime timestamp in
   (* compute the weekday of the first day of the month *)
   let first_weekday =
      let temp = {tm with Unix.tm_mday = 1} in
      let (_, first) = Unix.mktime temp in
      first.Unix.tm_wday
   in
   (* compute the last day of the month *)
   let last_day = 
      let temp = {tm with Unix.tm_mday = 32} in
      let (_, nextmonth) = Unix.mktime temp in
      32 - nextmonth.Unix.tm_mday
   in
   (* generate the title *)
   let year_s    = string_of_int (tm.Unix.tm_year + 1900) in
   let mon_year  = (full_string_of_tm_mon tm.Unix.tm_mon) ^ " " ^ year_s in
   let pad_len   = (20 - (String.length mon_year)) / 2 in
   let cal_title = (String.make pad_len ' ') ^ mon_year in
   (* generate the weekday strings *)
   let rec build_weekdays wkd_str wd_num count =
      if count > 7 then
         wkd_str
      else
         build_weekdays (wkd_str ^ " " ^ (short_string_of_tm_wday wd_num)) 
            ((succ wd_num) mod 7) (succ count)
   in
   let week_start_day = if start_monday then 1 else 0 in
   let cal_weekdays = build_weekdays (short_string_of_tm_wday week_start_day)
       ((succ week_start_day) mod 7) 2
   in
   (* generate the days of the month *)
   let rec build_monthdays weeks_list week_str weeknum_list mday wday =
      if mday > last_day then
         let weeknum_str = 
            let last_weekday = {tm with Unix.tm_mday = pred mday} in
            let weeknum = weeknum_of_tm last_weekday in
            Printf.sprintf "%2d" weeknum
         in
         (List.rev (week_str :: weeks_list), List.rev (weeknum_str :: weeknum_list))
      else
         if wday = week_start_day then
            let weeknum_str = 
               let last_weekday = {tm with Unix.tm_mday = pred mday} in
               let weeknum = weeknum_of_tm last_weekday in
               Printf.sprintf "%2d" weeknum
            in
            build_monthdays (week_str :: weeks_list) (Printf.sprintf "%2d" mday)
               (weeknum_str :: weeknum_list) (succ mday) ((succ wday) mod 7)
         else
            build_monthdays weeks_list (week_str ^ (Printf.sprintf " %2d" mday))
               weeknum_list (succ mday) ((succ wday) mod 7)
   in
   (* create the padding for the first few empty days of the calendar *)
   let padding =
      if first_weekday >= week_start_day then
         String.make ((first_weekday - week_start_day) * 3) ' '
      else
         String.make ((first_weekday + 7 - week_start_day) * 3) ' '
   in
   let (cal_monthdays, cal_weeknums) = 
      build_monthdays [] (padding ^ " 1") [] 2 ((succ first_weekday) mod 7)
   in {
      title    = cal_title;
      weekdays = cal_weekdays;
      days     = cal_monthdays;
      weeknums = cal_weeknums
   }



      
(* arch-tag: DO_NOT_CHANGE_4909df7f-9801-448d-9030-fb4b0232408d *)
