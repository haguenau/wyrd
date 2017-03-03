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

(* utility.ml
 *
 * miscellaneous helper functions that don't really fit elsewhere *)


exception String_of_tm_mon_failure of string
exception String_of_tm_wday_failure of string
exception Unicode_length_failure of string

(* append a file to a directory, with the proper number
 * of slashes *)
let join_path dirname filename =
   let dir_last   = dirname.[String.length dirname - 1]
   and file_first = filename.[0] in
   if dir_last = '/' && file_first = '/' then
      dirname ^ (Str.string_after filename 1)
   else if dir_last <> '/' && file_first <> '/' then
      dirname ^ "/" ^ filename
   else
      dirname ^ filename


(* Perform shell expansion of environment variables. *)
let shell_expand text =
   let split_regex = Str.regexp "=" in
   (* Get a list of all environment variable mappings *)
   let add_mapping env_mapping mapping_list =
      match Str.split split_regex env_mapping with
      | key :: value :: [] -> 
         (key, value) :: mapping_list 
      | _ ->
         mapping_list
   in
   let env_mappings = Array.fold_right add_mapping (Unix.environment ()) [] in
   let rec apply_mappings mapping_list s =
      match mapping_list with
      | [] ->
         s
      | (var, expansion) :: tail ->
         let var_regex = Str.regexp_string ("$" ^ var) in
         apply_mappings tail (Str.global_replace var_regex expansion s)
   in
   apply_mappings env_mappings text

   
(* If the filename starts with "~", substitute $HOME.  Then do shell
 * expansion on the resulting string. *)
let expand_file filename =
   let tilde_expansion = 
      if String.length filename >= 2 && Str.string_before filename 2 = "~/" then
         "$HOME" ^ Str.string_after filename 1
      else
         filename
   in
   shell_expand tilde_expansion


(* Do whatever is necessary to open up a file for writing.  If it already exists,
 * open it as-is.  If it does not exist, make sure that all prefix directories
 * do exist, then open a new file. *)
let open_or_create_out_gen is_binary filename =
   let exp_file = expand_file filename in
   (* Test whether the file exists *)
   if Sys.file_exists exp_file then
      if is_binary then
         open_out_bin exp_file
      else
         open_out exp_file
   else
      (* Check whether all directories exist *)
      let dir_path = Filename.dirname exp_file in
      let dir_list = Str.split (Str.regexp "/+") dir_path in
      (* if necessary, add the first "/" to the first directory *)
      let slash_dir_list =
         if not (Filename.is_relative dir_path) then
            ("/" ^ (List.hd dir_list)) :: (List.tl dir_list)
         else
            dir_list
      in
      let rec make_directories d_list =
         match d_list with
         | [] ->
            ()
         | d :: tail ->
            begin
               try Sys.chdir d
               with Sys_error err_msg ->
                  begin 
                     let _ = Sys.command ("mkdir " ^ d) in
                     Sys.chdir d
                  end
            end;
            make_directories tail
      in
      make_directories slash_dir_list;
      if is_binary then
         open_out_bin (Filename.basename exp_file)
      else
         open_out (Filename.basename exp_file)


let open_or_create_out_bin filename =
   open_or_create_out_gen true filename

let open_or_create_out_ascii filename =
   open_or_create_out_gen false filename



(* open a filename, with tilde/$HOME expansion *)
let expand_open_in_gen is_binary filename =
   (* If the filename starts with "~", substitute $HOME *)
   if is_binary then
      open_in_bin (expand_file filename)
   else
      open_in (expand_file filename)


let expand_open_in_bin filename =
   expand_open_in_gen true filename

let expand_open_in_ascii filename =
   expand_open_in_gen false filename



let string_of_tm_mon i =
   match i with
   | 0 -> "Jan"
   | 1 -> "Feb"
   | 2 -> "Mar"
   | 3 -> "Apr"
   | 4 -> "May"
   | 5 -> "Jun"
   | 6 -> "Jul"
   | 7 -> "Aug"
   | 8 -> "Sep"
   | 9 -> "Oct"
   |10 -> "Nov"
   |11 -> "Dec"
   | x -> raise (String_of_tm_mon_failure ("unknown month " ^ (string_of_int x)))

let full_string_of_tm_mon i =
   match i with
   | 0 -> "January"
   | 1 -> "February"
   | 2 -> "March"
   | 3 -> "April"
   | 4 -> "May"
   | 5 -> "June"
   | 6 -> "July"
   | 7 -> "August"
   | 8 -> "September"
   | 9 -> "October"
   |10 -> "November"
   |11 -> "December"
   | x -> raise (String_of_tm_mon_failure ("unknown month " ^ (string_of_int x)))

let short_string_of_tm_wday i =
   match i with
   | 0 -> "Su"
   | 1 -> "Mo"
   | 2 -> "Tu"
   | 3 -> "We"
   | 4 -> "Th"
   | 5 -> "Fr"
   | 6 -> "Sa"
   | x -> raise (String_of_tm_wday_failure ("unknown weekday " ^ (string_of_int x)))

let string_of_tm_wday i =
   match i with
   | 0 -> "Sun"
   | 1 -> "Mon"
   | 2 -> "Tue"
   | 3 -> "Wed"
   | 4 -> "Thu"
   | 5 -> "Fri"
   | 6 -> "Sat"
   | x -> raise (String_of_tm_wday_failure ("unknown weekday " ^ (string_of_int x)))

let full_string_of_tm_wday i =
   match i with
   | 0 -> "Sunday"
   | 1 -> "Monday"
   | 2 -> "Tuesday"
   | 3 -> "Wednesday"
   | 4 -> "Thursday"
   | 5 -> "Friday"
   | 6 -> "Saturday"
   | x -> raise (String_of_tm_wday_failure ("unknown weekday " ^ (string_of_int x)))



(* it's useful to have an empty date record to save some typing *)
let empty_tm = {
   Unix.tm_sec   = 0;
   Unix.tm_min   = 0;
   Unix.tm_hour  = 0;
   Unix.tm_mday  = 1;
   Unix.tm_mon   = 0;
   Unix.tm_year  = 1900;
   Unix.tm_wday  = 0;
   Unix.tm_yday  = 0;
   Unix.tm_isdst = false
}


(* strip whitespace *)

let lstrip s =
   (* Any amount of whitespace, followed by a non-whitespace char,
    * followed by any number of characters.  If match fails,
    * then the string must be entirely whitespace. *)
   let re = Str.regexp "[ \t]*\\([^ \t].*\\)" in
   if Str.string_match re s 0 then
      Str.replace_first re "\\1" s
   else
      ""

let rstrip s =
   (* Any number of characters, followed by a non-whitespace char,
    * followed by any number of whitespace chars.  If match
    * fails, then the string must be entirely whitespace. *)
   let re = Str.regexp "\\(.*[^ \t]\\).*" in
   if Str.string_match re s 0 then
      Str.replace_first re "\\1" s
   else
      ""

let strip s = lstrip (rstrip s)
      

(* Use the shell to open a process, read all output from both stdout and stderr, 
 * and close the pipes to the process again.  Returns a list of lines from
 * stdout, and a list of lines from stderr.
 *
 * Uses select(), so it should be robust to I/O buffering synchronization
 * issues. *)
let read_all_shell_command_output shell_command =
   let (in_read, in_write)   = Unix.pipe ()
   and (out_read, out_write) = Unix.pipe ()
   and (err_read, err_write) = Unix.pipe () in
   let rec read_output out_str err_str out_done err_done =
      if out_done && err_done then
         (out_str, err_str)
      else begin
         let out_lst = if out_done then [] else [out_read]
         and err_lst = if err_done then [] else [err_read] in
         (* find some output to read *)
         let (read_list, _, _) = Unix.select (out_lst @ err_lst) [] [] (10.0) in
         if List.length read_list > 0 then begin
            let chan = List.hd read_list in
            let buf = String.make 256 ' ' in
            let chars_read = Unix.read chan buf 0 256 in
            if chars_read = 0 then
               (* no chars read indicates EOF *)
               if chan = out_read then
                  read_output out_str err_str true err_done
               else
                  read_output out_str err_str out_done true
            else
               (* if 1-256 characters are read, append them to the proper
                * buffer and continue *)
               let s = String.sub buf 0 chars_read in
               if chan = out_read then
                  read_output (out_str ^ s) err_str out_done err_done
               else
                  read_output out_str (err_str ^ s) out_done err_done
         end else
            (out_str, err_str)
      end
   in
   (* launch the shell process *)
   let pid = 
      Unix.create_process "/bin/sh" [| "/bin/sh"; "-c"; shell_command |] 
      in_read out_write err_write 
   in
   (* these belong to remind, so close them off *)
   Unix.close in_read;
   Unix.close out_write;
   Unix.close err_write;
   let (out_str, err_str) = read_output "" "" false false in
   (* clean up remind zombie *)
   let _ = Unix.waitpid [] pid in
   (* close off our end of the IPC pipes *)
   Unix.close in_write;
   Unix.close out_read;
   Unix.close err_read;
   let newline_regex = Str.regexp "\n" in
   let out_lines = Str.split newline_regex out_str
   and err_lines = Str.split newline_regex err_str in
   (out_lines, err_lines)


(* Compute the number of bytes required to store a utf-8 character.
 * Input is the first byte of the character. *)
let utf8_width (byte : char) =
   let c = Char.code byte in
   if      c < 0x80 then 1
   else if c < 0xc0 then raise (Unicode_length_failure "illegal byte")
   else if c < 0xe0 then 2
   else if c < 0xf0 then 3
   else if c < 0xf8 then 4
   else if c < 0xfc then 5
   else if c < 0xfe then 6
   else
      raise (Unicode_length_failure "illegal byte")
   

(* Compute the number of UTF-8 characters contained in an ocaml String. *)
let utf8_len s =
   let s_len = String.length s in
   let rec len_aux byte_pos char_count =
      if byte_pos >= s_len then
         char_count
      else
         let num_bytes = utf8_width s.[byte_pos] in
         len_aux (byte_pos + num_bytes) (succ char_count)
   in
   if Curses.Curses_config.wide_ncurses then
      len_aux 0 0
   else
      (* If build process does not detect ncursesw, then fall back
       * on standard string behavior. *)
      s_len


(* Form the substring of all characters from 's' in positions before 'n',
 * where 'n' may be measured in characters rather than bytes.  Does the right
 * thing for utf-8 wide characters. *)
let utf8_string_before s n =
   let rec build_substr substr utf8_pos byte_pos =
      if utf8_pos >= n then
         substr
      else
         let num_new_bytes = utf8_width s.[byte_pos] in
         let new_bytes = String.make num_new_bytes s.[byte_pos] in
         for i = 1 to pred num_new_bytes do
            new_bytes.[i] <- s.[byte_pos + i]
         done;
         build_substr (substr ^ new_bytes) (succ utf8_pos) 
            (byte_pos + num_new_bytes)
   in
   if Curses.Curses_config.wide_ncurses then
      build_substr "" 0 0
   else
      (* If we're not using utf-8, fall back on standard string behavior. *)
      Str.string_before s n


(* Form the substring of all characters from 's' in positions 'n' or greater,
 * where 'n' may be measured in characters rather than bytes.  Does the right
 * thing for utf-8 wide characters. *)
let utf8_string_after s n =
   if Curses.Curses_config.wide_ncurses then begin
      let starting_byte = ref 0 in
      for utf8_char = 0 to pred n do
         starting_byte := !starting_byte + (utf8_width s.[!starting_byte])
      done;
      Str.string_after s !starting_byte
   end else
      (* If we're not using utf-8, fall back on standard string behavior. *)
      Str.string_after s n



(* arch-tag: DO_NOT_CHANGE_a87790db-2dd0-496c-9620-ed968f3253fd *)
