#!/usr/bin/env ocaml

#use "topfind";;
#thread;;   (* Batteries apparently needs this... *)
#require "batteries";;

open Batteries;;

let input_filename  = "_oasis.in" in
let output_filename = "_oasis" in

(* Extract the VERSION=x.y.z line from Makefile.in *)
let makefile_version_number = 
  let version_regex = Str.regexp "^VERSION[ \\t]*=\\([^ \\t]+\\)[ \\t]*$" in
  let lines = File.lines_of "Makefile.in" in
  let matching_line = Enum.find (fun line -> Str.string_match version_regex line 0) lines in
  Str.matched_group 1 matching_line
in

(* Plug this version into the appropriate place in _oasis.in *)
let oasis_content =
  let oasis_version_regex = Str.regexp "@WYRD_VERSION@" in
  let oasis_template = File.with_file_in input_filename IO.read_all in

  let replace_count = ref 0 in
  let subst input =
    let () = incr replace_count in
    makefile_version_number
  in

  let result = Str.global_substitute oasis_version_regex subst oasis_template in
  let () = assert (!replace_count = 1) in
  result
in

let () = File.with_file_out output_filename (fun ch -> IO.nwrite ch oasis_content) in
Printf.printf "Wrote ./%s using version number \"%s\".\n" output_filename makefile_version_number;;

