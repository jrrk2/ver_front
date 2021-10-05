(*
    <vscr - Verilog converter to abc format.>
    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

exception Error

open Vparser
type dmode = Pundef | PATH;;
type prep =
| P_CELLDEFINE
| P_DEFINE
| P_DELAY_MODE_PATH
| P_DISABLE_PORTFAULTS
| P_ELSE
| P_ENABLE_PORTFAULTS
| P_ENDCELLDEFINE
| P_ENDIF
| P_ENDPROTECT
| P_IFDEF
| P_IFNDEF
| P_INCLUDE of string
| P_NOSUPPRESS_FAULTS
| P_PROTECT
| P_RESETALL
| P_SUPPRESS_FAULTS
| P_TIMESCALE of string
| P_PREPROC of string
| P_ENDOFFILE

let timescale = ref "";;
let delay_mode = ref Pundef;;
let (includes:(string*in_channel) Stack.t) = Stack.create();;
let (ifdef_stk:bool Stack.t) = Stack.create();;
let celldefine = ref false and portfaults = ref false and suppress_faults = ref false and protect = ref false;;

let tsymbols = Hashtbl.create 255

let _ = List.iter (fun (str,key) -> Hashtbl.add tsymbols (Bytes.of_string str) key)
[
("`celldefine", P_CELLDEFINE );
("`define", P_DEFINE );
("`delay_mode_path", P_DELAY_MODE_PATH );
("`disable_portfaults", P_DISABLE_PORTFAULTS );
("`enable_portfaults", P_ENABLE_PORTFAULTS );
("`endcelldefine", P_ENDCELLDEFINE );
("`endprotect", P_ENDPROTECT );
("`else", P_ELSE );
("`endif", P_ENDIF );
("`ifdef", P_IFDEF );
("`ifndef", P_IFNDEF );
("`include", P_INCLUDE "");
("`nosuppress_faults", P_NOSUPPRESS_FAULTS );
("`protect", P_PROTECT );
("`resetall", P_RESETALL );
("`suppress_faults", P_SUPPRESS_FAULTS );
("`timescale", P_TIMESCALE "" );
];;

let my_openin f = if f.[0]='~' then open_in (Sys.getenv "HOME" ^ String.sub f 1 (String.length f - 1)) else open_in f

let from_special1 out_chan macro_raw =
(* first convert any tabs to spaces *)
for i = 0 to (Bytes.length macro_raw)-1 do if Bytes.get macro_raw i=='\t' then Bytes.set macro_raw i ' '; done;
let blank1 = Bytes.index macro_raw ' ' in begin
let substr = Bytes.sub macro_raw 0 blank1 in if (Hashtbl.mem tsymbols substr) then
  begin
  let right = Bytes.sub macro_raw (blank1+1) ((Bytes.length macro_raw)-blank1-1) in
  match Hashtbl.find tsymbols substr with
  | P_PREPROC replace -> replace^" "^Bytes.to_string right
  | P_INCLUDE _ -> ( try Scanf.sscanf (Bytes.to_string right) " \"%s@\"" (fun nam ->
						     Printf.fprintf out_chan "Open %s\n" nam;
						     Stack.push (nam,my_openin nam) includes)
						     with End_of_file -> () | Scanf.Scan_failure msg -> ()); ""
  | P_DEFINE -> 	(* check the replacement text is not null, if so define it to blank *)
	let macro = if (Bytes.contains_from macro_raw (blank1+1) ' ') then Bytes.to_string macro_raw else Bytes.to_string macro_raw^"  " in
	let blank2 = String.index_from macro (blank1+1) ' ' in
	let name = "`" ^ (String.sub macro (blank1+1) (blank2-blank1-1)) in
	let defn = String.sub macro (blank2+1) (String.length(macro)-blank2-1) in
	let idx = ref 0 in
  	  while (!idx < String.length defn) && (defn.[!idx] == ' ') do idx := !idx+1; done;
          let repl = String.sub (defn) (!idx) (String.length(defn)-(!idx)) in Hashtbl.add tsymbols (Bytes.of_string name) (P_PREPROC repl);
        ""
  | P_TIMESCALE _  -> timescale := Bytes.to_string right;
     ""
  | P_IFDEF -> let defn = "`"^Bytes.to_string (Bytes.sub right 0 (Bytes.length(right)-1)) in let cond = Hashtbl.mem tsymbols (Bytes.of_string defn) in 

     Stack.push cond ifdef_stk; ""
  | P_IFNDEF -> let defn = "`"^Bytes.to_string (Bytes.sub right 0 (Bytes.length(right)-1)) in let cond = Hashtbl.mem tsymbols (Bytes.of_string defn) in 
     Stack.push (not cond) ifdef_stk; ""
  | P_ELSE -> Stack.push (not (Stack.pop ifdef_stk)) ifdef_stk; ""
  | P_ENDIF -> ignore(Stack.pop ifdef_stk); ""
  | _ -> Bytes.to_string macro_raw
  end
else
  ""
end 

let from_special2 out_chan macro_raw =
let retval = ref "" in begin
if (Hashtbl.mem tsymbols macro_raw) then
  begin
  match Hashtbl.find tsymbols macro_raw with P_PREPROC replace -> retval := replace
  | P_ELSE -> Stack.push (not (Stack.pop ifdef_stk)) ifdef_stk;
  | P_ENDIF -> ignore(Stack.pop ifdef_stk);
  | _ -> retval := Bytes.to_string macro_raw
  end
else
  begin
  (* Printf.fprintf trace_file "%s is not `defined\n" macro_raw; *)
  end;
!retval;
end

let rec paste out_chan src (dst:bytes) dstlen = let tick1 = String.index src '`' and looping = ref true in (
      let tend = ref (tick1+1) in while !looping && (!tend < String.length src) do match src.[!tend] with
      | 'A'..'Z' -> tend := !tend+1
      | 'a'..'z' -> tend := !tend+1
      | '0'..'9' -> tend := !tend+1
      | '_' -> tend := !tend+1
      | _ -> looping := false
      done;
      let subst = from_special2 out_chan (Bytes.of_string (String.sub src tick1 (!tend-tick1))) in
      let combined = (String.sub src 0 tick1)^subst^(String.sub src (!tend) ((String.length src)-(!tend)))^"\n" in
      let totlen = String.length combined in
      (* Printf.fprintf trace_file "Source %s subst=%s combined=%s len=%d\n" src subst combined totlen; *)
      if (String.contains combined '`')&&(String.index combined '`'>tick1) then paste out_chan combined dst dstlen else
      (String.blit combined 0 dst 0 totlen;
      totlen))

let from_blit out_chan src dst dstlen =
  let looping = ref true and preproc = ref false in
  let tstart = ref 0 in while !looping && (!tstart < String.length src) do match src.[!tstart] with
    | ' ' -> tstart := !tstart+1
    | '\t' -> tstart := !tstart+1
    | '`' -> preproc := true; looping := false
    | _ -> looping := false
    done;
  preproc := !preproc && ((String.contains_from src !tstart ' ')||(String.contains_from src !tstart '\t'));
      (* Printf.fprintf trace_file "Source %s preproc=%s\n" src (string_of_bool !preproc); *)
  if (!preproc) then begin
    let subst = from_special1 out_chan (Bytes.of_string (String.sub src !tstart ((String.length src)- !tstart))) in
    let len = String.length subst in
    String.blit subst 0 dst 0 len;
    Bytes.set dst len '\n';
    len+1 end
  else if (String.contains src '`') then paste out_chan src dst dstlen
  else (
    String.blit src 0 dst 0 dstlen;
    Bytes.set dst dstlen '\n';
    dstlen+1)

let my_input_line chan cnt = 
  let idx = ref 0 and looping = ref true and str = Bytes.create cnt in
  while (!looping) && (!idx < cnt-2) do
    Bytes.set str !idx (input_char chan);
    if !idx > 0 && Bytes.get str (!idx) = '/' && Bytes.get str (!idx-1) = '/' then (
      let comment = ref true in while !comment do
          comment := input_char chan <> '\n';
        if not !comment then (decr idx; Bytes.set str !idx '\n')
        done);
    if !idx > 0 && Bytes.get str !idx = '*' && Bytes.get str (!idx-1) = '/' then (
      let comment = ref true and star = ref false in while !comment do
        if !star then
	  begin
          let nxt = input_char chan in
	  comment := nxt <> '/';
	  star := nxt = '*';
	  end
        else
	  star := input_char chan = '*';
        if not !comment then (decr idx; Bytes.set str !idx '\n')
        done);
    if Bytes.get str !idx == '\n' then looping := false;
    if (!idx > cnt/2) && ((Bytes.get str !idx == ' ') || (Bytes.get str !idx == '\t')) then looping := false;
    idx := !idx + 1;
  done;
  Bytes.sub str 0 !idx

let from_func out_chan dst cnt =
    try let retval = ref 0 and looping = ref true in while !looping do
      let src = my_input_line (snd(Stack.top includes)) cnt in
      retval := from_blit out_chan (Bytes.to_string src) dst (Bytes.length src);
      looping := Stack.top ifdef_stk == false;
      done;
      !retval
  with End_of_file ->
    Printf.fprintf out_chan "Close %s\n" (fst (Stack.top includes));
    close_in_noerr (snd(Stack.pop includes));
    if Stack.is_empty includes then
      begin
      Printf.fprintf out_chan "Stack empty\n";
      end
    else Printf.fprintf out_chan "Open %s\n" (fst (Stack.top includes));
    Bytes.set dst 0 '\n';
    if Stack.is_empty includes then 0 else 1

let trace_open () = ()

let preproc_open () = ()

let preproc_close () = ()

let rslth = Hashtbl.create 255

let rslt' looping = function
                  | P_RESETALL                          -> begin
                    celldefine := false;
                    portfaults := false;
                    suppress_faults := false;
                    protect := false;
                    timescale := ""
                  end
                  | P_CELLDEFINE                        -> celldefine := true
                  | P_ENDCELLDEFINE                     -> celldefine := false
                  | P_ENABLE_PORTFAULTS                 -> portfaults := true
                  | P_DISABLE_PORTFAULTS                -> portfaults := false
                  | P_SUPPRESS_FAULTS                   -> suppress_faults := true
                  | P_NOSUPPRESS_FAULTS                 -> suppress_faults := false
                  | P_PROTECT                           -> protect := true;
                  | P_ENDPROTECT                        -> protect := false;
                  | P_DELAY_MODE_PATH                   -> delay_mode := PATH
                  | P_ENDOFFILE -> looping := false
                  | oth -> Hashtbl.replace rslth oth ()

let getstr = function
| oth -> failwith "getstr"

let rec cat = function
  | [] -> ""
  | IDSTR oth :: tl -> oth ^ cat tl

let parse' str =
  let psuccess = ref true in
  trace_open();
      begin
        Stack.push (str, my_openin str) includes;
        Stack.push true ifdef_stk; (* toplevel ifdef default *)
        preproc_open();
        begin
            let lexbuf = Lexing.from_function (from_func stdout) in
            let looping = ref true in
            while !looping do
              let rslt = Vparser.start Vlexer.token lexbuf in
              match rslt with
              | toklst,ENDOFFILE -> looping := false;
                let outfile = open_out (str^".preproc") in
                output_string outfile (cat (List.rev toklst));
                close_out outfile
              | oth -> failwith "start"
(*
              rslt' looping rslt
*)
            done
        end;
        preproc_close();
      end;
  !psuccess

