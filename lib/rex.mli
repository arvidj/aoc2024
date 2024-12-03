(** {1 Regular Expressions} *)

type rex
(** Compiled regular expressions. *)

val rex : ?opts:Re.Perl.opt list -> string -> rex
(** Compile a regular expression using Perl syntax. *)

val rexf : ?opts:Re.Perl.opt list -> ('a, unit, string, rex) format4 -> 'a
(** Same as [rex @@ sf ...]. *)

val show : rex -> string
(** Convert a regular expression to a string using Perl syntax. *)

val ( =~ ) : string -> rex -> bool
(** Test whether a string matches a regular expression.

    Example: ["number 1234 matches" =~ rex "\\d+"] *)

val ( =~! ) : string -> rex -> bool
(** Negation of [=~]. *)

val ( =~* ) : string -> rex -> string option
(** Match a regular expression with one capture group. *)

val ( =~** ) : string -> rex -> (string * string) option
(** Match a regular expression with two capture groups. *)

val ( =~*** ) : string -> rex -> (string * string * string) option
(** Match a regular expression with three capture groups. *)

val ( =~**** ) : string -> rex -> (string * string * string * string) option
(** Match a regular expression with four capture groups. *)

val matches : string -> rex -> string list
(** Match a regular expression with one capture group and return all results. *)

val replace_string :
  ?pos:int ->
  (* Default: 0 *)
  ?len:int ->
  ?all:bool ->
  (* Default: true. Otherwise only replace first occurrence *)
  rex ->
  (* matched groups *)
  by:string ->
  (* replacement string *)
  string ->
  (* string to replace in *)
  string
(** [replace_string ~all rex ~by s] iterates on [s], and replaces every
    occurrence of [rex] with [by]. If [all = false], then only the first
    occurrence of [rex] is replaced. *)
