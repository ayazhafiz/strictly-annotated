(** A module for pretty printing with annotations.
    The pretty printing interface comes from "Strictly Prettier" (Lindig, 2000).
    The contribution of this module is the algorithms for pretty printing with
    annotations, described in <LINK ARTICLE>. *)

type annotation = Annot of string  (** An annotation on a document. *)

(** A document to be pretty-printed. *)
type doc =
  | Nil  (** An empty document. *)
  | Text of string * annotation option
      (** A string literal, with an optional [annotation]. *)
  | Break of string
      (** A "flexible break".
          The pretty printer may use the presence of a [Break s] to inject a
          newline in the rendered document. If a newline is not printed, [s]
          is printed instead. *)
  | Cons of doc * doc  (** Concatenation of two documents. *)
  | Nest of int * doc
      (** [Nest (i, doc)] instructs the pretty-printer to indent newlines broken in
          [doc] by [i] spaces. This indentation is cumulative between nested
          [Nest]s. *)
  | Group of doc
      (** [Group doc] describes a "pretty-printing group". All optional line
           breaks (those given by [Break]) in a group are either turned into a
           space or a newline. The choice for an outer group is independent of
           the choice made for a nested group, but the decision for an outer
           group is made first. *)

val empty : doc
(** [empty] is [Nil] *)

val text : string -> doc
(** [text s] is [Text(s, None)] *)

val texta : string -> string -> doc
(** [texta s a] is [Text(s, Some (Annot a))] *)

val nest : int -> doc -> doc
(** [nest i s] is [Nest(i, s)] *)

val space : doc
(** [space] is [Break " "] *)

val breakhint : doc
(** [breakhint] is [Break ""] *)

val break : string -> doc
(** [break s] is [Break s] *)

val group : doc -> doc
(** [group d] is [Group d] *)

val ( ^^ ) : doc -> doc -> doc
(** [x ^^ y] is [Cons(x, y)]. *)

val ( ^| ) : doc -> doc -> doc
(** [x ^| y] is [x ^^ space ^^ y], modulo [x] or [y] being Nil. *)

val ( ^. ) : doc -> doc -> doc
(** [x ^. y] is [x ^^ breakhint ^^ y], modulo [x] or [y] being Nil. *)

val pretty : ?global_align:bool -> int -> string -> doc -> string
(** [pretty ?global_align width doc] pretty-prints [doc] with a maximum line
    [width].
    [global_align] determines if annotations should be aligned across the entire
    document. By default [global_align] is false, and annotations are instead
    aligned relative to local sequences of annotations.
    For information on constructing documents, see [doc]. *)
