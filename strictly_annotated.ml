type annotation = Annot of string

type doc =
  | Nil
  | Text of string * annotation option
  | Break of string
  | Cons of doc * doc
  | Nest of int * doc
  | Group of doc

let ( ^^ ) x y = Cons (x, y)

let empty = Nil

let text s = Text (s, None)

let texta s a = Text (s, Some (Annot a))

let nest i x = Nest (i, x)

let space = Break " "

let breakhint = Break ""

let break s = Break s

let group d = Group d

let ( ^| ) x y =
  match (x, y) with _, Nil -> x | Nil, _ -> y | x, y -> x ^^ space ^^ y

let ( ^. ) x y =
  match (x, y) with _, Nil -> x | Nil, _ -> y | x, y -> x ^^ breakhint ^^ y

let strlen = String.length

let spaces n = String.make n ' '

type doc_ir =
  | IrDone
  | IrText of string * annotation list * doc_ir
  | IrNewline of int * doc_ir

type line = Line of int * string * annotation option

let string_of_line aprefix = function
  | Line (i, s, None) -> spaces i ^ s
  | Line (i, s, Some (Annot a)) -> spaces i ^ s ^ aprefix ^ a

let string_of_lines aprefix lines =
  List.map (string_of_line aprefix) lines |> String.concat "\n"

type break_mode = Flat | Break

(** [fits width d] determines if the document [d] can fit on a single
    line. Note that [d] is not a [doc], but a list of triples of form
    [[(indent-on-newline, mode, doc)]]. *)
let rec fits w = function
  | _ when w < 0 -> false
  | [] -> true
  | (_, _, Nil) :: rest -> fits w rest
  | (_, _, Text (s, _)) :: rest -> fits (w - strlen s) rest
  | (_, Flat, Break s) :: rest -> fits (w - strlen s) rest
  | (_, Break, Break _) :: _ ->
      failwith
        "fits should be used to determine if Break mode is necessary, not \
         called with a Break with Break mode"
  | (i, m, Cons (x, y)) :: rest -> fits w ((i, m, x) :: (i, m, y) :: rest)
  | (i, m, Nest (j, x)) :: rest -> fits w ((i + j, m, x) :: rest)
  | (i, _, Group x) :: rest -> fits w ((i, Flat, x) :: rest)

(** [sparse d] determines if the flat group [d] is sparsely annotated in the
    sense that there are no [Break]s between annotations. *)
let sparse d =
  let rec check annots seenbrk = function
    | [] -> true
    | Nil :: rest -> check annots seenbrk rest
    | Cons (x, y) :: rest -> check annots seenbrk (x :: y :: rest)
    | Nest (_, x) :: rest -> check annots seenbrk (x :: rest)
    | Text (_, None) :: rest -> check annots seenbrk rest
    | Text (_, Some _) :: _ when annots >= 1 && seenbrk -> false
    | Text (_, Some _) :: rest -> check (annots + 1) seenbrk rest
    | Break _ :: rest -> check annots true rest
    | Group x :: rest -> check annots seenbrk (x :: rest)
  in
  check 0 false d

(** [layout w d] determines the layout for document [d] given a maximum line
    width [w]. [layout] decides whether breaks become newlines and emits a
    [doc_ir] representation of [d]. *)
let layout w =
  let rec go l = function
    | [] -> IrDone
    | (_, _, Nil) :: rest -> go l rest
    | (_, _, Text (s, a)) :: rest ->
        IrText (s, Option.to_list a, go (l + strlen s) rest)
    | (_, Flat, Break s) :: rest -> IrText (s, [], go (l + strlen s) rest)
    | (i, Break, Break _) :: rest -> IrNewline (i, go i rest)
    | (i, m, Cons (x, y)) :: rest -> go l ((i, m, x) :: (i, m, y) :: rest)
    | (i, m, Nest (j, x)) :: rest -> go l ((i + j, m, x) :: rest)
    | (i, _, Group x) :: rest ->
        if sparse [ x ] && fits (w - l) [ (i, Flat, x) ] then
          go l ((i, Flat, x) :: rest)
        else go l ((i, Break, x) :: rest)
  in
  go

(** [linearize ir] flattens continuations in the [doc_ir] [ir], yielding a
    sequence of [line]s ready for printing. *)
let linearize ir =
  let distribute_line (indent, text, annots) =
    match annots with
    | [] -> [ Line (indent, text, None) ]
    | a :: rest ->
        let textspaces = spaces (strlen text) in
        Line (indent, text, Some a)
        :: List.map (fun a -> Line (indent, textspaces, Some a)) rest
  in
  let rec linear lines curline = function
    | IrDone -> lines @ distribute_line curline
    | IrNewline (i, ir) ->
        linear (lines @ distribute_line curline) (i, "", []) ir
    | IrText (s1, annots1, ir) ->
        let i, s, annots = curline in
        linear lines (i, s ^ s1, annots @ annots1) ir
  in
  linear [] (0, "", []) ir

let align_annotations blocks =
  let align_block = function
    | `Bare, lines -> lines
    | `Annot, lines ->
        let linewidth =
          List.fold_left
            (fun m (Line (indent, s, _)) -> max m (indent + strlen s))
            0 lines
        in
        List.map
          (function
            | Line (indent, s, Some a) ->
                Line (indent, s ^ spaces (linewidth - indent - strlen s), Some a)
            | l -> l)
          lines
  in
  List.concat_map align_block blocks

(** [align_local lines] groups lines into consecutive sequences of annotated or
    bare (unannotated) blocks, and then locally aligns all annotated blocks. *)
let align_local lines =
  let rec group blocks curblock lines =
    match (curblock, lines) with
    | _, [] -> curblock :: blocks
    | (`Annot, _), Line (_, _, None) :: _ ->
        group (curblock :: blocks) (`Bare, []) lines
    | (`Bare, _), Line (_, _, Some _) :: _ ->
        group (curblock :: blocks) (`Annot, []) lines
    | (kind, curlines), line :: rest ->
        group blocks (kind, line :: curlines) rest
  in
  let blocks =
    group [] (`Bare, []) lines
    |> List.map (fun (k, l) -> (k, List.rev l))
    |> List.rev
  in
  align_annotations blocks

let align_global lines = align_annotations [ (`Annot, lines) ]

let pretty ?(global_align = false) w annot_prefix doc =
  let ir =
    layout w 0 [ (0, Flat, Group doc) ]
    |> linearize
    |> if global_align then align_global else align_local
  in
  string_of_lines annot_prefix ir
