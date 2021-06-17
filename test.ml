open Strictly_annotated

let binop l op r = group (nest 2 (group (text l ^| text op) ^| text r))

let ifthenelse c t e =
  group
    (group (nest 2 (texta "if" "new conditional" ^| c))
    ^| group (nest 2 (texta "then" "true branch" ^| t))
    ^| group (nest 2 (texta "else" "false branch" ^| e)))

let page7 =
  ifthenelse (binop "a" "==" "b") (binop "a" "<<" "2") (binop "a" "+" "b")

let rm1 s = String.sub s 1 (String.length s - 1)

let mk_test name expect pretty =
  let test () = Alcotest.(check string) name expect (pretty ()) in
  (name, `Quick, test)

let page7_cases =
  [
    ( 32,
      rm1
        {|
if a == b   # new conditional
then a << 2 # true branch
else a + b  # false branch|},
      rm1
        {|
if a == b   # new conditional
then a << 2 # true branch
else a + b  # false branch|}
    );
    ( 15,
      rm1
        {|
if a == b   # new conditional
then a << 2 # true branch
else a + b  # false branch|},
      rm1
        {|
if a == b   # new conditional
then a << 2 # true branch
else a + b  # false branch|}
    );
    ( 10,
      rm1
        {|
if a == b # new conditional
then      # true branch
  a << 2
else a + b # false branch|},
      rm1
        {|
if a == b  # new conditional
then       # true branch
  a << 2
else a + b # false branch|}
    );
    ( 8,
      rm1
        {|
if # new conditional
  a == b
then # true branch
  a << 2
else # false branch
  a + b|},
      rm1
        {|
if       # new conditional
  a == b
then     # true branch
  a << 2
else     # false branch
  a + b|}
    );
    ( 7,
      rm1
        {|
if # new conditional
  a ==
    b
then # true branch
  a <<
    2
else # false branch
  a + b|},
      rm1
        {|
if      # new conditional
  a ==
    b
then    # true branch
  a <<
    2
else    # false branch
  a + b|}
    );
    ( 6,
      rm1
        {|
if # new conditional
  a ==
    b
then # true branch
  a <<
    2
else # false branch
  a +
    b|},
      rm1
        {|
if     # new conditional
  a ==
    b
then   # true branch
  a <<
    2
else   # false branch
  a +
    b|}
    );
  ]

let page7_tests =
  List.map
    (fun (w, expect, _) ->
      mk_test (string_of_int w) expect (fun () -> pretty w " # " page7))
    page7_cases

let page7_global_align_tests =
  List.map
    (fun (w, _, expect) ->
      mk_test (string_of_int w) expect (fun () ->
          pretty ~global_align:true w " # " page7))
    page7_cases

let dense_sparse_cases =
  [
    ( 10,
      ( group
          (group (text "add " ^^ texta "rax, " "arr[0]" ^^ texta "r14" "arr[1]")
          ^| group
               (text "mov " ^^ texta "[rbx], " "&arr[0]" ^^ texta "rax" "arr[0]")
          ),
        rm1
          {|
add rax, r14   ; arr[0]
               ; arr[1]
mov [rbx], rax ; &arr[0]
               ; arr[0]|}
      ),
      ( group
          (group
             (nest 4
                (text "add " ^^ texta "rax," "arr[0]" ^| texta "r14" "arr[1]"))
          ^| group
               (nest 4
                  (text "mov " ^^ texta "[rbx]," "&arr[0]"
                 ^| texta "rax" "arr[0]"))),
        rm1
          {|
add rax,   ; arr[0]
    r14    ; arr[1]
mov [rbx], ; &arr[0]
    rax    ; arr[0]|}
      ) );
  ]

let dense_tests =
  List.mapi
    (fun i (w, (doc, expect), _) ->
      mk_test ("dense " ^ string_of_int i) expect (fun () -> pretty w " ; " doc))
    dense_sparse_cases

let sparse_tests =
  List.mapi
    (fun i (w, _, (doc, expect)) ->
      mk_test
        ("sparse " ^ string_of_int i)
        expect
        (fun () -> pretty w " ; " doc))
    dense_sparse_cases

let () =
  Alcotest.run "Annot tests"
    [
      ("Page 7 Examples", page7_tests);
      ("Page 7 Examples (global align)", page7_global_align_tests);
      ("Dense Annotations", dense_tests);
      ("Sparse Annotations", sparse_tests);
    ]
