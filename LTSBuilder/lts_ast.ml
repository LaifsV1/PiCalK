open Format

(* Process AST *)
type var = string
type name = string
type meth = string
type label = Out of (name * name)
           | OutFresh of (name * name)
           | In of (name * name)
           | InFresh of (name * name)
           | Tau
type process = Empty
             | Apply of name * var list
             | Tau of process
             | Send of name * name * process
             | Receive of name * var * process
             | New of name * process
             | Sum of process list
             | Par of process list
type meth_body = var list * process
type program = Main of process
             | Def of meth * meth_body * program
             | Ext of name * program

(* LTS data abstract syntax *)
module NameSet = Set.Make(String)
module MethodRepo =
  Map.Make(struct type t = meth let compare = compare end)

(* global * local * processes *)
type lts_state = NameSet.t * NameSet.t * process list
type lts_delta = label * int * int
type lts = (int * lts_state) list * lts_delta list

(* process semantics *)
let name_counter = ref 0
let meth_counter = ref 0
let var_counter = ref 0

let new_name () =
  let i = !name_counter in
  name_counter := i + 1;
  sprintf "_name_%d_" i
let new_meth () =
  let i = !meth_counter in
  name_counter := i + 1;
  sprintf "_meth_%d_" i
let new_var () =
  let i = !var_counter in
  name_counter := i + 1;
  sprintf "_var_%d_" i

let lts_states = ref []
let lts_deltas = ref []
let lts_methods = ref MethodRepo.empty

let program_init program =
  match program with
  | Main process -> process_beta [process]
  | Def(meth,meth_body,program) ->
     lts_methods := (!lts_methods).add
                      (new_meth ()) method_body;
     program_init program
  | Ext(name,program) -> failwith "todo"

let rec process_beta global local processes = failwith "todo"
