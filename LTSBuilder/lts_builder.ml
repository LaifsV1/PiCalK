let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  (Buffer.contents buf)

let krun args =
  syscall (Printf.sprintf
             "krun --directory ../PiCalK/ %s"
             args)
let ksearch args =
  syscall (Printf.sprintf
             "krun --directory ../PiCalK/ --search %s %s"
             args)
let kpattern pattern args =
  syscall (Printf.sprintf
             "krun --directory ../PiCalK/ --search %s %s"
             pattern args)

type states = State of int * string
type delta = Delta of int * int
type lts = Lts of states list * delta list

(*returns a KAST string*)
let kast_of_conf (conf) = conf

(*returns a KAST string*)
let compare_confs conf1 conf2 = conf1 = conf2

(*returns new states to explore and an updated LTS*)
(*only adds to ss and ds if conf has new things to add*)
let conf_of_kast kast old_confs lts =
  let new_confs = old_confs in
  new_confs , lts

let lts_step conf confs lts =
  let results = ksearch conf in
  let new_confs , new_lts = conf_of_kast results lts in
  new_confs , new_lts

let rec lts_build (confs) (lts:lts) :(lts) =
  match confs with
  | [] -> lts
  | conf::confs' ->
     let new_confs , new_lts = lts_step conf confs' lts in
     lts_build new_confs new_lts
              
  
