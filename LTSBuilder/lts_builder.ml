open Kast
open Kast_tools
open Krun_sys
open Format

(* new name counter *)
let name_counter = ref 0

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
