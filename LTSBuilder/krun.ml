(* performs system call of command, returns output *)
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

(* krun functions *)
let krun (args:string) =
  syscall (Printf.sprintf
             "krun --directory ../PiCalK/ %s"
             args)
let ksearch (args:string) =
  syscall (Printf.sprintf
             "krun --directory ../PiCalK/ --search %s"
             args)
let kpattern pattern (args:string) =
  syscall (Printf.sprintf
             "krun --directory ../PiCalK/ --search %s %s"
             pattern args)

