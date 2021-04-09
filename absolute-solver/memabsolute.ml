(* this executable is just a wrapper that initializes a memory trace and then
   calls absolute. This permits the monitoring of the memory usage without add
   Memtrace as a dependency of the solver. *)

let () =
  Memtrace.trace_if_requested ~context:"absolute" () ;
  Config.main ()
