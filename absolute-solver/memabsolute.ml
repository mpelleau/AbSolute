(* this executable is just a wrapper that initializes a memory trace and then
   calls absolute. This makes possible the monitoring of the memory usage
   without adding Memtrace as a dependency of the solver. It is also compiled
   with 'landmarks' as a preprocessor to monitor the time consumption *)

let () =
  Memtrace.trace_if_requested ~context:"absolute" () ;
  Config.main ()
