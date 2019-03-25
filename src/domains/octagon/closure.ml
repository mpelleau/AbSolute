open Dbm

module type Closure_sig =
sig
  module DBM : DBM_sig

  val closure: DBM.t -> DBM.t
  val incremental_closure: DBM.t -> DBM.bound dbm_constraint -> DBM.t
  val is_consistent : DBM.t -> DBM.t
end

module GenericClosure(DBM: DBM_sig) =
struct
  module B=DBM.B

  (* Check the consistency of the DBM, and update the cells in the diagonal to 0 (Miné, 2004). *)
  let consistent_Q dbm =
  begin
    let dbm = ref dbm in
    let n = DBM.dimension !dbm in
    for i = 0 to (2*n-1) do
      let e = {l=i;c=i} in
      if B.lt (DBM.get !dbm e) B.zero then
        raise Bot.Bot_found
      else
        dbm := DBM.set !dbm {v=e; d=B.zero}
    done;
    !dbm
  end

  let shortest_path_closure dbm =
  begin
    let dbm = ref dbm in
    let m i j = DBM.get !dbm (make_var i j) in
    let n = DBM.dimension !dbm in
    for k = 0 to (2*n-1) do
      for i = 0 to (2*n-1) do
        for j = 0 to (2*n-1) do
          let d = B.min (m i j) (B.add_up (m i k) (m k j)) in
          dbm := DBM.set !dbm {v=(make_var i j);d=d}
        done
      done
    done;
    consistent_Q !dbm
  end

  (* The part of the modified Floyd-Warshall algorithm for a given 'k' (Mine, 2005). *)
  let strong_closure_k dbm k =
  begin
    let dbm = ref dbm in
    let m i j = DBM.get !dbm (make_var i j) in
    let n = DBM.dimension !dbm in
    for i = 0 to (2*n-1) do
      for j = 0 to (2*n-1) do
        let d = List.fold_left B.min (m i j) [
          B.add_up (m i (2*k+1)) (m (2*k+1) j) ;
          B.add_up (m i (2*k)) (m (2*k) j) ;
          B.add_up (B.add_up (m i (2*k)) (m (2*k) (2*k+1))) (m (2*k+1) j) ;
          B.add_up (B.add_up (m i (2*k+1)) (m (2*k+1) (2*k))) (m (2*k) j) ] in
        dbm := DBM.set !dbm {v=(make_var i j);d=d}
      done
    done;
    !dbm
  end

  (* Strengthening propagates the octagonal constraints in the DBM. *)
  let strengthening dbm =
  begin
    let dbm = ref dbm in
    let m i j = DBM.get !dbm (make_var i j) in
    let n = DBM.dimension !dbm in
    for i = 0 to (2*n-1) do
      for j = 0 to (2*n-1) do
        let i' = i lxor 1 in
        let j' = j lxor 1 in
        let d = B.div_up (B.add_up (m i i') (m j' j)) B.two in
        dbm := DBM.set !dbm {v=(make_var i j);d=d}
      done
    done;
    !dbm
  end
end

module ClosureZ = struct
  module DBM = Dbm.Make(Bound_int)
  module GClosure = GenericClosure(DBM)

  include GClosure

  let is_consistent dbm =
  begin
    let dbm = ref dbm in
    let m i j = DBM.get !dbm (make_var i j) in
    let n = DBM.dimension !dbm in
    for i = 0 to (2*n-1) do
      let i' = i lxor 1 in
      if B.lt (B.add_up (m i i') (m i' i)) B.zero then
        raise Bot.Bot_found
    done;
    !dbm
  end

  let tightening dbm =
  begin
    let dbm = ref dbm in
    let m i j = DBM.get !dbm (make_var i j) in
    let n = DBM.dimension !dbm in
    for i = 0 to (2*n-1) do
      let i' = i lxor 1 in
      let d = B.mul_up (B.div_down (m i i') B.two) B.two in
      dbm := DBM.set !dbm {v=(make_var i i');d=d}
    done;
    !dbm
  end

  (* Strong closure as appearing in (Bagnara and al., 2009) using classical Floyd-Warshall algorithm followed by the strengthening procedure. *)
  let closure dbm =
    shortest_path_closure dbm
    |> tightening
    |> is_consistent
    |> strengthening

  (* Incremental tight closure from (Chawdhary and al., 2018). *)
  let incremental_closure dbm {v=v;d=d} =
  begin
    let dbm = ref dbm in
    let m i j = DBM.get !dbm (make_var i j) in
    dbm := DBM.set !dbm {v=v;d=d};
    let n = DBM.dimension !dbm in
    let v' = inv v in
    let (a, b) = v.l, v.c in
    let (a', b') = v'.l, v'.c in
    let d2 = B.mul_up d B.two in
    for i = 0 to (2*n-1) do
      let i' = i lxor 1 in
      let d = List.fold_left B.min (m i i') [
        B.add_up (B.add_up (m i a) d) (m b i');
        B.add_up (B.add_up (m i b') d) (m a' i');
        B.add_up (B.add_up (B.add_up (m i b') d2) (m a' a)) (m b i');
        B.add_up (B.add_up (B.add_up (m i a) d2) (m b b')) (m a' i');
      ] in
      dbm := DBM.set !dbm {v=(make_var i i'); d=(B.mul_up (B.div_down d B.two) B.two)}
    done;
    dbm := is_consistent !dbm;
    for i = 0 to (2*n-1) do
      for j = 0 to (2*n-1) do
        let i' = i lxor 1 in
        let j' = j lxor 1 in
        if j <> i' then
          let d = List.fold_left B.min (m i j) [
            B.add_up (B.add_up (m i a) d) (m b j);
            B.add_up (B.add_up (m i b') d) (m a' j);
            B.add_up (B.add_up (B.add_up (m i b') d2) (m a' a)) (m b j);
            B.add_up (B.add_up (B.add_up (m i a) d2) (m b b')) (m a' j);
            B.div_up (B.add_up (m i i') (m j' j)) B.two
          ] in
          dbm := DBM.set !dbm {v=(make_var i j); d=d}
      done;
      if B.lt (m i i) B.zero then
        raise Bot.Bot_found
    done;
    !dbm
  end
end

module ClosureQ = struct
  module DBM = Dbm.Make(Bound_rat)
  module GClosure = GenericClosure(DBM)

  include GClosure

  let is_consistent = consistent_Q

  (* Strong closure as appearing in (Bagnara and al., 2009) using classical Floyd-Warshall algorithm followed by the strengthening procedure. *)
  let closure dbm =
    shortest_path_closure dbm
    |> is_consistent
    |> strengthening

  let incremental_closure dbm oc = dbm
end

module ClosureF = struct
  module DBM = Dbm.Make(Bound_float)
  module GClosure = GenericClosure(DBM)

  include GClosure

  let is_consistent = consistent_Q

  (* Strong closure as appearing in (Miné, 2005) using a modified Floyd-Warshall algorithm. *)
  let closure dbm =
    let dbm = ref dbm in
    let n = DBM.dimension !dbm in
    for k = 0 to n-1 do
      dbm := strong_closure_k !dbm k;
      dbm := strengthening !dbm
    done;
    is_consistent !dbm

  let incremental_closure dbm oc = dbm
end
