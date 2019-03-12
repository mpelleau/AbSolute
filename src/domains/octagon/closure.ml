open Dbm

module type Closure_sig =
sig
  module DBM : DBM_sig
  val closure: DBM.t -> unit
  val incremental_closure: DBM.t -> DBM.dbm_constraint -> unit
  val is_consistent : DBM.t -> unit
end

module GenericClosure(B: Bound_sig.BOUND)(DBM: DBM_sig with type cell = B.t) =
struct

  (* Check the consistency of the DBM, and update the cells in the diagonal to 0 (Miné, 2004). *)
  let consistent_Q dbm =
    let n = DBM.dimension dbm in
    for i = 0 to (2*n-1) do
      if B.lt (DBM.get dbm (i,i)) B.zero then
        raise Bot.Bot_found
      else
        DBM.set dbm (i,i) B.zero
    done

  let shortest_path_closure dbm =
  begin
    let m i j = DBM.get dbm (i,j) in
    let n = DBM.dimension dbm in
    for k = 0 to (2*n-1) do
      for i = 0 to (2*n-1) do
        for j = 0 to (2*n-1) do
          let v = B.min (m i j) (B.add_up (m i k) (m k j)) in
          DBM.set dbm (i,j) v
        done
      done
    done;
    consistent_Q dbm
  end

  (* The part of the modified Floyd-Warshall algorithm for a given 'k' (Mine, 2005). *)
  let strong_closure_k dbm k =
    let m i j = DBM.get dbm (i,j) in
    let n = DBM.dimension dbm in
    for i = 0 to (2*n-1) do
      for j = 0 to (2*n-1) do
        let v = List.fold_left B.min (m i j) [
          B.add_up (m i (2*k+1)) (m (2*k+1) j) ;
          B.add_up (m i (2*k)) (m (2*k) j) ;
          B.add_up (B.add_up (m i (2*k)) (m (2*k) (2*k+1))) (m (2*k+1) j) ;
          B.add_up (B.add_up (m i (2*k+1)) (m (2*k+1) (2*k))) (m (2*k) j) ] in
        DBM.set dbm (i,j) v
      done
    done

  (* Strengthening propagates the octagonal constraints in the DBM. *)
  let strengthening dbm =
    let m i j = DBM.get dbm (i,j) in
    let n = DBM.dimension dbm in
    for i = 0 to (2*n-1) do
      for j = 0 to (2*n-1) do
        let i' = i lxor 1 in
        let j' = j lxor 1 in
        let v = B.div_up (B.add_up (m i i') (m j' j)) B.two in
        DBM.set dbm (i,j) v
      done
    done
end

module ClosureZ(DBM: DBM_sig with type cell = Bound_int.t) = struct
  module DBM = DBM
  module B = Bound_int
  module GClosure = GenericClosure(B)(DBM)

  include GClosure

  let is_consistent dbm =
    let m i j = DBM.get dbm (i,j) in
    let n = DBM.dimension dbm in
    for i = 0 to (2*n-1) do
      let i' = i lxor 1 in
      if B.lt (B.add_up (m i i') (m i' i)) B.zero then
        raise Bot.Bot_found
    done

  let tightening dbm =
    let m i j = DBM.get dbm (i,j) in
    let n = DBM.dimension dbm in
    for i = 0 to (2*n-1) do
      let i' = i lxor 1 in
      let v = B.mul_up (B.div_down (m i i') B.two) B.two in
      DBM.set dbm (i,i') v
    done

  (* Strong closure as appearing in (Bagnara, 2009) using classical Floyd-Warshall algorithm followed by the strengthening procedure. *)
  let closure dbm =
    shortest_path_closure dbm;
    tightening dbm;
    is_consistent dbm;
    strengthening dbm

  (* Incremental tight closure from (Chawdhary and al., 2018). *)
  let incremental_closure dbm ((a,b), d) =
  let m i j = DBM.get dbm (i,j) in
  if B.leq (m a b) d then () else
  begin
    let n = DBM.dimension dbm in
    let a' = a lxor 1 in
    let b' = b lxor 1 in
    let d2 = B.mul_up d B.two in
    for i = 0 to (2*n-1) do
      let i' = i lxor 1 in
      let v = List.fold_left B.min (m i i') [
        B.add_up (B.add_up (m i a) d) (m b i');
        B.add_up (B.add_up (m i b') d) (m a' i');
        B.add_up (B.add_up (B.add_up (m i b') d2) (m a' a)) (m b i');
        B.add_up (B.add_up (B.add_up (m i a) d2) (m b b')) (m a' i');
      ] in
      DBM.set dbm (i,i') (B.mul_up (B.div_down v B.two) B.two)
    done;
    is_consistent dbm;
    for i = 0 to (2*n-1) do
      for j = 0 to (2*n-1) do
        let i' = i lxor 1 in
        let j' = j lxor 1 in
        if j <> i' then
          let v = List.fold_left B.min (m i j) [
            B.add_up (B.add_up (m i a) d) (m b j);
            B.add_up (B.add_up (m i b') d) (m a' j);
            B.add_up (B.add_up (B.add_up (m i b') d2) (m a' a)) (m b j);
            B.add_up (B.add_up (B.add_up (m i a) d2) (m b b')) (m a' j);
            B.div_up (B.add_up (m i i') (m j' j)) B.two
          ] in
          DBM.set dbm (i,j) v
      done;
      if B.lt (m i i) B.zero then
        raise Bot.Bot_found
    done
  end
end

module ClosureQ(DBM: DBM_sig with type cell = Bound_rat.t) = struct
  module DBM = DBM
  module B = Bound_rat
  module GClosure = GenericClosure(B)(DBM)

  include GClosure

  let is_consistent = consistent_Q

  (* Strong closure as appearing in (Bagnara, 2009) using classical Floyd-Warshall algorithm followed by the strengthening procedure. *)
  let closure dbm =
    shortest_path_closure dbm;
    is_consistent dbm;
    strengthening dbm

  let incremental_closure dbm oc = ()
end

module ClosureF(DBM: DBM_sig with type cell = Bound_float.t) = struct
  module DBM = DBM
  module B = Bound_float
  module GClosure = GenericClosure(B)(DBM)

  include GClosure

  let is_consistent = consistent_Q

  (* Strong closure as appearing in (Miné, 2005) using a modified Floyd-Warshall algorithm. *)
  let closure dbm =
    let n = DBM.dimension dbm in
    for k = 0 to n-1 do
      strong_closure_k dbm k;
      strengthening dbm
    done;
    is_consistent dbm

  let incremental_closure dbm oc = ()
end
