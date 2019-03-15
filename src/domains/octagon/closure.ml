open Dbm

module type Closure_sig =
sig
  module DBM : DBM_sig

  val closure: DBM.t -> unit
  val incremental_closure: DBM.t -> DBM.bound dbm_constraint -> unit
  val is_consistent : DBM.t -> unit
end

module GenericClosure(DBM: DBM_sig) =
struct
  module B=DBM.B

  (* Check the consistency of the DBM, and update the cells in the diagonal to 0 (Miné, 2004). *)
  let consistent_Q dbm =
    let n = DBM.dimension dbm in
    for i = 0 to (2*n-1) do
      let e = {x=i;y=i} in
      if B.lt (DBM.get dbm e) B.zero then
        raise Bot.Bot_found
      else
        DBM.set dbm {v=e; c=B.zero}
    done

  let shortest_path_closure dbm =
  begin
    let m i j = DBM.get dbm {x=i;y=j} in
    let n = DBM.dimension dbm in
    for k = 0 to (2*n-1) do
      for i = 0 to (2*n-1) do
        for j = 0 to (2*n-1) do
          let c = B.min (m i j) (B.add_up (m i k) (m k j)) in
          DBM.set dbm {v={x=i;y=j};c=c}
        done
      done
    done;
    consistent_Q dbm
  end

  (* The part of the modified Floyd-Warshall algorithm for a given 'k' (Mine, 2005). *)
  let strong_closure_k dbm k =
    let m i j = DBM.get dbm {x=i;y=j} in
    let n = DBM.dimension dbm in
    for i = 0 to (2*n-1) do
      for j = 0 to (2*n-1) do
        let c = List.fold_left B.min (m i j) [
          B.add_up (m i (2*k+1)) (m (2*k+1) j) ;
          B.add_up (m i (2*k)) (m (2*k) j) ;
          B.add_up (B.add_up (m i (2*k)) (m (2*k) (2*k+1))) (m (2*k+1) j) ;
          B.add_up (B.add_up (m i (2*k+1)) (m (2*k+1) (2*k))) (m (2*k) j) ] in
        DBM.set dbm {v={x=i;y=j};c=c}
      done
    done

  (* Strengthening propagates the octagonal constraints in the DBM. *)
  let strengthening dbm =
    let m i j = DBM.get dbm {x=i;y=j} in
    let n = DBM.dimension dbm in
    for i = 0 to (2*n-1) do
      for j = 0 to (2*n-1) do
        let i' = i lxor 1 in
        let j' = j lxor 1 in
        let c = B.div_up (B.add_up (m i i') (m j' j)) B.two in
        DBM.set dbm {v={x=i;y=j};c=c}
      done
    done
end

module ClosureZ = struct
  module DBM = Dbm.Make(Bound_int)
  module GClosure = GenericClosure(DBM)

  include GClosure

  let is_consistent dbm =
    let m i j = DBM.get dbm {x=i;y=j} in
    let n = DBM.dimension dbm in
    for i = 0 to (2*n-1) do
      let i' = i lxor 1 in
      if B.lt (B.add_up (m i i') (m i' i)) B.zero then
        raise Bot.Bot_found
    done

  let tightening dbm =
    let m i j = DBM.get dbm {x=i;y=j} in
    let n = DBM.dimension dbm in
    for i = 0 to (2*n-1) do
      let i' = i lxor 1 in
      let c = B.mul_up (B.div_down (m i i') B.two) B.two in
      DBM.set dbm {v={x=i;y=i'};c=c}
    done

  (* Strong closure as appearing in (Bagnara, 2009) using classical Floyd-Warshall algorithm followed by the strengthening procedure. *)
  let closure dbm =
    shortest_path_closure dbm;
    tightening dbm;
    is_consistent dbm;
    strengthening dbm

  (* Incremental tight closure from (Chawdhary and al., 2018). *)
  let incremental_closure dbm {v=v;c=d} =
    let m i j = DBM.get dbm {x=i;y=j} in
    begin
      let n = DBM.dimension dbm in
      let v' = inv v in
      let (a, b) = v.x, v.y in
      let (a', b') = v'.x, v'.y in
      let d2 = B.mul_up d B.two in
      for i = 0 to (2*n-1) do
        let i' = i lxor 1 in
        let c = List.fold_left B.min (m i i') [
          B.add_up (B.add_up (m i a) d) (m b i');
          B.add_up (B.add_up (m i b') d) (m a' i');
          B.add_up (B.add_up (B.add_up (m i b') d2) (m a' a)) (m b i');
          B.add_up (B.add_up (B.add_up (m i a) d2) (m b b')) (m a' i');
        ] in
        DBM.set dbm {v={x=i; y=i'}; c=(B.mul_up (B.div_down c B.two) B.two)}
      done;
      is_consistent dbm;
      for i = 0 to (2*n-1) do
        for j = 0 to (2*n-1) do
          let i' = i lxor 1 in
          let j' = j lxor 1 in
          if j <> i' then
            let c = List.fold_left B.min (m i j) [
              B.add_up (B.add_up (m i a) d) (m b j);
              B.add_up (B.add_up (m i b') d) (m a' j);
              B.add_up (B.add_up (B.add_up (m i b') d2) (m a' a)) (m b j);
              B.add_up (B.add_up (B.add_up (m i a) d2) (m b b')) (m a' j);
              B.div_up (B.add_up (m i i') (m j' j)) B.two
            ] in
            DBM.set dbm {v={x=i; y=j}; c=c}
        done;
        if B.lt (m i i) B.zero then
          raise Bot.Bot_found
      done
    end
end

module ClosureQ = struct
  module DBM = Dbm.Make(Bound_rat)
  module GClosure = GenericClosure(DBM)

  include GClosure

  let is_consistent = consistent_Q

  (* Strong closure as appearing in (Bagnara, 2009) using classical Floyd-Warshall algorithm followed by the strengthening procedure. *)
  let closure dbm =
    shortest_path_closure dbm;
    is_consistent dbm;
    strengthening dbm

  let incremental_closure dbm oc = ()
end

module ClosureF = struct
  module DBM = Dbm.Make(Bound_float)
  module GClosure = GenericClosure(DBM)

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
