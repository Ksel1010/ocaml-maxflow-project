open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold gr (fun acu id -> new_node acu id) empty_graph
let gmap (gr: 'a graph) (f: 'a -> 'b)  = 
    e_fold gr (fun acu arc -> new_arc acu {src=arc.src; tgt=arc.tgt; lbl=(f arc.lbl)}) (clone_nodes gr)

let add_arc gr id1 id2 n = 
    match (find_arc gr id1 id2) with
        | None ->  new_arc gr {src=id1; tgt=id2; lbl = n}
        | Some a -> new_arc gr {src=id1; tgt=id2; lbl = a.lbl+n}
