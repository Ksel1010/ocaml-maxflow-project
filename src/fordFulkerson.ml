open Graph
open Tools

type path = id list
type flow = {
  flow: int;
  capacity: int
}


let find_path graph forbidden id1 id2 = 
  let rec notForbidden (n:id) = function
    |[]-> true
    |x::rest-> if x=n then false else notForbidden n rest
  in
  let rec aux acc forbidden = function
    |[]-> None
    |arc::res-> if (arc.lbl=0||not(notForbidden arc.tgt forbidden)) then aux acc forbidden res
    else  if arc.tgt = id2 then Some (List.rev (id2::acc))
                  else if (notForbidden arc.tgt forbidden) then
                        let ch = aux (arc.tgt::acc) (arc.tgt::forbidden) (out_arcs graph arc.tgt) in 
                        if ch=None then aux acc forbidden  res else ch
                  else aux acc forbidden res
  in aux [id1] (id1::forbidden) (out_arcs graph id1)

let getArcs graph li = 
  let rec aux id acc = function
  |[]->acc
  |x::rest-> match find_arc graph id x with
            |None-> raise (Graph_error "Arc doesn't exist")
            |Some ar -> aux x (ar::acc) rest
  in
  match li with 
  |[]-> []
  |id::res-> aux id [] res

(*let minFlow graph li = 
  let rec aux acc id1 = function
  |[]-> acc
  |x::res-> match  (find_arc graph id1 x) with
      |None->raise  (Graph_error "Arc doesn't exist")
      |Some ar ->  if ar.lbl<acc   then aux ar.lbl x res


                   else aux acc x res
in match li with
  |[]-> 0
  |_::[]-> 0
  |id::res-> aux max_int id res
  *)

  let minFlow graph li = 
    let arcs = getArcs graph li 
  in
  let rec aux acc = function
    |[]-> acc
    |arc::res-> if arc.lbl<acc then aux arc.lbl res  else aux acc res
  in aux max_int arcs


let rec graphEcart graph id1 id2 = 
  let () = Printf.printf " appel graphEcart %!" in
  
  match find_path graph [] id1 id2 with
  |None-> graph
  |Some ch-> let mf = minFlow graph ch 
in
let arcs = getArcs graph ch in 

  let () = List.iter (fun n ->Printf.printf " %d %!" n.lbl) arcs
in let rec aux g = function 
  |[]-> graphEcart g id1 id2
  |x::res-> match res with 
            |[]-> graphEcart g id1 id2
            |y::_-> let g1 = add_arc g x y (-mf) in

                    let g2 = add_arc g1 y x mf in 
                    
                    aux g2 res
in aux graph ch 






  

let graphFlow graph id1 id2 = 
  let () = Printf.printf " appel graphFlow %!" in
  let graphE = graphEcart graph id1 id2 
in  
e_fold graph (fun aux arc -> new_arc aux {src = arc.src; tgt = arc.tgt; 
        lbl = string_of_int(match (find_arc graphE arc.tgt arc.src) with
        |None-> 0
        |Some arc_i -> arc_i.lbl)^"/"^string_of_int(arc.lbl)}) (clone_nodes graph)

                 


