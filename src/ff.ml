open Graph
open Tools

(* 
  Returns either a path s->t where each label > 0 
  Or None
*)
let find_path gr s t = 
  let rec dfs current visited =
    if current = t then Some (List.rev (current :: visited))
    else 
      List.find_map (fun {tgt; lbl; _} -> 
        if lbl > 0 && not (List.mem tgt visited) then 
          dfs tgt (current::visited)
        else None) (out_arcs gr current)
  in dfs s [] 

(* Returns the capacity between vertecies u and v *)
let get_capacity gr u v = 
  match find_arc gr u v with 
  | None -> 0
  | Some arc -> arc.lbl

let rec sommets_to_arcs = function
	| u :: v :: rest -> (u, v) :: sommets_to_arcs (v :: rest)
	| _ -> []

(* Returns the bottleneck of a path *)
let find_min_capacity gr path = 
	List.fold_left (fun acc (u, v) -> min acc (get_capacity gr u v)) max_int (sommets_to_arcs path)
		
(* 
  Sets the flow along the path : 
    decrement from forward arc by min_capacity 
    increments the backward path by min_capacity 
*)
let update_graph gr path flow = 
	List.fold_left (fun acc (u, v) -> add_arc (add_arc acc u v (-flow)) v u flow) gr (sommets_to_arcs path)

(* 
  Implements the Ford Fulkerson algorithm
   - first parameter is the capacity graph
*)
let rec ford_fulkerson gr s p = 
	assert (s <> p);
  match find_path gr s p with 
  | None -> gr
  | Some path -> ford_fulkerson (update_graph gr path (find_min_capacity gr path)) s p

(* 
  Returns the flow graph
*)
let build_flow_graph cap_graph res_graph =
  e_fold cap_graph (fun acc arc ->
    let res_cap = get_capacity res_graph arc.src arc.tgt in
    let flow = arc.lbl - res_cap in
    let lbl_str = "\"" ^ string_of_int flow ^ "/" ^ string_of_int arc.lbl ^ "\"" in
    new_arc acc { src = arc.src; tgt = arc.tgt; lbl = lbl_str }
  ) (clone_nodes cap_graph)
