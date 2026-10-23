(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = e_fold 
  gr 
  (fun g arc -> new_arc g {src=arc.src; tgt=arc.tgt; lbl=(f arc.lbl)}) 
  (clone_nodes gr)

let add_arc gr id1 id2 n = 
  assert ((node_exists gr id1) && (node_exists gr id2));
  match find_arc gr id1 id2 with
  | None -> new_arc gr ({src=id1; tgt=id2; lbl=n})
  | Some {src=src; tgt=tgt; lbl=lbl} -> new_arc gr ({src; tgt; lbl=(lbl+n)})

let find_path gr s p = ();

let rec find_min_capacity gr path = 
	match path with 
	| [] | [_] -> max_int
	| u :: v :: path_rest ->  
		match find_arc gr u v with
		| None -> assert false
		| Some arc -> min arc.lbl (find_min_capacity gr (v :: path_rest))
		
let rec update_graph gr path flot = 
	match path with 
	| [] | [_] -> gr
	| u :: v :: path_rest -> 
		let gr_1 = add_arc gr u v (-flot) in 
		  let gr_2 = add_arc gr_1 v u flot in 
		    update_graph gr_2 (v :: path_rest) flot

let rec ford_fulkerson gr s p = 
	match find_path s p with
	| None -> gr
	| Some path -> 
		let flot = find_min_capacity gr path in 
		  let updated_graph = update_graph gr path flot in
	    	ford_fulkerson updated_graph s p
		