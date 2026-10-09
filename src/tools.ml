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