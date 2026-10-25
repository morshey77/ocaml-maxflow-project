open Gfile
open Tools
open Ff
    
let () =
  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;

  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  and source = int_of_string Sys.argv.(2)
  and sink = int_of_string Sys.argv.(3)
  in

  (* Open file and convert to right format *)
  let graph = gmap (from_file infile) int_of_string in

  (* Executes Ford Fulkerson's algorithm on user's inputs *)
  let residual_graph = ford_fulkerson graph source sink in 

  (* Creates the final flow graph from the residual graph *)
  let flow_graph = build_flow_graph graph residual_graph in

  (* Export the graph that has been read. *)
  let () = export outfile flow_graph in

  ()

