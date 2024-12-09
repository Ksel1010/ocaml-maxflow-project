open Graph

type path = id list
type flow = {
  flow: int;
  capacity: int
}

val find_path: int graph -> id list -> id -> id -> path option

val graphFlow: int graph -> id -> id -> string graph

val graphEcart: int graph -> id -> id -> int graph

val minFlow: int graph -> id list -> int

val getArcs : 'a graph -> id list -> 'a arc list