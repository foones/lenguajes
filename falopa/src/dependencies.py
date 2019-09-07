import functools

# A directed graph is a dictionary such that graph[x] is
# the set of neighbors of x.

def partition_dependencies(graph):
    """Receives a (reflexive) graph of dependencies.
       Returns a partition [X1, ..., Xn] where each Xi is a strongly
       connected component, and such that if
         x ∈ Xi,
         y ∈ Xj, and
         x depends on y   (y ∈ graph[x])
       then:
         i <= j."""

    # Calculate strongly connected components
    representative = strongly_connected_components(graph)
    inv_representative = {}
    for x in representative:
        y = representative[x]
        inv_representative[y] = inv_representative.get(y, set())
        inv_representative[y].add(x)

    # Build a directed graph `sccs_graph`
    # whose nodes are representatives of the
    # strongly connected components,
    # and such that sccs_graph[x] are the (strict)
    # dependencies of x.
    sccs = set()
    sccs_graph = {}
    for x in graph:
        y = representative[x]
        sccs.add(y)
        sccs_graph[y] = set()

    for x in graph:
        y = representative[x]
        sccs_graph[y] |= graph[x] & (sccs - set([y]))

    # Calculate the transitive closure
    sorted_sccs = topological_sort(sccs_graph)
    
    # Partition
    partition = []
    for x in sorted_sccs:
        partition.append(inv_representative[x])
    return partition

def strongly_connected_components(graph):
    """Returns a dictionary `representative` such that
       x, y are in the same strongly connected component
       if and only if
       representative[x] == representative[y]."""
    lst = []
    visited = set()
    representative = {}

    inv_graph = {}
    for x in graph:
        for y in graph[x]:
            inv_graph[y] = inv_graph.get(y, [])
            inv_graph[y].append(x)

    def visit(x):
        if x in visited:
            return
        visited.add(x)
        for y in graph[x]:
            visit(y)
        lst.append(x) 

    def set_representative(x, c):
        if x in representative:
            return
        representative[x] = c
        for y in inv_graph[x]:
            set_representative(y, c)

    for x in graph:
        visit(x)

    for x in reversed(lst):
        set_representative(x, x)

    return representative

def topological_sort(graph):
    closure = transitive_closure(graph)
    return sorted(
      graph.keys(),
      key=functools.cmp_to_key(
        lambda x, y:
          0 if x == y else -1 if x in closure[y] else 1))

def transitive_closure(graph):
    visited = set()
    closure = {}

    def visit(x):
        if x not in visited:
            closure[x] = set([x])
            for y in graph[x]:
                visit(y)
                closure[x] |= closure[y]

    for x in graph:
        visit(x)

    return closure

