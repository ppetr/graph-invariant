# graph-isomorphism

_Disclaimer: This is not an official Google product._

A matrix-based algorithm to compute graph invariants (or more generally
arbitrary algebra invariants) and their automorphism groups.

Branch **breadth-first**: This is an experimental branch that traverses the
search tree breadth-first, rather than depth-first. For certain otherwise
difficult graphs with small automorphism groups this can make the algorithm run
considerably faster (at the expense of high memory complexity). However it
performs poorly on graphs with large automorphism groups such as empty or
clique graphs.

Given:

1. an input file path in the _bliss_ (DIMACS textual graph file format,
   described in http://www.tcs.hut.fi/Software/bliss/fileformat.shtml) as the
   first command line argument, and
2. an output file path in a custom JSON format (see
   src/Data/Graph/Invariant/Output.hs) as the second,

the program computes several properties of the input graph:

- So-called _canonical invariant coloring_: Each vertex is assigned an unique
  integer such that this labeling is independent of a graph representation.
  Therefore if two graphs are isomorphic:

  * They have the same canonical invariant coloring (up to rearrangement of
    vertices).
  * The isomorphism can be easily determined from the coloring: Each vertex is
    mapped to the only one with the same color integer.

  If two graphs are non-isomorphic, it's very  unlikely they'll have the same
  canonical invariant coloring. And if they have, it's enough to verify whether
  the mapping of vertices induced by the coloring is an isomorphism or not.

  However in some rare cases the invariant coloring can't be made unique.
  Then the output contains all possible (non-automorphic) colorings.
- A total _invariant number_ of the whole graph, which is computed from the
  canonical invariant coloring of its vertices. If two graphs are isomorphic,
  they have the same invariant number. If they are non-isomorphic, it's very
  unlikely they'll have the same invariant number.
- The [_automorphism group_](https://en.wikipedia.org/wiki/Automorphism_group)
  of the graph represented as a set of generators of the [permutation
  group](https://en.wikipedia.org/wiki/Permutation_group) on {0,…,n-1}.

## Dependencies

- lib{[lapack](https://en.wikipedia.org/wiki/LAPACK),[blas](https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms)}-dev
  for [hmatrix](https://hackage.haskell.org/package/hmatrix).

## Contributions and future plans

Please see [Code of Conduct](docs/code-of-conduct.md) and
[Contributing](docs/contributing.md).

- Use [accelerate](https://hackage.haskell.org/package/accelerate) do perform
  matrix multiplications on a GPU. This could speed up the algorithm
  considerably.
