# Hierarchical Temporal Memory

[![Clojars](https://img.shields.io/clojars/v/io.helins/htm.svg)](https://clojars.org/io.helins/htm)

[![Cljdoc](https://cljdoc.org/badge/io.helins/htm)](https://cljdoc.org/d/io.helins/htm)

This project was the beginning of an implementation of [Numenta's Hierarchical Temporal Memory model](https://en.wikipedia.org/wiki/Hierarchical_temporal_memory)
in pure Clojure.

Unlike common artificial intelligence models, HTM is solidly grounded is
neuroscience and genuinely aims to mimick mechanisms encountered in the neocortex. As
opposed to techniques such as "deep learning" which from neuroscience only
steal names, such as naming a unit a "neuron", rarely principles.

This repo mostly provides math utilities related to [Sparse Distributed
Representations](https://numenta.com/neuroscience-research/sparse-distributed-representations/) as
well as basic ideas related to spatial learning. In the end, it is not much, but everything
is documented extensively with clear references to academical papers, faithfully implementing
base algorithms.

It was publicly release "just in case". It is not finised work, will probably
never be, but it can be browsed and reused.


## License

Copyright © 2018 Adam Helinski

Licensed under the term of the Mozilla Public License 2.0, see LICENSE.
