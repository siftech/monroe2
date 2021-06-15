Monroe 2 is a new version of the Monroe random plan generator
originally developed by Nate Blaylock and described in his paper (with
James Allen), "Hierarchical Goal Recognition," which appears in
Sukthankar, G., Goldman, R.P., Geib, C., Pynadath, D.V., Bui,
H.H. (Eds.), 2014. *Plan, Activity, and Intent Recognition: Theory and
Practice*. Elsevier, Amsterdam.

Some discussion of Monroe 2 may be found in my paper ["Plan Recognition for Network Analysis: Preliminary Report,"](http://rpgoldman.goldman-tribe.org/papers/pair2018.pdf) which was presented at the 2018 Plan, Activity, and Intent Recognition (PAIR) workshop.

The Monroe2 system itself is contained in the [src](src) subdirectory.  It contains the original Monroe sources (by permission of the original author, Nate Blaylock) with the revisions made by SIFT to        transform it into Monroe2.

Monroe2 is a Common Lisp program, which is built on top of the [SHOP3](https://github.com/shop-planner/shop3) open source planning system.  Monroe2 may be loaded using the [ASDF](https://common-lisp.net/project/asdf/) system definition facility.

The [MonroePlanCorpus-1.0](MonroePlanCorpus-1.0) subdirectory contains Nate's original Monroe plan recognition corpus.

We hope to make documentation available going forward, but have not had time thus far.
