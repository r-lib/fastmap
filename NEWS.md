fastmap 1.1.0.9000
=============

* Updated hopscotch-map library to 2.3.0.

* Changed `fastmap`'s `$has()` method to use C++ `contains()` method (which is new in hopscotch-map 2.3.0).

* Closed #24: Added a `$clone()` method to `fastmap`. (#26)

fastmap 1.1.0
=============

* Added `faststack()` and `fastqueue()`. (#15)

fastmap 1.0.1
=============

* Fixed #13: fastmap.cpp now explicitly includes the algorithm header, which is needed on some platforms to successfully compile.
