fastmap 1.1.0.9001
=============

* Closed #25: Added the following functional methods to `fastmap` (#29):
    * `map()`, `map_with_key()`
    * `filter()`, `filter_with_key()`, `filter_key()`
    * `modify()`, `modify_with_key()`
    * `walk()`, `walk_with_key()`

* Changed `fastmap`'s `$has()` method to use C++ `contains()` method (which is new in hopscotch-map 2.3.0).

* Closed #24: Added a `$clone()` method to `fastmap`. (#26)

* Fixed #27: If a `fastmap` object has no holes in the lists storing keys and values, and then it is serialized and then unserialized, the new `fastmap` would contain zero items. (#28)

* Faster implementations of `fastmap` `$keys()` and `$as_list()` methods.

* Updated hopscotch-map library to 2.3.0.


fastmap 1.1.0
=============

* Added `faststack()` and `fastqueue()`. (#15)


fastmap 1.0.1
=============

* Fixed #13: fastmap.cpp now explicitly includes the algorithm header, which is needed on some platforms to successfully compile.
