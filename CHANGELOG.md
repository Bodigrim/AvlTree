# 4.3

* Remove previously deprecated functions.
* Class `Typeable1` is no longer a thing.

# 4.2

* A lot of function renaming (old names still available but deprecated).
* Gather all deprecations in 1 new module: `Data.Tree.AVL.Deprecated`.
* Added `findEmptyPath`, `nub`, `nubBy`.

# 4.1

* Added missing strictness to `genVenn`, `genVennMaybe`.

# 4.0

* Changed to derived `Read` / `Show` instances (instead of via lists).
  Hence the instances are incompatible with earlier versions.
* Added:
  * `genDisjointUnion`,`testGenDisjointUnion`,
  * `genVenn`,`testGenVenn`,
  * `genVennMaybe`,`testGenVennMaybe`,
  * `genVennToList`,
  * `genVennAsList`,
  * `genVennMaybeToList`,
  * `genVennMaybeAsList`.
* Added `UBT6` cpp macro to `ghcdefs.h` / `h98defs.h`.

# 3.2

* No code changes, just reclaiming ownership and bumping version No.

# 3.1

* Exposed `BinPath` primitives.
* Removed AVL tree based sorts.
* Removed `Data.Map` / `Data.Set` conversions. This eliminates the `containers` package dependency.
* Removed link to Haskell wiki homepage as this will never be done.
* Removed link to maintainer email.

# 3.0

* Included `MasterTable.txt` in the distro.
* `Eq` and `Ord` Instances now based on strict structural equality (derived).
* Exposed height-related functions.

# 2.4

* Initial Hackage/Cabal release.
  Version set to 2.4 to distinguish from the 2.3 (non-cabal) release on my home page.
