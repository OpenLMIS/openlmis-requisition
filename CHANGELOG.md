5.0.0 / 2017-05-08
==================

Compatibility breaking changes:

* [OLMIS-2107](https://openlmis.atlassian.net/browse/OLMIS-2107): Add breadcrumbs to top of page navigation
  * Main state has been added to the whole application and thus interceptors had to be modified to redirect to the correct states

New functionality added in a backwards-compatible manner:

* [OLMIS-2066](https://openlmis.atlassian.net/browse/OLMIS-2066): Profile and logout are confusing
  * Logout button has been moved to the navigation bar.

Bug fixes and performance improvements which are backwards-compatible:

* [OLMIS-2267](https://openlmis.atlassian.net/browse/OLMIS-2267): Email optional for user setup
* [OLMIS-2204](https://openlmis.atlassian.net/browse/OLMIS-2204): The administration menu item should always be the last menu item

Dev and tooling updates made in a backwards-compatible manner:

* [OLMIS-1853](https://openlmis.atlassian.net/browse/OLMIS-1853): Separate push and pull Transifex tasks in build
* [OLMIS-1609](https://openlmis.atlassian.net/browse/OLMIS-1609): UI i18N message strings are not standardized
