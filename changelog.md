# Changelog

## v0.5.2.0

- Fix #12: Compilation error with base 4.7

## v0.5.0.0

- Upgrade to Servant 0.9. User-facing API should remain unchanged.

## v0.4.0.0

- Upgrade to Servant 0.6. When running the Giphy monad it is now possible
  to provide a custom HTTP manager.

## v0.3.0.0

- Upgrade to Servant 0.5. No changes in the API, but it's not in Stackage yet
  (as of 2016-03-30).

## v0.2.5.0

- Clarify servant bounds.

## v0.2.4.0

- Include extra source files for stackage test suite.

## v0.2.3.0

- Losen upper bounds for Stackage inclusion (aeson 0.11 compat).

## v0.2.2.0

- Retrieve single GIFs via GifId.
- Another attempt in reducing the deps on Hackage.

## v0.2.1.0

- Expose RandomResponse(..) type for non-lens use.

## v0.2.0.0

- Make sample app optional via build flag `buildSample`. This dramatically
  reduces the dependency graph.

## v0.1.0.0

- Initial release.
