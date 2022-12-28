# Change Log
All notable changes to this project will be documented in this file.

## 0.1.1 - 2022-12-28
### Fix
- `commodity-channel-index` divide by zero exception
## 0.1.0 - 2022-12-13
### Changed
- `simple-moving-average` now works on logarithmic time, but it assumes only latest time points are added. For adding earlier that latest time points, the complete tail (from early time point to latest) needs to be recomputed. Most of the time only latest time points arrive.

## 0.0.2 - 2022-09-30
### Fix
- docstring of `timeseries.keep-latest` to be consistent with code behaviour.

## 0.0.1

Initial release