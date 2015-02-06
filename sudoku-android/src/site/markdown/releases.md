# Releases 

## Sudoku Capturer Release 1.5 (not yet published)

- Fixed a bug in the number detection: the dection of empty cells was not taken into account in the
algorithm for weighting the number detections - this was reason for many wrongly detected numbers in
the recognition phase. Now the algorithm also weights empty cells which rendered the algorithm to be
more stable than before.
- Introduced Preferences Dialog

## Sudoku Capturer Release 1.4 (15. May 2014)

- use color coded rectangles around recognized cells in order to improve visual feedback (good for slow devices)
- implemented back button
- change detection algorithm from a single frame strategy to an incremental approach