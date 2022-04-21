# gbif_playground

Some scripts that might be useful when dealing with GBIF data

## range_expansion.R

Calculates the shortest over-water distance between a new observation and all georeferenced GBIF occurrences of a list of species. Returns the shortest of all these distances.

Expects a .csv file with species names as provided in the example.

Known issues (working on it in the coming days):
- Script takes quite a while to run to calculate all over-water distances. Should be parallelized.
- Code might not work if a species name is misspelled.

## range_expansion_animation.R

Makes gifs for the reports of species on GBIF through time.
