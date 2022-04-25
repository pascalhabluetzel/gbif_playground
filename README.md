# gbif_playground

Some scripts that might be useful when dealing with GBIF data

## range_expansion.R

Calculates the shortest over-water distance between a new observation and all georeferenced GBIF occurrences of a list of species. Returns the shortest of all these distances.

### User guide

1. The script expects a .csv file with species names as provided in the example. This file should be in the same directory as the script.
2. Provide your GBIF credentials at the beginning of the script.
3. Run the code that saves 'range_expansion' as a function.
4. Use the long/lat parameters to locate your study site.
5. The 'o' parameter defines the polygon within which GBIF occurrences will be considered for analysis. (In the future, I will re-program this parameter to be more dynamic in order to speed up computing time for species with many occurrences.)
6. If necessary, adjust the name of the .csv file with species names. For now it is called 'species_list.csv'.

Known issues (working on it in the coming days):
- Script takes quite a while to run to calculate all over-water distances. Should be parallelized.
- Code might not work if a species name is misspelled.

## range_expansion_animation.R

Makes gifs for the reports of species on GBIF through time. Keep in mind that more points on the map might not only indicate range expansion, but possibly also more sampling effort or other biases.
