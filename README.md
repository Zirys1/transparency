# transparency_submission

1.	„0 results_raw“ are imported in R with the script „1 Data import“. This produces an excel file named „2 results_cleaned“ and a R Workspace file named „2 results_cleaned“
2.	With the R script „3 Data Analysis“ all data analyses except regression related that have been conducted in the manuscript can be done
3.	With the Do-file “3 Data Analysis” the data preparation, as well as all regression related analyses from the manuscript can be done. This script also produces a Stata dataset file named “4 results_prepared”

Attention: In all scripts, the respective file locations for import (in R: read.xlsx; in Stata: import) and export (in R: save.image & write.xlsx; in Stata: save) need to be edited in order to fit the respective locations where the files are stored on the computer.
