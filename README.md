# vensim2R
Set of functions to parse Vensim System Dynamic models to R.

Current version works with PLE and adjustments to the document export. Also, if a stock is set with a varaible for an initial condition, this will have to be fixed after the parse to run the produced equations. 

This work was motivated by Jim Duggan's work (https://github.com/JimDuggan/SDMR/tree/master/reader). I was unable to get his files to work, so I rewrote them with more conventional and tidyverse syntax. 
