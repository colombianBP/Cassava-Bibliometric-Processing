# Cassava-Bibliometric-Processing
This repo stores code used to clean and analyze bibliometric data of publications about cassava:

**1unify.R** performs unification and duplicate removal on bibliographic and deagregated wos data

**2cleanup.R** is a step by step automated aid for manual cleaning of interest variables, 'C1' and 'FU'

**scimago_Curated.ipynb** Performs enrichment of the unified database with data downloaded from sci-mago


Data download specifications: in order to succesfully use the presented scripts it is mandatory to use the same type of data and file structure in order to succesfully perform the analysis, files must be downloaded as indicated below:

Bibliographic WOS data: using Web of Science tool Clarivate you must export files using 'Plain text file' option 

Deagregated data: On the 'Citation Report' tab of the Web of science Clarivate tool, click on 'Export Full Report' and select 'Excel File' option

Sci-mago data: On the 'Journal Rankings' tab, select the desired year and download. all years downloaded must be stored in the same file


