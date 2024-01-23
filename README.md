## Git repository for ALSPAC individual differences and climate beliefs/behaviours analysis code (B4293)

This repository contains one Stata .do file ("B4293_IndivDiffsAndClimate_AnalysisCode.do") which
reads in the raw data, processes the variables for analysis, and then conducts analyses, stores
relevant output and generates plots. The 'B4293_SyntheticDataScript.R' script creates synthetic 
data using the 'synthpop' package.

The 'SyntheticData' folder contains a synthetic version of the ALSPAC dataset in Stata (.dta) format, 
created using R above. As raw ALSPAC data cannot be released, these synthesised 
datasets are modelled on the original ALSPAC data, thus maintaining variable distributions and relations 
among variables (albeit not pefectly), while at the same time preserving participant anonymity and 
confidentiality. Please note that while these synthetic datasets can be used to follow the analysis 
scripts, as data are simulated they should not be used for research purposes; only the actual, observed, 
ALSPAC data should be used for formal research and analyses reported in published work.

Note that ALSPAC data access is through a system of managed open access. Information
about access to ALSPAC data is given on the ALSPAC website 
(http://www.bristol.ac.uk/alspac/researchers/access/). These datasets used in these
scripts are linked to ALSPAC project number B4293; if you are interested in accessing
these datasets, please quote this number during your application.
