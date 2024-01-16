## Git repository for 'Religion and Climate Beliefs and Behaviours' ALSPAC project (B4123)

The main directory in this repository contains five R scripts for the actual ALSPAC analyses. These files are:
 - Script1_DataProcessingAndCleaning.R - Script to clean and process the raw ALSPAC data
 - Script2_SyntheticDatasets.R - Script to create synthetic datasets using the 'synthpop' package
 - Script3_MothersAnalysis.R - Script to analyse the G0 mothers data
 - Script4_PartnersAnalysis.R - Script to analyse the G0 partners data
 - Script5_OffspringAnalysis.R - Script to analyse the G1 offspring data

 
The 'SyntheticData' folder contains synthetic versions of the mother's, partner's and offspring's ALSPAC datasets, created
using Script 2 above. As raw ALSPAC data cannot be released, these synthesised datasets are modelled on the original 
ALSPAC data, thus maintaining variable distributions and relations among variables (albeit not pefectly), while 
at the same time preserving participant anonymity and confidentiality. Please note that while these synthetic datasets 
can be used to follow the analysis scripts, as data are simulated they should *not* be used for research purposes; 
only the actual, observed, ALSPAC data should be used for formal research and analyses reported in published work.
All files are available in R ('.RData'), CSV ('.csv') and Stata ('.dta') formats.

Note that ALSPAC data access is through a system of managed open access. Information about access to ALSPAC data is 
given on the ALSPAC website (http://www.bristol.ac.uk/alspac/researchers/access/). The datasets used in these
scripts are linked to ALSPAC project number B4123; if you are interested in accessing these datasets, please quote 
this number during your application.
