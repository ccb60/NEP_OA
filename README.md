# NEP_OA

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

Preliminary analysis of Ocean Acidification Data From Multiple National Estuary Programs

Casco Bay Estuary Partnership (CBEP) staff was involved with a project to look
at carbonate chemistry parameters from several National Estuaries,including
CBEP.  THe project we being led principally by Nick Rosenau, of the U.S.
Environmental Protection Agency, in Washington D.C.  The effort has produuced 
a draft scientific paper, which will be submitted for publication soon
(fall 2020).

The files included in this Github repository were part of CBEP's effort to
explore analytic methods and graphics for the scientific paper. 

These files were all "preliminary" in the sense that they were part of our
efforts to improve our understanding of carbonate chemistry, explore analytic
methods, or look at  ideas for graphic presentation of results. None of the code
included here was used to produce final graphics or figures for the paper.

# Statement of Purpose
CBEP is committed to the ideal of open science.  Our data archives ensure our
science is documented and reproducible by others. The purpose of these archives
is to release raw data and data analysis code whenever possible to allow others
to review, critique, learn from, and build upon CBEP science.

# Archive Structure
 CBEP data analysis repositories are usually divided into from two to four
 sub-folders.

- Original Data.  Original data, with a "DATA_SOURCES.md" or "READ ME.txt" file
that documents data sources. *DATA IN THIS FOLDER IS AS ORIGINALLY PROVIDED.* 

- **Derived Data**.  Data derived from the original raw data.  Includes R
notebooks that embody data transformation and reorganization, along with CSV
files containing data used elsewhere in the repository.

- **Complete NEP Data**.  Contains data from all NEPs included in the study, as well
as code to separate ehat file into smaller files, and a long R Markdown
file documenting our current understanding of temperature dependence of pCO~2~.
The R Markdown document produced a PDF summary, also stored here.

- **Analysis**.  Contains R Notebooks proceeding through one approach to analysis
of pCO~2~ data using generalized additive mixed models (GAMMs).  GAMMs are well
suited to this problem, as they easily integrate non-linear terms, and properly
address the high serial autocorrelation common in data from automated sensors.
FIles presented here walk through a simplified GAMM analyses of pCO~2~ data. 
The files are a subset of analyses we conducted. Other analyses follow similar
methods, looking at differnet dependent variables (temperature corrected pCO~2~
and pH) and more complex models incorporating a richer set of nonlinear and 
interaction terms.  Additional analysis examples can be found in our CBEP_OA
Github repository.

-  **Analysis_Daily**.  Contains files related to analysis of, or preparation of
graphics from, daily summaries of hourly pCO~2~ and pH data.  Analysis and
graphics focus on diurnal range and daily medians.

# Summary of Data Sources
Data was either derived from CBEP OA data analyzed as part of our 2020 State of
the BAy report, or e-mailed to Curtis C. Bohlen by Nick Rosenau of the U.S.
Environmental Protection Agency (EPA), who was coordinating an effort to
compare carbonate chemistry in National Estuary Waters.

Data sets were gathered through projects coordinated by, and often funded by,
National Estuary Programs around the U.S. Data was assembles by staff at U.S.
EPA.  CBEP did not produce the data, but participated in efforts to consolidate
data analysis and review. CBEP was not the producer of the data, cannot vouch
for its accuracy,  does not hold copyright, and cannot grant useage rights to
others.

Data is provided here on an "AS IS" basis for study only. Data was provided to
CBEP on a preliminary basis as part of collaborative research.  Users are
cautioned that data may not have gone through complete QA/QC processes and may 
contain inaccurate information.  

