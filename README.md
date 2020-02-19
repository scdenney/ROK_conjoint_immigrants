# ROK_conjoint_immigrants
Replication materials for “Who Should be Admitted? Conjoint Analysis of South Korean Attitudes toward Immigrants”

There are three data files and two recplication code files.

Data files:
- "rep_data_skconjoint.csv" is the main data file and contains the data used for the main effects model and other models.
- "rep_data2_skconjoint_ran.csv" is from a robustness check and contains data used from the conjoint where the question order was fully randomized (see section D in the supplementary information document).
- "rep_data3_LDA.csv" is the answers from the manipulation check -- respondents asked to provide reasons for why they chose an immigrant profile. It contains both the original Korean and the translated English (by Steven Denney).

.R code:

- "rep_estimates+figures.R" contains the .R code necessary to replicate the OLS estimates and figures in the manuscript and SI document.
- "rep_LDA.R" contains .R code necessary to replicate the Latent Dirichlet allocation (LDA) analysis (section D of the SI document).
