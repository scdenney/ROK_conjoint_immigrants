# ROK_conjoint_immigrants
Replication materials for “Who Should be Admitted? Conjoint Analysis of South Korean Attitudes toward Immigrants," a paper by Steven Denney (University of Toronto) and Christopher Green (Leiden University).

Source and citation:
>Denney, S., & Green, C. (2020). Who should be admitted? Conjoint analysis of South Korean attitudes toward immigrants. _Ethnicities_. https://doi.org/10.1177/1468796820916609


Abstract:

>South Korea is slowly but steadily becoming a country of immigrants. In 1998, there were barely 300,000 foreign residents in South Korea. As of 2018, there were more than 2.3 million. The immigrant population has yet to reach five percent of the total population, but it is predicted to rise significantly in the years to come. Despite the increase in newcomers, it is not well understood who native South Koreans prefer as immigrants and why. Are immigrant attitudes motivated by co-ethnic solidarity, or are they primarily based on economic and sociotropic concerns? To isolate attitudes on these crucial questions, this research uses a conjoint experiment that simultaneously tests the influence of seven immigrant attributes in generating support for admission. Our results show that broad sociotropic concerns largely drive attitudes towards immigrants in South Korea, but an immigrant’s origin also matters. Prospective newcomers from culturally similar and higher-status countries who can speak Korean and have clear plans to work are most preferred. The research findings will be relevant to the comparative study of immigration attitudes, as well as to researchers interested in the specifics of the South Korean case.

There are three data files and two replication code files.

Data files:
- "rep_data_skconjoint.csv" is the main data file and contains the data used for the main effects model and other models.
- "rep_data2_skconjoint_ran.csv" is from a robustness check and contains data used from the conjoint where the question order was fully randomized (see section D in the supplementary information document).
- "rep_data3_LDA.csv" is the answers from the manipulation check -- respondents asked to provide reasons for why they chose an immigrant profile. It contains both the original Korean and the translated English (by Steven Denney).

.R code:

- "rep_estimates+figures.R" contains the .R code necessary to replicate the OLS estimates and figures in the manuscript and SI document.
- "rep_LDA.R" contains .R code necessary to replicate the Latent Dirichlet allocation (LDA) analysis (section D of the SI document).
