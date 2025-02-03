# Covid-19 pandemic-related changes in teleworking, emotional exhaustion, and occupational burnout: a cross-sectional analysis of a cohort study

This repository contains code for the analysis of the Specchio-COVID19 study presented in the manuscript:  

Anshu Uppal; Nick Pullen; Hélène Baysson; Stephanie Schrempft; Aminata Rosalie Bouhet; María-Eugenia Zaballa; Julien Lamour; Mayssam Nehme; Idris Guessous; Silvia Stringhini; Elsa Lorthe; Specchio-COVID19 study group (2025). ***Covid-19 pandemic-related changes in teleworking, emotional exhaustion, and occupational burnout: a cross-sectional analysis of a cohort study.*** *BMC Public Health* 25:282  
https://doi.org/10.1186/s12889-024-21142-z

## Abstract  
#### Background  
The COVID-19 pandemic prompted significant shifts to teleworking, raising questions about potential impacts on employee wellbeing. This study examined the association between self-reported changes to teleworking frequency (relative to before the pandemic) and two indicators of occupational burnout: emotional exhaustion and professionally diagnosed burnout.

#### Methods  
Data were derived from two samples from a digital cohort study based in Geneva, Switzerland: one population-based, and one from a sample of workers who were likely mobilized in the early stages of the COVID-19 pandemic. Emotional exhaustion was measured using the Maslach Burnout Inventory (EE-MBI), while self-reported diagnosed burnout was assessed by asking participants if they had received a professional diagnosis of occupational burnout within the previous 12 months. Participants were categorized based on self-reported telework frequency changes: “no change,” “increase,” “decrease,” “never telework,” and “not possible to telework.” Adjusted regression models for each of the study samples were used to estimate associations between telework changes and burnout outcomes, accounting for sociodemographic, household, and work-related factors.

#### Results  
In the population-based sample of salaried employees (n = 1,332), the median EE-MBI score was 14 (interquartile range: 6–24), and 7.3% reported diagnosed burnout. Compared to those reporting no change in telework frequency (19% of the sample), those reporting a decrease (4%) and those reporting that teleworking was not possible (28.7%) had significantly higher emotional exhaustion scores (adjusted beta (aβ) 5.26 [95% confidence interval: 1.47, 9.04] and aβ 3.51 [0.44, 6.59], respectively) and additionally reported higher odds of diagnosed burnout (adjusted odds ratio (aOR) 10.59 [3.24, 34.57] and aOR 3.42 [1.22, 9.65], respectively). “Increased” (28.9%) and “never” (19.4%) telework statuses were not significantly associated with burnout outcomes. These trends were mirrored in the “mobilized-workers” sample, with the exception that those reporting that teleworking was not possible did not report significantly higher odds of diagnosed burnout compared to those reporting no change in telework frequency.

#### Conclusions  
Decreased teleworking frequency and not having the possibility of telework were associated with higher emotional exhaustion and diagnosed burnout. As organizations reconsider their telework policies in a post-pandemic era, they should consider the impact of such organizational changes on employee wellbeing.

## Data protection  
Data in this study is protected by the ethics committee authorization of the research project, and is available upon request to the corresponding author.

## Repository structure  
The R code to run the analysis steps are located in the "code" folder. **First run the three 01_prep files in sequence (a,b,c) to generate the datasets used for nearly all downstream analyses (these are placed into data/Generated datasets/)**  

You should also **install the `pacman` package** (package management tool) for automatic installing / loading of all packages used in these analyses.
