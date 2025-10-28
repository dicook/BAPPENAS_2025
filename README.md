# Working with big data: tidying, wrangling, visualising, modelling repoducibly and collaboratively

**Presenter**: [Dianne Cook](https://www.dicook.org), a Professor of 
Statistics at Monash University in Melbourne, Australia, is a global leader
in data visualisation. She has delivered over 100 invited talks 
internationally and published extensively on various aspects of data 
visualisation. Dr. Cook is a Fellow of the American Statistical 
Association, an elected member of the International Statistical 
Institute, past editor of the Journal of Computational and Graphical 
Statistics, and the R Journal. She has served as a Board Member of the 
R Foundation and is currently the co-chair of the Statistical Computing 
and Visualisation Section of the [Statistical Society of Australia](https://www.statsoc.org.au).

The workshop will use a hands-on teaching methodology that combines short lectures with 
longer practice sessions. Whether you decide to follow along or not, you will be able to reproduce 
all the slides and examples, later. 

We assume some familiarity with R, and will teach using R and Rstudio.

## Outline

| Session | topic | description |
|------:|:-------|:-------|
| 1  | [Introduction, tidy data analysis and workflow](slides1.html) | This session covers key principles for handling large, complex datasets and tools, e.g. identifying data as in tidy format, or not, data cleaning and integration and how to build a robust workflow, that enables collaborative data analysis and reproducible reporting. |
| 2  | Data wrangling, and visualisation | From data in tidy form to elegant graphics, using a grammar to make good visualisations, and identifying patterns and communicating evidence.|
| 3  | Statistical modeling and machine learning | Extending the notion of tidy data to models provides a cleaner way to think about the process of prediction and interpretation. |
| 4  | Application 1 | Working with Indonesian economic data. |
| 5  | Interactive and dynamic graphics | Here we learn how to visualise data beyond two dimensions, to assess relationships between many variables. |
| 6  | Application 2 | Working with interactive plots for spatial and temporal data about Indonesia. |

## Getting started

To follow along with the intructor, you should have a reasonably up to date version of [R](https://cran.r-project.org) and [R Studio](https://posit.co/download/rstudio-desktop/), and these R packages:

```
install.packages(c("ggplot2", 
                   "tidyr", 
                   "dplyr", 
                   "readr", 
                   "readxl",
                   "stringr", 
                   "forcats",
                   "colorspace", 
                   "patchwork",
                   "broom", 
                   "ggbeeswarm", 
                   "ggmosaic",
                   "nullabor", 
                   "gapminder",
                   "ggthemes",
                   "conflicted"), 
                   dependencies=c("Depends", "Imports"))
```

## Resources

- Wickham, Cetinskaya-Rundell, Grolemund (2023) [R for Data Science (2e)](https://r4ds.hadley.nz)
- [tidymodels website](https://www.tidymodels.org)
- [Wilke (2019) Fundamentals of Data Visualization](https://clauswilke.com/dataviz/)
- [Happy Git and GitHub for the useR](https://happygitwithr.com)
- [The Turing Way](https://book.the-turing-way.org/)

Copyright: Dianne Cook 2025

These materials are licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License][cc-by-nc-sa].

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg

