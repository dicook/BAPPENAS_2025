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
| 1  | Tidy data | This session covers identifying data as in tidy format, and how to wrangle it into tidy form if not. |
| 2  | Grammatically specifying data plots | Here we learn how to script plots of data using a grammar to make good visualisations.|
| 3  | Tidy models | Extending the notion of tidy data to models provides a cleaner way to think about the process of prediction and interpretation. |
| 4  | Statistical inference for data visualisation | We often make decisions on the basis of what is seen in plots. Here we describe the process of of defining plots using a grammar helps to build statistical inference into interoreting data visualisations. |
| 5  | Interactive and dynamic graphics | Here we learn how to visualise data beyond two dimensions, to assess relationships between many variables. |
| 6  | Workflow | How do you build a robust workflow, that enables collaborative data analysis and reproducible reporting. |

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

Copyright: Dianne Cook 2025

These materials are licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License][cc-by-nc-sa].

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg

