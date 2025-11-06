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

| Session | topic | description | source |
|------:|:-------|:-------|:-------|
| 1  | [Introduction, tidy data analysis and workflow](slides1.html) | This session covers key principles for handling large, complex datasets and tools, e.g. identifying data as in tidy format, or not, data cleaning and integration and how to build a robust workflow, that enables collaborative data analysis and reproducible reporting. | [R code](slides1.R), [quarto](slides1.qmd) |
| 2  | [Data wrangling, and visualisation](slides2.html) | From data in tidy form to elegant graphics, using a grammar to make good visualisations, and identifying patterns and communicating evidence.| [R code](slides2.R), [quarto](slides2.qmd) |
| 3  | [Statistical modeling and machine learning](slides3.html) | Extending the notion of tidy data to models provides a cleaner way to think about the process of prediction and interpretation. |  [R code](slides3.R), [quarto](slides3.qmd) |
| 4  | [Application 1](slides4.html) | Working with Indonesian economic data. | [R code](slides4.R), [quarto](slides4.qmd) |
| 5  | [Interactive and dynamic graphics](slides5.html) | Here we learn how to visualise data beyond two dimensions, to assess relationships between many variables. | [R code](slides5.R), [quarto](slides5.qmd) |
| 6  | [Application 2](slides6.html) | Working with interactive plots for spatial and temporal data about Indonesia. |  [R code](slides6.R), [quarto](slides6.qmd) |

And setup code files: [libraries.R](libraries.R), [chunk_options_and_themes.R](chunk_options_and_themes.R). 

## To work on application 1

1. Make sure to have a reasonably recent version of [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/).

2. Install these R packages <br>
```
install.packages(c("ggplot2", 
                   "tidyr", 
                   "dplyr", 
                   "stringr", 
                   "tidymodels",
                   "ggbeeswarm", 
                   "nullabor", 
                   "ranger",
                   "patchwork",
                   "naniar",
                   "visdat",
                   "lubridate",
                   "ggthemes",
                   "conflicted"), 
                   dependencies=c("Depends", "Imports"))
```
<br>
3. Download the [application1.zip](application1.zip) file, `unzip` to your working directory.

## To produce reproducible documents

Install quarto by following the instructions [here](https://quarto.org/docs/get-started/).

## To work on application 2

1. Install these additional packages<br>
```
install.packages(c("readr", 
                   "plotly",
                   "sf",
                   "crosstalk",
                   "cartogram"
), 
                   dependencies=c("Depends", "Imports"))
```
<br>
2. Download the [application2.zip](application2.zip) and unzip in the same folder as for application 1.

## Resources

The end of each slide set has references and links to more information.

<br>

**Copyright: Dianne Cook 2025**

These materials are licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License][cc-by-nc-sa].

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg

