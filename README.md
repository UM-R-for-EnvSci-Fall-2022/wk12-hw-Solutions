# Week 12 - Homework <br/> Model outputs and `purrr`

Welcome to your assignment for **week 12**. As usual, **Clone** this repo into your own :computer: using RStudio, as we saw in class.

For this week's assignment you will need to create a short **"report" in an rmarkdown** format with its associated `.md` and `.html` outputs.

## The data

For this week's assignment we are going to use another dataset from [Zuur et al. (2007)](https://highstat.com/index.php/analysing-ecological-data). In this case it is data collected from nine inter-tidal areas along the Dutch coast where biological aspects (like species richness of the macro-fauna) are compared to abiotic factors like the the height of a sampling station compared to mean tidal level (denoted here as NAP)

## Your tasks

For this assignment we are going to focus on the relationship between tow variables: **species richness** (the number of different species) and **NAP** (the height of a sampling station compared to mean tidal level), or more precisely whether NAP has an influence on species richness. We will be assessing this relationship **for each** of the 9 beaches that were sampled for this study. in particular, I would like you to:

1.  Using the tools from the `purrr` package fit a linear regression of the species richness vs NAP for each of the sampled beaches
2.  Generate a summary table of the model results showing the beach as well as the slope, intercept, R-squared and the p-value of the model fit
3.  Format this summary table using the tools seen in class
4.  Using the tools from the `purrr` package generate a `.pdf` figure for each beach showing the data, the linear regression through the data, as well as the equation and R-quared of the regression

## wrap-up

In addition to being shown in the final report, the final table and figure should be saved in `.pdf` format with a width of 190 mm

Finally, once you have completed the exercises, as usual:

-   Once you are done with the R script files, save the changes, make sure scripts are properly saved in the **R** folder.
-   Commit all the changes to the *repo/R project* (remember to write a commit message!)
-   **Push** all changes back to **GitHub**
-   Go to GitHub and check that it all worked out

## Reminder

-   In the TidyTuesday assignment and Exam you will be deducted points for not following proper file structure inside your repo/project, so make sure you start developing good practices now. This applies as well to coding style, so make sure to review the [Tidyverse style guide](https://style.tidyverse.org/)

As always, feel free to use the [Homework Issues](https://github.com/orgs/UM-R-for-EnvSci-Fall-2022/discussions/categories/homework-issues) section of the [discussion](https://github.com/orgs/UM-R-for-EnvSci-Fall-2022/discussions) section to reach out to your classmates if you have any questions. Remember that this is a way to practice how to engage in something like Stack Overflow or similar. I will be monitoring it, and if you are not getting any help, I'll jump in! Remember you can always tag me or any classmate in a comment for quicker replies!

*Happy coding!*

Pepe
