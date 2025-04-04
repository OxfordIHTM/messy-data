
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Working with real world data

<!-- badges: start -->

<!-- badges: end -->

This repository contains data and scripts in support of discussions on
data structure, data format, and data tools and processes for the
University of Oxford MSc in International Health and Tropical Medicine.

## Tidy data

**Tidy data** is a standard way of mapping the meaning of a dataset to
its structure. A dataset is messy or tidy depending on how rows, columns
and tables are matched up with observations, variables and types. In
tidy data:

1.  Each variable forms a column.
2.  Each observation forms a row.
3.  Each type of observational unit forms a table.

## Tidying up messy datasets

Real datasets can, and often do, violate the three precepts of tidy data
in almost every way imaginable. While occasionally you do get a dataset
that you can start analysing immediately, this is the exception, not the
rule. The five most common problems with messy datasets, along with
their remedies:

  - Column headers are values, not variable names
  - Multiple variables are stored in one column
  - Variables are stored in both rows and columns
  - Multiple types of observational units are stored in the same table
  - A single observational unit is stored in multiple tables

The following messy datasets are available from this repository:

### Tropical cyclones of the Philippines

This dataset is found at `data/cyclones.xlsx`.

This is a dataset that has been used for teaching in class previously.
In the `XLSX` file, there are 6 worksheets with the first being the
original *long* format dataset provided to the class. To use as an
example in this exercise, this dataset has been split into 5 worksheets
for each of the year that tropical cyclones data is avaiable for (from
2017 to 2021). This is a good example of a dataset with *a single
observational unit stored in multiple tables*.

The R script `data_cleaning_cyclones.R` demonstrates how you can
re-organise this dataset to meet *tidy* criteria.

### Survey of IHTM 2024-2025 students

This dataset is found at `data/ihtm_2025.xlsx`.

This is a dataset that was created through a pretend survey exercise
among the IHTM 2024-2025 students during one of the classes. Each
student present on the day were asked to fill in their responses via an
online `XLSX` spreadsheet. The purpose of the exercise was to
demonstrate common data entry errors that can happen when using a
spreadsheet as a data colletion tool.

The R script `data_cleaning_ihtm.R` demonstrates how this dataset can be
cleaned.

### Occupational health services for 2021

This dataset is found at `data/occupational_health.xlsx`.

This is a dataset from actual Ministry of Health records of a particular
country for records of occupational health services availed by the
population for each month of 2021. This is a good example of a dataset
with *column headers as values rather than variable names* and with
*multiple variables stored in one column*.

The R script `data_cleaning_oh.R` demonstrates how you can re-organise
this dataset to meet *tidy* criteria.

### Population and number of deaths by sex and age in years from 2011 to 2021

This dataset is found at `data/pop_death.xlsx`.

This is a dataset from actual Ministry of Health information system of a
particular country for population and number of deaths by sex and age in
years from 2011 to 2021 used to calculate lifetables for the country.
This is a good example of a dataset with *column headers as values
rather than variable names*, with *multiple variables stored in one
column*, and with *a single observational unit stored in multiple
tables*.

The R script `data_cleaning_pop_death.R` demonstrates how this dataset
can be cleaned.

### Vaccine study of school-going adolescents

This dataset is found at `data/vaccine.xlsx`.

This is a dataset from an actual Ministry of Health vaccine study of
school going adolescents in a particular country to test the efficacy of
COVID vaccine. This is a good example of a dataset with *multiple
variables stored in one column*.

The R script `data_cleaning_vaccine.R` demonstrates how this dataset can
be cleaned.

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->

<!-- prettier-ignore-start -->

<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the
[`allcontributors` package](https://github.com/ropensci/allcontributors)
following the [all-contributors](https://allcontributors.org)
specification. Contributions of any kind are welcome\!

<table>

<tr>

<td align="center">

<a href="https://github.com/ernestguevarra">
<img src="https://avatars.githubusercontent.com/u/5742010?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/OxfordIHTM/messy-data/commits?author=ernestguevarra">ernestguevarra</a>

</td>

</tr>

</table>

<!-- markdownlint-enable -->

<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

## References

Wickham, H. . (2014). Tidy Data. *Journal of Statistical Software*,
59(10), 1–23. <https://doi.org/10.18637/jss.v059.i10>

Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R,
Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E,
Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K,
Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.”
*Journal of Open Source Software*, 4(43), 1686.
<doi:10.21105/joss.01686>.
