# An Open Repository of Real-Time COVID-19 Indicators

This directory contains code, data, and text for "An Open Repository of
Real-Time COVID-19 Indicators". The final published version is available at
<https://doi.org/10.1073/pnas.2111452118>.

All figures in the paper are generated using `code/indicator-paper-figures.Rmd`,
a standard R Markdown file. It contains instructions on the packages required to
knit the file and produce the final figures. Note that the included data file
`code/COVID-19_Vaccinations_in_the_United_States_County.csv` was obtained [from
the 
CDC](https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh).
Data downloaded on August 2, 2021. This data is in the public domain as a United
States government work.

The LaTeX source of the paper is presented in `paper/` in PNAS's standard LaTeX
template. This can be rendered using a standard LaTeX distribution. The main
file is `paper/paper.tex`. Supplementary figures are generated using the R code
in `paper/supplement.Rmd`.
