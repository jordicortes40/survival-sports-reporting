# Methodological Quality and Reporting of Time-to-event Studies in Sports Science: A Scoping Review

This repository contains the data and analysis code for the manuscript:

**Cortés, J., Nielsen, R. Ø., & Casals, M. (2025). Methodological Quality and Reporting of Time-to-event Studies in Sports Science: A Scoping Review.** *(Submitted to BMJ Open Sport & Exercise Medicine)*

## Authors

This dataset was compiled and curated by:

- **Jordi Cortés Martínez** – [@jordicortes40](https://github.com/jordicortes40)  

  Affiliation: Universitat Politècnica de Catalunya - BarcelonaTech (UPC)

  [![ORCID iD](https://img.shields.io/badge/ORCID-0000--0002--3764--0795-a6ce39?logo=orcid&logoColor=white&style=flat-square)](https://orcid.org/0000-0002-3764-0795)

  Email: jordi.cortes-martinez@upc.edu

- **Rasmus Østergaard Nielsen** 

  Affiliation: Aarhus University

  [![ORCID iD](https://img.shields.io/badge/ORCID-0000--0001--5757--1806-a6ce39?logo=orcid&logoColor=white&style=flat-square)](https://orcid.org/0000-0001-5757-1806)

  Email: roen@ph.au.dk
  

- **Martí Casals Toquero** – [@marticasals](https://github.com/marticasals)

  Affiliation: Universitat de Barcelona (UB)

  [![ORCID iD](https://img.shields.io/badge/ORCID-0000--0002--1775--8331-a6ce39?logo=orcid&logoColor=white&style=flat-square)](https://orcid.org/0000-0002-1775-8331)

  Email: marticasals@gmail.com


---

If you use this dataset, please cite the authors accordingly.


## 📌 Project Summary

This scoping review evaluates the methodological quality and reporting practices 
of time-to-event (survival) analyses in sports science research. We reviewed 138 
peer-reviewed articles published between 2013 and 2023, using the SAMPL 
(Statistical Analyses and Methods in the Published Literature) guidelines as a 
reference framework.

The aim is to highlight common reporting shortcomings and provide evidence-based 
recommendations to improve transparency, reproducibility, and methodological 
rigor in the application of survival models in sports science.

---

## 📁 Repository Structure

```text
survival-sports-reporting/
├── Data/                        # Data files used for the analysis
│   ├── database_survival_sports_reporting.csv
│   └── SAMPL_table.txt
│
├── Scripts/                     # R scripts for processing and analysis
│   ├── script_review_survival_sports.R
│   ├── parameters_review_survival_sport.R
│   └── functions_review_survival_sport.R
│
└── README.md                   # Description and usage instructions
```


## 🔧 Requirements

To reproduce the analysis, you need the following software and R packages installed:

### Software

- **R** version ≥ 4.2.0  

### R Packages

The following R packages are required:

- `readxl`
- `dplyr`
- `tidyr`
- `stringr`
- `ggplot2`
- `purrr`

You can install them by running the following command in R:

```r
install.packages(c("readxl", "dplyr", "tidyr", "stringr", "ggplot2", "purrr"))
```

## 📄 License

This repository is licensed under the **MIT License**.

You are free to use, modify, and distribute this work, provided that proper credit is given to the original authors. 
