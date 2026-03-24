# Entrepreneurship Education and Entrepreneurial Intention

This repository contains the data, scripts, supplementary materials, and manuscript source files associated with the study:

**“Entrepreneurship Education and Entrepreneurial Intention among University Students: The Mediating Roles of Entrepreneurial Self-Efficacy and Motivation”**

## Overview

This project examines whether entrepreneurship education influences entrepreneurial intention among university students in Honduras indirectly through entrepreneurial self-efficacy and entrepreneurial motivation.

The repository has been organized to support transparency and reproducibility. It includes the anonymized dataset used for the analyses, the codebook, the questionnaire in English and Spanish, the construct framework with dimensions, measurement items, and sources, the R scripts used for statistical analyses, and the LaTeX files used to generate the manuscript.

## Repository Structure

```text
Entrepreneurship_Education/
├── README.md
├── LICENSE
├── .gitignore
├── run_all.R
├── scripts/
│   ├── 00_setup.R
│   ├── 01_import_clean.R
│   ├── 02_screening.R
│   ├── 03_efa_pa.R
│   ├── 04_cfa_reliability.R
│   ├── 05_exportar_CFA.R
│   ├── 06_sem_model.R
│   └── 07_sem_figures.R
├── data/
│   ├── raw/
│   ├── processed/
│   └── codebook/
├── results/
│   ├── tables/
│   ├── figures/
│   └── models/
├── manuscript/
│   ├── Entrepreneurship_Education.tex
│   └── Bibliografia.bib
├── supplementary/
│   ├── questionnaire/
│   └── constructs/
└── docs/