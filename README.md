# Global Sustainability Analysis

This PSTAT 131 final project predicts country-level carbon dioxide emissions from economic, social, energy, geographic, and temporal indicators in the Global Data on Sustainable Energy dataset.

## Project contents

- `Global_Sustainability_Analysis.Rmd`: complete written report
- `Global_Sustainability_Analysis.html`: knitted, self-contained report
- `code/`: modular R scripts for preparation, EDA, model setup, five model types, model comparison, and final evaluation
- `data/raw/`: original Kaggle dataset
- `data/processed/`: cleaned data and chronological training/testing sets
- `data/resamples/`: saved cross-validation and setup objects
- `data/models/`: tuned model objects and final fitted workflows
- `data/codebook.csv` and `data/codebook.txt`: complete variable codebook
- `data/results/`: summaries, tuning results, predictions, and evaluation metrics
- `graphs/`: report-ready figures

## Reproduce the analysis

Open the project folder in VS Code or RStudio and use the project root as the working directory. Run:

```r
source("code/07_run_all.R")
```

Then render the report:

```r
rmarkdown::render("Global_Sustainability_Analysis.Rmd")
```

The analysis uses `tidyverse`, `tidymodels`, `janitor`, `naniar`, `patchwork`, `glmnet`, `kknn`, `ranger`, and `xgboost`.

## Data source

Tanwar, A. (n.d.). *Global Data on Sustainable Energy (2000–2020)*. Kaggle: https://www.kaggle.com/datasets/anshtanwar/global-data-on-sustainable-energy/data
