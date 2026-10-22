# OASIS Brain Aging & Dementia Explorer (`pips-oasis-project`)

# OASIS Brain Aging & Dementia Explorer (pips-oasis-project)

This repository contains an interactive R Shiny dashboard and statistical analysis pipeline investigating age-related brain volume decline and cognitive impairment across the adult lifespan. We created this project for the Programming in Psychological Science (PIPS) course within the Research Master in Psychology at the University of Amsterdam.

Using cross-sectional MRI and clinical data from the Open Access Series of Imaging Studies (OASIS) (Marcus et al., 2007), this project allows users to:
1. Clean and preprocess clinical neuroimaging metadata (handling longitudinal reliability scans and missing clinical ratings).
2. Conduct inferential statistical analyses (multiple linear regression and group interaction models) testing how age and Clinical Dementia Rating (CDR) relate to normalized whole-brain volume (nWBV) and cognitive status (MMSE).
3. Interactively explore bivariate associations, demographic filters, and subgroup regressions in a custom bslib + plotly Shiny application.

## Getting Started

### Prerequisites & Dependencies
* **Software:** R (version 4.2 or newer) and RStudio.
* **R Packages:** `shiny`, `bslib`, `plotly`, `rstatix`, `dplyr`, `readxl`, `knitr`, and `rmarkdown`.

### Instructions for Use
1. **Clone the repository to your local machine:**
   ```bash
   git clone [https://github.com/](https://github.com/)<your-username>/<your-repo-name>.git
   cd <your-repo-name>
