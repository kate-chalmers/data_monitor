# OECD Well-being Data Monitor

This repository contains the code and documentation to build and run the **OECD Well-being Data Monitor**, an interactive dashboard for exploring:

- **Current and future well-being** across countries and indicators  
- **Gaps between groups**, such as sex, age, education, and other population breakdowns  

The project is implemented in R (Shiny) and uses OECD well-being and related statistical datasets.

---

## 1. Repository Structure

```text
.
├─ README.md                # This file
├─ renv/                    # R dependency management (if used)
├─ renv.lock
├─ global.R                 # Global objects, paths, and helper functions
├─ app.R / ui.R / server.R  # Shiny app entry point
├─ R/                       # R functions (data prep, plotting, helpers)
│   ├─ data_download.R
│   ├─ data_cleaning.R
│   ├─ indicator_builders.R
│   ├─ inequality_functions.R
│   └─ utils_*.R
├─ data_raw/                # Raw data (if stored locally)
├─ data_intermediate/       # Intermediate processed datasets
├─ data_final/              # Final datasets consumed by the app
├─ config/                  # Configuration files (e.g. indicator lists, thresholds)
│   ├─ indicators_current_future.csv
│   ├─ indicators_gaps.csv
│   └─ metadata.yml
└─ www/                     # Static assets (CSS, JS, images)
