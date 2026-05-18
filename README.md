# Text Analysis and NLP for Public Policy

Coursework repository for **DOPP 5688 — Text Analysis and NLP for Public Policy
(Spring 2026)**.

The repository is organized around homework submissions first. Research proposal
and final paper materials are intentionally out of scope for now.

## Current Priorities

| Item | Status | Local file |
|------|--------|------------|
| Assignment 1 | Submitted | `Homework_1_Huang.R` |
| Assignment 2 | Submitted | `Homework_2_Huang.R` |
| Assignment 3 | In progress | `Assignment_3_Huang.R` |
| Assignment 4 | Drafted | `austria_wordfish_assignment.R` |

## Repository Structure

```text
.
├── Data/                         # Course data and local inputs
├── Scripts/                      # Instructor scripts and completed class code
├── Slides/                       # Course slides
├── Homework_1_Huang.R            # Submitted Assignment 1
├── Homework_2_Huang.R            # Submitted Assignment 2
├── Assignment_3_Huang.R          # Working submission script for Assignment 3
├── DOPP5688_03_03_homework.R     # Original Assignment 3 template
├── austria_wordfish_assignment.R # Working draft for Assignment 4
├── PROGRESS.md                   # Working log and next steps
└── NLP.Rproj                     # RStudio project file
```

## Setup

Open `NLP.Rproj` in RStudio, or run scripts from the repository root:

```r
setwd("/Users/huangduo/Desktop/NLP")
```

Required packages used across the assignments include:

```r
install.packages(c(
  "tidyverse", "manifestoR", "quanteda", "quanteda.textstats",
  "quanteda.textplots", "quanteda.textmodels", "stopwords", "tidytext"
))
```

The Manifesto Project API key is stored locally as `Data/apikey.txt`. It is
excluded from git.

## Notes

- `.gitignore` excludes local R session files and the Manifesto Project API key.
- Assignment scripts are kept at the repository root so they are easy to submit
  directly to Moodle.
- Instructor-provided class scripts remain in `Scripts/` for reference.
