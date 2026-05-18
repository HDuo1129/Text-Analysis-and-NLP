# NLP Coursework Progress Log

This file tracks the homework-focused workflow for DOPP 5688. It follows the
same lightweight pattern as the GIS project: keep the repository understandable,
record decisions, and make the next submission step obvious.

## Session 1 — 2026-05-18

### Starting State

- Local folder: `/Users/huangduo/Desktop/NLP`
- GitHub remote: `https://github.com/HDuo1129/Text-Analysis-and-NLP.git`
- The local folder was not yet a git repository.
- Remote repository already had one initial commit containing only `README.md`.
- Existing submitted files:
  - `Homework_1_Huang.R`
  - `Homework_2_Huang.R`
- Relevant unfinished homework:
  - Assignment 3: original template existed as `DOPP5688_03_03_homework.R`
  - Assignment 4: working draft existed as `austria_wordfish_assignment.R`

### Git Setup

- Initialized git in `/Users/huangduo/Desktop/NLP`.
- Connected remote `origin` to `HDuo1129/Text-Analysis-and-NLP.git`.
- Pulled the existing remote `main` branch before committing local files.
- Added `.gitignore` to keep local files and secrets out of version control.

### Files Intentionally Ignored

| Pattern | Reason |
|---------|--------|
| `.DS_Store` | macOS metadata |
| `.RData`, `.Rhistory`, `.Rproj.user/` | local RStudio/session state |
| `Data/apikey.txt` | Manifesto Project API key |
| `manifesto_apikey.txt` | alternate local API key filename |
| `cache/` | generated local cache files |

### Assignment Work State

- Created `Assignment_3_Huang.R` as a working submission copy rather than
  overwriting the instructor template.
- Ran an initial validation attempt for Assignment 3 using the Manifesto
  Project API.
- Validation found script issues during development; assignment completion is
  paused because the immediate user request changed to git setup.

## Next Steps

- Commit and push the current repository state to GitHub.
- When returning to homework work:
  - finish validating `Assignment_3_Huang.R` from top to bottom;
  - decide whether Assignment 4 should be submitted as the existing Wordfish
    script or copied into a final `Assignment_4_Huang.R` file;
  - run each submission script once from a clean R session.
