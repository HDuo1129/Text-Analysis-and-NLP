# Text Analysis and NLP for Public Policy

Coursework repository for **DOPP 5688 — Text Analysis and NLP for Public Policy (Spring 2026)**.

---

## Final Assignment: Wordfish Scaling of Austrian Party Manifestos (2013–2019)

**File:** `austria_wordfish_assignment.R`  
**Method:** Wordfish (Day 4 — Unsupervised Spatial Scaling)  
**Data:** Austrian party manifestos from three federal elections via the Manifesto Project API (MARPOR)

### Research Question

What is the dominant dimension of ideological disagreement across Austrian party manifestos from 2013 to 2019, and how do party positions shift across elections?

### Data

Five parties across three elections (15 documents total), downloaded automatically via `manifestoR`:

| Party | Abbreviation | Elections |
|-------|-------------|-----------|
| Austrian Social Democratic Party | SPÖ | 2013, 2017, 2019 |
| Austrian People's Party | ÖVP | 2013, 2017, 2019 |
| Austrian Freedom Party | FPÖ | 2013, 2017, 2019 |
| The Greens | GRÜNE | 2013, 2017, 2019 |
| The New Austria and Liberal Forum | NEOS | 2013, 2017, 2019 |

The three elections cover a decade of significant Austrian political realignment:
- **2013**: NEOS entered parliament for the first time; SPÖ–ÖVP grand coalition continued
- **2017**: Sebastian Kurz led ÖVP to first place; formed a right-wing coalition with FPÖ
- **2019**: Post-Ibiza snap election; ÖVP formed an unprecedented coalition with the Greens

### Key Findings

**The recovered axis** is not classical economic left–right. The top discriminating words reveal a mix of GAL–TAN ideology and rhetorical style:
- **Negative end (GRÜNE-leaning):** `menschenrechte`, `gleichstellung`, `zivilgesellschaft`, `fossilen`, `klimaschutz`, `tierschutz` — progressive, rights-based, environmental vocabulary
- **Positive end (ÖVP/FPÖ-leaning):** `schulden`, `bürokratie`, `lohnnebenkosten`, `progression`, `lehrer`, `schulsystem` — economic-conservative policy agenda, plus conversational rhetorical markers (`natürlich`, `deswegen`, `nämlich`) characteristic of Kurz's campaign style

**Three main results from the drift plot:**

1. **ÖVP's dramatic shift in 2017** (θ: +0.16 → +1.65) is the most striking finding. Under Kurz, ÖVP adopted nationalist and security-focused language to such a degree that its manifesto scored higher on the axis than FPÖ itself — reflecting the well-documented strategy of absorbing FPÖ's agenda to win the election.

2. **FPÖ became relatively more centrist** (θ: +1.01 → +0.27 → +0.43). This does not mean FPÖ moderated; rather, ÖVP occupied the extreme end of the linguistic space, shifting FPÖ's relative position toward the centre. This illustrates a key limitation of unsupervised scaling: it measures *relative* position, not absolute radicalism.

3. **GRÜNE drifted further left** across all three elections (−1.47 → −1.56 → −1.91), indicating its manifesto language became increasingly distinctive and separated from all other parties.

### How to Reproduce

The script is fully self-contained (Option A submission). You need:

1. A free Manifesto Project API key — register at [manifesto-project.wzb.eu](https://manifesto-project.wzb.eu)
2. Save the key as a plain text file and update the path in `mp_setapikey()` at the top of the script
3. Install the required packages:

```r
install.packages(c("manifestoR", "quanteda", "quanteda.textmodels",
                   "quanteda.textplots", "tidyverse"))
```

Then run `austria_wordfish_assignment.R` from top to bottom. All data is downloaded automatically on first run and cached locally by `manifestoR`.

---

## Repository Structure

```
.
├── Data/                          # Local data inputs (API key excluded from git)
├── Scripts/                       # Instructor lab scripts for reference
├── austria_wordfish_assignment.R  # Final assignment (Method Application)
├── Homework_1_Huang.R             # Assignment 1: Tidyverse & Regex
├── Homework_2_Huang.R             # Assignment 2: Web scraping & word frequency
├── Assignment_3_Huang.R           # Assignment 3: Text cleaning with manifestoR
└── NLP.Rproj                      # RStudio project file
```
