# Physical Effort Scale (PES) - Japanese Version Validation Study

## Overview

This project validates the Japanese version of the Physical Effort Scale (PES), which measures psychological effort towards physical activity. 

## Study Components

### 1. Measures
- **PES Japanese Version**: Approach-avoidance tendencies toward physical effort (8 items, 2-factor structure)
- **IPAQ**: International Physical Activity Questionnaire for measuring physical activity levels
- **Motivation Scale**: Autonomous motivation, controlled motivation, and amotivation
- **Attitude Scale**: Instrumental attitudes and affective attitudes
- **Automaticity Scale**: Automaticity of physical activity behavior

### 2. Analyses
- Exploratory Factor Analysis (EFA)
- Confirmatory Factor Analysis (CFA)
- Reliability Analysis (Cronbach's α)
- Validity Testing (concurrent, convergent, and discriminant validity)
- Test-retest Reliability (ICC, κ coefficients)
- Multiple Regression Analysis

## File Structure

```
├── Analysis_main_pse_ver14_Git.r    # Main analysis script
├── datasets.csv          # Data file (to be placed separately)
└── README.md                        # This file
```

## Required Packages

```r
# Data processing and visualization
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)

# Statistical analysis
library(psych)
library(car)
library(lm.beta)
library(lme4)
library(lmerTest)

# Factor analysis and structural equation modeling
library(FactoMineR)
library(factoextra)
library(lavaan)
library(semPlot)
library(lavaanPlot)

# Reliability analysis
library(psy)
library(irr)

# Specialized analysis
library(pscl)      # Zero-inflated models
library(MASS)      # Box-Cox transformation

# Report generation
library(sjPlot)
library(knitr)
library(xtable)

# Visualization aids
library(gghalves)
library(MetBrewer)
library(corrplot)
```

## Usage

### 1. Environment Setup
```r
# Install required packages
install.packages(c("ggplot2", "tidyverse", "psych", "lavaan", "sjPlot"))

# Set working directory
setwd("your/project/directory")
```

### 2. Data Preparation
- Place `ver2_main_data_5_21.csv` in the project directory
- Check and adjust data file paths

### 3. Run Analysis
```r
# Execute the entire script
source("Analysis_main_pse_ver14_Git.r")
```

## Key Results

### 1. Factor Structure
- Confirmed 2-factor structure (approach-avoidance)
- Reliability of each factor: α > .80

### 2. Validity
- **Concurrent validity**:
- **Convergent validity**: 
- **Discriminant validity**: 

### 3. Reliability
- Test-retest reliability: 
- Internal consistency:

## Data Structure

### Primary Variables
- `q1_1` - `q1_8`: PES items (7-point Likert scale)
- `q2_a1` - `q2_a3`: IPAQ physical activity frequency
- `q2_1b_1` - `q2_4_2`: IPAQ physical activity duration
- `q3_1` - `q3_2`: Physical activity intention
- `q4_1_1` - `q4_2_3`: Attitude scales
- `q5_1` - `q5_12`: Motivation scales
- `q6_1` - `q6_4`: Automaticity scale

### Computed Variables
- `pes_approach`: PES approach score
- `pes_avoid`: PES avoidance score
- `PES_Global`: PES global score (approach - avoidance)
- `mod_vig_PA`: Moderate-to-vigorous physical activity (MET-min/week)

## Analysis Workflow

### 1. Data Preprocessing
```r
# Convert variables to factors
pes$sc0 <- factor(pes$sc0)
pes$sc1 <- factor(pes$sc1)
pes$sc3 <- factor(pes$sc3)

# Rename variables
names(pes)[names(pes) == "sc1"] <- "sex"
names(pes)[names(pes) == "sc2_1"] <- "Age_num"
```

### 2. IPAQ Physical Activity Calculation
```r
# Vigorous physical activity (MET-min/week)
pes$vigorous_PA <- 8 * pes$TL_Vig_AP * pes$q2_a1

# Moderate physical activity (MET-min/week)
pes$moderate_PA <- 4 * pes$TL_Mod_AP * pes$q2_a2

# Walking (MET-min/week)
pes$walking <- 3.3 * pes$TL_Walking * pes$q2_a3

# Total MVPA
pes$mod_vig_PA <- moderate_PA + vigorous_PA + walking
```

### 3. PES Score Calculation
```r
# Approach dimension
pes$pes_approach <- (effort_approach_1 + effort_approach_2 + 
                     effort_approach_3 + effort_approach_4) / 4

# Avoidance dimension
pes$pes_avoid <- (effort_avoid_1 + effort_avoid_2 + 
                  effort_avoid_3 + effort_avoid_4) / 4

# Global score
pes$PES_Global <- pes_approach - pes_avoid
```

## Statistical Analysis Details

### Factor Analysis
- **EFA**: 2-factor extraction with Promax rotation
- **CFA**: Confirmatory factor analysis using lavaan package
- **Fit indices**: CFI, TLI, RMSEA, SRMR

### Validity Testing
- **Concurrent validity**: Correlations with physical activity levels (MVPA, sitting time)
- **Convergent validity**: Correlations with autonomous motivation, affective attitudes, and intention
- **Discriminant validity**: Correlations with controlled motivation and instrumental attitudes

### Reliability Analysis
- **Internal consistency**: Cronbach's α coefficient
- **Test-retest**: Intraclass correlation coefficient (ICC), weighted κ coefficient

## Important Notes

1. **Data file**: `ver2_main_data_5_21.csv` is not included due to research ethics considerations
2. **Working directory**: Please adjust the paths in the script appropriately
3. **Packages**: Install all required packages before first execution
4. **Execution time**: Some analyses (Bootstrap, simulations) may take considerable time

## Troubleshooting

### Common Issues
1. **Package errors**: Reinstall packages using `install.packages()`
2. **Data loading errors**: Check file paths and character encoding
3. **Memory errors**: Free up memory before running analyses with large datasets

### Recommended Environment
- R version 4.0 or higher
- RStudio (recommended)
- Memory: 4GB or more


## Contact

For inquiries about this research, please contact:
- Email: [keishi.soga.b4@tohoku.ac.jp]
- Affiliation: [Tohoku University]


## Research Background

The Physical Effort Scale (PES) was developed to measure individual differences in approach and avoidance tendencies toward physical effort. This study aimed to validate the Japanese version of the PES by examining its psychometric properties and relationships with established measures of physical activity behavior and psychological constructs.

## Methodology

### Participants
The study included Japanese adults aged [20-59] years. Participants completed online questionnaires.

### Procedure
1. **Time 1**: Participants completed all measures including PES, IPAQ, motivation scales, attitude scales, and automaticity measures
2. **Time 2**: Participants completed the PES again for test-retest reliability assessment

### Statistical Approach
Analyses were conducted in R using a comprehensive approach including:
- Descriptive statistics and data visualization
- Exploratory and confirmatory factor analyses
- Reliability assessment (internal consistency and test-retest)
- Validity testing through correlation and regression analyses
- Multiple regression models controlling for age and sex

---

**Note**: Please ensure data file paths and variable names are correct before running this script. Some analyses may require extended processing time. 


## References

### Original PES Development
Cheval, B., Maltagliati, S., Courvoisier, D. S., et al. (2024). Development and validation of the physical effort scale (PES). *Psychology of Sport and Exercise*, *72*, 102607. https://doi.org/10.1016/j.psychsport.2024.102607

### French Version Validation
St-Denis, B., Beaudry, S., Boigonier, M. P., et al. (2024). Validation de la version francophone de l'échelle d'effort physique (PES). [Journal information to be completed]

## Citation

If you use this research, please cite as follows: 

```
Development and psychometric evaluation of the Japanese version of the Physical Effort Scale (preparation). 
Keishi Soga, Hiroyuki Sasai, Yasuyuki Taki, Boris Cheval, & Keita Kamijo
GitHub repository: https://github.com/KeishiSoga/PES_JP
```
