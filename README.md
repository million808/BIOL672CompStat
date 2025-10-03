# BIOL672 Computational Statistics - Unit 1 Assignment

**Author:** Maximillian Balter  
**Course:** BIOL672 - Computational Statistics  
**Assignment:** Unit 1 - Statistical Analysis and Multivariate Methods  
**Date:** October 2025  

## 📋 Project Overview

This repository contains the complete Unit 1 assignment implementing 10 statistical analysis steps using R. The assignment demonstrates proficiency in both univariate and multivariate statistical methods, data visualization, and biological interpretation.

## 📁 Repository Structure

```
BIO672_COMPSTAT/
├── Unit_1/
│   ├── Maximillian_unit1_BIOL672.r           # Main script (Steps 2-6)
│   ├── trevally_multivariate_analysis.r      # Multivariate script (Steps 7-10)
│   ├── Dataset.datatab                        # JSON ANOVA input data
│   ├── trevally_multivariate_data.txt         # Giant Trevally morphology data
│   ├── [25+ output files]                    # Generated results and plots
│   └── README.md
├── Week1Workshop/                             # Workshop materials
├── desc.txt                                  # Descriptive statistics output
├── histo.pdf                                 # Histogram plot
└── random_numbers.txt                        # Generated random data
```

## 🔬 Script Descriptions

### Script 1: `Maximillian_unit1_BIOL672.r` (Steps 2-6)

**Purpose:** Fundamental statistical analysis methods  
**Lines of Code:** ~190 functional lines  
**Packages Used:** ggplot2, jsonlite, dplyr

#### Features:
- **Random Number Generation:** 5000 numbers from normal distribution
- **Descriptive Statistics:** Mean, SD, histogram with density overlay
- **One-way ANOVA:** Parametric group comparisons with error bar charts
- **Pairwise Comparisons:** t-tests with Bonferroni & Benjamini-Hochberg corrections
- **Nonparametric Tests:** Kruskal-Wallis and Kolmogorov-Smirnov normality tests
- **Correlation Analysis:** Pearson and Spearman correlations with scatterplots
- **Linear Regression:** Simple regression modeling with ggplot2 visualization

#### Output Files:
- `desc.txt` - Descriptive statistics
- `histo.pdf` - Data distribution histogram
- `anova_results.txt` - ANOVA analysis results
- `anova_plot.pdf` - Error bar chart
- `pairwise_ttests.txt` - Multiple comparison results
- `kruskal_results.txt` - Nonparametric test results
- `ks_tests.txt` - Normality test results
- `correlations.txt` - Correlation analysis
- `correlation_scatterplots.pdf` - Correlation visualizations
- `linear_regression_results.txt` - Regression analysis
- `linear_regression_plot.pdf` - Regression plot
- `assumptions_and_consistency.txt` - Test assumptions and interpretations

### Script 2: `trevally_multivariate_analysis.r` (Steps 7-10)

**Purpose:** Advanced multivariate statistical analysis  
**Lines of Code:** ~195 functional lines  
**Packages Used:** ggplot2, dplyr, corrplot, tidyr

#### Features:
- **Multivariate Dataset:** Giant Trevally (Caranx ignobilis) intestinal morphology (60 observations, 5 variables)
- **MANOVA:** Multivariate analysis of variance with proper error handling
- **Multiple Regression:** Predicting morphological outcomes across and within treatment groups
- **Principal Component Analysis (PCA):** Dimensionality reduction and pattern identification
- **Composite Variables:** Biologically meaningful combinations (Surface Area Index, Length-Width Ratio)
- **ANCOVA:** Analysis of covariance using composite variables as covariates
- **Data Citation:** Proper academic referencing of source dataset (Muchlisin et al. 2020)

#### Composite Variables Created:
1. **Villi Surface Index:** `Average Length × Average Width` (functional absorption capacity)
2. **Length-Width Ratio:** `Average Length ÷ Average Width` (morphological shape characteristic)
3. **Morphological Index:** `Average Length + Average Width` (overall size measure)

#### Output Files:
- `trevally_correlation_plot.pdf` - Correlation matrix heatmap
- `trevally_length_vs_width.pdf` - Length vs width scatterplot
- `trevally_length_vs_morphindex.pdf` - Length vs morphological index plot
- `trevally_width_vs_lengthsd.pdf` - Width vs length SD plot
- `trevally_widthsd_vs_morphindex.pdf` - Width SD vs morphological index plot
- `trevally_boxplots_by_treatment.pdf` - Treatment group comparisons
- `trevally_manova_results.txt` - MANOVA analysis results
- `trevally_regression_results.txt` - Multiple regression analysis
- `trevally_ancova_results.txt` - ANCOVA analysis results
- `trevally_ancova_length_surface.pdf` - ANCOVA visualization (length vs surface area)
- `trevally_ancova_ratio_morphindex.pdf` - ANCOVA visualization (ratio vs morphological index)
- `trevally_pca_biplot.pdf` - PCA biplot visualization

## 📊 Statistical Methods Implemented

### Univariate Methods (Script 1)
- One-way ANOVA
- Pairwise t-tests with multiple comparison corrections
- Kruskal-Wallis nonparametric test
- Kolmogorov-Smirnov normality tests
- Pearson and Spearman correlations
- Simple linear regression

### Multivariate Methods (Script 2)
- Multivariate Analysis of Variance (MANOVA)
- Multiple regression analysis
- Principal Component Analysis (PCA)
- Analysis of Covariance (ANCOVA)
- Correlation matrix analysis
- Composite variable construction

## 🧬 Biological Context

The multivariate analysis focuses on Giant Trevally intestinal villi morphology, examining how different dietary treatments (activated charcoal sources) affect:
- Villi length and width measurements
- Morphological variability within individuals
- Functional surface area for nutrient absorption
- Shape characteristics related to digestive efficiency

## 🛠️ Technical Requirements

### System Requirements
- **Operating System:** macOS
- **R Version:** 4.3+ recommended
- **Required R Packages:**
  - `ggplot2` - Data visualization
  - `dplyr` - Data manipulation
  - `jsonlite` - JSON data parsing
  - `corrplot` - Correlation visualizations
  - `tidyr` - Data tidying

### Installation
```r
# Install required packages
install.packages(c("ggplot2", "dplyr", "jsonlite", "corrplot", "tidyr"))
```

### Running the Scripts
```bash
# Navigate to Unit_1 directory
cd Unit_1/

# Run main statistical analysis (Steps 2-6)
Rscript Maximillian_unit1_BIOL672.r

# Run multivariate analysis (Steps 7-10)
Rscript trevally_multivariate_analysis.r
```

## 📈 Key Results

### Script 1 Results
- Generated 5000 normally distributed random numbers (mean ≈ 0, SD ≈ 1)
- Comprehensive ANOVA analysis with significant group differences
- Strong correlations identified between variables
- Robust nonparametric confirmations of parametric results
- Linear regression with high explanatory power

### Script 2 Results  
- MANOVA analysis with proper handling of rank deficiency issues
- PCA identified major patterns: PC1 explains 69.7% of variance
- Multiple regression achieved R² = 0.9997 for morphological predictions
- ANCOVA confirmed treatment effects persist after controlling for covariates
- Composite variables provide biological insight into functional morphology

## 📖 Data Sources

**Primary Dataset:** Muchlisin, Zainal Abidin; Firdus, Firdus; Samadi, Samadi; Muhammadar, Abdullah A.; Sarong, Muhammad A.; Sari, Widya; et al. (2020). Gut and intestinal biometrics of the giant trevally, Caranx ignobilis, fed an experimental diet with difference sources of activated charcoal. figshare. Dataset. https://doi.org/10.6084/m9.figshare.12203525.v2

## 🎯 Learning Objectives Achieved

1. ✅ **Statistical Programming:** Proficient R scripting with proper documentation
2. ✅ **Data Visualization:** Professional plots using ggplot2
3. ✅ **Statistical Theory:** Understanding of parametric vs nonparametric methods
4. ✅ **Multivariate Analysis:** Implementation of advanced statistical techniques
5. ✅ **Biological Interpretation:** Connecting statistical results to biological meaning
6. ✅ **Reproducible Research:** Well-documented, version-controlled analysis pipeline
7. ✅ **Error Handling:** Robust code with proper exception management
8. ✅ **Academic Standards:** Proper citation and professional presentation

## 📞 Contact

**Maximillian Balter**  
BIOL672 Computational Statistics  
MacOS Development Environment  
GitHub: [million808/BIOL672CompStat](https://github.com/million808/BIOL672CompStat)

---

*This repository represents a comprehensive implementation of fundamental and advanced statistical methods in biological research, demonstrating proficiency in computational statistics and data analysis.*
