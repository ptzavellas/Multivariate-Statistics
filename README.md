# Multivariate Statistical Analysis – PCA, Factor Analysis & Classification

This repository contains two comprehensive assignments developed as part of the MSc in Biostatistics & Health Data Science. The projects demonstrate advanced multivariate statistical modeling, dimensionality reduction, classification methods, and diagnostic testing using R.

---

# 📘 Assignment 1 – PCA, Factor Analysis & Logistic Regression

## Objective

To investigate dietary consumption patterns using multivariate techniques and assess their association with mortality outcomes.

---

## Dataset

Weekly consumption frequencies of:

- Legumes
- Vegetables
- Salads
- Meat
- Chicken
- Fish

Outcome variable:
- In-hospital mortality (binary)

---

## Statistical Workflow

### 1. Descriptive & Correlation Analysis
- Univariate summaries (Mean, SD, Median, IQR)
- Correlation matrix
- Scatterplot matrix
- Identification of dietary clustering patterns

Observed strong clustering between:
- Pescatarian-type foods (legumes, vegetables, salads, fish)
- Meat and chicken consumption

---

### 2. Factorability Testing

- Kaiser–Meyer–Olkin (KMO = 0.81)
- Bartlett’s test of sphericity

Conclusion: Data suitable for factor analysis.

---

### 3. Principal Component & Factor Analysis

- PCA with 2 components (Kaiser criterion: eigenvalues > 1)
- Varimax (orthogonal) rotation
- Promax & Oblimin (oblique) rotation
- Communality estimation
- Scree plot and explained variance (≈93% cumulative variance)

Interpretation:
- Component 1 → Pescatarian dietary pattern
- Component 2 → Carnivore dietary pattern

Promax rotation selected for realistic correlated factors.

---

### 4. Logistic Regression on Factor Scores

Model:
death ~ RC1 + RC2 (logistic regression)

Results:
- Pescatarian component: OR ≈ 0.85 (protective)
- Carnivore component: OR ≈ 1.25 (increased mortality risk)

Both statistically significant (p < 0.001).

---

### 5. Maximum Likelihood Factor Model

- Compared PCA-based solution to ML factor analysis
- Normality assumption evaluated
- Concluded PCA solution more appropriate due to distributional deviations

---

# 📗 Assignment 2 – Discriminant Analysis & Bayesian Classification

## Objective

To classify individuals into two populations (carriers vs non-carriers) using multivariate classification methods.

Variables:
- X1: log(AHF activity)
- X2: log(AHF antigen)

---

## Part A – Fisher Linear Discriminant Analysis (LDA)

### Steps

- Descriptive analysis by group
- Mean vector comparison
- Pooled covariance matrix estimation
- Fisher discriminant function:

LD = 9.0347*X1 + 8.0039*X2

### Performance

Confusion matrix:
Misclassification rate ≈ 14.67%

New observations classified using learned decision rule.

---

## Assumption Checking

- Shapiro–Wilk tests (marginal normality)
- Mardia test (multivariate normality)

Conclusion: Bivariate normality assumption satisfied.

---

## Part B – Bayesian Classification

- Implemented Bayes classifier using prior probabilities
- Compared posterior densities:
  f1(x) / f2(x)

Observed:
- Prior probabilities influence classification boundary
- Increased overall misclassification (≈24%) compared to Fisher LDA
- Bayesian rule favored majority class

New observations classified consistently with Fisher results.

---

# 🧠 Methods Demonstrated

- Multivariate normal simulation
- Covariance & correlation transformations
- PCA & Factor Analysis
- Varimax & Promax rotations
- Logistic regression modeling
- Fisher Linear Discriminant Analysis
- Bayesian classification
- Confusion matrix evaluation
- Multivariate normality diagnostics (Mardia test)
- Visualization of decision boundaries

---

# 🛠 Tools Used

- R
- psych
- MASS
- MVN
- klaR
- mvtnorm
- ggplot2
- readxl

---

# 📊 Key Learning Outcomes

- Interpretation of correlated dietary patterns
- Dimensionality reduction and latent structure modeling
- Linking latent components to clinical outcomes
- Linear vs Bayesian classification trade-offs
- Impact of prior probabilities on classification
- Importance of distributional assumptions in multivariate modeling
