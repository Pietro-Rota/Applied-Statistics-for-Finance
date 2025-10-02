# Applied Statistics for Finance

A comprehensive course implementation covering advanced statistical methods and computational techniques for financial modeling and derivatives pricing.

**Professors:** Alice Pignatelli di Cerchiara & Luca Fraone  
**Institution:** Università Cattolica del Sacro Cuore  
**Languages:** R, Python  

## Course Overview

This repository contains a complete implementation of statistical methods applied to finance, ranging from Monte Carlo simulation to advanced option pricing models. The course bridges theoretical finance with practical computational implementation, emphasizing hands-on coding and real-world applications.

## Key Technologies & Methods

### Statistical Computing

- **Monte Carlo Methods** - Variance reduction techniques, antithetic sampling
- **Stochastic Process Simulation** - Wiener processes, Geometric Brownian Motion
- **Parameter Estimation** - MLE, Quasi-MLE, Method of Moments
- **Time Series Analysis** - Change-point detection, volatility modeling

### Financial Models

- **Black-Scholes Framework** - European & American option pricing
- **Lévy Processes** - Variance Gamma, Meixner models
- **Advanced SDEs** - CIR, CKLS, Ornstein-Uhlenbeck processes
- **FFT Pricing** - Fast Fourier Transform for complex derivatives

### Programming Implementation

- **R Packages:** `quantmod`, `tseries`, `sde`, `yuima`, `stats4`
- **Python Libraries:** Scientific computing and numerical optimization
- **Numerical Methods:** Euler-Maruyama, Milstein discretization schemes

## Getting Started

### Usage Options

1. **Download HTML Report** - Complete analysis with visualizations
2. **Render Quarto Document** - For reproducible research workflow
3. **Run R Scripts** - Execute individual components without Quarto

## Core Topics Implemented

1. Random Number Generation & Monte Carlo

- Inverse transformation and acceptance-rejection methods
- Pseudo-random number generation with reproducible seeds
- Monte Carlo integration for financial derivatives pricing

2. Stochastic Processes Simulation

3. Advanced Parameter Estimation

- **Maximum Likelihood Estimation** for complex distributions
- **Quasi-MLE** for model misspecification robustness
- **Method of Moments** for distributions without closed-form densities

4. European Options Pricing

- Risk-neutral valuation framework
- Multiple risk-neutralization methods (Esscher, Mean-correcting martingale)
- Variance reduction techniques implementation

5. Lévy Processes & Jump Models

- Beyond Gaussian assumptions in financial modeling
- Variance Gamma and Meixner process implementations
- Lévy-Khintchine representation and characteristic functions

6. American Options & Longstaff-Schwartz method implementation

7. Volatility Modeling & Market Microstructure

- Implied volatility extraction from market prices
- Volatility smile/skew analysis
- Liquidity effects and bid-ask spread considerations

## Practical Applications

### Real Market Analysis

- **Data Sources:** Yahoo Finance, FRED, multiple asset classes
- **Assets Covered:** Equity indices (SPY, QQQ), individual stocks, commodities, bonds
- **Market Phenomena:** Volatility clustering, fat tails, asymmetric returns

### Risk Management Tools

- **Greeks Calculation** - Delta, Gamma, Vega, Theta, Rho
- **Sensitivity Analysis** - Linear and quadratic approximations
- **Model Validation** - Kolmogorov-Smirnov tests, AIC model selection

## Learning Outcomes Demonstrated

1. **Computational Finance Proficiency** - Advanced R/Python implementation
2. **Statistical Modeling** - From basic distributions to complex stochastic processes
3. **Derivatives Pricing** - Multiple methodologies and model comparisons
4. **Market Data Analysis** - Real-world financial time series processing
5. **Risk Assessment** - Quantitative risk metrics and scenario analysis

## Technical Highlights

### Advanced Numerical Methods

- **FFT Pricing** for models without closed-form solutions
- **Numerical Integration** for complex payoff structures
- **Optimization Algorithms** (Nelder-Mead, BFGS) for parameter estimation

### Statistical Rigor

- **Change-point Detection** for structural breaks in time series
- **Model Diagnostics** - QQ plots, density fitting, goodness-of-fit tests
- **Variance Reduction** techniques for Monte Carlo efficiency

### Industry-Relevant Implementation

- **Market Data Integration** via quantmod and tseries packages
- **Production-Ready Code** with error handling and optimization
- **Comprehensive Documentation** with mathematical foundations

## Key Insights & Findings

- **Non-normality in Returns** - Empirical evidence across asset classes
- **Model Limitations** - When Black-Scholes assumptions break down
- **Computational Trade-offs** - Accuracy vs. efficiency in numerical methods
- **Market Microstructure** - Impact of liquidity on option pricing

## Technical Skills Showcased

### R Programming

- Advanced statistical computing and financial modeling
- Package development and optimization techniques
- Reproducible research with Quarto integration

### Financial Engineering

- Derivatives pricing across multiple methodologies
- Risk-neutral measure transformations
- Advanced stochastic calculus implementation

### Data Science

- Large-scale financial data processing
- Time series analysis and forecasting
- Model validation and backtesting frameworks

**Note:** This repository represents a comprehensive exploration of computational finance, combining theoretical rigor with practical implementation. The code demonstrates both academic understanding and industry-applicable skills in quantitative finance.

**Academic Integrity:** All work represents original analysis and implementation based on course materials and established financial mathematics literature.
