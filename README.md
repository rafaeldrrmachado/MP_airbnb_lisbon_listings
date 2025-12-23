# 📊 Lisbon Airbnb Listings Predictive Model

<div align="center">
  
**A data-driven project exploring Airbnb listings in Lisbon, utilizing predictive modeling techniques.**

[View the Full Report Here](report.pdf)

</div>

## 📖 Overview

This repository contains a project developed for the "Predictive Models" course, focusing on the analysis of Airbnb listings in Lisbon, Portugal. The project leverages R to perform data cleaning, exploratory data analysis, and implement predictive models to derive insights from the dataset. The primary goal is to understand the factors influencing Airbnb listings and predict prices.

## ✨ Key Deliverables & Analysis

-   📈 **Data Ingestion & Preprocessing:** Handles the `listings_Lisbon.csv` dataset, preparing it for analysis by cleaning and transforming raw data.
-   📊 **Exploratory Data Analysis (EDA):** Identifies patterns, trends, and relationships within the Airbnb listing data, providing a foundational understanding of the market in Lisbon.
-   🧠 **Predictive Modeling:** Implements machine learning models (details within `script_lisbon_final.R` and `report.pdf`) to forecast or classify aspects of Airbnb listings.
-   📄 **Comprehensive Report:** A detailed PDF document (`report.pdf`) outlining the project's methodology, analysis findings, model evaluation, and conclusions.

## 🚀 Getting Started

To reproduce the analysis or explore the project, you'll need R and preferably RStudio.

### Prerequisites

-   **R**: A recent stable version of R (e.g., 4.x.x).
-   **RStudio (Recommended)**: An integrated development environment for R.

### Installation & Setup

1.  **Clone the repository**
    ```bash
    git clone https://github.com/rafaeldrrmachado/MP_airbnb_lisbon_listings.git
    cd MP_airbnb_lisbon_listings
    ```

2.  **Install Required R Packages**
    Open the `script_lisbon_final.R` file in RStudio or a text editor. Identify all packages loaded using `library()` or `require()`. Install any missing packages from CRAN (the Comprehensive R Archive Network) in your R console:
    ```R
    # Example: Install common data science packages
    install.packages(c("dplyr", "ggplot2", "readr", "caret"))
    # Replace with the actual list of packages from script_lisbon_final.R
    ```

3.  **Run the Analysis Script**
    Once all dependencies are installed, you can execute the R script to run the analysis.
    ```R
    # In RStudio, open script_lisbon_final.R and click 'Source'
    # Alternatively, from an R console in the project directory:
    source("script_lisbon_final.R")
    ```
    This script will perform the data loading, processing, modeling, and may generate additional outputs or figures.

## 📁 Project Structure

```
MP_airbnb_lisbon_listings/
├── listings_Lisbon.csv      # Raw dataset of Airbnb listings in Lisbon.
├── script_lisbon_final.R    # The primary R script containing all data analysis, preprocessing, and predictive modeling code.
└── report.pdf               # A comprehensive report summarizing the project's objectives, methodology, findings, and conclusions.
```

## 📄 Report

The detailed analysis, methodology, statistical findings, model evaluations, and conclusions are presented in the PDF report.

[Access the Full Report: `report.pdf`](report.pdf)

## 🤝 Contributing

This project was created as a submission for a university course. While direct contributions in the form of pull requests are not actively sought, you are welcome to fork the repository for your own learning, experimentation, or adaptation.

## 🙏 Acknowledgments

-   This project was developed as part of the **Predictive Models** course.
-   Data sourced from Airbnb listings in Lisbon provided by course professor.

## 📞 Support & Contact

-   🐛 Issues: Feel free to open an issue on [GitHub Issues](https://github.com/rafaeldrrmachado/MP_airbnb_lisbon_listings/issues) if you encounter problems or have questions about the project setup.

---

<div align="center">

**⭐ Star this repo if you find this analysis helpful or insightful!**

Made by [Rafael Machado](https://github.com/rafaeldrrmachado)

</div>
