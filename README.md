Gender Discrimination Lawsuit – Analytics Case Study
📖 Project Overview

This project analyzes the Gender Discrimination Lawsuit dataset (Kaggle source
) to investigate whether female doctors at a U.S. medical school were systematically underpaid or under-promoted compared to male doctors.

The work was completed as part of the MSc Business Analytics program at Nanyang Technological University (NTU). Our team approached the problem as analytics consultants for the female doctors, aiming to prove whether discrimination existed in salary and promotion outcomes.

🎯 Objectives
Examine salary differences across gender, rank, and department.
Investigate promotion pathways and representation at higher academic ranks.
Apply statistical models (linear regression, logistic regression, CART) to test discrimination claims.
Present findings in a structured, non-technical way for business and legal stakeholders.

🗂️ Dataset

Source: Kaggle – Gender Discrimination Lawsuit
Key variables: salary (1994–1995), department, rank, certification, clinical experience, years of experience, number of publications.

🛠️ Methods
Exploratory Data Analysis (EDA):
- Salary distribution by gender, department, and rank.
- Promotion gaps and representation at higher levels.

Regression Analysis:
- Linear regression controlling for confounders (department, rank, experience, publications).
- Logistic regression for promotion and department placement odds.

📊 Key Findings
Female faculty consistently earned less than male counterparts, even after controlling for years of experience and publications.
Women were underrepresented in higher-paying departments (e.g., Surgery) and more concentrated in lower-paying ones (e.g., Pediatrics).
Promotion likelihood was significantly lower for female faculty, with men being ~2x more likely to reach full professorship.
Model choice and framing (e.g., CART vs logistic regression) could influence the case outcome — underscoring the importance of integrity and transparency in analytics.

💡 Learnings
The angle shapes the insight: The same dataset can support opposing narratives depending on framing.
Integrity matters: Upholding rigor (rejecting false claims of multicollinearity) led to more thoughtful, credible insights.
Leverage strengths: Diverse team backgrounds (engineering, math, business) enriched both technical analysis and storytelling.

📁 Repository Structure
├── data/                 # Raw dataset (Kaggle source)
├── notebooks/            # Jupyter/R notebooks for EDA and models
├── scripts/              # R/Python scripts for regression and CART
├── presentation/         # Final slides (PDF)
├── results/              # Charts, tables, model outputs
└── README.md             # Project documentation

🚀 How to Run

Clone this repo:

git clone https://github.com/<your-username>/gender-discrimination-lawsuit.git
cd gender-discrimination-lawsuit


Install dependencies:

pip install -r requirements.txt
(or use R with the tidyverse, MatchIt, and rpart packages)

Run notebooks in /notebooks for exploratory analysis and models.

🙏 Acknowledgements
Dataset: Kaggle – Gender discrimination: https://www.kaggle.com/datasets/hjmjerry/gender-discrimination
Team Members: Delwin Ramses van Strien, Jo Bum Joon, Zhang Jianyu, Zhang Ning, Zhao Hengjie, Nguyen Ngoc Thien Huong
Course: AN6002 Analytics & Machine Learning, NTU MSc Business Analytics
