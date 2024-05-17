
# Cancer Survival Analysis

This repository contains an analysis of factors influencing cancer patient survival using mixed models. It includes a detailed examination of the CWsample dataset, exploring various predictors of patient survival, and a proposed trial design and sample size analysis for evaluating a public health intervention.

## Project Structure

### 1. Understanding Factors Influencing Cancer Patient Survival: A Mixed Models Approach

 # Mixed Models Report


#### Introduction

Cancer is a major global health issue that makes diagnosis and treatment challenging. This study examines a dataset named CWsample, which includes 490 patient records from twelve hospital sites. The dataset provides information on age at death, age at diagnosis, cancer type, and other demographic details. On average, patients die at 52.6 years. Important factors influencing survival time include age at diagnosis, gender, and quality of life (measured by the ‘HighQualityOfLife’ variable). This report uses linear regression and mixed models to validate these findings.

#### Exploring Model Analysis Techniques

1. **Linear Regression Model:** Looks at the relationship between age at death and age at diagnosis.
2. **Exploring Additional Covariates:** Examines how other patient characteristics like Sex, High Quality of Life, and Stage3or4 status affect survival.
3. **Random Intercept Model:** Considers differences between hospital sites.
4. **Enhanced Model Incorporating Random Slopes:** Adds a random slope for age at diagnosis.

#### Discussion

The analysis shows that the average age at death for patients is about 52.6 years. There is a positive correlation between age at diagnosis and age at death. The hospital site significantly affects this relationship. High Quality of Life also has an impact on survival, highlighting the need for targeted interventions.

### 2. Assessing the Impact of a Public Health Intervention on Cancer Patients: A Trial Design and Sample Size Analysis

**Mixed Models with Medical Applications Report**



#### Trial Design

This section describes the main goal of the trial and the proposed design to achieve it. The trial aims to determine if a new public health intervention can delay the age at which patients receive their initial cancer diagnosis. The analysis will be based on an outcome model, and assumptions within this model will be identified and discussed.

#### Determining Sample Size Requirements

1. **Parameter Estimations:** Uses the CWsample dataset to estimate the variances in the model.
2. **Determining Sample Size:** Calculates the sample size needed for specific error rates.
3. **Sensitivity Analysis:** Looks at the intracluster correlation coefficient and adjusts the sample size accordingly.

#### Conclusion

This report identifies key factors influencing cancer patient survival, stressing the importance of hospital site and quality of life. The trial design and sample size analysis offer a framework for evaluating the public health intervention, ensuring reliable and meaningful outcomes.


