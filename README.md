# Oxygen Uptake Efficiency Slope (OUES) as a Predictor of Mortality Risk

## Summary:
The oxygen uptake efficiency slope (OUES) is defined as the slope of the linear relationship between oxygen consumption (VO<sub>2</sub>) and the semilog transformed ventilation rate (V<sub>E</sub>) measured during an incremental exercise test. This study investigated the relationship between different measures of OUES and all-cause mortality in a cohort of apparently healthy adults. **OUES was found to be related to all-cause mortality in males but not females suggesting a determination of OUES has prognostic utility in males. Further, submaximal determinations of OUES could be valuable when a maximal exercise test is not feasible or desirable in males**.

## The Rationale:
The traditional variable of interest from cardiopulmonary exercise testing, and the most commonly used as an indicator of health is maximum or peak oxygen uptake (VO<sub>2peak</sub>). Other variables collected during CPX have received less attention but may also have prognostic utility for assessing health in both apparently healthy and clinical populations. One such variable is the oxygen uptake efficiency slope (OUES). 

The OUES is defined as the slope of the linear relationship between oxygen consumption (VO<sub>2</sub>) and the semilog transformed ventilation rate (V<sub>E</sub>) measured during an incremental exercise test. Some researchers normalize OUES to body surface area although this is not always the case. Additionally, because OUES is calculated from a linear relationship, it can be determined from a submaximal exercise test, highlighting its potential clinical value compared to variables measured from a maximal exercise test. 

Previous research has reported on the prognostic potential of OUES in clinical populations, yet no studies have examined OUES within a cohort of apparently healthy individuals. Moreover, research is needed on the prognostic potential of submaximal determinations of OUES as well as OUES normalized to body surface area. Thus, **the purpose of this study was to evaluate the relationship between OUES and all-cause mortality in a cohort of apparently healthy males and females**.

## The Final Product/Results:
Three scripts for R were used to create the dataset from the BALL ST cohort and then analyze/visualize the data (dataset_creation.R, data_analysis.R, and Kaplan_meier_curves.R). Prediction models using absolute and normalized OUES measured from a maximal test were related to mortality in males independent of traditional risk factors. Submaximal determinations of OUES varied in their ability to assess risk; using 75% of the data from a maximal test was better than using 50% of the data. The predictive capability of all the OUES measures was poor in females indicating that the predictive contribution from the model covariates outweighed those from OUES.

The Kaplan-Meier curves below illustrate the findings from the absolute and normalized OUES in the male-specific analyses ¬— those with a higher OUES had a greater probability of survival. 

![Kaplan_Plot](images/kaplan_curves_males.pdf)

Comparing the concordance index values from the models indicated the fully-adjusted OUES models did not statistically differ compared to the fully-adjusted VO<sub>2peak</sub> models. OUES measures also did not complement peak VO<sub>2peak</sub> models. Overall, these findings suggest assessments of OUES may have prognostic utility within apparently healthy males and submaximal determinations of OUES could be valuable when accurate assessment of VO<sub>2peak</sub> is not feasible.
Following the data analysis, I summarized the findings and submitted the scientific manuscript to the Journal of Cardiopulmonary Rehabilitation and Prevention. The manuscript is currently under peer review by experts in the field.

