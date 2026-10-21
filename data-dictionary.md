# OASIS Cross-Sectional Data Dictionary

This document outlines the demographic, clinical, and derived anatomical variables included in the `oasis_cross-sectional.xlsx` dataset[cite: 2]. 

## Demographic Variables
* **ID:** Unique participant and imaging session identifier (Format: `OAS1_xxxx_MRy`)[cite: 1].
* **M/F:** Gender (M = Male, F = Female)[cite: 2].
* **Hand:** Handedness (Note: all subjects in this dataset are Right-handed)[cite: 1, 2].
* **Age:** Participant age in years (Range: 18 to 96)[cite: 1, 2].
* **Educ:** Education level codes[cite: 2]:
  * 1: Less than high school graduate
  * 2: High school graduate
  * 3: Some college
  * 4: College graduate
  * 5: Beyond college
* **SES:** Socioeconomic status measured by the Hollingshead Index (1 = Highest status, 5 = Lowest status)[cite: 2].

## Clinical Variables
* **MMSE:** Mini-Mental State Examination score (measures cognitive impairment)[cite: 2].
* **CDR:** Clinical Dementia Rating[cite: 2]:
  * 0 = Nondemented
  * 0.5 = Very mild dementia
  * 1 = Mild dementia
  * 2 = Moderate dementia
* *Note: All participants with dementia (CDR > 0) were diagnosed with probable Alzheimer's Disease (AD)*[cite: 2].

## Derived Anatomical Volumes
* **eTIV:** Estimated total intracranial volume (in mm³)[cite: 2].
* **nWBV:** Normalized whole brain volume (represents the proportion of all voxels in the atlas space that are classified as tissue)[cite: 2].
* **ASF:** Atlas scaling factor (computed volume scaling factor required to normalize the subject's brain to the target atlas)[cite: 2].