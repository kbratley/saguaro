# Assessing Buffel Grass Treatment Efficacy in Saguaro National Park: Panel Regression Analysis

This repository contains code for assessing the effectiveness of Buffel grass treatment in Saguaro National Park using panel regression analysis.

## Usage

The code is broken up into two parts:

1. **Matching:** 
   - Consult `1_Matching.R` to run Match on the entire dataset. This step creates a mask as a preprocessing step to ensure that we are selecting the best controls for the treated points.

2. **Panel Model:** 
   - Run `2_DiDPanelModel.R` to 1) organize the panel data and apply the mask derived from the matching step, and 2) run the difference-in-difference panel model.

