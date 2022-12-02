# SWATdoctR <img src="man/figures/swatdoctr_hex.svg" align="right" />

`SWATdoctR` is a collection of functions and routines for SWAT model calibation and model diagnostics. The R package includes routines for a guided model calibration, functions for the evaluation of the model performance, as well as functions for the visualization and diagnosis of simulation outputs. The aim of the `SWATdoctR` is to identify potential issues in the model setup early in the calibration process and to support the SWAT modeler to focus on a plausible process representation in the model calibration process.

## First ToDos for model verification
- Write SWAT run function to extract simulation outputs for model verification
- Step 1 in verification: Simulation of climate variables
  * read basin water balance file
  * overview figure precip (snow, rain), yearly, monthly, allocation to water balance components
- Step 2: check triggered management
  * read mgt_out
  * function to extract triggered management schedules
- Step 2.1: Verification of decision tables?
- Step 3: plant growth without stress
  * read mgt_out
  * read hru_pw_day? (maybe too large to read)
  * boxplot of phu fraction at harvest for all crops
- Step 4: plant growth stress active
  * read mgt_out
  * boxplot stress factors 
