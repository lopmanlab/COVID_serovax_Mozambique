# Summary
This repository contains the code used in simulations comparing serologically-triggered versus fixed-time interval long-term COVID-19 vaccination strategies in Mozambique. Our findings are described in this preprint entitled ["Can long-term COVID-19 vaccination be improved by serological surveillance?: a modeling study for Mozambique"](https://www.medrxiv.org/content/10.1101/2023.08.29.23294793v1)

# Code description
## Main forward model simulation and analysis
Code for the main modeling work and analysis can be found in [2_main_simulation](https://github.com/lopmanlab/COVID_serovax_Mozambique/tree/main/2_main_simulation). The 10-year forward model simulations were run using a High Performance Cluster. Relevant pieces of the main code are detailed in the table below. For the serologically-triggered vaccination scenarios, vaccination was implemented using an event function in the model setup through the DeSolve package framework for solving ODEs. For fixed-time interval vaccination, the event function was removed and vaccination was implemented directly in the model code to avoid accidental triggering of vaccination. While the codes are separated, all other codes, initial conditions and parameters were the same between the two vaccination scenarios. Model outputs of deaths, cases, seroprevalence over time, vaccine doses from each model run from the randomly sampled annual Rts were then summarized and interim results are made available in [0_res](2_main_simulation/0_res). These results can be further summarized for each vaccine scenario (seroprevalence triggeres of 50%-80% and biennial and annual fixed-time vaccinations) into medians and ranges which are used to produce tables and figures estimating vaccine impact. 

| File                   | Description |Category|
| ---------------------- | ------------- |------------- |
| [0_sweep_sero](2_main_simulation/0_sweep_sero.RDS)           |Data frame of model parameters for serologically-triggered vax scenarios| Sero-trigger model sims|
| [0c_model_setup](2_main_simulation/0c_model_setup.R)        | Setup model with seroprevalence vax trigger | Sero-trigger model sims|
| [0d_model_code_sero](2_main_simulation/0d_model_code_sero.R) | Model code function without fixed time vax| Sero-trigger model sims|
| [1_sweep_int](2_main_simulation/1_sweep_int.RDS)| Data frame of model parameters for fixed time interval vax scenarios| Fixed-interval model sims|
| [1c_model_setup](2_main_simulation/1c_model_setup.R)         |Setup model without seroprevalence vax trigger |Fixed-interval model sims|
| [1d_model_code_int](2_main_simulation/1d_model_code_int.R)      | Model code function with fixed time vax|Fixed-interval model sims|
| [2_compile_res_hpc](2_main_simulation/2_compile_res_hpc.R)      | Takes raw outputs from simulations and summarizes into medians and ranges|Compile results &summarise|
| [2_compile_annual_hpc](2_main_simulation/2_compile_annual_hpc.R)      | Takes raw outputs from simulations and summarizes for annual NNT|Compile results &summarise|
| [3_plots](2_main_simulation/3_plots.Rmd)      | Takes summarized outputs in [0_res](2_main_simulation/0_res) and makes plots/tables in manuscript|Make figures|
| [9_last_Rrand](2_main_simulation/9_last_Rrand.RDS)      | Distribution of compartments at end of calibration|Model input|
| [9_mixing_matrix_gmix](2_main_simulation/9_mixing_matrix_gmix.R)      | Social mixing matrix input|Model input|
| [9_spec_humid](2_main_simulation/9_spec_humid.csv)      | Specific humidity over calendar year|Model input|

## Sensitivity analysis
The structure of code for the sensitivity analysis repicate the same structure as the main analysis. The following sensitiivty analysis were conducted
| Sensitivity analysis       | 
| ---------------------- | 
| Varying rate of antibody waning (3_sens_abwaning)          |
| High immune escape where future waves are primarily driven by increasingly transmissible variants  (4_sens_hiescape)          |
| Randomly-timed annual epidemics as opposed to seasonal epidemics(5_sens_randtime)          |
## Model calibration
