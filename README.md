# Summary
This repository contains the code used in simulations comparing serologically-triggered versus fixed-time interval long-term COVID-19 vaccination strategies in Mozambique. Our findings are described in this preprint entitled ["Can long-term COVID-19 vaccination be improved by serological surveillance?: a modeling study for Mozambique"](https://www.medrxiv.org/content/10.1101/2023.08.29.23294793v1)

# Code description
## Main analysis
Code for the main modeling work and analysis can be found in [2_main_simulation](https://github.com/lopmanlab/COVID_serovax_Mozambique/tree/main/2_main_simulation). The 10-year forward model simulations were run using a High Performance Cluster. Relevant pieces of the main code are detailed in the table below

| File                   | Description |
| ---------------------- | ------------- |
| [0_sweep_sero](2_main_simulation/0_sweep_sero.RDS)           |Data frame of model parameters for serologically-triggered vax scenarios|
| [0c_model_setup](2_main_simulation/0c_model_setup.R)        | Setup model with seroprevalence vax trigger |
| [0d_model_code_sero](2_main_simulation/0d_model_code_sero.R) | Model code function without fixed time vax|
| [1_sweep_int](2_main_simulation/1_sweep_int.RDS)| Data frame of model parameters for fixed time interval vax scenarios|
| [1c_model_setup](2_main_simulation/1c_model_setup.R)         |Setup model without seroprevalence vax trigger |
| [1d_model_code_int](2_main_simulation/1d_model_code_int.R)      | Model code function with fixed time vax|
| [2_compile_res_hpc](2_main_simulation/2_compile_res_hpc.R)      | Takes raw outputs from simulations and summarizes into medians and ranges|
| [2_compile_annual_hpc](2_main_simulation/2_compile_annual_hpc.R)      | Takes raw outputs from simulations and summarizes for annual NNT|
| [3_plots](2_main_simulation/3_plots.Rmd)      | Takes summarized outputs in [0_res](2_main_simulation/0_res) and makes plots/tables in manuscript|
| [9_last_Rrand](2_main_simulation/9_last_Rrand.RDS)      | Distribution of compartments at end of calibration|
| [9_mixing_matrix_gmix](2_main_simulation/9_mixing_matrix_gmix.R)      | Social mixing matrix input|
| [9_spec_humid](2_main_simulation/9_spec_humid.csv)      | Specific humidity over calendar year|


