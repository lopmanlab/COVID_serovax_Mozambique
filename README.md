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
