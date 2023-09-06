# Summary
This repository contains the code used in simulations comparing serologically-triggered versus fixed-time interval long-term COVID-19 vaccination strategies in Mozambique. Our findings are described in this preprint entitled ["Can long-term COVID-19 vaccination be improved by serological surveillance?: a modeling study for Mozambique"](https://www.medrxiv.org/content/10.1101/2023.08.29.23294793v1)

# Code description
## Main analysis
Code for the main modeling work and analysis can be found in [2_main_simulation](https://github.com/lopmanlab/COVID_serovax_Mozambique/tree/main/2_main_simulation). The 10-year forward model simulations were run using a High Performance Cluster. 

| File                   | Description |
| ---------------------- | ------------- |
| [0_sweep_sero](2_main_simulation/0_sweep_sero.RDS)           | Loads relevant packages  |
| [0c_model_setup](2_main_simulation/0c_model_setup.R)        | Loads function for transmission model   |
| [0d_model_code_sero](2_main_simulation/0d_model_code_sero.R) | Loads paramemeters, initial conditions and control settings  |
| [1_sweep_int](2_main_simulation/1_sweep_int.RDS)| Loads function used to optimize beta distribution for the probabilistic senstivity analysis (PSA) |
| [1c_model_setup](2_main_simulation/1c_model_setup.R)         | Loads parameters and distributions used for the PSA  |
| [1d_model_code_int](2_main_simulation/1d_model_code_int.R)      | Loads plot function used for the PSA  |
