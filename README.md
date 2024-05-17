# Information Criteria vs Internal performance measures
This repository forms the computational heart of my Master Thesis <br>
[`Decoding Predictive Performance: A Simulation Study on Information Criteria vs. Internal Performance Measures`](./docs/Thesis_Manuscript/Thesis-Manuscript.pdf). 

This study investigates the performance of Information Criteria and internal performance measures in different simulated contexts. Here in this repository, you can find all code used in our project and full instructions for how to replicate and reproducde the simulation.

To download a version of this repository with all simulation output and processed results, please contact me (Hidde van de Beek) the owner and maintainer of this Github repository. 

Email: h.vandebeek@uu.nl

| File / Folder                              | Contents                                                         |
| :----------------------------------------- | :--------------------------------------------------------------- |
| [`LICENSE`](./LICENSE)                     | The contents of this repository are made publicly available unter a `GNU General Public License v3.0`. Please see this document for details. |
| [`5-predictor population code`](./scripts/hpc/generate_data_5predictors.R) | This is the code that simulated the 5-predictor populations using five different data generating mechanisms. |
| [`10-predictor population code`](./scripts/hpc/generate_data_10predictors.R) | This is the code that simulated the 10-predictor populations using five different data generating mechanisms. |
| [`analysis template`](scripts/hpc/execute_analysis_template.R) | This is the code that provides a template for the analysis of the different data generating mechanisms. |
| [`visualize results`](./scripts/own_device) | In this folder you will find two R-scripts that are used for the visualize the analysis for the Results section and Appendix.|
| [`functions`](./scripts/shared/functions.R) | This R file contains all created R functions in the different files of data generation, analysis, and visualization.|
| [`manuscript`](./docs/Thesis_Manuscript) | In this folder, you will find all necessary files to generate the [`Thesis`](./docs/Thesis_Manuscript/Thesis-Manuscript.pdf)|
| [`Population data`](./data/full_data) | In this folder you can find all population data for the 5- and 10-predictor models per data generating mechansim. |
| [`Analysis data`](./data/analysis) | In this folder you can find all data for the 5- and 10-predictor models per data generating mechansim after the analysis is performed. |


## Data generating models
In the following models the candidate predictors contribute in the data generating mechanism, with their respective file and folder names. 
```math
\mathrm{Model \ \  1:} \beta_0 + \beta(X_1 + X_2 + X_3 + X_4 + 0.5*X_5) | \mathrm{(1normal)}
```
```math
\mathrm{Model \ \  2:} \beta_0 + \beta(X_1 + X_2 + X_3 + X_4 + 0.5*X_1X_2) | \mathrm{(1interaction)}
```
```math
\mathrm{Model \ \  3:} \beta_0 + \beta(X_1 + X_2 + X_3 + 0.5*X_4 + 0.5*X_5) | \mathrm{(2normal)}
```
```math
\mathrm{Model \ \  4:} \beta_0 + \beta(X_1 + X_2 + X_3 + 0.5*X_4 + 0.5*X_1X_2) | \mathrm{(2normalinteraction)}
```
```math
\mathrm{Model \ \  5:} \beta_0 + \beta(X_1 + X_2 + X_3 + X_4 + 0.5*X_1X_2 + 0.5*X_2X_3) | \mathrm{(2interaction)}
```

In the following models the candidate predictors do not contribute in the data generating mechanism, however they are named the same as above, with the added "_anti" in their filename
```math
\mathrm{Model \ \  1:} \beta_0 + \beta(X_1 + X_2 + X_3 + X_4) | \mathrm{(1normal)}
```
```math
\mathrm{Model \ \  2:} \beta_0 + \beta(X_1 + X_2 + X_3 + X_4) | \mathrm{(1interaction)}
```
```math
\mathrm{Model \ \  3:} \beta_0 + \beta(X_1 + X_2 + X_3) | \mathrm{(2normal)}
```
```math
\mathrm{Model \ \  4:} \beta_0 + \beta(X_1 + X_2 + X_3) | \mathrm{(2normalinteraction)}
```
```math
\mathrm{Model \ \  5:} \beta_0 + \beta(X_1 + X_2 + X_3) | \mathrm{(2interaction)}
```


## To reproduce the entire project please follow the procedure below:
1. Go to [`5-predictor population code`](./scripts/hpc/generate_data_5predictors.R) and [`10-predictor population code`](./scripts/hpc/generate_data_10predictors.R) to generate the populations used in the Research Report. The script is designed for a high performance cluster computer with the shell.sh file, provided a Slurm Workload Manager is used. Local running is also an option. The output is saved in the folder [`Population data`](./data/full_data) in .Rdata form.
5. To start the analysis, open [`analysis template`](scripts/hpc/execute_analysis_template.R) and run the code. By specifying if you're using the 5- or 10-predictor model and which data generating mechansim (e.g. "1normal"), the correct population data is used. The script is designed for a high performance cluster computer with the shell.sh file, provided a Slurm Workload Manager is used. Local running is not recommended, due  to high computation times. The results are saved in the folder [`Analysis data`](./data/analysis) in .Rdata form. 
6. Finally, to create the visualization for the results, open the [`visualize results`](./scripts/own_device/Plots_results.Rmd). This automatically visualizes the analysis you calculated earlier for the four sub-studies in the paper. The plots are saved in the folder [`plots`](./docs/Thesis_Manuscript/plot/results).
7. To create the visualization for the appendix, open the [`visualize results`](./scripts/own_device/Plots_appendix.Rmd). This automatically visualizes all analyses you calculated earlier. The plots are saved in the folder [`plots`](./docs/Thesis_Manuscript/plot/appendix).  
8. Go to [`manuscript`](./docs/Thesis_Manuscript). In this step, we generate all figures, tables, and supplementary materials as well as a manuscript in the style of the journal Statistics in Medicine. The plots you created earlier are saved in their respective folder.

## Privacy / Ethics / Security
Ethical Approval for the simulation study was granted by the Utrecht University Ethics committee and filed under number 23-1780.

All data are simulated, and not based on any empirical data.  Consequently, there are no privacy or security restrictions. This repository is accessible to the public on GitHub under the license type of `GNU General Public License v3.0`.


## Contact 
The maintenance and public accessibility of this repository are managed by Hidde van de Beek. If you have any questions please feel free to contact me via email: h.vandebeek@uu.nl .
