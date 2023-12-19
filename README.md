# Information Criteria vs Internal performance measures:
This repository forms the heart of my Master Thesis <br>
[`Decoding Predictive Performance: A Simulation Study on Information Criteria vs. Internal Performance Measures`](./docs/Thesis_Manuscript/Thesis-Manuscript.pdf). 

In this project, we investigated the performance of Information Criteria and internal performance measures in different simulated contexts. Here in this repository, you will find all code used in our project and full instructions for how to replicate this simulation study.

To download a version of this repository with all simulation output and processed results, please contact me (Hidde van de Beek) the owner and maintainer of this Github repository. 

Email: h.vandebeek@uu.nl

| File / Folder                              | Contents                                                         |
| :----------------------------------------- | :--------------------------------------------------------------- |
| [`LICENSE`](./LICENSE)                     | The contents of this repository are made publicly available unter a `GNU General Public License v3.0`. <br> Please see this document for details.|
| [`simulation code`](./R/Data_simulation/Simulation_Report_Covariance.Rmd) | This is the code that simulated the populations using two different data generating mechanisms.
| [`experiment code`](./R/Experiments/Analysis_Report_Experiment.Rmd) | This is the code that used the populations to do the experiments. |
| [`visualization code`](./R/Visualization/Visualization_Report.Rmd) | This is the code that produces the visualization of the experiments. |
| [`results`](./results) | In this folder, you will find the visualizations of the two experiments. |
| [`functions`](./R/Functions.R) | This R file has all created R functions in the different files of data generation, analysis, and visualization.
| [`manuscript`](./docs/Thesis_Manuscript) | In this folder, you will find all necessary files to generate the [`manuscript`](./docs/Thesis_Manuscript/Theses-Manuscript.pdf)|           


## To reproduce the entire project please follow the procedure below:
1. Go to [`simulation code`](./R/Data_simulation/Simulation_Report_Covariance.Rmd) to generate the populations used in the Research Report. The output is saved in the folder ['populations'](.data/simulation) in .Rdata form. The .Rmd also provides some visualizations of the data and has parameters to change the number of respondents in the populations and the data generating mechanisms (Mu-matrices, variance-covariance-matrices, and size of coefficients).
2. To start the experiments, open [`experiment code`](./R/Experiments/Analysis_Report_Experiment.Rmd) and run the code. This automatically opens the populations you created earlier. The results are saved in the folder [`experiments results`](.data/analysis). 
3. Finally, to create the visualization, open [`visualization code`](./R/Visualization/Visualization_Report.Rmd) and run the code. This automatically open the results you calculated earlier. The plots are saved in the folder [`results`](./results).
4. (optional) Go to [`manuscript`](./docs/Thesis_Manuscript).  In this step, we generate all figures, tables, and supplementary materials as well as a manuscript in the style of the journal Statistics in Medicine. You will have to replace the visualization of the experiments with your own results. You can copy-paste the folders [`experiment 1`](./results/experiment_1) and [`experiment 2`](./results/experiment_2) and replace them in folder [`manuscript`](./docs/Thesis_Manuscript).

## Privacy / Ethics / Security
Ethical Approval for the simulation study was granted by the Utrecht University Ethics committee and filed under number 23-1780.

All data are simulated, and not based on any empirical data.  Consequently, there are no privacy or security restrictions. This repository is accessible to the public on GitHub under the license type of `GNU General Public License v3.0`.


## Contact 
The maintenance and public accessibility of this repository are managed by Hidde van de Beek. If you have any questions please feel free to contact me via email: h.vandebeek@uu.nl .
