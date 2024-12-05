# STA702Fa24 Course Project

This repository contains all the Rcodes, .Rdata files and the required plots in .pdf format that have been used in the project. Below we describe the files as they have been created:

* Foder **0 Final Documents** contain the final presentation slides and the project report.
* Folder **Data Clean** contain the raw .nc files obtained from the data source and **Data Clean.R** files which cleans the data to accessible format.
* Folder **Plots** contains all the figures in .pdf format. Figures starting with **Image** have been used in the report, whereas the figures starting with **Plot** have been used in the slides.
* **01 Monsoon_rain_imd.Rdata**: The .Rdata files obrained from **Data Clean.R** that containes the required data set.
* **02 EDA and Data for model.R**: The .R file that contains code for generating EDA plots. This also generates the actual data formats that aredirectly used in model fitting.
* **03 Data for model.test.Rdata**: The .Rdata file that contains data for model testing obtained from **02 EDA and Data for model.R**.
* **03 Data for model.train.Rdata**: The .Rdata file that contains data for model training obtained from **02 EDA and Data for model.R**.
* **04 Model Fit.R**: The .R file that contains code for model fitting.
* **05 Model Fit_model.Rdata**: The .Rdata file that contains the output of the MCMC chains obtained form **04 Model Fit.R**.
* **06 Model Convergence Test.R**: The .R file that contains codes to generate convergence plots and ESS.
* **07 Model Fitting.R**: The .R file that contains codes to generate the model fitting plot.
* **08 Fitting Different Model.R**: The .R file that contains codes for fitting simpler models.
