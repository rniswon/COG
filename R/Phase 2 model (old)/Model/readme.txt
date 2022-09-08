Larger work citation:
Valder, J.F., McShane, R.R., Thamke, J.N., McDowell, J.S., Ball, G.P., Houston, N.A., and Galanter, A.E., 2021, Estimates of water use associated with continuous oil and gas development in the Permian Basin, Texas and New Mexico, 2010–19: U.S. Geological Survey Scientific Investigations Report 2021–5090, 27 p., https://doi.org/10.3133/sir20215090.

Input data release citation:
Ball, G.P., Galanter, A.E., McDowell J.S., Houston, N.A., McShane, R.R., Haines, S., Verela, B., Valder, J.F., and Thamke, J.N., 2020, Data to Estimate Water Use Associated with Continuous Oil and Gas Development, Permian Basin, United States, 1980-2019: U.S. Geological Survey data release, https://doi.org/10.5066/P9LAWIPH.

This data release citation:
McShane, R.R., and McDowell J.S., 2021, R Scripts and Results of Estimated Water Use Associated with Continuous Oil and Gas Development, Permian Basin, United States, 2010-2019 (ver. 2.0, April, 2022): U.S. Geological Survey data release, https://doi.org10.5066/P9JIOU3V.


Information about the tool is provided in the cited report (Valder and others, 2021). The tool requires running a number of scripts that were coded in R [4.0.2; June 22, 2020] using the integrated development environment of RStudio [1.3.959; May 18, 2020]. See directory structure and "R Session Info" at end of this file for the file structure and list of packages used in these scripts.

To run the scripts, R 4.0.2 or newer must be installed on the computer. Although not required, it is recommended that RStudio 1.3.959 or newer also be installed. The scripts rely on numerous packages in R that will be downloaded and installed on the computer; however, newer releases than R 4.0.2 may cause errors in running the scripts if package functions have been changed since the scripts were originally coded. 

Reading this file, the "readme.txt" document, means that the zipped archive was properly extracted. The folder that the tool was extracted to will be the working directory in R for running the scripts. Do not change the structure of the extracted contents; doing so will cause errors in running the scripts saved in the "Code" folder.


Download instuctions:
  1. Download the data in the cited data release (https://doi.org/10.5066/P9LAWIPH).

  2. Save the CSV files from the data release to the "Raw" folder.

Run instructions:	
  0. If R or RStudio is already open, close it.

  1. Open the "mungeDataRelease.R" script by double-clicking it.

  2. Run (source) the script by typing Ctrl+A then Ctrl+R if using R or Ctrl+Shift+Enter if using RStudio. Raw data files from the data release will be prepared into input data and saved to the "Data" folder in 7 CSV files. Additionally, 1 TXT file, 4 XLS files, and 2 XLSX files will be downloaded from USGS servers and saved to the "Raw" folder.

  3. Open the "runProject.R" script by double-clicking it.

  4. Run (source) the script by typing Ctrl+A then Ctrl+R if using R or Ctrl+Shift+Enter if using RStudio. Input data will be processed into model output and saved to the "Product" folder in 12 CSV, 4 RDS and 24 SVG files, along with 4 intermediate RDS files in the "Data" folder.

  5. Model output in 2 CSV files with the prefix "data_release_" are the same as the CSV files in this cited data release (https://doi.org10.5066/P9JIOU3V).


Although these data have been processed successfully on a computer system at the U.S. Geological Survey, no warranty expressed or implied is made regarding the display or utility of the data for other purposes, nor on all computer systems, nor shall the act of distribution constitute any such warranty. The U.S. Geological Survey or the U.S. Government shall not be held liable for improper or incorrect use of the data described and/or contained herein. Any use of trade, firm, or product names is for descriptive purposes only and does not imply endorsement by the U.S. Government.


Data release structure:
\Model\Code: contains scripts for running the model
	0prereqs.R
	1wrangle.R
	2model.R
	3visualize.R
\Model\Data: will contain the input data processed from mungeDataRelease.R
\Model\Product: will contain the model output from runProject.R
\Model\Raw: must contain the files from Ball and others 2020 (https://doi.org/10.5066/P9LAWIPH)
\Model\mungeDataRelease.R: R script to process the data release files as input to the model
\Model\readme.txt: this file; readme file describing how to run the model
\Model\runProject.R: R script to run the model and visualize output from the model
\Output\coefficients.csv: file containing coefficients from the model on water use.
\Output\predictions.csv: file containing predictions from the model on water use.
\metadata.xml: Metadata file for this data release.

R Session Info: R version 4.0.2 (2020-06-22)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18363)
attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     
other attached packages:
 [1] dplyr_1.0.4     tibble_3.0.6    tidyr_1.1.2     broom_0.7.5     purrr_0.3.4     readr_1.4.0     readxl_1.3.1   
 [8] stringr_1.4.0   plyr_1.8.6      ggplot2_3.3.3   scales_1.1.1    showtext_0.9-2  showtextdb_3.0  sysfonts_0.8.3 
[15] quantreg_5.83   SparseM_1.78    segmented_1.3-2 sf_0.9-7        raster_3.4-5    sp_1.4-5        magrittr_2.0.1 
[22] svglite_1.2.3.2
loaded via a namespace (and not attached):
 [1] Rcpp_1.0.6         lattice_0.20-41    class_7.3-18       assertthat_0.2.1   digest_0.6.27      utf8_1.1.4        
 [7] R6_2.5.0           cellranger_1.1.0   backports_1.2.1    MatrixModels_0.4-1 evaluate_0.14      e1071_1.7-4       
[13] pillar_1.5.0       gdtools_0.2.3      rlang_0.4.10       rstudioapi_0.13    Matrix_1.2-18      rmarkdown_2.7     
[19] splines_4.0.2      munsell_0.5.0      tinytex_0.29       compiler_4.0.2     xfun_0.21          pkgconfig_2.0.3   
[25] systemfonts_1.0.1  htmltools_0.5.1.1  tidyselect_1.1.0   codetools_0.2-18   matrixStats_0.58.0 fansi_0.4.2       
[31] crayon_1.4.1       conquer_1.0.2      withr_2.4.1        gtable_0.3.0       lifecycle_1.0.0    DBI_1.1.1         
[37] units_0.6-7        KernSmooth_2.23-18 cli_2.3.1          stringi_1.5.3      ellipsis_0.3.1     generics_0.1.0    
[43] vctrs_0.3.6        tools_4.0.2        glue_1.4.2         hms_1.0.0          yaml_2.2.1         colorspace_2.0-0  
[49] classInt_0.4-3     knitr_1.31        
