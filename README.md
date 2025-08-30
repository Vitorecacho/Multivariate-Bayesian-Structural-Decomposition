This guide outlines the necessary steps to run the analysis.

## Prerequisites
All required R libraries are listed at the top of each script file. When you install these primary libraries, 
their necessary dependencies should be installed automatically. Ensure all libraries are installed before proceeding.

## Execution Workflow
To ensure the analysis runs correctly, please execute the scripts in the following order. All data files are located in the /data folder.

create_dataset.R

Purpose: This script performs the initial data processing and preparation. It cleans and structures the raw data to make it suitable for analysis.

stack.R

Purpose: This script takes the prepared data from the previous step and creates the data stack that will be used for the regression model.

model_multivar.R

Purpose: In this final step, the script configures and runs the multivariate regression model using the data stack. It generates the final output of the analysis.
