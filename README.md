# ICP-NPI
**Analysis**: Meghan Hutch & Dr. Charlene Ong

## Objective
To evaluate the temporal relationships between intracranial pressure (ICP) and pupil reactivity in critically ill patients. 

Leveraging data from two intensive care units in the Boston area, this study evaluates trends in ICP and pupil metrics in patients with traumatic brain injuries, intraparenchymal hemorrhage, and aneurysmal subarachnoid hemorrhage. Pupil data was recorded using Quantiative Pupillometers from **[NeurOptics](https://neuroptics.com/)**.

The scripts in this repository clean and aggreagte ICP and pupillometry data in order to facilitate statistical analysis of the trends of these two physiological biomarkers and associated patient health outcomes.

## Exploratory Analysis

**[View Exploratory Analysis](https://meghutch.github.io/ICP-NPi/NPi_and_ICP_Analysis.html)**

## Perform the Analysis

Currently, raw data is stored privately in the Ong Lab. If to have access to the data, the respository can be copied and run locally. 

Clone this repository to your local system by typing the following into your terminal/shell:

````
git clone https://github.com/meghutch/ICP-NPI.git
````

Alternatively, follow these helpful instructions for **[setting up git in Rstudio](https://gge-ucd.github.io/R-DAVIS/setting_up_git.html)**

## Directory Structure

```
├── README.md                         <- You are here
|
├── .gitignore                        <- file indicating the folders not to upload to the repo (data confidentiality purposes)
|
├── analysis/
│   ├── NPi_and_ICP_Analysis.Rmd      <- Primary script to run the analysis
|   ├── Table1.Rmd                    <- Format Table1
|
├── data_processing/                  <- Scripts to process data
|   ├── 1_Clean_ICP_Pupils_Data.Rmd   <- Clean ICP and Pupillometry data
|   ├── 2_Merge_Pupil_ICP_Data.Rmd    <- Merge cleaned ICP and Pupillometry data within structured time intervals
|   ├── 3_EVD_Drainage.Rmd            <- Processed EVD drainage
|   ├── 4_Demographics.Rmd            <- Process REDCap demographics data
|   ├── 5_NPI_ICP_Burden.Rmd          <- Calculate burden of NPi and ICP
|
|── R/                                <- Functions to perform the analysis
|   ├── time_sensitivity_funcs.Rmd    <- Functions to perform the NPi_and_ICP_Analysis.Rmd

```

## Run Analysis

To run the analysis: 

1. First, in the root project directory, create two empty folders: 1) **processed_data** and 2) **results**

2. Next, pre-process the data using scripts 1-5 in the **data_processing/** folder. Scripts should be run in numerical order. 

3. In the **analysis** folder, first run **Table1.Rmd**

4. Lastly, run **NPi_and_ICP_Analysis.Rmd** to run the preliminary analysis. Knit to generate the markdown file.
