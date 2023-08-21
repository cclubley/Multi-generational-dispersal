# Multi-generational-dispersal and dynamic patch occupancy reveals spatial and temporal stability of seascapes.

### Repository contains the data and code used to generate the figures and tables in the paper.

**Process for running the scripts:**
- Step 1: Create a settlement grid that includes only those areas of the study domain that contain suitable habitat for Pacific oysters by running the scripts in the '(1)-Settlement-grid' folder
- Step 2: Generate initial spawning locations for the model using the script 'Initial_spawning_locations.R' in the '(2)-Initial-spawning-locations' folder - *The Pacific oyster distirbution data was collated from four online repositories and cleaned, and the steps for doing so can be found in the separate 'Assessing-invasion-dynamics' repository on my profile*
- Step 3: Run the biophysical model for the year 2000 using the script 'HPC_2000_228_100000.py' in the '(3)-Biophysical-models/2000' folder
- Step 4: Convert the output of the model to a .csv file using the script in the '(4)-Post-processing/csv-conversion' folder
- Step 5: Use the .csv file to generate new spawning locations for the year 2001, using the 'HPC_spawning_file_2000.R' script in the '(4)-Post-processing/New-spawning-locations' folder
- Step 6: Repeat steps 3-5 for each year until 2012
- Step 7: Format the resultant .csv files for use in igraph analysis using the scripts in the '(4)-Post-processing/Format-connectivity-file' folder
- Step 8: Create an igraph, and save the R environment, for each year using the two scripts in the '(4)-Post-processing/iGraph-creation' folder

**Folder/file descriptions:**

| First Header  | Second Header |
| ------------- | ------------- |
| Content Cell  | Content Cell  |
| Content Cell  | Content Cell  |
