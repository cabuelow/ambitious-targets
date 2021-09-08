## Code and data for ‘Ambitious targets lead to coastal ecosystem recovery'

Citation: Buelow et al. (*in prep*) *'Ambitious targets lead to coastal ecosystem recovery'*

[About](#about) | [Abstract](#abstract) | [Scripts](#scripts) | [Data sources](#data-sources) | [License](LICENSE)

### About

This repository provides the code and data to reproduce all figures and supplemental figures/tables found in Buelow et al. (*in prep*)

### Abstract

### Scripts

#### Prepare data for model projections and produce summary information for supplementary material

1. 01_outlier-capping.R
    - Identify and cap annual rate of extent change outliers
    - Produce Supplementary Table S1
2. 02_seagrass-country-summary.R
    - Produce summary for information for seagrass countries (number of sites, and temporal range of observations)

#### Project mangrove and seagrass extent under baseline and conservation scenarios

3. 03_scenario-projections_mangrove.R
    - Project mangrove extent to 2070 under baseline, protection and restoration scenarios 
4. 03_scenario-projections_seagrass.R
    - Project seagrass extent to 2070 under baseline, protection and restoration scenarios 

#### Plot mangrove and seagrass projections

5. 04_plot-state-trajectories.R
   - Produce Fig 2, a plot of habitat state trajectories through time under scenarios
6. 05_plot-extent-change.R
   - Calculate extent change summary stats under each scenario
   - Produce Fig 3, a bar plot of extent change (with percentages) under each scenario
7. 06_map-protection-restoration.R
   - Produce Fig 4, a map of required protection and restoration in each country to reach ambitious conservation targets
8. 07_plot-restoration-feasibility.R
   - Produce Fig 5, correlation between projected and empirical restoration rates

#### Sensitivity analyses

9. 08_....

### Data sources
