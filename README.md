## Code for ‘Ambitious global targets for mangrove and seagrass recovery'

Citation: Buelow et al. (2022) *'Ambitious global targets for mangrove and seagrass recovery'* in **Current Biology**

An interactive web application and associated code can be found [here](https://github.com/cabuelow/target-setting-app)

[About](#about) | [Summary](#summary) | [Scripts](#scripts) | [Data sources](#data-sources) | [License](LICENSE)

### About

This repository provides the code to reproduce all figures and supplemental figures/tables found in Buelow et al. (*under review*). All data underpinning the analyses is publicly available and can be downloaded from ['Data sources'](#data-sources) table below. 

### Summary

There is an urgent need to halt and reverse loss of mangroves and seagrass to protect and increase the ecosystem services they provide to coastal communities, such as enhancing coastal resilience and contributing to climate stability 1,2. Ambitious targets for their recovery can inspire public and private investment in conservation 3, but the expected outcomes of different protection and restoration strategies is unclear. We estimated potential recovery of mangroves and seagrass through gains in ecosystem extent to the year 2070 under a range of protection and restoration strategies implemented until the year 2050. Under a protection-only scenario, the current trajectories of net mangrove loss slowed, and a minor net gain in global seagrass extent (~1%) was estimated. Protection alone is therefore unlikely to drive sufficient recovery. However, if action is taken to both protect and restore, net gains of up to 5% and 35% of mangroves and seagrasses, respectively, could be achieved by 2050. Further, protection and restoration can be complementary, as protection prevents losses that would otherwise occur post-2050, highlighting the importance of implementing protection measures. Our findings provide the scientific evidence required for setting strategic and ambitious targets to inspire significant global investment and effort in mangrove and seagrass conservation.


<p align="center">
  <img width="600" height="350" src="https://github.com/cabuelow/ambitious-targets/blob/main/map.png">
</p>

### Scripts

#### Prepare data for model projections and produce summary information for supplementary material

1. 01_seagrass-country-summary.R
    - Summary for information for seagrass countries (number of sites, and temporal range of observations)
    - Produce Table S2
2. 02_outlier-capping.R
    - Identify and cap annual rate of extent change outliers

#### Project mangrove and seagrass extent under baseline and conservation scenarios

3. 03_scenario-projections_mangrove.R
    - Project mangrove extent to 2070 under baseline, protection and restoration scenarios 
4. 03_scenario-projections_seagrass.R
    - Project seagrass extent to 2070 under baseline, protection and restoration scenarios 

#### Plot mangrove and seagrass projections

7. 04_plot-extent-change.R
   - Calculate extent change summary stats under each scenario
   - Produce Fig 1, a bar plot of extent change (with percentages) under each scenario
8. 05_map-protection-restoration.R
   - Produce Fig 2, a map of relative proportion of observed ecosystem extent required to be protected or restored in each country to reach ambitious conservation targets

#### Sensitivity analyses

9. 06_sensitivity-loss-rates.R
   - Produce panel for Fig S1, test the sensitivity of extent change projections to increases or decreases in annual rates of loss
10. 07_sensitivity-expansion-rates.R
    - Produce panel for Fig S1, test the sensitivity of extent change projections to increases or decreases in annual rates of expansion
11. 08_sensitivity-restorable-area.R
    - Produce panel for Fig S1, test the sensitivity of extent change projections to increases or decreases in restorable area
12. 09_sensitivity-protection-efficacy.R
    - Produce panel for Fig S2, test the sensitivity of extent change projections to the effectiveness of protection
13. 10_sensitivity-protection-assumptions.R
    - Produce panel for Fig S2, test the sensitivity of extent change projections to PADD, leakage and whether seagrass protection is targeted
14. 11_sensitivity-restoration-success.R
    - PProduce panel for Fig S1, test the sensitivity of extent change projections to variable restoration success

### Data sources

| Ecosystem  | Data source(s) for processing/analysis |
| ------------- | ------------- |
| Mangrove | Bunting P., Rosenqvist A., Lucas R., Rebelo L-M., Hilarides L., Thomas N., Hardy A., Itoh T., Shimada M. and Finlayson C.M. 2018. The Global Mangrove Watch – a New 2010 Global Baseline of Mangrove Extent. Remote Sensing 10(10): 1669. doi:10.3390/rs1010669
| Mangrove | Goldberg, L., Lagomasino, D., Thomas, N., & Fatoyinbo, T. 2020. Global declines in human‐driven mangrove loss. Global Change Biology, gcb.15275. https://doi.org/10.1111/gcb.15275
| Mangrove | UNEP-WCMC, IUCN. 2021. Protected Planet: The World Database on Protected Areas (WDPA). Available at: https://www.protectedplanet.net/en
| Seagrass | Dunic, J. C., Brown, C. J., Connolly, R. M., Turschwell, M. P., & Côté, I. M. 2021. Long‐term declines and recovery of meadow area across the world’s seagrass bioregions. Global Change Biology, gcb.15684. https://doi.org/10.1111/gcb.15684
| Both | Flanders Marine Institute. 2020. ESRI Country shapefile and the Exclusive Economic Zones (version 3). Available online at http://www.marineregions.org/ https://doi.org/10.14284/403 |

