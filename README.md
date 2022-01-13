## Code and data for ‘Ambitious targets for mangrove and seagrass recovery'

Citation: Buelow et al. (*under review*) *'Ambitious targets for mangrove and seagrass recovery'* in **Current Biology**

An interactive web application and associated code can be found [here](https://github.com/cabuelow/target-setting-app)

[About](#about) | [Summary](#summary) | [Scripts](#scripts) | [Data sources](#data-sources) | [License](LICENSE)

### About

This repository provides the code and data to reproduce all figures and supplemental figures/tables found in Buelow et al. (*under review*)

### Summary

There is an urgent need to halt and reverse loss of mangroves and seagrass to protect the ecosystem services they provide, such as enhancing coastal resilience and contributing to climate stability. Ambitious targets for their recovery can inspire public and private investment in conservation, but the expected benefits of different protection and restoration strategies is unclear. We estimated potential recovery of mangrove and seagrass ecosystems to the year 2050 under a range of intermediate and ambitious protection and restoration strategies. We measured recovery as gains in ecosystem extent against a baseline scenario of no additional conservation. By 2050, protection alone is unlikely to drive sufficient recovery, achieving a maximum global net gain in seagrass extent of only 1.3% additional to total observed extent, and slowing, but not stopping, current trajectories of net mangrove loss. However, if action is taken to both protect and restore, net gains of up to 35% and 5% of seagrasses and mangroves, respectively, could be achieved by 2050. Further, protection and restoration are complementary, as protection prevents losses that would otherwise occur post-2050. Our findings provide the scientific evidence required for setting strategic and ambitious targets to inspire significant global investment and effort in mangrove and seagrass conservation. 

### Scripts

#### Prepare data for model projections and produce summary information for supplementary material

1. 01_seagrass-country-summary.R
    - Summary for information for seagrass countries (number of sites, and temporal range of observations)
    - Produce Table S2
2. 02_outlier-capping.R
    - Identify and cap annual rate of extent change outliers
    - Produce Table S3

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
   - Produce Fig S1, test the sensitivity of extent change projections to increases or decreases in annual rates of loss
10. 07_sensitivity-expansion-rates.R
    - Produce Fig S2, test the sensitivity of extent change projections to increases or decreases in annual rates of expansion
11. 08_sensitivity-restorable-area.R
    - Produce Fig S3, test the sensitivity of extent change projections to increases or decreases in restorable area
12. 09_sensitivity-protection-efficacy.R
    - Produce Fig S4, test the sensitivity of extent change projections to the effectiveness of protection
13. 10_sensitivity-protection-assumptions.R
    - Produce Fig S5, test the sensitivity of extent change projections to PADD, leakage and whether seagrass protection is targeted
14. 11_sensitivity-restoration-success.R
    - Produce Fig S6, test the sensitivity of extent change projections to variable restoration success

### Data sources

| Ecosystem  | Data source(s) for processing/analysis |
| ------------- | ------------- |
| Mangrove | Bunting P., Rosenqvist A., Lucas R., Rebelo L-M., Hilarides L., Thomas N., Hardy A., Itoh T., Shimada M. and Finlayson C.M. 2018. The Global Mangrove Watch – a New 2010 Global Baseline of Mangrove Extent. Remote Sensing 10(10): 1669. doi: 10.3390/rs1010669.
| Mangrove | Goldberg, L., Lagomasino, D., Thomas, N., & Fatoyinbo, T. 2020. Global declines in human‐driven mangrove loss. Global Change Biology, gcb.15275. https://doi.org/10.1111/gcb.15275
| Mangrove | UNEP-WCMC. 2017. World Database on Protected Areas User Manual 1.5. UNEP-WCMC: Cambridge, UK. Available at: http://wcmc.io/WDPA_Manual; |
| Seagrass | Dunic, J. C., Brown, C. J., Connolly, R. M., Turschwell, M. P., & Côté, I. M. 2021. Long‐term declines and recovery of meadow area across the world’s seagrass bioregions. Global Change Biology, gcb.15684. https://doi.org/10.1111/gcb.15684
| Seagrass | Waycott, M., Duarte, C. M., Carruthers, T. J. B., Orth, R. J., Dennison, W. C., Olyarnik, S., … Williams, S. L. 2009. Accelerating loss of seagrasses across the globe threatens coastal ecosystems. Proceedings of the National Academy of Sciences of the United States of America, 106, 12377–12381. |
| Both | Flanders Marine Institute. 2020. ESRI Country shapefile and the Exclusive Economic Zones (version 3). Available online at http://www.marineregions.org/ https://doi.org/10.14284/403 |

