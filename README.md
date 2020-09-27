# davidsonetal-keensmouse
Repository for code used in Davidson et al publication on marine subsidy and niche diversity in Keen's mice. URL will be given upon publication.

Data are stored on Hakai Institute's data repository: https://hecate.hakai.org/geonetwork/srv/eng/catalog.search#/home. Specific URL available upon publication.

Most code files are still messy and not fully annotated, and call intermediate tables not given. Contact khdavidson if any issues or require access to these data files. See below for outline of which scripts were used for each analysis:
- Capture distance figure and analysis (incl sex ~ dist): capture_distshore.R
- Gender, breeding, etc.: mouse_stats.R
- Forest invertebrate biomass: distshore_forestinvertbiomass.R *new*
- Beach invertebrate biomass: biomass_beachinverts.R
- Guild biomass forest and beach: invert_guild_biomasses.R
- Hair distance trends: distshore_hair.R
- Poo distance trends: distshore_poo.R
- Prey items distance trends isotopes: distshore_focalfood_iso.R *new*
- *old* Testing whether more males/females or reproductive/non-reproductive individuals are found near the beach or not: glmm_new_2july2018 
- Signature comparisons of food items between regions: anova_wilcox_fooditems.R 
- Simple food groups for MixSIAR: region_foodgroups_11232019.R
- Isospace figure: isospace_plot_2020.R *new*
- Final MixSIAR models: mixsiar_new_6nov17.R includes models for both CV and GS. Note: output files are saved in `RESULTS>Data files> summary_CV_model_IND_xt and summary_GS_model_IND_xt (and variations on that name like ‘diagnostics’ instead of ‘summary’) 
- GLMM for MixSIAR (median 50% estimate response var): GLMM_dietproportions_14012019_11182019.R
- 'Sensitivity' GLMM for MixSIAR (5% and 95% CI response var) and LMM for d13C and d15N: GLMM_sensitivity.R 
- NDVI adjacent trap calculations: NDVI_adjcalcs.R
- TAB adjacent trap calculations: TAB_adjcalcs.R
