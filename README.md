# davidsonetal-keensmouse
Repository for code used in Davidson et al publication on marine subsidy and niche diversity in Keen's mice. Code will be uploaded upon publication.

Data are stored on Hakai Institute's data repository: https://hecate.hakai.org/geonetwork/srv/eng/catalog.search#/home. Specific URL available upon publication.

Most code files are still messy and not fully annotated, and call intermediate tables not given. Contact khdavidson if any issues or require access to these data files. See below for outline of which scripts were used for each analysis:
- Capture distance figure and analysis: capture_distshore
- Gender, breeding, etc.: mouse_stats
- Forest invertebrate biomass: biomass_forestinverts
- Beach invertebrate biomass: biomass_beachinverts
- Guild biomass forest and beach: invert_guild_biomasses 
- Hair distance trends: distshore_hair  
- Poo distance trends: distancefromshore_isopoo
- Prey items distance trends isotopes: distshore_keyfooditems
- Testing whether more males/females or reproductive/non-reproductive individuals are found near the beach or not: glmm_new_2july2018 
- Signature comparisons of food items between regions: anova_wilcox_fooditems 
- Simple food groups for MixSIAR: region_foodgroups_11232019 
- Isospace figure: isospace_6nov17
- Final MixSIAR models: mixsiar_FINAL mixsiar_new_6nov17 includes models for both CV and GS. Note: output files are saved in `RESULTS>Data files> summary_CV_model_IND_xt and summary_GS_model_IND_xt (and variations on that name like ‘diagnostics’ instead of ‘summary’) 
- GLMM for MixSIAR: GLMM_dietproportions_14012019_11182019. 
- NDVI adjacent trap calculations: NDVI_adjcalc
- TAB adjacent trap calculations: TAB_adjcalc
