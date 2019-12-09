# Reading notes

## Pre-reading premises & notes

### "Angles" to the study

(Listed more-or-less from broadest to most specialised.)

- Macroecological theory/general questions:
  - What determines the species richness in an area?
  - What causes areas to differ in species completents?
- Mediterranean biodiversity hotspot questions:
  - How and why do these hotspots arise?
  - How do these hotspots differ in their function (particularly w.r.t. patterns of species occurence)?
    - $\rightarrow$ Comparing the Cape and SWA
- The importance of environmental heterogeneity (EH) in macroecological models of species richness and turnover
  - Identifying EH as possible mechanism by demonstrating its patterns of covariation with species richness and turnover
  - The importance of soil data (and how bad SoilGrids data is!)
- Following on from Cramer & Verboom (2016. _J. Biogeog._)
  - The importance of studying the Cape flora

### Key findings to the study

Hypothesis         | Cape vs SWA       | $\checkmark$?
------------------:|:-----------------:|:-------------:
Degree of EH       | >                 | $\checkmark$
Scale EH           | <                 | $\checkmark$
Floristic turnover | >                 | $\checkmark$
$S \sim$ EH        | Both              | $\checkmark$
Types of EH        | Topography? Soil? | $\frac{1}{2}$

- The Cape is more environmentally heterogeneous than SWA
  - Consequently more species rich
  - Greater floristic turnover supports this.  
- Different axes of EH are biologically important in the Cape and SWA.

### Interpreting the major correlates of species richness and turnover

Summary of drivers of patterns of species occurence:

- Cape $\sim$
  - Productivity (MAP, NDVI, SoilC, SurfT)
  - Soil
    - ... conditions (pH)
    - ... variety (RClay, RCEC, RpH)
  - Topography (RElev, RSurfT)
- SWA $\sim$
  - Productivity (MAP, NDVI, SoilC)
  - Climatic gradients (PDQ, RElev, RMAP)
  - CEC

### Issues and/or related topics to address

- Theoretical
  - Species-area relationships
  - Whittaker's additive model ($\gamma = \alpha + \beta$)
- Methodological
  - Species turnover indices
  - Spatial autocorrelation (not an issue b/c (1) autocorrelated predictors and (2) permutation tests)
  - Temporal range of data sources << that of the question(s)
- Application
  - Furthering generality of the importance of EH
    - Comment on soil data quality sensu GCFRSoils paper (Cramer et al. In rev. _J. Biogeog._)
  - Conservation importance of heterogeneous environments
  - Differing environments of different hotspots = different management/priorities

## Reading notes proper

### Autocorrelation

#### Legendre 1993. _Ecology_

- NB paper outlining how to identify, remedying and accounting for spatial autocorrelation in data
- Notes the intuitive prevalance of of spatial structure in ecological datasets
- "Spatial heterogeneity is then _functional_ in ecosystems, and not the results of some random, noise-generating process, [...]"

### Biodiversity hotpots

#### Cook et al. 2015. _New Phytol._

- NB summary of mediterranean region mechanisms and hypotheses (see my summary list below), with references for each
- NB ref for SWAFR delimitation
- Found that steep climatic gradients contribute to SWAFR richness

Hypotheses for high plant diversity in mediterranean regions[^1]:

1. Diversification
    a. **Speciation b/o ecological specialisation**
    b. Low disperal rates
    c. Low extinction rates (b/of climatic stability)
    d. High speciation rates (e.g. b/o shorter generation times b/o fire)
    e. Pollinator specialisation
2. Co-existence
    a. **Spatial heterogeneity**
    b. Disturbance
    c. Low competition b/o ecological/life-history specialisation
3. Time for species accumulation
    a. Climatic stability
    b. Stable oligotrophic soils
    c. Pre-adaptation for xerophylly/sclerophylly

[^1]: Hypotheses/aspects that my study concerns are in bold

#### Molina-Venegas et al. 2015. _J. Biogeog._

- Plant communities show strong structuring compositionally, phylogenetically and functionally among elevational belts in the Western Mediterranean biodiversity hotspots.
- Elevation + temperature correlations

#### Noroozi et al. 2018. _Sci. Reports_

"Both endemic richness and degree of endemicity were positively related to topographic complexity and elevational range."

#### Zanne et al. 2018. _New Phytol._

"Nonlinear relationships between species and their environments are believed common in ecology and evolution, including during angiosperms’ rise to dominance."

### Cape & SWA studies

#### Beard et al. 2000. _J. Biogeog._

"The Italian mediterranean zone also contains about this number but in a smaller area, while the much smaller Cape Floristic Region has almost twice as many native species."

"While those life forms are prominent in the Cape, that region differs in the great importance of herbaceous families and succulents, both of which are virtually absent from Western Australia."

"It is suggested that the dominant factor shaping the South-West Province flora is the extreme poverty of the area’s soils, a feature that emphasizes sclerophylly, favours habitat specialization and ensures relatively many local endemic species."

#### Born et al. 2007. _J. Biogeog._

(The paper that "defined" the GCFR most recently)

On p. 158, under "Richness and endemism":

> [...] which is probably a consequence of high topographic diversity and rainfall patterns in these regions.

#### Hagger et al. 2017. _PeerJ_

- <https://www.researchgate.net/project/Soil-climate-or-both-Which-variables-are-better-predictors-of-the-distributions-of-Australian-shrub-species>
- The importance of both soil and climate variables in SDMs of Australian shrubs!
- Only soil is not the best, but it a climate+soil model is better than an only climate model.

#### Hopper 1979. _Ann. Rev. Ecol. Syst._

##### General zonations

- SWA has high levels of endemism (up to 80% of the flora)
- "the rich flora of the southwest is all the more remarkable in view of its low relief and subdued topography [...] [in] contrast [to] other regions of the world rich in endemic species [which] have much higher and more extensive moun- tain systems, e.g. South Africa, California, Turkey and Greece, and some oceanic islands"
  - The Great Plateau of Western Australia dominant (< 500m)
  - Relatively high relief only along SWA southern and western margins (< 1000m)
- Rainfall-based zonation:
  1. "High-rainfall zone (800-1400 mm annual rainfall)"
    - "forests and woodlands dominated by jarrah (Eucalyptus marginata), marri (E. calophylla), and karri (E. diversicolor)"
    - "occupies the lower southwest and extends northwards along the Swan Coastal Plain and Darling Range to the vicinity of Perth"
  2. "Transitional-rainfall zone (300-800 mm annual rainfall)"
    - "woodlands, mallees and heathlands-reaches the west coast from Perth to Shark Bay, extends southeast inland of the forests through an area known locally as the Wheatbelt"
    - "occurs as an elongate strip along the south coast eastward to Israelite Bay"
  3. "Arid zone (less than 300 mm annual rainfall)"
    - "Eucalyptus woodlands, Acacia shrublands, and Triodia hummock grassland"
    - "occupies much of central Australia and is marginal to the area under review, extends along the western coast northwards of Shark Bay to the tropics and along the south coast from Israelite Bay eastward across the low open shrubland of the Nullarbor Plain to temperate South Australia"
- Soil-based zonation:
  - "Although there is an approximate parallel between vegetation structure and annual rainfall across these three zones, soil characteristics are of primary importance in determining local distribution patterns of vegetation" (many refs in here for this)

##### Patterns of richness

- Notably rich flora of the transitional zone
  - _Similar_ but not exactly the same as in my study
  - But, this zone is bigger, and has roughly the number of species you'd expect given the size of a SWAFR sub-area
  
##### Landscape development in the Tertiary & Quaternary

###### The Tertiary

####### Eocene

A "mosaic of numerous islands and peninsulas" during the "maximum marine intrusion onto the Australian continent during the Eocene"

####### Post-Eocene geomorphologies

These may have positively affected the diversity of habitats and speciation.

1. Uplift of the Great Plateau and its peripheral sedimentary basins
2. Lateritic soil development during the Oligocene--Miocene $\because$ of regular supply of seasonal rainfall $\rightarrow$ weathering/"differential erosion of lateritic landforms" $\rightarrow$ soil mosaic/soil texture varying regionally?

$\rightarrow$ Soil and topography mosaic $\because$ of palaeo drainage patterns and rainfall.

These patterns are most pronounced in the contemporary transitional rainfall zone. 

###### The Quaternary

Hypothesised aridification (similar to GCFR)

##### Conclusions

- Species richness of heathlands (transitional rainfall zone)
- Due to relatuvely recent geological changes
- High levels of endemism and richness b/o:
  1. Tertiary-generated topographic and edaphic diversity of habitats persist to today
  2. Marine, edaphic and/or climatic barriers isolating SWA
  3. Lateritic soil formation $\rightarrow$ sclerophyllous flora (diverse)
  4. Repeated climatic stress in the transitional rainfall zone

#### Power et al. 2017. _J. Biogeog._

"Genus- and family-level dissimilarities between biomes were positively correlated with differences in LAI"

#### Richardson et al. 2001. _Nature_

"diversification began approximately 7--8Myr ago, coincident with extensive aridification caused by changes in ocean currents. The recent origin of endemic species diversity in the Cape fora shows that large continental bursts of speciation can occur rapidly over timescales comparable to those previously associated with oceanic island radiations"

#### Shane et al. 2008. _Plant, Cell Enviro._

- Species-specific respones of Proteaceae to soil P availability in the context of high levels of habitat turnover $\rightarrow$ likely a big part of maintaining high levels of coexistence in CFR (and SWA)
- The importance of soils and root traits!

### Community assembly/ecology

#### Ricklefs 1987. _Science_

- Local diversity $\sim$
  - "deterministic outcome of local processes within a biological community" in a "small, ecologically homogeneous area", e.g.:
    1. Competition
    2. Predation
    3. Disturbance (= spatial heterogeneity)
  - and regional-scale processes, e.g.:
    1. Disersal & migration
    2. Speciation & extinction

My thoughts: as local determinism and history are both important, we should not expect our BRT-models to predict richness 100%---there are necessarily elements missing from our study and that's fine! We only wish to tackle the local determinism + 1/2 aspect of history: in-situ ecological speciation and facilitation of the arrival of diverse assemblages in a region through the existance of diverse habitats there---i.e. environmental heterogeneity.

### Species-turnover/$\beta$-diversity/biome-boundary studies

#### Konig et al. 2017. _Glob. Ecol. Biogeog._

- "Beta diversity describes two independent sources of composi-tional variation: the replacement or turnover component, reflecting the amount of change in species identities among study sites, and the nestedness component, reflecting the compositional variation owing to differences in species numbers (Lennonet al., 2001; Baselga, 2010; Legendre, 2014). We quantified compositional variation using the $\beta_{sim}$ index which is insensitive to richness differences and thus only quantifies the turnover component of beta diversity (Baselga, 2010)"
- "The distance decay of similarity was stronger for mainland units than for islands. Among taxonomic and functional groups, the rate of decay was lowest for pteridophytes and highest for shrubs"

#### Pinto-Ledezma et al. 2018. _Front. Ecol. Evol._

- "Understanding why species composition and diversity varies spatially and with environmental variation is a long standing theme in macroecological research"
- "[Little] attention has been invested in explaining patterns of beta diversity"
- Expected increasing $\beta$-diversity to associate with biome boundaries b/o:
  - Contrasting environments (Re: my study)
  - Differing patterns of climatic stability
- Undertanding ecological and evolutionary process across spatial scales is very NB
  - Analysing $\btea$-diversity aids this!
  - The link between biodiversity and environmental heterogeneity is species turnover
- $\beta$-diversity = species turnover (i.e. replacement) + nestedness
- Few studies consider $\beta$!!
- Link to Power et al. (2017):
  - Niche conservatism evidence: habitat conservatism in lineages and rarity of biome transitions
  - Cf. commonality of biome transitions over evolutionary time (often at boundaries)
- $\beta$-diversity-nestedness is higher at high northern latitudes $\because$ of more geologically recent colonisation. Cf. mediterranean and deciduous forest biomes with lower nestedness $\because$ of greater species persistance and accumualtion.
- They also found a lot of spatial autocorrelation among measures of diversity
  - They interpret this as shared processes driving these patterns
  - I note that if at any point I draw comparisons or even regress $S$ and $T$ or $J$, I need to use spatially explicit models (e.g. GWR), $\because$ these measures are not intrinsically spatial, per se. Cf. environmental conditions and heterogeneity, which is in a sense more spatial, and thus captures the correlation struture when used a predictor of $S$ or $T$ or $J$, thus precluding the need for something like a GWR.

#### **Also see Power et al. (2017) above**

### Environmental heterogeneity studies

#### Dufour et al. 2006. _Ecography_

- "simple hypothesis that local species diversity is related to spatial environmental heterogeneity"
- "change in underlying mechanisms" across scales, but EH was NB across all scales
- They took into account spatial autocorrelation directly in their models...
- The roles of plant-plant competition at small scales vs spatial configuration of habitats at larger scales

#### Jimenez & Ricklefs. 2014. _Glob. Ecol. Biogeog._

- Greater proportion of climatic variation in CFR explained by spatially explicit sinusoidal patterns of climatic heterogeneity than in SWA, which, conversely, is more explained by spatially explicit linear patterns of climatic heterogeneity

### Jucker et al. 2018. _Ecol. Letters_

- <https://onlinelibrary.wiley.com/doi/full/10.1111/ele.12964>
- "We found that subtle differences in elevation – which control soil chemistry and hydrology – profoundly influenced the structure, composition and diversity of the canopy"

#### Meynard et al. 2011. _Glob. Ecol. Biogeog._

- This study partitions $\gamma$ into $\alpha$ and $\beta$, where $\beta$ is treated as the average turnover between localities and the broader region
- SAR and partition regression analyses (SAR is very common in diversity studies!)
- Results support extension of relationships between taxonomic $\gamma$, $\beta$ and $\alpha$ to their phylogenetic and functional equivalents
- "[R]egions with higher vegetation productivity [...] and regions with a mosaic of landscape features [...] should [...] harbour higher diversity (Hawkins et al. 2003; Rahbek et al. 2007)"

#### Reed et al. 1993. _J. Veg. Sci._

- The lack of generality and power from stuides considered at only one spatial scale
- Recommendation to include more than one scale in ecological studies where-ever possible
- Hypothesised that environmental axes that matter should be similar across scales
- Competition at small scales vs environmental patterns at broader scales

#### Tamme et al. 2010. _J. Veg. Sci._

- Large, regional scale heterogeneity (like that in my study) is positively associated with diversity (with reasons like those in my study)
- "Spatial heterogeneity occurs simultaneously at different scales."
- "$\beta$-scale processes", "$\beta$-scale patches", "$\beta$-niches"
- Grain = scale at which heterogeneity is measured, or patch-size
- They suggest grain as being most NB of scale things

### Phylogenetic $\beta$-diversity

#### Graham & Fine 2008. _Ecol. Letters_

- Scale-dependence (spatial, taxonomic, temporal) of ecological and evolutionary questions
- Phylogenetic turnover NB in this!

## Reading list

- http://www.pnas.org/content/early/2018/05/17/1721464115?collection=
- http://www.pnas.org/content/early/2018/05/17/1802091115?collection=
- http://www.pnas.org/content/early/2018/05/15/1720141115?collection=
- http://onlinelibrary.wiley.com/doi/10.1111/jbi.13150/abstract
- http://onlinelibrary.wiley.com/doi/10.1111/geb.12711/abstract
- http://onlinelibrary.wiley.com/doi/10.1002/ecy.2129/full
- http://onlinelibrary.wiley.com/doi/10.1111/ecog.03416/full
- https://onlinelibrary.wiley.com/doi/full/10.1111/ddi.12736
- http://onlinelibrary.wiley.com/doi/10.1111/1365-2435.13079/full
- http://onlinelibrary.wiley.com/doi/10.1111/ecog.02596/full
- http://onlinelibrary.wiley.com/doi/10.1111/jbi.13164/abstract
- https://onlinelibrary.wiley.com/doi/10.1111/geb.12887
- https://www.researchgate.net/publication/330527918_Environmental_filtering_predicts_plant-community_trait_distribution_and_diversity_Kettle_holes_as_models_of_meta-community_systems
- https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2745.13158
- https://www.researchgate.net/publication/332253146_Time_explains_regional_richness_patterns_within_clades_more_often_than_diversification_rates_or_area