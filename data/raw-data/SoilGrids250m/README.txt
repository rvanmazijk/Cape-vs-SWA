**README COPIED FROM /RUAN_UCT/GIS/**

(Some) ISRIC SoilGrids250m data layers
Downloaded by Ruan van Mazijk

These data are some of what I used during my Honours project for comparing the environmental correlates of vascualr plant species richness in the Cape (Greater Cape Florsistic Region, GCFR) and Southwest Australia (Southwest Australia Floristic Region, SWAFR).

Introduction
============

I downloaded the tiles (e.g. 01--06) on the map that covered the GCFR and SWAFR for the following layers, at all depths (sl1--sl7):

- AWCh1 (???)
- BLDFIE (Soil bulk density)
- CECSOL (Soil cation exchange capacity)
- CLYPTT (Clay proportion)
- CRFVOL (Soil coarse particle volumetic
- OCDENS (Soil carbon)
- PHIKCL (Soil pH (KCl))
- SLTPPT (Silt proportion)
- SNDPPT (Sand proportion)

These tiles (e.g. 01--06) were then stitched together for each region for each variable, reprojected to a standard co-ordinate reference system (WGS48), and re-aggregated to 0.05 degree resolution (a.o.t. 250m resolution).

Directory structure
===================

Take the GCFR as an example (same for SWAFR/)

	---- GCFR/
		|
		|---- depths-separate-tiles/
		|		e.g. <variable>_M_<depth no.>_250m_<tile no.>.tiff
		|
		|---- depths-separate/
		|		e.g. <variable>_M_<depth no.>_250m.tiff
		|
		\---- depths-averaged/
			|
			|---- untransformed/
			|	e.g. <variable>_M_250m.tiff
			|
			|---- reprojected-WGS84/
			|	e.g. <region>_<variable>_M_250m_std_CRS.tiff
			|
			\---- reprojected-WGS84-0.05/
				e.g. <region>_<variable>_M_250m_std_CRS_0.05.tiff
