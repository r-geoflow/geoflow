# geoflow

[![Build Status](https://github.com/r-geoflow/geoflow/actions/workflows/r-cmd-check.yml/badge.svg?branch=master)](https://github.com/r-geoflow/geoflow/actions/workflows/r-cmd-check.yml)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/geoflow)](https://cran.r-project.org/package=geoflow)
[![Github_Status_Badge](https://img.shields.io/badge/Github-0.20231128-blue.svg)](https://github.com/r-geoflow/geoflow)
[![DOI](https://zenodo.org/badge/DOI//10.5281/zenodo.3138920.svg)](https://doi.org//10.5281/zenodo.3138920)

**R engine to orchestrate and run (meta)data workflows**

``geoflow`` provides an engine to facilitate the orchestration and execution of metadata-driven data management workflows, in compliance with FAIR (Findable, Accessible, Interoperable and Reusable) data management principles. By means of a pivot metadata model, relying on the [DublinCore](https://dublincore.org/) standard, a unique source of metadata can be used to operate multiple and inter-connected data management actions. Users can also customise their own workflows by creating specific actions but the library comes with a set of native actions that have been identified as key steps most data managers, in particular actions oriented to the publication on the web of metadata and data resources to provide standard discovery and access services. 

At first, default actions of the library were meant to focus on providing turn-key actions for geospatial (meta)data:

* by creating manage geospatial (meta)data complying with [ISO/TC211](https://committee.iso.org/home/tc211) and [OGC](https://www.ogc.org/standards) geographic information standards (eg 19115/19119/19110/19139) and related best practices (eg. INSPIRE); and
* by facilitating extraction, reading and publishing of standard geospatial (meta)data within widely used software that compound a Spatial Data Infrastructure (SDI), including spatial databases (eg. 'PostGIS'), metadata catalogues (eg. 'GeoNetwork', CSW servers), data servers (eg. GeoServer). 

The library was then extended to actions for other domains:

* biodiversity (meta)data standard management including handling of EML metadata, and their management with DataOne servers,
* in situ sensors, remote sensing and model outputs  (meta)data standard management by handling part of CF conventions, 'NetCDF' data format and OPeNDAP access protocol, and their management with Thredds servers, 
* generic / domain agnostic (meta)data standard managers (Dublin Core, DataCite), to facilitate the publication of data within (meta)data repositories such as [Zenodo](https://zenodo.org) or [DataVerse](https://dataverse.org/). 

The execution of several actions will then allow to cross-reference (meta)data resources in each action performed, offering a way to bind resources between each other (eg. reference 'Zenodo' DOIs in 'GeoNetwork'/'Geoserver' metadata, or vice versa reference 'Geonetwork'/Geoserver' links in 'Zenodo' or EML metadata). The use of standardized configuration files (JSON format) allows fully reproducible workflows to facilitate the work of data and information managers.

Please check the [online documentation](https://github.com/r-geoflow/geoflow/wiki) for more details! **(documentation in preparation)**

For questions about using or contributing to geoflow, you can ask them in the discussions panel: https://github.com/r-geoflow/geoflow/discussions

## Sponsors

Many thanks to the following organizations that have provided fundings for strenghtening the ``geoflow`` package:

<div style="float:left;">
  <a href="https://www.fao.org/home/en/"><img height=100 width=100 src="http://www.fao.org/fileadmin/templates/family-farming-decade/images/FAO-IFAD-Logos/FAO-Logo-EN.svg">
<a href="https://en.ird.fr/"><img src="https://en.ird.fr/sites/ird_fr/files/2019-08/logo_IRD_2016_BLOC_UK_COUL.png" height=100 width=100/></a>
  <a href="https://www.inrae.fr"><img height=100 width=100 src="https://www.inrae.fr/themes/custom/inrae_socle/logo.svg"></a>
</div>
<br>
<div style="float:left;">
  <a href="http://www.cnrs.fr"><img src="http://www.cnrs.fr/themes/custom/cnrs/logo.svg" height=100 width=100/></a>
  <a href="https://inee.cnrs.fr/fr/zones-ateliers"><img src="https://inee.cnrs.fr/sites/institut_inee/files/inline-images/logo-za_0_0.jpg" height=100 width=200/></a>
  <a href="https://letg.cnrs.fr"><img height=100 width=50 src="https://letg.cnrs.fr/plugins/letg/images/letg.png"></a>
</div>

The following projects have contributed to strenghten ``geoflow``:

* **Blue-Cloud** _Blue-Cloud has received funding from the European Union's Horizon programme call BG-07-2019-2020, topic: [A] 2019 - Blue Cloud services, Grant Agreement No.862409._ 

<a href="https://www.blue-cloud.org"><img height=100 width=300 src="https://hackathon.blue-cloud.org/wp-content/uploads/2021/11/Blue-cloud_extended_color.png"/></a> 

* **CCSAFE** 

<a href="https://www6.inrae.fr/cc-safe"><img height=100 width=300 src="https://www6.inrae.fr/var/internet6_national_cc_safe/storage/images/configuration-graphique/haut/logo-cc-safe/34567-1-fre-FR/Logo-cc-safe_inra_logo.png"/></a>

* **G2OI** project, cofinanced by the European Union, the Reunion region, and the French Republic.
<div align="center">
<img src="https://github.com/IRDG2OI/geoflow-g2oi/blob/main/img/logos_partenaires.png?raw=True" height="80px">
<div align="left">
  
## Sponsoring

For geoflow sponsoring/funding new developments, enhancements, support requests, please contact me by [e-mail](mailto:eblondel.pro@gmail.com)

## Citation

We thank in advance people that use ``geoflow`` for citing it in their work / publication(s). For this, please use the citation provided at this link [![DOI](https://zenodo.org/badge/DOI//10.5281/zenodo.3138920.svg)](https://doi.org//10.5281/zenodo.3138920)


