#### Data

<u>Access to storage of data source </u>
* **access** : name of access resource to use for reaching sources. This parameter should be a valid accessor **id** as defined in `geoflow::list_data_accessors()`. For the default web access endpoint (http, https), there is no need to attach a software, although for other protocols, this is the pattern that we can assume.

Various data accessors are available, each one defined with a `download` function specific to it and to the associated `software`. The `download` function maybe used early to check if a resource exists on the access endpoint, prior to its download. 

The data accessors currently available are: 
 
	 - *default* a local file, remote source (URL), or a named DB resource, in case where the source is of type `dbtable` or `dbquery` (related to a DB software declared as configuration input).
	 - *googledrive* for access to Google Drive. Must be declared under the same name (id) as a access listed in **software** section of json configuration file. Use: `access:googledrive`
	 - *zenodo*  for access to Zenodo software and download data resources associated to the Zenodo record based on DOI(s). Use: `access:zenodo`
	 - *d4storagehub*  for access to [D4science](https://www.d4science.org/)  Storage Hub (workspace) software and download data resources stored on D4science workspace Use: `access:d4storagehub`
	 - *gbif*  for access to [gbif](https://www.gbif.org/) open access biodiversity data ressources e.g. `access:gbif`. See the detailed documentation sheet (in preparation).
	 - *thredds*  for access to [Thredds Data Server](https://www.unidata.ucar.edu/software/tds/current/) and read NetCDF datasets e.g. `access:thredds`. See the detailed documentation sheet (in preparation).

* **source** : name with extension of files to source. For spatial source, at now only a single source can be used (the first one is selected if multiple are declared), for other type several files can be declare for a same workflow, they must be on the same extension and separated by a comma (,). 
If sourced files are in local directory the complete path of file must be declared too. Also a alias must be declared for your file, for this use the typology `alias.ext@file.ext`, this either matches the file name, or provides an alternative file name to use for download. 
*  **sourceType** : type of the files declared, must be part of the following list :
	* *`dbtable`* for Database object
	* *`dbquery`* to execute an SQL statement against a database object
	* *`shp`* for a (zipped) ESRI Shapefile object
	* *`csv`* for CSV table object. If containing spatial information, an attempt will be made to spatialize the data, based on known geometry field names as follows: `"the_geom", "geom", "wkt", "geom_wkt", "wkb", "geom_wkb"`.
	* *`gpkg`* for GeoPackage object
	* *`nc`* for NetCDF4 object
	* *`other`* for other type of file
* **sourceSql**: a plain SQL query to use as source

<u>Access to storage and upload data</u>
If data must be uploaded, the target must be declared in **software** section of the JSON configuration file.
* **upload**: a boolean argument whether the source data should be uploaded to the software output declared in the geoflow. By default it is `true` to perform upload.
* **uploadSource**: name given to uploaded file. Caution, name must be without extension for a DB object, but must be a `.zip` file for a (zipped) ESRI shapefile. The file extension is mandatory for file upload (zip, shp, gpkg).
* **uploadType** type of resource to upload, must be one of these values :
	* *`dbtable`* for Database object
	* *`dbquery`* for executes an SQL statement against a database object
	* *`shp`* for shapefile object
	* *`csv`* for CSV table object
	* *`gpkg`* for geopackage object
	* *`nc`* for netcfd4 object
	* *`other`* for other type of files
* **workspace**: name of target workspace to connect and upload to remote storage (can also be specified in json configuration). To declare this parameter in the `Data` column of entities sheet you must indicate the name or path of workspace followed by the type of software  with the `@` key.  Two softwares can be invoked at now :`geoserver`,`d4storagehub`. e.g.`workspace:MyWorkspace@geoserver` 

<u>Enrich the data content</u>

* **featureType**: set dictionary identifier if declared on `FeatureType` column of Dictionary sheet.
* **attribute**: simplified declaration of attributes definition if `featureType` is not used. It is an alternative usage without dictionary sheet, you can declare the list of attribute item by id with a label, and a reference to a register. This register must be declared in the configuration file (see below JSON example) . e.g. `field1[Specie]@eml_species_register,field2[Area]@registre_pyrenees`
* **variable**: simplified declaration of variables definition if `featureType` is not used. (same usage than `attribute`)
* **dimensions**: declared data dimensions (only used at now for NetCDF case) 
* **ogc_dimensions**:  declare data dimensions to communicate with ogc services (only used at now for NetCDF case) 
```
"registers" : [
	{
		"id": "registre_pyrenees",
		"def": "Register to interrogate ASFIS Species list",
		"script": "registres_pyrenees.R"
	}
  ]
  ```
<u>Data processing</u>
* **action**:Adds an entity local action to be run prior to global actions defined in the JSON configuration file (see specific section for more details)
* **run**: a boolean argument whether if local action(s) must be runs. Default value is `TRUE`
* **action_option** :  capacity to pass option for local action with `action_option` prefix e.g. `action_option_nameOfOption:value of option` would pass an action option named `nameOfOption` with value `value of option`. The use of local action options is interesting when a local action is used for various entities but with different parametrizations.

<u>Parameter specific to software - Geoserver</u>
* **uploadStyle**: a specific boolean argument whether SLD style files (if listed/available in `source`) should be uploaded (specific to GeoServer publishing action).
* **style**: Used as layer style name(s) for GeoServer action. When using multiple styles, the first one will be used as layer default style.
* **store** : name of target store in the target GeoServer (can also be specified in json configuration)
* **layername**: name given to layer. Optional. If omitted, the entity `id` identifier will be used.
* **layertitle**: title given to layer. Optional. If omitted, the entity `title` will be used
* **layerdesc**: description/abstract given to layer. Optional. If omitted, the entity `abstract` will be used
* **layeruri**: URI given to layer. Optional. May be used to add additional URI to layer online resource in ISO 19115
* **for a 'dbquery' upload type**
	* **sql**: a sql query applicable on SQL source to upload.
	* **parameters**: Set parameters definition for setting SQL view parameterized layers. It is compound by a name (alias) matching the one declared in the SQL query, a regular expression for validation of parameter values and a default value.These three arguments must be declared of the order described upper and separated by `,`. e.g. `parameter:high,^[\d]+$,100000`
	* **geometry**: a concatenation of the geometry field and geometry type in the target GeoServer SQL view parameterized layer. They must be indicated in this order and separated by comma(,) e.g. `geom_wkt,Polygon`. Note that the SRID is also required to define a SQL view parameterized layer, but this information is fetched from the entity `SpatialCoverage` element.
* **for spatial data source type**
	* **cqlfilter**: add a CQL filter to apply to the source data.

#### Add local actions

TODO section to REFINE

* prepare dataset before upload
* navigation inside geoflow
	* data source is stored as R object in `entity$data$features` (for vector data) or `entity$data$coverages` (for gridded data)
* convert format to upload
* manage data parameters
	* `setParameter(name, fieldname, regexp, defaultvalue)`
* automatically enrich features
	* `enrichWithFeatures(config)`This function will enrich the entity with data features, but trying to read the spatial data (eg shapefile, sql query - if a database input software is declared in the geoflow config). This method will overwrite spatial metadata such as the bounding box and temporal extent. Note that the user spatial extent is not overwriten since it may content finer geometries than a bounding box.
* automatically enrich relations
	* `enrichWithRelations(config)` This function will enrich the entity with relations. At now this is essentially related to adding relations if a Geoserver (geosapi) publishing action is enabled in which case this function will add 1) a thumbnail link built from OGC WMS service, 2) a WMS protocol relation, 3) WFS data protocols in common formats (GML, GeoJSON, ESRI Shapefile).
* automaticaly enrich metadata
	* `enrichWithMetadata(config)`This function will enrich the entity with metadata. 
* manage metadata keys using directly R function 
	* `setTemporalExtent()`
	* `setSpatialExtent(wkt,bbox,data,crs)`
