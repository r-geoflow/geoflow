#### Data

<u>Access to storage of data source </u>
* **access** : name of access resource to use for reaching sources. This parameter should be a valid accessor id as defined in `geoflow::list_data_accessors()`. For the default access endpoint (http, https), there is no need to attach a software, although for other protocols, this is the pattern that we can assume.
As now, three software access are available, a downloadHandler function associated to the access endpoint / software that would be specific to each software (and might include a checker if the resource exists on the remote access endpoint). As starting point, this function will include arguments file (resource to find/download), path (target path include filename). 
	 - *default* a file local or remote source (not need to specify this key for this usage) and in case of dbtable or dbquery with a software declared in input
	 - *googledrive* for access to google drive, must be declared under the same name (id) as a access listed in **software** section of json configuration file. e.G. `access:googledrive`
	 - *zenodo*  for access to zenodo software and download data resources associated to the Zenodo record based on DOI. e.g. `access:zenodo`
	 - *d4storagehub*  for access to [d4science](https://www.d4science.org/)  storage hub software and download data resources store on d4science workspace e.g. `access:d4storagehub`
	 - *gbif*  for access to [gbif](https://www.gbif.org/) open access biodiversity data ressources e.g. `access:gbif`. See the detailed documentation sheet (in preparation).
	 - *thredds*  for access to [Thredds Data Server](https://www.unidata.ucar.edu/software/tds/current/) and read netcdf datasets e.g. `access:thredds`. See the detailed documentation sheet (in preparation).
```{json}
	{
		"id": "googledrive",
		"type": "input",
		"software_type": "googledrive",
		"parameters": {
			"email": "xxx",
			"token": "****"
		},
		"properties" : {}
	}
```
	- 
	- *otherAccess*(in dev)* other application with api (owncloud, etc)

* **source** : name with extension of files to source. For spatial source, a single source can be used, for other type several files can be declare for a same workflow, they must be on the same extension and separated by a comma (,). 
If sourced files are in local repertory the complete path of file must be declared too. Also a alias must be declared for your file, for this use the typology `alias@file.ext` 
*  **sourceType** : extension of the files declared, must be part of the following list :
	* *`dbtable`* for Database object
	* *`dbview`* *deprecated*
	* *`dbquery`* to execute an SQL statement against a database object
	* *`shp`* for shapefile object
	* *`csv`* for CSV table object. If spatialized, the current geometry field expression supported are `"the_geom", "geom", "wkt", "geom_wkt", "wkb", "geom_wkb"`.
	* *`gpkg`* for geopackage object
	* *`nc`* for netcfd4 object
	* *`other`* for other type of file
* **sourceSql**: a sql request as source 
* **sourceZip**: a boolean argument whether if a zipped version of data file(s) should be created from source files. Default value is `FALSE`
* **sourceZipOnly**:a boolean argument whether if a zipped version of data file(s) only should be created from source files. Default value is `FALSE`

<u>Access to storage and upload data</u>
If data must be uploaded, server must be declared in **software** section of json configuration file.
* **upload**: a boolean argument whether the source data should be uploaded to the software output declared in the geoflow. By default it is `true` to perform upload.
* **uploadSource**: name to give to upload file. Caution, is a name without extension for a db object but must be a `.zip` file for shp object.Must include extention for file upload (zip,shp, gpkg)
* **uploadType** extension of the file to upload, must be in list :
	* *`dbtable`* for Database object
	* *`dbview`* *deprecated*
	* *`dbquery`* for executes an SQL statement against a database object
	* *`shp`* for shapefile object
	* *`csv`* for CSV table object
	* *`gpkg`* for geopackage object
	* *`nc`* for netcfd4 object
	* *`other`* for other type of files
* **workspace**: name of target workspace to connect and upload to remote storage (can also be specified in json configuration). To declare this parameter in data column of entites sheet you must indicate the name or path of workspace followed by the type of software  with `@` key.  Two softwares can be invoke :`geoserver`,`d4storagehub`. e.g.`workspace:MyWorkspace@geoserver` 

<u>Enrich the data content</u>

* **featureType**: set dictionary identifier if declared on FeatureType column of Dictionary sheet.
* **attribute**: declared attributes definition if featureType (*not use, depracated*). It is an alternative usage without dictionary sheet, you can declare the list of attribute item by id with a label and a register. This register must be declared in the configuration file like below  . e.g. `field1[Specie]@eml_species_register,field2[Area]@registre_pyrenees`
* **variable**: declared variables definition if featureType (*not use, depracated*) (same usage than `attribute`)
* **dimensions**: declared data dimensions (only use for netcdf case) 
* **ogc_dimensions**:  declare data dimensions to communicate with ogc services (only use for netcdf case) 
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
* **action**:Adds an entity local action to be run (see specific section for more details)
* **run**: a boolean argument whether if local action must be runs. Default value is `TRUE`
* **action_option** :  capacity to pass option for local action with `action_option` prefix e.g. action_option_nameOfOption:value of option`

<u>Parameter specific to software - Geoserver</u>
* **style**: Used as layer style name(s) for GeoServer action.
* **datastore** : name of target datastore to server declared (can also be specified in json configuration)
* **layername**: name give to layer if need (db, gpkg and shp object)
* **for a 'dbquery' upload type**
	* **sql**: a sql query applicable on sql source to upload.
	* **parameters**: Set parameter definition for setting SQL view parametized layers. It is composed of a name (alias) identically of declared in sql query, a regular expression for validation of parameter values and a list of defaults values.These three arguments must be declared of the order described upper and separated by `,` and each item of defaults values must be separated by `+`. e.g. `parameter:high,^[\d]+$,100000`
	* **geometry**: a concatenation of name of the geometry field and geometry type in the target GeoServer SQL view parameterized layer. They must be indicated in this order and separated by comma(,) e.g. `geom_wkt,Polygon`. Srid is also necessary but collected in the spatial coverage column.
* **for spatial data source type**
	* **cqlfilter**:    add a CQL filter to applied to the sourced data.

#### Adds local actions

* prepare dataset before upload
* navigation inside geoflow
	* data sourced is stock in `entity$data$features`
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
