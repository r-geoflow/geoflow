#### Data

<u>Access to storage of data source </u>
 - **access** : name of access ressource to use for reaching sources. For the moment two format are supported : 
	 - *default* for local source (not need to specify this key for this usage) 
	 - *googledrive* for access to google drive repertory, must be declared under the same name (id) as a access listed in **software** section of json configuration file.
```{r}
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

* **source** : name with extension of files to source. For spatial source, a single source can be used, for other type several files can be declare for a same workflow, they must be on the same extension and separated by a comma (,). 
If sourced files are in local repertory the complete path of file must be declared too. 
You have the possibility to add a alias to your file, for this use the typology `alias@file.ext` 
*  **sourceType** : extension of the files declared, must be in list :
	* *`dbtable`* for Database object
	* *`dbview`* for 
	* *`dbquery`* for executes an SQL statement against a database object
	* *`shp`* for shapefile object
	* *`csv`* for CSV table object
	* *`gpkg`* for geopackage object
	* *`other`* for other type of files
* **SourceSql**:
* **SourceZip**: a boolean argument whether if a zipped version of the data file(s) should be created from source files. Default value is `FALSE`
* **SourceZipOnly**:a boolean argument whether if a zipped version of the data file(s) only should be created from source files. Default value is `FALSE`

<u>Access to storage and upload data</u>
If data must be upload, declare server in **software** section of json configuration file.
* **upload**: a boolean argument whether the source data should be uploaded to the software output declared in the geoflow. By default is `TRUE` to perform upload.
* **uploadSource**: name to give to upload file. Caution, is a name without extension for a db object but must be a `.zip` file for shp object.
* **uploadType** extension of the file to upload, must be in list :
	* *`dbtable`* for Database object
	* *`dbview`* for 
	* *`dbquery`* for executes an SQL statement against a database object
	* *`shp`* for shapefile object
	* *`csv`* for CSV table object
	* *`gpkg`* for geopackage object
	* *`other`* for other type of files
* **workspace**: name of target workspace to server declared (can also be specified in json configuration)  [Geoserver action]
* **datastore** : name of target datastore to server declared (can also be specified in json configuration)  [Geoserver action]
* **layername**: name give to layer if need (db, gpkg and shp object)

<u>Enrich the data content</u>
* **style**: Used as layer style name(s) for GeoServer action.
* **featureType**: set dictionary identifier if declared on FeatureType column of Dictionary sheet.
* **attribute**: declared attributes definition if featureType not use
* **variable**: declared variables definition if featureType not use

<u>Prepare data</u>
* **action**:Adds an entity local action to be run (see specific section for more details)
* **run**: a boolean argument whether if local action must be runs. Default value is `TRUE`

<u>Specific uses</u>
* **for a 'dbquery' upload type**
	* **sql**: a sql query applicable on sql source to upload.
	* **parameters**: Set parameter definition for setting SQL view parametized layers. It is composed of a name (alias) identically of declared in sql query, a regular expression for validation of parameter values and a list of defaults values.These three arguments must be declared of the order described upper and separated by `,` and each item of defaults values must be separated by `+`.
	* **geometry**: a concatenation of name of the geometry field and geometry type in the target GeoServer SQL view parameterized layer. They must be indicated in this order and separated by comma(,) e.g. `geom_wkt,Polygon`.  The current geometry field expression supported are `"the_geom", "geom", "wkt", "geom_wkt", "wkb", "geom_wkb"`.
* **for spatial data source type**
	*  * **cqlfilter**:    add a CQL filter to applied to the sourced data.

### workfow without format conversion

### workflow with format conversion

#### Adds local actions

* prepare dataset before upload
* navigation inside geoflow
	* Three principals list objects
		* config
		* entity = entity sheet information
		* entity$data
		* 
		* dictionary?
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