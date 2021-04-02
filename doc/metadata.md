### **Metadata**

The _metadata_ configuration can be constituted of three different inputs:

* *entities*: correspond to the dataset descriptions. [geoflow](https://github.com/eblondel) models each dataset metadata as *entity*. An *entity* is compound by core metadata building blocks essentially inspired from the [Dublin Core metadata standard][https://dublincore.org/] . In order to build richer metadata, each building block can be described with finer levels of information. For more information, see details on how to describe a dataset with geoflow.
* _contacts_: correspond to a simplified directory of contacts (people / organizations) used by geoflow to manage contact/roles associated to *entities*. Having such directory of contacts will simplify the use and binding of *responsible parties* to dataset metadata.
* *dictionary*: allows to describe the structure of datasets. This is required for advanced [geoflow](https://github.com/eblondel) use cases where one wants to produce finer dataset description (dataset structural metadata).

In [geoflow](https://github.com/eblondel), Each type of metadata input can be structured through different input formats, called *handlers*. 



## **How to create a geoflow entity**

### from a spreadsheet (for basic users)

To facilitate the edition and maintenance of metadata, [geoflow](https://github.com/eblondel) opted for a simplified tabular data handler that allows user handling their *entities* (ie dataset metadata) in a table, that could be of different sources (CSV, Excel, Google Spreadsheet, or a database source), and where each table row will represent one single *entity*.

Although handling a dataset metadata description in one tabular data row may facilitate the edition and maintenance, adding rich metadata requires to  introduce some syntax to fill each of the metadata elements. [geoflow](https://github.com/eblondel) has adopted some writing syntax which essentially relies on key/value pairs, which allow to enter various properties in one single tabular data cell (eg. for a column on dataset "Date", the user will be able to specify multiple dates, depending on their nature: creation, revision, publication, etc).

Basic examples of key/value pairs:

* Identifier: ``id:my-metadata-identifier`` (key = `` id``, value = ``my-metadata-identifier`` )
* Creation date: ``creation:2010-01-01`` (key = ``creation``, value = ``2010-01-01``)

Handling such complexity of content within a same cell requires a way to distinguish each of the key/value pairs. Because [geoflow](https://github.com/eblondel) cannot know a priori what metadata content will be filled in by user, key/value pairs within a same cell have to be separated by a _line separator_ which by default is ``_\n`` (underscore symbol followed by a new line).

For example, a user wants to fill in 2 types of dates (creation and publication) associated with an *entity*, the following will be used:

``creation:2020-01-01_`` 

``publication:2020-02-01``

This syntax is later described in depth  for each of the columns required for describing a dataset, and illustrated with concrete examples.

For each column described thereafter, it is indicated whether content for the column considered is Optional (O) / Recommended (R) / Mandatory (M). 

Examples cover both:

* simple usage: where content corresponds to the main metadata text content the user wants to use for the column, in which case, the ``key`` of the metadata element can be omitted.
* advanced usage: where content is separated in metadata sub-elements.

#### Identifier

| **Column name**              | <u>Identifier</u>                                        |
| ---------------------------- | ------------------------------------------------------------ |
| **Definition**               | Provides the unique unambigous identifier for the dataset metadata, and eventual additional identifiers associated with the *entity* (e.g. DOI). Identifiers are expected to be used by information systems / software tools, etc hence it is recommended to use string concatenation (with no space). The user is free to define how the main identifier is compound to guarantee unicity for datasets, and to define it with semantics to make it self-explaining. |
| **Need**                     | Mandatory                                                    |
| **Default key (if omitted)** | ``id`` (unique identifier)                                   |
| **other keys available**     | ``doi``(digital object identifier_geometa_iso19115) |
| **Examples**                 |                                                              |
| Simple usage                 | A simple string representing a unique *entity* identifier (``id``). ``my-metadata-identifier`` , equivalent to ``id:my-metadata-identifier``. |
| Advanced usage               | Main identifier + DOI: <br />``id:my-metadata-identifier_``<br />``doi:10.5281/zenodo.3138920`` |

#### Title

| **Column name**              | <u>Title</u>                 |
| ---------------------------- | ---------------------------- |
| **Definition**               | A name given to the dataset. |
| **Need**                     | Mandatory                    |
| **Default key (if omitted)** | ``title`` (main title)             |
| **other keys available**     | ``alternative`` (short title, subtitle or alternative title)              |
| **Examples**                 |                              |
| Simple usage                 | ``This is my dataset title`` |
| Advanced usage               | *Not applicable*             |

#### Description

| **Column name**              | <u>Description</u>                                           |
| ---------------------------- | ------------------------------------------------------------ |
| **Definition**               | Dataset summary description.                                 |
| **Need**                     | Mandatory                                                    |
| **Default key (if omitted)** | ``abstract`` (basic abstract of the dataset)                 |
| **other keys available**     | ``purpose``()<br />``info`` () <br />``project`` () |
| **Examples**                 |                                                              |
| Simple usage                 | *This is my abstract* , equivalent to **abstract**:*This is my abstract*. |
| Advanced usages              | Abstract extended with supplemental information:  <br />`abstract:This is my abstract_` <br />`info:some more information about this dataset` |

#### Subject

| **Column name**              | <u>Subject</u>                                               |
| ---------------------------- | ------------------------------------------------------------ |
| **Definition**               | Subjects(s) associated to the dataset. A subject can be summarized as thesaurus to which will be associated a set of keywords that describe the dataset. Multiple thesauri and keyword sets can be added to a dataset. Each thesaurus (key) should be set with a comma-separated list of keywords |
| **Need**                     | Recommended                                                  |
| **Default key (if omitted)** | *Not applicable* : <br />Note: To add one subject, the subject name should be the key to use. |
| **Examples**                 |                                                              |
| Simple usage                 | **INSPIRE**:*Atmospheric conditions*,*Environmental monitoring facilities*,*Geology*,*Meteorological geographical features* (a "INSPIRE" "thesaurus" with 4 keywords) |
| Advanced usages              | Adding a hyperlinks to thesaurus and keywords (using @ symbol):<br /><br />**INSPIRE**@[http://www.eionet.europa.eu/gemet/inspire_themes](http://www.eionet.europa.eu/gemet/inspire_themes):*Atmospheric conditions*@[http://inspire.ec.europa.eu/theme/ac](http://inspire.ec.europa.eu/theme/ac),*Environmental monitoring facilities*@[http://inspire.ec.europa.eu/theme/ef](http://inspire.ec.europa.eu/theme/ef),*Geology*@[http://inspire.ec.europa.eu/theme/ge](http://inspire.ec.europa.eu/theme/ge),*Meteorological geographical features*@[http://inspire.ec.europa.eu/theme/mf](http://inspire.ec.europa.eu/theme/mf) |
|                              | Adding 2 thesauri:<br /><br />**General**:*keyword1,keyword2,keyword3*_<br />**INSPIRE**@[http://www.eionet.europa.eu/gemet/inspire_themes](http://www.eionet.europa.eu/gemet/inspire_themes):*Atmospheric conditions*@[http://inspire.ec.europa.eu/theme/ac](http://inspire.ec.europa.eu/theme/ac),*Environmental monitoring facilities*@[http://inspire.ec.europa.eu/theme/ef](http://inspire.ec.europa.eu/theme/ef),*Geology*@[http://inspire.ec.europa.eu/theme/ge](http://inspire.ec.europa.eu/theme/ge),*Meteorological geographical features*@[http://inspire.ec.europa.eu/theme/mf](http://inspire.ec.europa.eu/theme/mf) |

#### Creator

| **Column name**              | <u>Creator</u>    |
| ---------------------------- | ----------------- |
| **Definition**               | Role assigned of contact listed in contact file |
| **Need**                     | Recommended       |
| **Default key (if omitted)** | *Not applicable*  |
| **other keys available**     | ``owner``()<br />``publisher`` () <br />``metadata`` () <br />``pointOfContact`` (other contributors) | 
| **Examples**                 |                   |
| Simple usage                 |*role:email*  e.g. owner:xxx@mail.com                 |
| Advanced usage               |on owner with multiples contributors:  <br />`owner:owner@mail.com_` <br />`pointOfcontact:contact1@mail.com,contact2@mail.com`                   | 

#### Date

| **Column name**              | <u>Date</u> |
| ---------------------------- | ----------- |
| **Definition**               | Dates associated with events in the lifecycle of dataset |
| **Need**                     |Recommended, if no indicate a creation date is automatically crate at the date of the computation|
| **Default key (if omitted)** |``creation``()     |
| **other keys available**     |``publication`` () |
| **Examples**                 |             |
| Simple usage                 |*YYYY-MM-DD*, equivalent to **creation**:*YYYY-MM-DD*. |
| Advanced usage               |`creation:YYYY-MM-DD_` <br />`publication:YYYY-MM-DD`             |

#### Type

| **Column name**              | <u>Type</u> |
| ---------------------------- | ----------- |
| **Definition**               |Type of production |
| **Need**                     |Recommended, if no indicated, *dataset* choose by default |
| **Default key (if omitted)** |``generic``  |
| **other keys available**     |*Not applicable*|
| **Examples**                 |             |
| Simple usage                 |*Dataset*, equivalent to **dataset**:*Dataset*.|
| Advanced usage               |*Not applicable*|

#### Language

| **Column name**              | <u>Language</u> |
| ---------------------------- | --------------- |
| **Definition**               | Language use in the dataset, indicate as abbreviation |
| **Need**                     |Recommended, If no indicated, *eng* is choose by default |
| **Default key (if omitted)** |*Not applicable* |
| **other keys available**     |*Not applicable* |
| **Examples**                 |                 |
| Simple usage                 | *eng*           |
| Advanced usage               |*Not applicable* |

#### SpatialCoverage

| **Column name**              | <u>SpatialCoverage</u> |
| ---------------------------- | ---------------------- |
| **Definition**               |Spatial Coverage and SRID of the dataset |                      |
| **Need**                     |Optional, automatically enrish if no indicate |
| **Default key (if omitted)** |*Not applicable*                         |
| **other keys available**     |*Not applicable* 		|
| **Examples**                 |                        |
| Simple usage                 |must be a valid EWKT string, starting with the SRID definition (e.g. SRID=4326), followed by a semicolon and the WKT geometry ``SRID=4326;POLYGON((-71 -21,-71 28,14 28,14 -21,-71 -21)``)                        |
| Advanced usage               |*Not applicable*                        |

#### TemporalCoverage

| **Column name**              | <u>TemporalCoverage</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |Spatial Coverage and SRID of the dataset |
| **Need**                     |Optional, automatically enrish if no indicate                         |
| **Default key (if omitted)** |*Not applicable*         |
| **other keys available**     |*Not applicable* 		 |
| **Examples**                 |                         |
| Simple usage                 |must be two dates or date and hours in ISO format separated by a forward slash **/**, **e.g.** ``2007-03-01T13:00:00Z/2008-05-11T15:30:00Z ``|
| Advanced usage               |*Not applicable*                          |

#### Relation

| **Column name**              | <u>Relation</u> |
| ---------------------------- | --------------- |
| **Definition**               | Refer  to some external links |
| **Need**                     |Optional                 |
| **Default key (if omitted)** |*Not applicable*           |
| **other keys available**     |``thumbnail``(link to image) <br /> ``parent``(link to parent file) <br /> ``http``(link to web site) <br /> ``wms``(automatically add if concern) <br /> ``wfs``(automatically add if concern) <br /> ``wcs``(automatically add if concern)  |
| **Examples**                 |                 |
| Simple usage                 |                 |
| Advanced usage               |``thumbnail:fileDescription@fileName_`` <br /> ``parent:parentMetadataIdentifier@linkage_`` <br /> ``http:name[description]@linkage_`` <br /> ``wms:layerName[description]@wmsBaseUrl_`` <br /> ``wfs:layerName[description]@wfsUrl_`` <br /> ``wcs:layerName[description]@wcsUrl``          |

each key can be used multiple times to indicate multiple links

#### Rights

| **Column name**              | <u>Rights</u> |
| ---------------------------- | ------------- |
| **Definition**               | Indicate some rights relate with dataset |
| **Need**                     |Optional               |
| **Default key (if omitted)** |*Not applicable*|
| **other keys available**     |``license``()  <br /> ``use``()  <br /> ``uselimitation``()  <br /> ``useconstraint``() <br /> ``accessconstraint``() <br />  ``otherconstraint``()|
| **Examples**                 |               |
| Simple usage                 |rule:key       |
| Advanced usage               |               |

#### Format

| **Column name**              | <u>Format</u> |
| ---------------------------- | ------------- |
| **Definition**               | Indicate information about the data format |
| **Need**                     |Optional               |
| **Default key (if omitted)** |``resource``(format to source data)|
| **other keys available**     |``distribution`` (format to upload data)|
| **Examples**                 |               |
| Simple usage                 |the mormat declaration must be a mimetype``resource:text/csv``  |
| Advanced usage               |a description part can be add to format ``distribution:image/tiff[GeoTIFF]``               |

#### Provenance

| **Column name**              | <u>Provenance</u> |
| ---------------------------- | ----------------- |
| **Definition**               |Add some provenance information and process to format the dataset                  |
| **Need**                     |Optional                   |
| **Default key (if omitted)** |*Not applicable*                   |
| **other keys available**     |``statement`` (name or general description to the provenace or process, mandatory if process and processor are declared)<br /> ``process`` (name and description on each process) <br /> ``processor``(mail of processor relate to the process in the same order of delaration, number of processor must be the same of the number of process. <br /> If the same processor is at the origin of several processes, declare it several times)|
| **Examples**                 |                   |
| Simple usage                 |                   |
| Advanced usage               |``statement:My data management workflow_``<br /> ``process:rationale1[description1],rationale2[description2],rationale3[description3]_``<br /> ``processor:processor1@mail.com,processor2@mail.com,processor3@mail.com``|

#### Data

**access** : name of access ressource to use for reaching sources, must be declared under the same name (id) as a access listed in json configuration file.
source : 
sourceSql
sourceType
sourceZip
sourceZipOnly
**sql** : a sql query (not a sql file link) applicable on sql source to upload.
upload
uploadSource
uploadType : 
cqlfilter
features
**workspace**: a target workspace name for GeoServer action
**datastore**: a target datastore name for GeoServer action
**layername**: nome of layer
styles
**parameters** : Set parameter definition for setting SQL view parametized layers. It is composed of a name (alias) identically of declared in sql query, a regular expression for validation of parameter values and list of defaults values.These three arguments must be declared of the order described upper and separated by `,` and each item of defaults values must be separated by `+`.
geometryField
geometryType
featureType
featureTypeObj
attributes
variables
actions
run





**using R geoflow data model (for advanced R users)**

FOR LATER --> DEVELOPER ORIENTED