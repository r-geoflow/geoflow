### **Metadata**

The _metadata_ configuration can be constituted of three different inputs:

* *entities*: correspond to the dataset descriptions. [geoflow](https://github.com/r-geoflow/geoflow) models each dataset metadata as *entity*. An *entity* is compound by core metadata building blocks essentially inspired from the [Dublin Core metadata standard](https://dublincore.org/) . In order to build richer metadata, each building block can be described with finer levels of information. For more information, see details on how to describe a dataset with geoflow.
* _contacts_: correspond to a simplified directory of contacts (people / organizations) used by geoflow to manage contact/roles associated to *entities*. Having such directory of contacts will simplify the use and binding of *responsible parties* to dataset metadata.
* *dictionary*: allows to describe the structure of datasets. This is required for advanced [geoflow](https://github.com/r-geoflow/geoflow) use cases where one wants to produce finer dataset description (dataset structural metadata).

In [geoflow](https://github.com/r-geoflow/geoflow), Each type of metadata input can be structured through different input formats, called *handlers*. 

## **How to create a geoflow entity**

### from a spreadsheet (for basic users)

To facilitate the edition and maintenance of metadata, [geoflow](https://github.com/r-geoflow/geoflow) opted for a simplified tabular data handler that allows user handling their *entities* (ie dataset metadata) in a table, that could be of different sources (CSV, Excel, Google Spreadsheet, or a database source), and where each table row will represent one single *entity*.

Although handling a dataset metadata description in one tabular data row may facilitate the edition and maintenance, adding rich metadata requires to  introduce some syntax to fill each of the metadata elements. [geoflow](https://github.com/r-geoflow/geoflow) has adopted some writing syntax which essentially relies on key/value pairs, which allow to enter various properties in one single tabular data cell (eg. for a column on dataset "Date", the user will be able to specify multiple dates, depending on their nature: creation, revision, publication, etc).

Basic examples of key/value pairs:

* Identifier: ``id:my-metadata-identifier`` (key = `` id``, value = ``my-metadata-identifier`` )
* Creation date: ``creation:2010-01-01`` (key = ``creation``, value = ``2010-01-01``)

Handling such complexity of content within a same cell requires a way to distinguish each of the key/value pairs. Because [geoflow](https://github.com/r-geoflow/geoflow) cannot know a priori what metadata content will be filled in by user, key/value pairs within a same cell have to be separated by a _line separator_ which by default is ``_\n`` (underscore symbol followed by a new line).

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
| **Definition**               | Provides the unique unambigous identifier for the dataset metadata, and eventual additional identifiers associated with the *entity* (e.g. DOI). Identifiers are expected to be used by information systems / software tools, etc hence it is recommended to use string concatenation (with no space). The user is free to define how the main identifier is compound to guarantee unicity for datasets, and to define it with semantics to make it self-explaining. A good practice could be to use a doi if existing|
| **Need**                     | Mandatory                                                    |
| **Default key (if omitted)** | `id` (unique identifier)                                   |
| **other keys available**     | `uuid`(universally unique identifier) <br/>`doi`(digital object identifier_geometa_iso19115) <br/> `packageId` (use for publication of eml package) |
| **Examples**                 |                                                              |
| Simple usage                 | A simple string representing a unique *entity* identifier (`id`). `my-metadata-identifier` , equivalent to `id:my-metadata-identifier`. |
| Advanced usage               | Main identifier + DOI: <br/>`id:my-metadata-identifier_`<br />`doi:10.5281/zenodo.3138920` |

#### Title

| **Column name**              | <u>Title</u>                 |
| ---------------------------- | ---------------------------- |
| **Definition**               | A name given to the dataset. |
| **Need**                     | Mandatory                    |
| **Default key (if omitted)** | ``title`` (main title)             |
| **other keys available**     | ``alternative`` (short title, subtitle or alternative title)              |
| **Examples**                 |                              |
| Simple usage                 | ``This is my dataset title`` |
| Advanced usage               | ``title:This is my dataset title_``<br/>``alternative:This is an alternative title``           |

#### Description

| **Column name**              | <u>Description</u>                                           |
| ---------------------------- | ------------------------------------------------------------ |
| **Definition**               | Dataset summary description.                                 |
| **Need**                     | Mandatory                                                    |
| **Default key (if omitted)** | ``abstract`` (basic abstract of the dataset)                 |
| **other keys available**     | ``purpose`` <br />``credit`` <br />``info`` <br />``edition``  <br />``status`` <br />``maintenance``  |
| **Examples**                 |                                                              |
| Simple usage                 | *This is my abstract* , equivalent to ``abstract:This is my abstract.`` |
| Advanced usages              | Abstract extended with supplemental information and statut:  <br />`abstract:This is my abstract_` <br />`info:some more information about this dataset_`<br/>`statut:completed` |

#### Subject

| **Column name**              | <u>Subject</u>                                               |
| ---------------------------- | ------------------------------------------------------------ |
| **Definition**               | Subject(s) associated to the dataset. A subject can be summarized as a set of keywords that describes the dataset for a given type of keywords. In addition the keyword set can be associated to a thesaurus. The thesaurus name can be added with in bracket associated to a key. Hyperlink can be added with @. Each keyword can be enriched with hyperlink with @ Multiple keyword sets can be added to a dataset. Keyword should be added with a coma separated list. If default key is omited .  |
| **Need**                     | Recommended                                                  |
| **Default key (if omitted)** | *Not applicable* : <br />Note: To add one subject, the key used will be the type of key word. The subjet name ca be added as bracket |
| **Examples**                 |                                                              |
| Simple usage                 | theme[general]:keyword1,keyword2 |
| Advanced usages              | theme[general]@https://mythesaurus:environment@https://mythesaurus/environment,soil@https://mythesaurus/soil|
|                              |  |

#### Creator

| **Column name**              | <u>Creator</u>    |
| ---------------------------- | ----------------- |
| **Definition**               | Parties having a role in the management of the dataset. Each party should be identified with one identifier in a contact table. For keys specific to a geoflow action please see the table (link to add)|
| **Need**                     | Recommended       |
| **Default key (if omitted)** | *Not applicable*  |
| **other keys available**     | ``owner``<br />``publisher`` <br />``metadata``  <br />``pointOfContact`` (other contributors) | 
| **Examples**                 |                   |
| Simple usage                 |*role:email*  e.g. owner:xxx@mail.com                 |
| Advanced usage               |on owner with multiples contributors:  <br />`owner:owner@mail.com_` <br />`pointOfContact:contact1@mail.com,contact2@mail.com`                   | 

#### Date

| **Column name**              | <u>Date</u> |
| ---------------------------- | ----------- |
| **Definition**               | Dates associated with events in the lifecycle of dataset . For keys specific to a geoflow action please see the table(link to add)|
| **Need**                     |Recommended, if no indicate a creation date is automatically create at the date of the computation|
| **Default key (if omitted)** |``creation``()     |
| **other keys available**     |``publication`` ()<br/> ``edition``() |
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
| Simple usage                 |*dataset*, equivalent to ``generic:dataset``.|
| Advanced usage               |*Not applicable*|

#### Language

| **Column name**              | <u>Language</u> |
| ---------------------------- | --------------- |
| **Definition**               | Language use in the dataset, indicate as abbreviation. Best practice is to use ISO3 |
| **Need**                     |Recommended, If no indicated, *eng* is choose by default |
| **Default key (if omitted)** |*Not applicable* |
| **other keys available**     |*Not applicable* |
| **Examples**                 |                 |
| Simple usage                 | `eng`           |
| Advanced usage               |*Not applicable* |

#### SpatialCoverage

| **Column name**              | <u>SpatialCoverage</u> |
| ---------------------------- | ---------------------- |
| **Definition**               |Spatial Coverage and SRID of the dataset |                      |
| **Need**                     |Optional, automatically enriched if no indicate in case of temporal dataset |
| **Default key (if omitted)** |`ewkt`(must be a valid EWKT string, starting with the SRID definition (e.g. SRID=4326), followed by a semicolon and the WKT geometry)                        |
| **other keys available**     |`srid`(SRID definition, can be use with or without the  `wkt` argument)	<br/> `wkt`(the WKT geometry, can be use with or without the  `srid` argument)	|
| **Examples**                 |                        |
| Simple usage                 |`SRID=4326;POLYGON((-71 -21,-71 28,14 28,14 -21,-71 -21)`)                        |
| Advanced usage               |`srid:4326_`<br/>`wkt:POLYGON((-71 -21,-71 28,14 28,14 -21,-71 -21)`                     |

#### TemporalCoverage

| **Column name**              | <u>TemporalCoverage</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |Temporal extend of the data set, can be a instant or a period |
| **Need**                     |Optional                 |
| **Default key (if omitted)** |*Not applicable*         |
| **other keys available**     |*Not applicable* 		 |
| **Examples**                 |                         |
| Simple usage                 |must be two dates or date and hours in ISO format separated by a forward slash **/**, **e.g.** ``2007-03-01T13:00:00Z/2008-05-11T15:30:00Z ``<br/>``2007-03-01``<br/>``2007/2010``|
| Advanced usage               |*Not applicable*                          |

#### Relation

| **Column name**              | <u>Relation</u> |
| ---------------------------- | --------------- |
| **Definition**               | Refer  to some external links. Each key can be used multiple times to indicate multiple links |
| **Need**                     |Optional                 |
| **Default key (if omitted)** |*Not applicable*           |
| **other keys available**     |``thumbnail``(link to image for graphic overview) <br /> ``parent``(link to parent metadata) <br /> ``http``(link to web site) <br /> ``wms``(link to OGC WMS  ) <br /> ``wfs``(automatically added in case of action is connect)  |
| **Examples**                 |                 |
| Simple usage                 | ``http:name[description]@linkage_``                |
| Advanced usage               |``thumbnail:fileDescription@fileName_`` <br /> ``parent:parentMetadataIdentifier@linkage_`` <br /> ``http:name[description]@linkage_`` <br /> ``wms:layerName[description]@wmsBaseUrl_`` <br /> ``wfs:layerName[description]@wfsUrl_`` |



#### Rights

| **Column name**              | <u>Rights</u> |
| ---------------------------- | ------------- |
| **Definition**               | Description of the access and/or use constraints. Restriction code are map ISO 19115, the 2 constraints are related to code list . Control to term , use limitation can be use for any term of use (license, prefered citation, disclaimer). other constraint free text can be used |
| **Need**                     |Optional               |
| **Default key (if omitted)** |*Not applicable*|
| **other keys available**     |``license``  <br /> ``use``  <br /> ``useLimitation``  <br /> ``termsOfuSe`` <br /> ``disclaimer`` <br /> ``citation`` <br />  ``useConstraint`` <br /> ``accessConstraint`` <br />  ``otherConstraint``|
| **Examples**                 |               |
| Simple usage                 |key:rule       |
| Advanced usage               |`useLimitation:Free usage_` <br/> `accessConstraint:otherRestrictions_` <br/> `useConstraint:intellectualPropertyRights_` <br/> `otherConstraint:The supplier is unable to guarantee the accuracy, updating, integrity, completeness of the data`               |

#### Format

| **Column name**              | <u>Format</u> |
| ---------------------------- | ------------- |
| **Definition**               | Indicate information about the data format . A best practise is to use official mine type [https://www.iana.org/assignments/media-types/media-types.xhtml]|
| **Need**                     |Optional               |
| **Default key (if omitted)** |``resource``(format of original data set)|
| **other keys available**     |``distribution`` (fdata format is with data is distributage)|
| **Examples**                 |               |
| Simple usage                 |the format declaration must be a mimetype``resource:text/csv``  |
| Advanced usage               |a description part can be add to format ``distribution:image/tiff[GeoTIFF]``               |

#### Provenance

| **Column name**              | <u>Provenance</u> |
| ---------------------------- | ----------------- |
| **Definition**               |Add some provenance information and processes to format the dataset                  |
| **Need**                     |Optional                   |
| **Default key (if omitted)** |*Not applicable*                   |
| **other keys available**     |``statement`` (name or general description to the provenace or process, mandatory if process and processor are declared)<br /> ``process`` (name and description on each process) <br /> ``processor``(mail of processor relate to the process in the same order of delaration, number of processor must be the same of the number of process. <br /> If the same processor is at the origin of several processes, declare it several times|
| **Examples**                 |                   |
| Simple usage                 |                   |
| Advanced usage               |``statement:My data management workflow_``<br /> ``process:rationale1[description1],rationale2[description2],rationale3[description3]_``<br /> ``processor:processor1@mail.com,processor2@mail.com,processor3@mail.com``|

#### Data

*Please see metadata_data documentation* 
