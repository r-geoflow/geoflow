## **How to create a geoflow dictionary**
### From a spreadsheet (for basic users)
**Dictionary** allows to describe the structure of datasets. This is required for advanced [geoflow](https://github.com/eblondel) use cases where one wants to produce finer dataset description (dataset structural metadata). 
#### General recommendation
A same dictionary file can be use to refer multiple dictionnaries/data structure definition(dsd). Each dictionnary set is identify by a unique `FeatureType`. This `FeatureType` is the name  to refer in **featureType** key of **Data** column of the **entity** file  to link this dictionnary with dataset. 
Each row of the file refer to a unique column of the dataset. A same column name (MemberCode) can't be use twice for a same `FeatureType`. 

#### Columns description

##### FeatureType

| **Column name**              | <u>FeatureType</u> |
| ---------------------------- | ----------------------- |
| **Definition**               | Unique unambigous identifier to identify a set of dictionary item to use in *Data* column of entity sheet.|
| **Need**                     |Mandatory                |
| **Type of content** 		   |a single expression composed of text,symbol and numeric   |
| **Example**                 |`my_dictionary1_`<br/>`dsd1`       |

##### MemberCode

| **Column name**              | <u>MemberCode</u> |
| ---------------------------- | ----------------------- |
| **Definition**               | exact column name within the target data set|
| **Need**                     |Mandatory                |
| **Type of content** 		   |a single expression composed of text,symbol and numeric without space   |
| **Example**                 |`flag`,`species`        |

##### MemberName
| **Column name**              | <u>MemberName</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |add a human readible name to Member Code (label)  |
| **Need**                     |Optional                |
| **Type of content** 		   | accepted space, text, symbol and numeric |
| **Example**                 |`country flag`,`species name`        |

##### MemberType
| **Column name**              | <u>MemberType</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |indicate the category of each column it is a attribute or a variable  |
| **Need**                     |Optional                |
| **Type of content** 		   | `attribute`or`variable` |
| **Example**                 |`variable`        |

##### MinOccurs
| **Column name**              | <u>MinOccurs</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |manage the minimal number of items of the MemberCode.|
| **Need**                     |Mandatory                |
| **Type of content** 		   | numeric  |
| **Example**                 |`O`, `1`         |

For geoflow usecase related to openfair viewer use min occurs `O` to indicate that selection of item is optional, reversely  with `1` it is mandatory.
##### MaxOccurs
| **Column name**              | <u>MaxOccurs</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |manage the maximal number of items of the MemberCode. |
| **Need**                     |Mandatory                |
| **Type of content** 		   | numeric,' or `Inf`  |
| **Example**                 |`1` ,`Inf`  |
For geoflow usecase is related to openfair viewer use max occurs `1` for enable multiple selection or use `Inf` for no limitation of selection.


##### Definition
| **Column name**              | <u>Definition</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |add a explaination text about the column or a definition of dimension|
| **Need**                     |Optional                |
| **Type of content** 		   | text  |
| **Example**                 |`Definitiion of this variable`        |

##### DefinitionSource
| **Column name**              | <u>DefinitionSource</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |link a uri relate to definition|
| **Need**                     |Optional                |
| **Type of content** 		   |alias@uri  |
| **Example**                 |`My definition@https://link.com`  |

##### MeasurementUnit
| **Column name**              | <u>MeasurementUnit</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |add an information about unit of measure in case of memberType is a variable |
| **Need**                     |Optional                |
| **Type of content** 		   |unit symbol and abbreviation. The best practice is to use the official unit symbol from [udunits](https://www.unidata.ucar.edu/software/udunits). For a complete list of unite see `units::valid_udunits()`|
| **Example**                 |`$`,`kg` |

##### RegisterId
| **Column name**              | <u>RegisterId</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |identifier or a register(reference dataset, code list) giving information about name of R object of table or dataframe type including register information about each item of the colum. In the case of `RegisterScript` is use, `RegisterId` is the name of R function.|
| **Need**                     |Optional                |
| **Type of content** 		   | *please see the register documentation*|
| **Example**                 |`register_month` |

##### RegisterScript
| **Column name**              | <u>RegisterScript</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |a local or remote R script containing the register id objects. Each object should be a function returning a dataframe with 4 columns:<br/>- **code** : exact name of item as write inside the dataframe (mandatory)<br/>- **uri**: a link about item information (optional)<br/>- **label**:label of corresponding item (mandatory) <br/>- **definition**: a definition of each item (optional)<br>see below example |
| **Need**                     |Mandatory if RegisterId declared                |
| **Type of content** 		   |R script path |
| **Example**                 |`C://Mydirectory/Myscript.R` |
```{r} 
# Myscript.R
`register_month <- function(config){  
  out <- data.frame(
    code = as.integer(1:12),
    uri = NA,
    label = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
    definition = NA
  )
  return(out)		
}
}
```
