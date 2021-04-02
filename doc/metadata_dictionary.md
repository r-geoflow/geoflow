## **How to create a geoflow dictionary**
### from a spreadsheet (for basic users)
**dictionary** allows to describe the structure of datasets. This is required for advanced [geoflow](https://github.com/eblondel) use cases where one wants to produce finer dataset description (dataset structural metadata). 
#### General recommendation
A same dictionary file can be use to refer multiple dictionnary. Each dictionnary set is identify by a unique FeatureType. This FeatureType is the name  to refer in **featureType** key of **Data** column of the **entity** file  to link this dictionnary with dataset. 
Each row of the file refer to a unique column of the dataset. A same column name (MemberCode) can't be use twice for a same Feature Type. 

#### Columns description

##### FeatureType

| **Column name**              | <u>FeatureType</u> |
| ---------------------------- | ----------------------- |
| **Definition**               | Unique unambigous identifier to identify a set of dictionary item to use in *Data* column of entity sheet.|
| **Need**                     |Mandatory                |
| **Type of content** 		   |a single expression composed of text,symbol and numeric   |
| **Example**                 |`my_dictionary1`        |

##### MemberCode

| **Column name**              | <u>MemberCode</u> |
| ---------------------------- | ----------------------- |
| **Definition**               | exact column name of the dataset in DataSource.  |
| **Need**                     |Mandatory                |
| **Type of content** 		   |a single expression composed of text,symbol and numeric   |
| **Example**                 |`flag`        |

##### MemberName
| **Column name**              | <u>MemberName</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |add a enrish name to Member Code  |
| **Need**                     |Optional                |
| **Type of content** 		   | accepted space, text, symbol and numeric |
| **Example**                 |`country flag`        |

##### MemberType
| **Column name**              | <u>MemberType</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |indicate the category of each column  |
| **Need**                     |Optional                |
| **Type of content** 		   | `attribute`or`variable` |
| **Example**                 |`variable`        |

##### MinOccurs
| **Column name**              | <u>MinOccurs</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |manage the minimal number of selectable items of the MemberCode.|
| **Need**                     |Mandatory                |
| **Type of content** 		   | numeric  |
| **Example**                 |`O` to allow the possibility to not select item; `1` to contrain to select item        |
##### MaxOccurs
| **Column name**              | <u>MaxOccurs</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |add a enrish name to Member Code |
| **Need**                     |Mandatory                |
| **Type of content** 		   | numeric or `Inf`  |
| **Example**                 |`1` to contrain to not use more of one item; `Inf` to allow the possibility to select all items        |

##### Definition
| **Column name**              | <u>Definition</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |add a explication text about the column|
| **Need**                     |Optional                |
| **Type of content** 		   | text  |
| **Example**                 |        |

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
| **Definition**               |add an information about unit |
| **Need**                     |Optional                |
| **Type of content** 		   |unit symbol and abbreviation  |
| **Example**                 |`$` |

##### RegisterId
| **Column name**              | <u>RegisterId</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |name of R object of table or dataframe type including register information about each item of the colum.|
| **Need**                     |Optional                |
| **Type of content** 		   |unit symbol and abbreviation  |
| **Example**                 |`$` |

##### RegisterScript
| **Column name**              | <u>RegisterScript</u> |
| ---------------------------- | ----------------------- |
| **Definition**               |a local R script with entire link directory containing the registerId object. This object must contain 4 codify columns and each row corresponding to an item :<br/>- **code** : exact name of item as write inside the dataframe (mandatory)<br/>- **uri**: a link about item information (optional)<br/>- **label**:label of corresponding item (mandatory) <br/>- **definition**: a definition of each item (optional)<br> |
| **Need**                     |Mandatory if RegisterId declared                |
| **Type of content** 		   |R script directory  |
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