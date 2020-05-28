# OData entity set operations handler for SAP Gateway
OData operations handler for any entity set.  
Requires whole data to be selected and passed in to the methods, so it is useful in reasonable cases like small data sets.  
An entity set can be processed in one touch using *handle_set_query_options* or using specific methods - *order_by*, *apply_paging* and *filter_set*.  
Handles $filter, $top, $skip, $orderby, $count, $inlinecount query options.
Example usage in DPC_EXT class:  
```ABAP
SELECT * FROM my_table INTO TABLE et_entityset.
DATA(operations) = NEW zcl_gw_entityset_operations( me ).
  
operations->handle_set_query_options(
  EXPORTING
    entity_name = iv_entity_name
    technical_context = io_tech_request_context
  CHANGING
    entity_set_table = et_entityset
    response_context = es_response_context
).
```
# License
Licensed under the [MIT license](http://opensource.org/licenses/MIT).

# Author
Feel free to contact me:  
- wozjac@zoho.com 
- Twitter (https://twitter.com/jacekwoz)  
- LinkedIn (https://www.linkedin.com/in/jacek-wznk)
