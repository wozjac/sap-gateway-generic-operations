# OData entity set generic operations handler for SAP Gateway
A generic OData operations handler for any entity set, helpful for custom implemetations without SADL etc.  
Requires whole data to be selected and passed in to the methods, so it is useful in reasonable cases like small data sets.  
An entity set can be processed in one touch using *handle_set_query_options* or using specific methods - *order_by*, *apply_paging* and *filter_set*.  
Handles $filter, $top, $skip, $orderby, $count, $inlinecount query options.
Example usage in DPC_EXT class:  
```ABAP
SELECT * FROM my_table INTO TABLE et_entityset.
DATA(operations) = NEW zcl_gw_entityset_operations( 
  data_provider = me
  response_context = es_response_context
  technical_context = io_tech_request_context ).
  
operations->handle_set_query_options( CHANGING entity_set = et_entityset ).
```
# License
Licensed under the [MIT license](http://opensource.org/licenses/MIT).

# Author
Feel free to contact me:  
- wozjac@zoho.com
- [jackew.dev](https://jacekw.dev)
- Bluesky (<https://jacekwoz.bsky.social>)  
- LinkedIn (https://www.linkedin.com/in/jacek-wznk)
