"! <p class="shorttext synchronized" lang="en">oData operations handler for any entity set.</p>
"! <p>Requires whole data to be selected and passed in to the methods, so it is useful
"! in reasonable cases like small data sets. </p>
"! <p>An entity set can be processed in one touch using <em>handle_set_query_options</em>
"! or using specific methods - <em>order_by</em>, <em>apply_paging</em> and <em>filter_set</em>.</p>
"! <p>Handles $filter, $top, $skip, $orderby, $count, $inlinecount query options</p>
"! <p>Example usage in DPC_EXT class:</p>
"! <p>SELECT * FROM my_table INTO TABLE et_entityset.<br/>
"! DATA(operations) = NEW zcl_gw_entityset_operations( me ).<br/>
"! <br/>
"! operations-&gt;handle_set_query_options(<br/>
"! &nbsp;&nbsp;EXPORTING<br/>
"! &nbsp;&nbsp;&nbsp;entity_name = iv_entity_name<br/>
"! &nbsp;&nbsp;&nbsp;technical_context = io_tech_request_context<br/>
"! &nbsp;&nbsp;CHANGING<br/>
"! &nbsp;&nbsp;&nbsp;entity_set_table = et_entityset<br/>
"! &nbsp;&nbsp;&nbsp;response_context = es_response_context<br/>
"! &nbsp;&nbsp;).<br/>
"! </p>
CLASS zcl_gw_entityset_operations DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">
      "! Constructor. DPC_EXT instance must be passed in</p>
      "! @parameter data_provider | <p class="shorttext synchronized" lang="en">DPC_EXT instance</p>
      constructor IMPORTING data_provider TYPE REF TO /iwbep/if_mgw_conv_srv_runtime,

      "! <p class="shorttext synchronized" lang="en">
      "! Handles filtering, sorting, paging and counting.
      "! </p>
      "! <p>Handles filtering ($filter), sorting ($orderby), paging ($top, $skip) and counting ($count, $inlinecount)
      "! for the given entity set. Requires the whole data to be passed in.</p>
      "!
      "! @parameter technical_context | <p class="shorttext synchronized" lang="en">io_tech_request_context object from the DPC_EXT method</p>
      "! @parameter entity_name | <p class="shorttext synchronized" lang="en">External entity name (iv_entity_name from the DPC_EXT method</p>
      "! @parameter entity_set_table | <p class="shorttext synchronized" lang="en">Entity set complete data</p>
      "! @parameter response_context | <p class="shorttext synchronized" lang="en">es_response_context from the DPC_EXT method</p>
      "! @raising /iwbep/cx_mgw_tech_exception | <p class="shorttext synchronized" lang="en">Gateway technical exception</p>
      handle_set_query_options IMPORTING technical_context TYPE REF TO /iwbep/if_mgw_req_entityset
                                         entity_name       TYPE string
                               CHANGING  entity_set_table  TYPE ANY TABLE
                                         response_context  TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context
                               RAISING   /iwbep/cx_mgw_tech_exception,


      "! <p class="shorttext synchronized" lang="en">Filters ($filter) entity set</p>
      "! <p>Please note: searching is case-insensitive and multiple filters are handled with OR logic!</p>
      "!
      "! @parameter filter_select_options | <p class="shorttext synchronized" lang="en">Filter select options</p>
      "! @parameter entity_name | <p class="shorttext synchronized" lang="en">External entity name, iv_entity_name from the DPC_EXT method</p>
      "! @parameter entity_set_table | <p class="shorttext synchronized" lang="en">Entity set table</p>
      "! @raising /iwbep/cx_mgw_tech_exception | <p class="shorttext synchronized" lang="en">Gateway technical exception</p>
      filter_set IMPORTING filter_select_options TYPE /iwbep/t_mgw_select_option
                           entity_name           TYPE string
                 CHANGING  entity_set_table      TYPE table
                 RAISING   /iwbep/cx_mgw_tech_exception,


      "! <p class="shorttext synchronized" lang="en">Applies paging ($top,$skip) to the entity set</p>
      "! <p>Forwards to <em>/iwbep/cl_mgw_data_util=&gt;paging</em></p>
      "!
      "! @parameter paging_preferences | <p class="shorttext synchronized" lang="en">Top and Skip values</p>
      "! @parameter entity_set_table | <p class="shorttext synchronized" lang="en">Entity set table</p>
      apply_paging IMPORTING paging_preferences TYPE /iwbep/s_mgw_paging
                   CHANGING  entity_set_table   TYPE table,


      "! <p class="shorttext synchronized" lang="en"></p>
      "!
      "! @parameter order_criteria | <p class="shorttext synchronized" lang="en">Ordering, io_tech_request_context-&gt;get_orderby( )</p>
      "! @parameter entity_set_table | <p class="shorttext synchronized" lang="en">Entity set table</p>
      order_by IMPORTING order_criteria   TYPE /iwbep/t_mgw_tech_order
               CHANGING  entity_set_table TYPE STANDARD TABLE.

  PRIVATE SECTION.
    DATA:
      data_provider TYPE REF TO /iwbep/if_mgw_conv_srv_runtime.
ENDCLASS.

CLASS zcl_gw_entityset_operations IMPLEMENTATION.
  METHOD constructor.
    me->data_provider = data_provider.
  ENDMETHOD.

  METHOD handle_set_query_options.
    IF entity_set_table IS NOT INITIAL.
      DATA(filters) = technical_context->get_filter( )->get_filter_select_options( ).

      IF filters IS NOT INITIAL.
        filter_set(
          EXPORTING
            filter_select_options = filters
            entity_name = entity_name
          CHANGING entity_set_table = entity_set_table
        ).
      ENDIF.

      IF technical_context->has_inlinecount( ) = abap_true.
        DESCRIBE TABLE entity_set_table LINES response_context-inlinecount.
      ENDIF.

      apply_paging(
        EXPORTING paging_preferences = VALUE #(
          top = technical_context->get_top( )
          skip = technical_context->get_skip( )
        )
        CHANGING entity_set_table = entity_set_table
      ).

      DATA(order) = technical_context->get_orderby( ).
      order_by( EXPORTING order_criteria = order CHANGING entity_set_table = entity_set_table ).

      IF technical_context->has_count( ) = abap_true.
        DESCRIBE TABLE entity_set_table LINES response_context-count.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD filter_set.
    DATA: checked_value               TYPE string,
          result_table                TYPE REF TO data,
          entity_set_table_loop_index TYPE i,
          filter_loop_count           TYPE i.

    FIELD-SYMBOLS: <entity_line_checked_value> TYPE data,
                   <result_table>              TYPE table,
                   <entity_set_table_line>     TYPE data.

    DATA(filters_count) = lines( filter_select_options ).

    IF filters_count = 0.
      RETURN.
    ENDIF.

    DATA(data_provider_facade) = me->data_provider->get_dp_facade( ).
    DATA(data_provider_model) = CAST /iwbep/if_mgw_dp_int_facade( data_provider_facade )->get_model( ).

    DATA(entity_properties) = data_provider_model->get_entity_type( CONV #( entity_name ) )->get_properties( ).
    DATA(table_description) = CAST cl_abap_tabledescr( cl_abap_datadescr=>describe_by_data( entity_set_table ) ).
    CREATE DATA result_table TYPE HANDLE table_description.
    ASSIGN result_table->* TO <result_table>.

    LOOP AT filter_select_options INTO DATA(filter_select_options_line).
      filter_loop_count = sy-tabix.

      READ TABLE entity_properties INTO DATA(entity_properties_line)
        WITH KEY technical_name = filter_select_options_line-property.

      IF sy-subrc = 0.
        LOOP AT entity_set_table ASSIGNING <entity_set_table_line>.
          entity_set_table_loop_index = sy-tabix.
          ASSIGN COMPONENT entity_properties_line-technical_name OF STRUCTURE <entity_set_table_line> TO <entity_line_checked_value>.

          IF sy-subrc = 0 AND <entity_line_checked_value> IS ASSIGNED.
            checked_value = <entity_line_checked_value>.
            checked_value = to_upper( <entity_line_checked_value> ).

            LOOP AT filter_select_options_line-select_options ASSIGNING FIELD-SYMBOL(<filter_select_option>).
              <filter_select_option>-low = to_upper( <filter_select_option>-low ).
              <filter_select_option>-high = to_upper( <filter_select_option>-high ).
            ENDLOOP.

            IF checked_value NOT IN filter_select_options_line-select_options.
              IF filters_count = filter_loop_count. "last filter.
                DELETE entity_set_table INDEX entity_set_table_loop_index.
              ENDIF.
            ELSE.
              APPEND <entity_set_table_line> TO <result_table>.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    entity_set_table = <result_table>.
  ENDMETHOD.

  METHOD apply_paging.
    /iwbep/cl_mgw_data_util=>paging(
      EXPORTING is_paging = paging_preferences
      CHANGING ct_data = entity_set_table
    ).
  ENDMETHOD.

  METHOD order_by.
    DATA: sort_order      TYPE abap_sortorder_tab,
          sort_order_line TYPE abap_sortorder.

    LOOP AT order_criteria INTO DATA(order_criteria_line).
      sort_order_line-name = order_criteria_line-property.
      IF order_criteria_line-order = 'desc'.
        sort_order_line-descending = abap_true.
      ELSE.
        sort_order_line-descending = abap_false.
      ENDIF.
      APPEND sort_order_line TO sort_order.
    ENDLOOP.

    SORT entity_set_table BY (sort_order).
  ENDMETHOD.
ENDCLASS.
