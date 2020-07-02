"! <p class="shorttext synchronized" lang="en">oData operations handler for any entity set.</p>
"! <p>Requires whole data to be selected and passed in to the methods, so it is useful
"! in reasonable cases like small data sets. </p>
"! <p>An entity set can be processed in one touch using <em>handle_set_query_options</em>
"! or using specific methods - <em>order_by</em>, <em>apply_paging</em> and <em>filter_set</em>.</p>
"! <p>Handles $filter, $top, $skip, $orderby, $count, $inlinecount query options</p>
"! <p>Example usage in DPC_EXT class:</p>
"! <p>SELECT * FROM my_table INTO TABLE et_entityset.<br/>
"! DATA(operations) = NEW zcl_gw_entityset_operations( <br/>
"! &nbsp;&nbsp;data_provider = me <br/>
"! &nbsp;&nbsp;&nbsp;&nbsp;response_context = es_response_context <br/>
"! &nbsp;&nbsp;&nbsp;&nbsp;technical_context = io_tech_request_context ). <br/>
"! <br/>
"! operations-&gt;handle_set_query_options( CHANGING entity_set = et_entityset ). <br/>
"! </p>
CLASS zcl_gw_entityset_operations DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor.</p>
      "! @parameter data_provider | <p class="shorttext synchronized" lang="en">DPC_EXT instance</p>
      "! @parameter technical_context | <p class="shorttext synchronized" lang="en">io_tech_request_context object from the DPC_EXT method</p>
      "! @parameter response_context | <p class="shorttext synchronized" lang="en">es_response_context from the DPC_EXT method</p>
      constructor IMPORTING data_provider     TYPE REF TO /iwbep/if_mgw_conv_srv_runtime
                            technical_context TYPE REF TO /iwbep/if_mgw_req_entityset
                            response_context  TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context,

      "! <p class="shorttext synchronized" lang="en">
      "! Handles filtering, sorting, paging and counting.
      "! </p>
      "! <p>Handles filtering ($filter), sorting ($orderby), paging ($top, $skip) and counting ($count, $inlinecount)</p>
      "!
      "! @parameter multiple_filters_operator | <p class="shorttext synchronized">Operator used if multiple filters are present</p>
      "! @parameter entity_set | <p class="shorttext synchronized" lang="en">Entity set table (whole data set)</p>
      "! @raising /iwbep/cx_mgw_tech_exception | <p class="shorttext synchronized" lang="en">Gateway technical exception</p>
      handle_set_query_options IMPORTING multiple_filters_operator TYPE string DEFAULT 'OR'
                               CHANGING  entity_set                TYPE STANDARD TABLE
                               RAISING   /iwbep/cx_mgw_tech_exception,

      "! <p class="shorttext synchronized" lang="en">Filters ($filter) entity set</p>
      "! <p>Please note: filtering is case-insensitive, multiple filters are handled with OR logic by default (multiple_filters_operator
      "! parameter)</p>
      "!
      "! @parameter filter_select_options | <p class="shorttext synchronized" lang="en">Filter select options</p>
      "! @parameter multiple_filters_operator | <p class="shorttext synchronized">Operator used if multiple filters are present</p>
      "! @parameter entity_set | <p class="shorttext synchronized" lang="en">Entity set table (whole data set)</p>
      "! @raising /iwbep/cx_mgw_tech_exception | <p class="shorttext synchronized" lang="en">Gateway technical exception</p>
      filter_set IMPORTING filter_select_options     TYPE /iwbep/t_mgw_select_option
                           multiple_filters_operator TYPE string DEFAULT 'OR'
                 CHANGING  entity_set                TYPE STANDARD TABLE
                 RAISING   /iwbep/cx_mgw_tech_exception,

      "! <p class="shorttext synchronized" lang="en">Filters entity set, AND logic for multiple filters</p>
      "! <p>Note: filtering is case-insensitive</p>
      "!
      "! @parameter filter_select_options | <p class="shorttext synchronized" lang="en">Filter select options</p>
      "! @parameter entity_set | <p class="shorttext synchronized" lang="en">Entity set table (whole data set)</p>
      "! @raising /iwbep/cx_mgw_tech_exception | <p class="shorttext synchronized" lang="en">Gateway technical exception</p>
      filter_set_with_and IMPORTING filter_select_options TYPE /iwbep/t_mgw_select_option
                          CHANGING  entity_set            TYPE STANDARD TABLE
                          RAISING   /iwbep/cx_mgw_tech_exception,

      "! <p class="shorttext synchronized" lang="en">Filters entity set, OR logic for multiple filters</p>
      "! "! <p>Note: filtering is case-insensitive</p>
      "!
      "! @parameter filter_select_options | <p class="shorttext synchronized" lang="en">Filter select options</p>
      "! @parameter entity_set | <p class="shorttext synchronized" lang="en">Entity set table (whole data set)</p>
      "! @raising /iwbep/cx_mgw_tech_exception | <p class="shorttext synchronized" lang="en">Gateway technical exception</p>
      filter_set_with_or IMPORTING filter_select_options TYPE /iwbep/t_mgw_select_option
                         CHANGING  entity_set            TYPE STANDARD TABLE
                         RAISING   /iwbep/cx_mgw_tech_exception,

      "! <p class="shorttext synchronized" lang="en">Applies paging ($top,$skip) to the entity set</p>
      "! <p>Forwards to <em>/iwbep/cl_mgw_data_util=&gt;paging</em></p>
      "!
      "! @parameter paging_preferences | <p class="shorttext synchronized" lang="en">Top and Skip values</p>
      "! @parameter entity_set | <p class="shorttext synchronized" lang="en">Entity set table (whole data set)</p>
      apply_paging IMPORTING paging_preferences TYPE /iwbep/s_mgw_paging
                   CHANGING  entity_set         TYPE STANDARD TABLE,

      "! <p class="shorttext synchronized" lang="en"></p>
      "!
      "! @parameter order_criteria | <p class="shorttext synchronized" lang="en">Ordering, io_tech_request_context-&gt;get_orderby( )</p>
      "! @parameter entity_set | <p class="shorttext synchronized" lang="en">Entity set table (whole data set)</p>
      order_by IMPORTING order_criteria TYPE /iwbep/t_mgw_tech_order
               CHANGING  entity_set     TYPE STANDARD TABLE.

  PRIVATE SECTION.
    DATA:
      data_provider     TYPE REF TO /iwbep/if_mgw_conv_srv_runtime,
      technical_context TYPE REF TO /iwbep/if_mgw_req_entityset,
      response_context  TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context,
      entity_name       TYPE string.
ENDCLASS.

CLASS zcl_gw_entityset_operations IMPLEMENTATION.
  METHOD apply_paging.
    /iwbep/cl_mgw_data_util=>paging(
      EXPORTING is_paging = paging_preferences
      CHANGING ct_data = entity_set ).
  ENDMETHOD.

  METHOD constructor.
    me->data_provider = data_provider.
    me->response_context = response_context.
    me->technical_context = technical_context.
    me->entity_name = technical_context->get_entity_type_name( ).
  ENDMETHOD.

  METHOD filter_set.
    DATA(filters_count) = lines( filter_select_options ).

    IF filters_count = 0.
      RETURN.
    ENDIF.

    IF multiple_filters_operator = 'AND'.
      filter_set_with_and(
        EXPORTING filter_select_options = filter_select_options
        CHANGING entity_set = entity_set ).
    ELSE.
      filter_set_with_or(
        EXPORTING filter_select_options = filter_select_options
        CHANGING entity_set = entity_set ).
    ENDIF.
  ENDMETHOD.

  METHOD filter_set_with_and.
    TYPES: BEGIN OF ty_hits,
             row_index TYPE i,
             no_hits   TYPE i,
           END OF ty_hits.

    DATA: value             TYPE string,
          final_table       TYPE REF TO data,
          table_index       TYPE i,
          hits              TYPE STANDARD TABLE OF ty_hits WITH KEY row_index,
          hit               LIKE LINE OF hits,
          filter_loop_count TYPE i.

    FIELD-SYMBOLS: <checked_value>    TYPE data,
                   <final_table>      TYPE table,
                   <input_table_line> TYPE data.

    DATA(filters_count) = lines( filter_select_options ).

    IF filters_count = 0.
      RETURN.
    ENDIF.

    DATA(data_provider_facade) = me->data_provider->get_dp_facade( ).
    DATA(data_provider_model) = CAST /iwbep/if_mgw_dp_int_facade( data_provider_facade )->get_model( ).
    DATA(entity_properties) = data_provider_model->get_entity_type( CONV #( entity_name ) )->get_properties( ).
    DATA(table_description) = CAST cl_abap_tabledescr( cl_abap_datadescr=>describe_by_data( entity_set ) ).
    CREATE DATA final_table TYPE HANDLE table_description.
    ASSIGN final_table->* TO <final_table>.

    LOOP AT filter_select_options INTO DATA(filter_select_option).
      filter_loop_count = sy-tabix.

      READ TABLE entity_properties INTO DATA(entity_property)
        WITH KEY technical_name = filter_select_option-property.

      IF sy-subrc = 0.
        LOOP AT entity_set ASSIGNING <input_table_line>.
          table_index = sy-tabix.
          ASSIGN COMPONENT entity_property-technical_name OF STRUCTURE <input_table_line> TO <checked_value>.

          IF sy-subrc = 0 AND <checked_value> IS ASSIGNED.
            value = <checked_value>.
            value = to_upper( <checked_value> ).

            LOOP AT filter_select_option-select_options ASSIGNING FIELD-SYMBOL(<select_option>).
              <select_option>-low = to_upper( <select_option>-low ).
              <select_option>-high = to_upper( <select_option>-high ).
            ENDLOOP.

            IF value IN filter_select_option-select_options.
              READ TABLE hits INTO hit WITH KEY row_index = table_index.

              IF sy-subrc = 0.
                hit-no_hits = hit-no_hits + 1.
                MODIFY TABLE hits FROM hit.
              ELSE.
                hit-no_hits = 1.
                hit-row_index = table_index.
                APPEND hit TO hits.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    LOOP AT hits ASSIGNING FIELD-SYMBOL(<hit>).
      IF <hit>-no_hits = filters_count.
        READ TABLE entity_set ASSIGNING <input_table_line> INDEX <hit>-row_index.
        APPEND <input_table_line> TO <final_table>.
      ENDIF.
    ENDLOOP.

    entity_set = <final_table>.
  ENDMETHOD.

  METHOD filter_set_with_or.
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
    DATA(table_description) = CAST cl_abap_tabledescr( cl_abap_datadescr=>describe_by_data( entity_set ) ).
    CREATE DATA result_table TYPE HANDLE table_description.
    ASSIGN result_table->* TO <result_table>.

    LOOP AT filter_select_options INTO DATA(filter_select_options_line).
      filter_loop_count = sy-tabix.

      READ TABLE entity_properties INTO DATA(entity_properties_line)
        WITH KEY technical_name = filter_select_options_line-property.

      IF sy-subrc = 0.
        LOOP AT entity_set ASSIGNING <entity_set_table_line>.
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
                DELETE entity_set INDEX entity_set_table_loop_index.
              ENDIF.
            ELSE.
              APPEND <entity_set_table_line> TO <result_table>.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    entity_set = <result_table>.
  ENDMETHOD.

  METHOD handle_set_query_options.
    IF entity_set IS NOT INITIAL.
      DATA(filters) = technical_context->get_filter( )->get_filter_select_options( ).

      IF filters IS NOT INITIAL.
        filter_set(
          EXPORTING
            filter_select_options = filters
            multiple_filters_operator = 'OR'
          CHANGING
            entity_set = entity_set ).
      ENDIF.

      IF technical_context->has_inlinecount( ) = abap_true.
        DESCRIBE TABLE entity_set LINES response_context-inlinecount.
      ENDIF.

      apply_paging(
        EXPORTING paging_preferences = VALUE #(
          top = technical_context->get_top( )
          skip = technical_context->get_skip( ) )
        CHANGING entity_set = entity_set ).

      DATA(order) = technical_context->get_orderby( ).

      order_by(
        EXPORTING order_criteria = order
        CHANGING entity_set = entity_set ).

      IF technical_context->has_count( ) = abap_true.
        DESCRIBE TABLE entity_set LINES response_context-count.
      ENDIF.
    ENDIF.
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

    SORT entity_set BY (sort_order).
  ENDMETHOD.
ENDCLASS.
