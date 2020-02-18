CLASS lcl_unit_test DEFINITION DEFERRED.
CLASS zcl_gw_entityset_operations DEFINITION LOCAL FRIENDS lcl_unit_test.

CLASS lcl_unit_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF entity_row,
        order_number TYPE i,
        name         TYPE string,
      END OF entity_row,

      example_entity_set TYPE STANDARD TABLE OF entity_row.

    CLASS-METHODS:
      class_setup.
    CLASS-DATA:
      mock_data_provider        TYPE REF TO /iwbep/if_mgw_conv_srv_runtime,
      mock_data_provider_facade TYPE REF TO /iwbep/if_mgw_dp_int_facade,
      mock_data_provider_model  TYPE REF TO /iwbep/if_mgw_odata_re_model,
      mock_entity_type          TYPE REF TO /iwbep/if_mgw_odata_re_etype,
      mock_filter               TYPE REF TO /iwbep/if_mgw_req_filter,
      mock_technical_context    TYPE REF TO /iwbep/if_mgw_req_entityset.
    METHODS:
      setup,
      sort_entity_set_asc1 FOR TESTING, "by order_number property
      sort_entity_set_desc1 FOR TESTING, "by order_number property
      sort_entity_set_asc2 FOR TESTING, "by name property
      sort_entity_set_desc2 FOR TESTING, "by name property
      transparent_paging FOR TESTING, "top = 0 skip = 0
      paging_first_5 FOR TESTING, "top = 5, skip = 0,
      paging_from_3 FOR TESTING, "top = 0, skip = 3
      paging_from_2_to_5 FOR TESTING, "top = 3, skip = 2
      filter_by_cp_name FOR TESTING, "by name property, covers pattern type (CP X*
      filter_by_cp_name_or_order_no FOR TESTING, "by name or order_number, covers pattern type (CP X*)
      filter_by_name_eq FOR TESTING, "by name property, equals (mixed case)
      handle_all_query_options FOR TESTING, "all options
      given_entity_set_is_filled,
      given_technical_context_is_set,
      when_set_is_sorted_ascending1, "by order_number property
      when_set_is_sorted_descending1, "by order_number property
      when_set_is_sorted_ascending2, "by name property
      when_set_is_sorted_descending2, "by name property
      when_paging_is_initial,
      when_paging_top_5_skip_0,
      when_paging_top_0_skip_3,
      when_paging_top_3_skip_2,
      when_filter_and_sort_and_page,
      when_filter_by IMPORTING filter_options TYPE /iwbep/t_mgw_select_option,
      then_entity_set_is_not_changed,
      then_set_is_filtered_by_name,
      then_set_is_correct,
      then_is_filt_by_name_and_order,
      then_set_is_filtered_by_equals,
      then_entity_subset_is_present IMPORTING start_index TYPE i end_index TYPE i,
      then_asc_sorting_is_correct1, "by order_number property
      then_desc_sorting_is_correct1, "by order_number property
      then_asc_sorting_is_correct2, "by name property
      then_desc_sorting_is_correct2. "by name property
    DATA:
      class_under_test            TYPE REF TO zcl_gw_entityset_operations,
      entity_set_under_test       TYPE example_entity_set,
      response_context_under_test TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context,
      expected_entity_set         LIKE entity_set_under_test.
ENDCLASS.

CLASS lcl_unit_test IMPLEMENTATION.
  METHOD class_setup.
    mock_data_provider ?= cl_abap_testdouble=>create( '/IWBEP/IF_MGW_CONV_SRV_RUNTIME' ).
    mock_data_provider_facade ?= cl_abap_testdouble=>create( '/IWBEP/IF_MGW_DP_INT_FACADE' ).
    mock_data_provider_model ?= cl_abap_testdouble=>create( '/IWBEP/IF_MGW_ODATA_RE_MODEL' ).
    mock_entity_type ?= cl_abap_testdouble=>create( '/IWBEP/IF_MGW_ODATA_RE_ETYPE' ).
    mock_technical_context ?= cl_abap_testdouble=>create( '/IWBEP/IF_MGW_REQ_ENTITYSET' ).
    mock_filter ?= cl_abap_testdouble=>create( '/IWBEP/IF_MGW_REQ_FILTER' ).

    cl_abap_testdouble=>configure_call( mock_data_provider )->returning( mock_data_provider_facade ).
    mock_data_provider->get_dp_facade( ).

    cl_abap_testdouble=>configure_call( mock_data_provider_facade )->returning( mock_data_provider_model ).
    mock_data_provider_facade->get_model( ).

    cl_abap_testdouble=>configure_call( mock_data_provider_model )->returning( mock_entity_type ).
    mock_data_provider_model->get_entity_type( 'TestEntity' ).

    DATA(entity_properties) = VALUE /iwbep/if_mgw_odata_re_prop=>ty_t_mgw_odata_properties(
      ( technical_name = 'NAME' )
      ( technical_name = 'ORDER_NUMBER' )
    ).

    cl_abap_testdouble=>configure_call( mock_entity_type )->returning( entity_properties ).
    mock_entity_type->get_properties(  ).

    cl_abap_testdouble=>configure_call( mock_technical_context )->returning( mock_filter ).
    mock_technical_context->get_filter( ).
  ENDMETHOD.

  METHOD setup.
    class_under_test = NEW #( mock_data_provider ).
    CLEAR response_context_under_test.
  ENDMETHOD.

  METHOD given_entity_set_is_filled.
    entity_set_under_test = VALUE #(
      ( order_number = 1  name = 'Zname')
      ( order_number = 2  name = 'yname')
      ( order_number = 3  name = 'Wname')
      ( order_number = 4  name = 'wwname')
      ( order_number = 5  name = 'tname')
      ( order_number = 6  name = 'TNAme')
      ( order_number = 7  name = 'Mname')
      ( order_number = 8  name = 'Kname')
      ( order_number = 9  name = 'Ename')
    ).

    expected_entity_set = entity_set_under_test.
  ENDMETHOD.

  METHOD sort_entity_set_asc1. "by order_number property
    given_entity_set_is_filled( ).
    when_set_is_sorted_ascending1( ).
    then_asc_sorting_is_correct1( ).
  ENDMETHOD.

  METHOD sort_entity_set_desc1. "by order_number property
    given_entity_set_is_filled( ).
    when_set_is_sorted_descending1( ).
    then_desc_sorting_is_correct1( ).
  ENDMETHOD.

  METHOD sort_entity_set_asc2. "by name property
    given_entity_set_is_filled( ).
    when_set_is_sorted_ascending2( ).
    then_asc_sorting_is_correct2( ).
  ENDMETHOD.

  METHOD sort_entity_set_desc2. "by name property
    given_entity_set_is_filled( ).
    when_set_is_sorted_descending2( ).
    then_desc_sorting_is_correct2( ).
  ENDMETHOD.

  METHOD then_asc_sorting_is_correct1. "by order_number property
    SORT expected_entity_set BY order_number ASCENDING.
    cl_abap_unit_assert=>assert_equals( exp = expected_entity_set act = entity_set_under_test  ).
  ENDMETHOD.

  METHOD then_desc_sorting_is_correct1. "by order_number property
    SORT expected_entity_set BY order_number DESCENDING.
    cl_abap_unit_assert=>assert_equals( exp = expected_entity_set act = entity_set_under_test ).
  ENDMETHOD.

  METHOD when_set_is_sorted_ascending1. "by order_number property
    class_under_test->order_by(
      EXPORTING order_criteria = VALUE #(
        (
          property = 'ORDER_NUMBER'
          order = 'asc'
        )
      )
      CHANGING entity_set_table = entity_set_under_test
    ).
  ENDMETHOD.

  METHOD when_set_is_sorted_descending1. "by order_number property
    class_under_test->order_by(
      EXPORTING order_criteria = VALUE #(
        (
          property = 'ORDER_NUMBER'
          order = 'desc'
        )
      )
      CHANGING entity_set_table = entity_set_under_test
    ).
  ENDMETHOD.

  METHOD then_asc_sorting_is_correct2. "by name property
    SORT expected_entity_set BY name ASCENDING.
    cl_abap_unit_assert=>assert_equals( exp = expected_entity_set act = entity_set_under_test  ).
  ENDMETHOD.

  METHOD when_set_is_sorted_ascending2. "by name property
    class_under_test->order_by(
     EXPORTING order_criteria = VALUE #(
       (
         property = 'NAME'
         order = 'asc'
       )
     )
     CHANGING entity_set_table = entity_set_under_test
   ).
  ENDMETHOD.

  METHOD when_set_is_sorted_descending2. "by name property
    class_under_test->order_by(
     EXPORTING order_criteria = VALUE #(
       (
         property = 'NAME'
         order = 'desc'
       )
     )
     CHANGING entity_set_table = entity_set_under_test
   ).
  ENDMETHOD.

  METHOD then_desc_sorting_is_correct2. "by name property
    SORT expected_entity_set BY name DESCENDING.
    cl_abap_unit_assert=>assert_equals( exp = expected_entity_set act = entity_set_under_test  ).
  ENDMETHOD.

  METHOD transparent_paging.
    given_entity_set_is_filled( ).
    when_paging_is_initial( ).
    then_entity_set_is_not_changed(  ).
  ENDMETHOD.

  METHOD then_entity_set_is_not_changed.
    cl_abap_unit_assert=>assert_equals( exp = expected_entity_set act = entity_set_under_test ).
  ENDMETHOD.

  METHOD when_paging_is_initial.
    class_under_test->apply_paging(
      EXPORTING paging_preferences = VALUE #( top = 0 skip = 0 )
      CHANGING entity_set_table = entity_set_under_test
    ).
  ENDMETHOD.

  METHOD paging_first_5.
    given_entity_set_is_filled( ).
    when_paging_top_5_skip_0( ).
    then_entity_subset_is_present( start_index = 0 end_index = 5 ).
  ENDMETHOD.

  METHOD paging_from_2_to_5.
    given_entity_set_is_filled( ).
    when_paging_top_3_skip_2( ).
    then_entity_subset_is_present( start_index = 2 end_index = 5 ).
  ENDMETHOD.

  METHOD paging_from_3.
    given_entity_set_is_filled( ).
    when_paging_top_0_skip_3( ).
    then_entity_subset_is_present( start_index = 3 end_index = 8 ).
  ENDMETHOD.

  METHOD then_entity_subset_is_present.
    CLEAR expected_entity_set.

    LOOP AT entity_set_under_test INTO DATA(line) FROM start_index TO end_index.
      APPEND line TO expected_entity_set.
    ENDLOOP.
  ENDMETHOD.

  METHOD when_paging_top_0_skip_3.
    class_under_test->apply_paging(
      EXPORTING paging_preferences = VALUE #( top = 0 skip = 0 )
      CHANGING entity_set_table = entity_set_under_test
    ).
  ENDMETHOD.

  METHOD when_paging_top_3_skip_2.
    class_under_test->apply_paging(
      EXPORTING paging_preferences = VALUE #( top = 3 skip = 2 )
      CHANGING entity_set_table = entity_set_under_test
    ).
  ENDMETHOD.

  METHOD when_paging_top_5_skip_0.
    class_under_test->apply_paging(
      EXPORTING paging_preferences = VALUE #( top = 5 skip = 0 )
      CHANGING entity_set_table = entity_set_under_test
    ).
  ENDMETHOD.

  METHOD filter_by_cp_name.
    given_entity_set_is_filled( ).

    when_filter_by( filter_options = VALUE #(
        (
          property = 'NAME'
          select_options = VALUE #( ( sign = 'I' option = 'CP' low = 'T*' ) )
        )
      )
    ).

    then_set_is_filtered_by_name( ).
  ENDMETHOD.

  METHOD filter_by_cp_name_or_order_no.
    given_entity_set_is_filled( ).

    when_filter_by( filter_options = VALUE #(
        (
          property = 'NAME'
          select_options = VALUE #( ( sign = 'I' option = 'EQ' low = 'TNAME' ) )
        )
        (
          property = 'ORDER_NUMBER'
          select_options = VALUE #( ( sign = 'I' option = 'EQ' low = '2' ) )
        )
      )
    ).

    then_is_filt_by_name_and_order( ).
  ENDMETHOD.

  METHOD filter_by_name_eq.
    given_entity_set_is_filled( ).

    when_filter_by( filter_options = VALUE #(
        (
          property = 'NAME'
          select_options = VALUE #( ( sign = 'I' option = 'EQ' low = 'TNAME' ) )
        )
      )
    ).

    then_set_is_filtered_by_equals(  ).
  ENDMETHOD.

  METHOD then_set_is_filtered_by_equals.
    expected_entity_set = VALUE #(
     ( order_number = 5  name = 'tname')
     ( order_number = 6  name = 'TNAme')
   ).

    cl_abap_unit_assert=>assert_equals( exp = expected_entity_set act = entity_set_under_test ).
  ENDMETHOD.

  METHOD then_is_filt_by_name_and_order.
    expected_entity_set = VALUE #(
      ( order_number = 5  name = 'tname')
      ( order_number = 6  name = 'TNAme')
      ( order_number = 2  name = 'yname')
    ).

    cl_abap_unit_assert=>assert_equals( exp = expected_entity_set act = entity_set_under_test ).
  ENDMETHOD.

  METHOD then_set_is_filtered_by_name.
    expected_entity_set = VALUE #(
      ( order_number = 5  name = 'tname')
      ( order_number = 6  name = 'TNAme')
    ).

    cl_abap_unit_assert=>assert_equals( exp = expected_entity_set act = entity_set_under_test ).
  ENDMETHOD.

  METHOD when_filter_by.
    class_under_test->filter_set(
      EXPORTING
        entity_name = 'TestEntity'
        filter_select_options = filter_options
       CHANGING
        entity_set_table = entity_set_under_test
    ).
  ENDMETHOD.

  METHOD handle_all_query_options.
    given_entity_set_is_filled( ).
    given_technical_context_is_set( ).
    when_filter_and_sort_and_page( ).
    then_set_is_correct(  ).
  ENDMETHOD.

  METHOD then_set_is_correct.
    expected_entity_set = VALUE #(
     ( order_number = 2  name = 'yname')
     ( order_number = 6  name = 'TNAme')
   ).

    cl_abap_unit_assert=>assert_equals( exp = expected_entity_set act = entity_set_under_test ).
  ENDMETHOD.

  METHOD when_filter_and_sort_and_page.
    class_under_test->handle_set_query_options(
      EXPORTING
        entity_name = 'TestEntity'
        technical_context = mock_technical_context
      CHANGING
        entity_set_table = entity_set_under_test
        response_context = response_context_under_test
    ).
  ENDMETHOD.

  METHOD given_technical_context_is_set.
    DATA(filters) = VALUE /iwbep/t_mgw_select_option(
      (
        property = 'NAME'
        select_options = VALUE #( ( sign = 'I' option = 'EQ' low = 'TNAME' ) )
      )
      (
        property = 'ORDER_NUMBER'
        select_options = VALUE #( ( sign = 'I' option = 'EQ' low = '2' ) )
      )
    ).

    cl_abap_testdouble=>configure_call( mock_filter )->returning( filters ).
    mock_filter->get_filter_select_options( ).

    cl_abap_testdouble=>configure_call( mock_technical_context )->returning( abap_true ).
    mock_technical_context->has_inlinecount( ).

    cl_abap_testdouble=>configure_call( mock_technical_context )->returning( 2 ).
    mock_technical_context->get_top( ).

    cl_abap_testdouble=>configure_call( mock_technical_context )->returning( 1 ).
    mock_technical_context->get_skip( ).

    DATA(ordering) = VALUE /iwbep/t_mgw_tech_order(
      ( property = 'ORDER_NUMBER' order = 'asc' )
    ).

    cl_abap_testdouble=>configure_call( mock_technical_context )->returning( ordering ).
    mock_technical_context->get_orderby( ).
  ENDMETHOD.

ENDCLASS.
