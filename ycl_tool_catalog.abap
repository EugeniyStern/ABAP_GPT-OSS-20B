*&---------------------------------------------------------------------*
*& Class YCL_TOOL_CATALOG
*&---------------------------------------------------------------------*
*& LLM Tools Catalog
*& Tool management via YLLM_INTENTS table
*& Package: Y_LLM_CHAT
*&---------------------------------------------------------------------*
CLASS ycl_tool_catalog DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    TYPE-POOLS yst.

    " Singleton pattern - get instance
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO ycl_tool_catalog.

    " Get list of all categories
    METHODS get_categories
      RETURNING VALUE(rt_categories) TYPE string_table.

    " Get intents for category
    METHODS get_intents_by_category
      IMPORTING iv_category TYPE string
      RETURNING VALUE(rt_intents) TYPE yst_t_intent.

    " Get filled TOOLS by intent
    METHODS get_tools_for_intent
      IMPORTING iv_intent TYPE string
      RETURNING VALUE(rt_tools) TYPE yif_tool=>ytt_tools_definition
      RAISING cx_root.

    " Dynamic tool invocation by intent
    METHODS execute_tool
      IMPORTING iv_intent TYPE string
                iv_json_arguments TYPE string
      RETURNING VALUE(rv_result_json) TYPE string
      RAISING cx_root.

  PRIVATE SECTION.

    CLASS-DATA: go_instance TYPE REF TO ycl_tool_catalog.

    " Intents cache
    DATA: mt_intents_cache TYPE yst_t_intent,
          mv_cache_loaded TYPE abap_bool.

    " Load intents from table
    METHODS load_intents_from_db.

    " Get intent information
    METHODS get_intent_info
      IMPORTING iv_intent TYPE string
      RETURNING VALUE(rs_intent) TYPE yst_intent
      RAISING cx_root.

    " Create tool class instance
    METHODS create_tool_instance
      IMPORTING iv_class_name TYPE string
      RETURNING VALUE(ro_tool) TYPE REF TO yif_tool
      RAISING cx_root.

    " Initialize cache
    METHODS initialize_cache.

ENDCLASS.



CLASS YCL_TOOL_CATALOG IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_TOOL_CATALOG->CREATE_TOOL_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASS_NAME                  TYPE        STRING
* | [<-()] RO_TOOL                        TYPE REF TO YIF_TOOL
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_tool_instance.
    DATA: lo_object TYPE REF TO object.
    DATA: lv_class_name TYPE string.
    DATA: lv_exc TYPE REF TO cx_root.

    lv_class_name = iv_class_name.

    " Check class existence
    TRY.
        DATA(lo_class) = cl_abap_classdescr=>describe_by_name( lv_class_name ).
      CATCH cx_class_not_existent.
        DATA(lv_msg1) = |Class not found: { lv_class_name }|.
        RAISE EXCEPTION TYPE ycx_llm_error
          EXPORTING error_text = lv_msg1.
    ENDTRY.

    " Create instance using dynamic CREATE OBJECT
    TRY.
        " Use dynamic CREATE OBJECT
        CREATE OBJECT lo_object TYPE (lv_class_name).

        IF lo_object IS NOT BOUND.
          DATA(lv_msg3) = |Failed to create instance of { lv_class_name }|.
          RAISE EXCEPTION TYPE ycx_llm_error
            EXPORTING error_text = lv_msg3.
        ENDIF.

        " Cast to interface - this will fail if class doesn't implement YIF_TOOL
        ro_tool ?= lo_object.

      CATCH cx_sy_create_object_error INTO lv_exc.
        DATA(lv_msg4) = |Failed to create instance: { lv_exc->get_text( ) }|.
        RAISE EXCEPTION TYPE ycx_llm_error
          EXPORTING error_text = lv_msg4.
      CATCH cx_sy_move_cast_error INTO lv_exc.
        DATA(lv_msg5) = |Class { lv_class_name } does not implement YIF_TOOL: { lv_exc->get_text( ) }|.
        RAISE EXCEPTION TYPE ycx_llm_error
          EXPORTING error_text = lv_msg5.
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YCL_TOOL_CATALOG->EXECUTE_TOOL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTENT                      TYPE        STRING
* | [--->] IV_JSON_ARGUMENTS              TYPE        STRING
* | [<-()] RV_RESULT_JSON                 TYPE        STRING
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD execute_tool.
    DATA: lo_tool TYPE REF TO yif_tool,
          ls_intent TYPE yst_intent.

    " Get intent information
    ls_intent = get_intent_info( iv_intent ).

    " Check that tool is active
    IF ls_intent-active = abap_false.
      DATA: lv_msg TYPE string.
      lv_msg = |Tool { iv_intent } is not active|.
      RAISE EXCEPTION TYPE ycx_llm_error
        EXPORTING error_text = lv_msg.
    ENDIF.

    " Create tool class instance
    lo_tool = create_tool_instance( |{ ls_intent-class_name }| ).

    " Execute tool
    rv_result_json = lo_tool->execute(
      iv_json_arguments = iv_json_arguments
      iv_intent = iv_intent
    ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YCL_TOOL_CATALOG->GET_CATEGORIES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_CATEGORIES                  TYPE        STRING_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_categories.
    " Load intents if not loaded yet
    IF mv_cache_loaded = abap_false.
      load_intents_from_db( ).
    ENDIF.

    " Collect unique categories
    DATA: lt_categories TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    DATA: lv_category TYPE string.

    LOOP AT mt_intents_cache ASSIGNING FIELD-SYMBOL(<fs_intent>) WHERE active = abap_true.
      lv_category = <fs_intent>-category.
      IF lv_category IS NOT INITIAL.
        INSERT lv_category INTO TABLE lt_categories.
      ENDIF.
    ENDLOOP.

    " Convert to standard table
    LOOP AT lt_categories INTO lv_category.
      APPEND lv_category TO rt_categories.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YCL_TOOL_CATALOG=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_INSTANCE                    TYPE REF TO YCL_TOOL_CATALOG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_instance.
    IF go_instance IS INITIAL.
      CREATE OBJECT go_instance.
      go_instance->initialize_cache( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YCL_TOOL_CATALOG->GET_INTENTS_BY_CATEGORY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CATEGORY                    TYPE        STRING
* | [<-()] RT_INTENTS                     TYPE        YST_T_INTENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_intents_by_category.
    " Load intents if not loaded yet
    IF mv_cache_loaded = abap_false.
      load_intents_from_db( ).
    ENDIF.

    " Filter by category and activity
    LOOP AT mt_intents_cache ASSIGNING FIELD-SYMBOL(<fs_intent>)
      WHERE category = iv_category
        AND active = abap_true.
      APPEND <fs_intent> TO rt_intents.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_TOOL_CATALOG->GET_INTENT_INFO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTENT                      TYPE        STRING
* | [<-()] RS_INTENT                      TYPE        YST_INTENT
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_intent_info.
    " Load intents if not loaded yet
    IF mv_cache_loaded = abap_false.
      load_intents_from_db( ).
    ENDIF.

    " Search for intent
    READ TABLE mt_intents_cache INTO rs_intent
      WITH KEY intent_name = iv_intent.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_llm_error
        EXPORTING error_text = |Intent not found: { iv_intent }|.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YCL_TOOL_CATALOG->GET_TOOLS_FOR_INTENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTENT                      TYPE        STRING
* | [<-()] RT_TOOLS                       TYPE        YIF_TOOL=>YTT_TOOLS_DEFINITION
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_tools_for_intent.
    DATA: lo_tool TYPE REF TO yif_tool,
          ls_intent TYPE yst_intent.

    " Get intent information
    ls_intent = get_intent_info( iv_intent ).

    " Create tool class instance
    lo_tool = create_tool_instance( |{ ls_intent-class_name }| ).

    " Call TOOLS filling method
    lo_tool->fill_tools(
      EXPORTING iv_intent = iv_intent
      CHANGING cv_tools = rt_tools
    ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_TOOL_CATALOG->INITIALIZE_CACHE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD initialize_cache.
    CLEAR: mt_intents_cache.
    mv_cache_loaded = abap_false.
    load_intents_from_db( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_TOOL_CATALOG->LOAD_INTENTS_FROM_DB
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_intents_from_db.
    SELECT intent_name,
           category,
           class_name,
           description,
           active
      FROM yllm_intents
      INTO CORRESPONDING FIELDS OF TABLE @mt_intents_cache.

    mv_cache_loaded = abap_true.
  ENDMETHOD.
ENDCLASS.