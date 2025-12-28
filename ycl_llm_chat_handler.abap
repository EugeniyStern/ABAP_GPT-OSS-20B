*&---------------------------------------------------------------------*
*& Class YCL_LLM_CHAT_HANDLER
*&---------------------------------------------------------------------*
*& Main REST API class for LLM Chat
*& Package: Y_LLM_CHAT
*&---------------------------------------------------------------------*
CLASS ycl_llm_chat_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPE-POOLS yst.

    INTERFACES if_http_extension.

  PRIVATE SECTION.

    " Services
    DATA: mo_llm_service TYPE REF TO ycl_llm_service,
          mo_tool_catalog TYPE REF TO ycl_tool_catalog.

    " Request handling
    METHODS handle_chat_message
      IMPORTING io_server TYPE REF TO if_http_server
      RAISING cx_root.

    METHODS handle_session_management
      IMPORTING io_server TYPE REF TO if_http_server
      RAISING cx_root.

    METHODS handle_get_categories
      IMPORTING io_server TYPE REF TO if_http_server
      RAISING cx_root.

    METHODS handle_get_intents
      IMPORTING io_server TYPE REF TO if_http_server
      RAISING cx_root.

    METHODS handle_get_tools
      IMPORTING io_server TYPE REF TO if_http_server
      RAISING cx_root.

    " User management
    METHODS get_or_create_user
      IMPORTING iv_sap_logon TYPE username
                iv_nickname TYPE char30
      RETURNING VALUE(rs_user) TYPE yst_user
      RAISING cx_root.

    " Session management
    METHODS create_session
      IMPORTING iv_sap_logon TYPE username
                iv_nickname TYPE char30
                iv_session_title TYPE string OPTIONAL
      RETURNING VALUE(rv_session_id) TYPE char32
      RAISING cx_root.

    METHODS load_session
      IMPORTING iv_session_id TYPE char32
      RETURNING VALUE(rs_session) TYPE yst_session
      RAISING cx_root.

    METHODS update_session_title
      IMPORTING iv_session_id TYPE char32
                iv_title TYPE string
      RAISING cx_root.

    METHODS get_user_sessions
      IMPORTING iv_sap_logon TYPE username
                iv_nickname TYPE char30
      RETURNING VALUE(rt_sessions) TYPE yst_t_session
      RAISING cx_root.

    " Message management
    METHODS save_message
      IMPORTING iv_session_id TYPE char32
                iv_role TYPE char20
                iv_content TYPE string
                iv_tool_calls TYPE string OPTIONAL
                iv_tool_call_id TYPE char32 OPTIONAL
      RAISING cx_root.

    METHODS load_conversation_history
      IMPORTING iv_session_id TYPE char32
      RETURNING VALUE(rt_messages) TYPE yst_t_chat_message
      RAISING cx_root.

    METHODS get_conversation_context
      IMPORTING iv_session_id TYPE char32
      RETURNING VALUE(rt_llm_messages) TYPE ycl_llm_service=>ytt_llm_message
      RAISING cx_root.

    " Task breakdown
    METHODS break_down_into_tasks
      IMPORTING iv_user_message TYPE string
                iv_session_id TYPE char32
      RETURNING VALUE(rt_tasks) TYPE string_table
      RAISING cx_root.

    METHODS execute_task_sequence
      IMPORTING it_tasks TYPE string_table
                iv_session_id TYPE char32
      RETURNING VALUE(rv_result) TYPE string
      RAISING cx_root.

    METHODS merge_task_results
      IMPORTING it_results TYPE string_table
      RETURNING VALUE(rv_merged_result) TYPE string.

    " Helper methods
    METHODS send_json_response
      IMPORTING io_server TYPE REF TO if_http_server
                iv_json TYPE string
                iv_status_code TYPE i DEFAULT 200.

    METHODS send_error_response
      IMPORTING io_server TYPE REF TO if_http_server
                iv_error_message TYPE string
                iv_status_code TYPE i DEFAULT 500.

    METHODS get_request_body
      IMPORTING io_server TYPE REF TO if_http_server
      RETURNING VALUE(rv_body) TYPE string.

    METHODS get_header_value
      IMPORTING io_server TYPE REF TO if_http_server
                iv_header_name TYPE string
      RETURNING VALUE(rv_value) TYPE string.

    " Initialization
    METHODS initialize_services.

ENDCLASS.



CLASS YCL_LLM_CHAT_HANDLER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->BREAK_DOWN_INTO_TASKS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_USER_MESSAGE                TYPE        STRING
* | [--->] IV_SESSION_ID                  TYPE        CHAR32
* | [<-()] RT_TASKS                       TYPE        STRING_TABLE
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD break_down_into_tasks.
    " Simplified implementation - can be improved using LLM
    " For now return one task
    APPEND iv_user_message TO rt_tasks.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->CREATE_SESSION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SAP_LOGON                   TYPE        USERNAME
* | [--->] IV_NICKNAME                    TYPE        CHAR30
* | [--->] IV_SESSION_TITLE               TYPE        STRING(optional)
* | [<-()] RV_SESSION_ID                  TYPE        CHAR32
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_session.
    DATA: ls_session TYPE yst_session.
    DATA: lv_uuid TYPE sysuuid_c22.

    " Generate UUID for session
    lv_uuid = cl_system_uuid=>create_uuid_c22_static( ).
    rv_session_id = lv_uuid.

    " Fill session data
    ls_session-session_id = rv_session_id.
    GET TIME STAMP FIELD ls_session-start_timestamp.
    ls_session-sap_logon = iv_sap_logon.
    ls_session-nickname = iv_nickname.
    ls_session-session_title = COND #( WHEN iv_session_title IS NOT INITIAL
                                        THEN iv_session_title
                                        ELSE |Session { rv_session_id }| ).

    " Save to database
    INSERT yllm_sessions FROM ls_session.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_llm_error
        EXPORTING error_text = 'Failed to create session'.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->EXECUTE_TASK_SEQUENCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TASKS                       TYPE        STRING_TABLE
* | [--->] IV_SESSION_ID                  TYPE        CHAR32
* | [<-()] RV_RESULT                      TYPE        STRING
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD execute_task_sequence.
    " Simplified sequential execution implementation
    DATA: lv_result TYPE string.

    LOOP AT it_tasks INTO DATA(lv_task).
      " Task execution logic should be here
      " For now just accumulate results
      IF lv_result IS NOT INITIAL.
        lv_result = lv_result && cl_abap_char_utilities=>newline.
      ENDIF.
      lv_result = lv_result && lv_task.
    ENDLOOP.

    rv_result = lv_result.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->GET_CONVERSATION_CONTEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SESSION_ID                  TYPE        CHAR32
* | [<-()] RT_LLM_MESSAGES                TYPE        YCL_LLM_SERVICE=>YTT_LLM_MESSAGE
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_conversation_context.
    DATA: lt_messages TYPE yst_t_chat_message.
    DATA: ls_llm_msg TYPE ycl_llm_service=>yst_llm_message.

    " Load history
    lt_messages = load_conversation_history( iv_session_id ).

    " Convert to LLM format
    LOOP AT lt_messages INTO DATA(ls_msg).
      CLEAR ls_llm_msg.
      ls_llm_msg-role = ls_msg-role.
      ls_llm_msg-content = ls_msg-content.

      " Deserialize tool_calls JSON string from DB to table
      IF ls_msg-tool_calls IS NOT INITIAL.
        /ui2/cl_json=>deserialize(
          EXPORTING
            json = ls_msg-tool_calls
          CHANGING
            data = ls_llm_msg-tool_calls
        ).
      ELSE.
        CLEAR ls_llm_msg-tool_calls.
      ENDIF.

      APPEND ls_llm_msg TO rt_llm_messages.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->GET_HEADER_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SERVER                      TYPE REF TO IF_HTTP_SERVER
* | [--->] IV_HEADER_NAME                 TYPE        STRING
* | [<-()] RV_VALUE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_header_value.
    DATA: lt_headers TYPE tihttpnvp.

    io_server->request->get_header_fields( CHANGING fields = lt_headers ).
    READ TABLE lt_headers INTO DATA(ls_header) WITH KEY name = iv_header_name.
    IF sy-subrc = 0.
      rv_value = ls_header-value.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->GET_OR_CREATE_USER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SAP_LOGON                   TYPE        USERNAME
* | [--->] IV_NICKNAME                    TYPE        CHAR30
* | [<-()] RS_USER                        TYPE        YST_USER
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_or_create_user.
    SELECT SINGLE sap_logon,
                  nickname,
                  llm_name,
                  gender,
                  language_pref
      FROM yllm_users
      INTO CORRESPONDING FIELDS OF @rs_user
      WHERE sap_logon = @iv_sap_logon
        AND nickname = @iv_nickname.

    IF sy-subrc <> 0.
      " Create new user
      rs_user-sap_logon = iv_sap_logon.
      rs_user-nickname = iv_nickname.
      rs_user-llm_name = iv_nickname. " By default use nickname
      rs_user-gender = ''.
      rs_user-language_pref = sy-langu.

      INSERT yllm_users FROM rs_user.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE ycx_llm_error
          EXPORTING error_text = 'Failed to create user'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->GET_REQUEST_BODY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SERVER                      TYPE REF TO IF_HTTP_SERVER
* | [<-()] RV_BODY                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_request_body.
    rv_body = io_server->request->get_cdata( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->GET_USER_SESSIONS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SAP_LOGON                   TYPE        USERNAME
* | [--->] IV_NICKNAME                    TYPE        CHAR30
* | [<-()] RT_SESSIONS                    TYPE        YST_T_SESSION
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_user_sessions.
    SELECT session_id,
           start_timestamp,
           sap_logon,
           nickname,
           session_title
      FROM yllm_sessions
      INTO CORRESPONDING FIELDS OF TABLE @rt_sessions
      WHERE sap_logon = @iv_sap_logon
        AND nickname = @iv_nickname
      ORDER BY start_timestamp DESCENDING.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->HANDLE_CHAT_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SERVER                      TYPE REF TO IF_HTTP_SERVER
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_chat_message.
    DATA: lv_request_body TYPE string,
          ls_user TYPE yst_user,
          lv_session_id TYPE char32,
          ls_session TYPE yst_session,
          lt_llm_messages TYPE ycl_llm_service=>ytt_llm_message,
          ls_llm_request TYPE ycl_llm_service=>yst_llm_request,
          ls_llm_response TYPE ycl_llm_service=>yst_llm_response,
          lv_response_json TYPE string.

    " Get request body
    lv_request_body = get_request_body( io_server = io_server ).

    " Parse JSON request
    DATA: BEGIN OF ls_chat_request,
            sap_logon TYPE username,
            nickname TYPE char30,
            session_id TYPE char32,
            message TYPE string,
            session_title TYPE string,
          END OF ls_chat_request.

    /ui2/cl_json=>deserialize(
      EXPORTING json = lv_request_body
      CHANGING data = ls_chat_request
    ).

    " Get or create user
    ls_user = get_or_create_user(
      iv_sap_logon = ls_chat_request-sap_logon
      iv_nickname = ls_chat_request-nickname
    ).

    " Get or create session
    IF ls_chat_request-session_id IS NOT INITIAL.
      lv_session_id = ls_chat_request-session_id.
      ls_session = load_session( lv_session_id ).
    ELSE.
      lv_session_id = create_session(
        iv_sap_logon = ls_chat_request-sap_logon
        iv_nickname = ls_chat_request-nickname
        iv_session_title = COND #( WHEN ls_chat_request-session_title IS NOT INITIAL
                                    THEN ls_chat_request-session_title
                                    ELSE ls_chat_request-message )
      ).
    ENDIF.

    " Save user message
    save_message(
      iv_session_id = lv_session_id
      iv_role = 'user'
      iv_content = ls_chat_request-message
    ).

    " Load conversation history
    lt_llm_messages = get_conversation_context( iv_session_id = lv_session_id ).

    " Two-stage process: determine category → get intents → get TOOLS
    " Stage 1: Get all available categories
    DATA(lt_categories) = mo_tool_catalog->get_categories( ).

    " Send categories to LLM for selection (simplified - use first suitable)
    " In reality, need to send categories to LLM and get selection
    DATA: lv_selected_category TYPE string.
    " Simplified logic: if message contains keywords, select category
    IF ls_chat_request-message CS 'select' OR ls_chat_request-message CS 'data' OR
       ls_chat_request-message CS 'table' OR ls_chat_request-message CS 'query'.
      lv_selected_category = 'SAP_DATA'.
    ELSE.
      " By default use first category
      IF lines( lt_categories ) > 0.
        READ TABLE lt_categories INTO lv_selected_category INDEX 1.
      ENDIF.
    ENDIF.

    " Stage 2: Get intents for selected category
    DATA(lt_intents) = VALUE yst_t_intent( ).
    IF lv_selected_category IS NOT INITIAL.
      lt_intents = mo_tool_catalog->get_intents_by_category( lv_selected_category ).
    ENDIF.

    SELECT * FROM yllm_intents INTO CORRESPONDING FIELDS OF TABLE lt_intents .

    " Send intents to LLM for selection (simplified - use first)
    DATA(lt_tools) = VALUE yif_tool=>ytt_tools_definition( ).
    IF lines( lt_intents ) > 0.
      READ TABLE lt_intents INTO DATA(ls_intent) INDEX 1.
      IF sy-subrc = 0.
        " Get TOOLS for selected intent
        TRY.
            DATA(lv_intent_name) = |{ ls_intent-intent_name }|.
            lt_tools = mo_tool_catalog->get_tools_for_intent( lv_intent_name ).
          CATCH cx_root INTO DATA(lo_tool_error).
            " If failed to get TOOLS, continue without them
            DATA(lv_error_msg) = |Failed to get tools: { lo_tool_error->get_text( ) }|.
        ENDTRY.
      ENDIF.
    ENDIF.

    " Build request to LLM
    ls_llm_request-model = 'openai/gpt-oss-20b'.
    ls_llm_request-reasoning_effort = 'low'.
    ls_llm_request-verbosity = 'medium'.
    ls_llm_request-messages = lt_llm_messages.
    ls_llm_request-tools = lt_tools.

    " Send request to LLM
    ls_llm_response = mo_llm_service->send_request( ls_llm_request ).

    " Process LLM response (check tool_calls)
    DATA(lt_tool_calls) = mo_llm_service->extract_tool_calls( ls_llm_response ).

    " If there are tool_calls, execute tools
    IF lines( lt_tool_calls ) > 0.
      " Simplified processing - take first tool_call
      READ TABLE lt_tool_calls INTO DATA(ls_tool_call) INDEX 1.
      IF sy-subrc = 0.
        " Extract tool name and arguments from JSON
        DATA: lv_function_name TYPE string,
              lv_arguments_json TYPE string,
              ls_function TYPE ycl_llm_service=>yst_tool_call_function.

        " Parse JSON function - deserialize function object to get arguments
        /ui2/cl_json=>deserialize(
          EXPORTING
            json = ls_tool_call-function
          CHANGING
            data = ls_function
        ).
        lv_function_name = ls_function-name.
        lv_arguments_json = ls_function-arguments.

        " Execute tool (for this need to know intent)
        " Simplified - use first intent
        IF lines( lt_intents ) > 0.
          READ TABLE lt_intents INTO ls_intent INDEX 1.
          DATA(lv_intent_name_exec) = |{ ls_intent-intent_name }|.
          DATA(lv_tool_result) = mo_tool_catalog->execute_tool(
            iv_intent = lv_intent_name_exec
            iv_json_arguments = lv_arguments_json
          ).

          " Save result as tool message
          DATA: lv_tool_call_id_char32 TYPE char32.
          lv_tool_call_id_char32 = |{ ls_tool_call-id }|.
          save_message(
            iv_session_id = lv_session_id
            iv_role = 'tool'
            iv_content = lv_tool_result
            iv_tool_call_id = lv_tool_call_id_char32
          ).

          " Send second request to LLM with tool result
          lt_llm_messages = get_conversation_context( iv_session_id = lv_session_id ).
          ls_llm_request-messages = lt_llm_messages.
          ls_llm_request-tools = VALUE #( ). " Remove tools for final response
          ls_llm_response = mo_llm_service->send_request( ls_llm_request ).
        ENDIF.
      ENDIF.
    ENDIF.

    " Extract final response from LLM
    READ TABLE ls_llm_response-choices INTO DATA(ls_choice) INDEX 1.
    IF sy-subrc = 0.
      " Save assistant response
      " Serialize tool_calls table to JSON string for storage
      DATA: lv_tool_calls_json TYPE string.
      IF lines( ls_choice-message-tool_calls ) > 0.
        /ui2/cl_json=>serialize(
          EXPORTING
            data        = ls_choice-message-tool_calls
            compress    = abap_true
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
          RECEIVING
            r_json      = lv_tool_calls_json
        ).
      ELSE.
        lv_tool_calls_json = ''.
      ENDIF.

      save_message(
        iv_session_id = lv_session_id
        iv_role = 'assistant'
        iv_content = ls_choice-message-content
        iv_tool_calls = lv_tool_calls_json
      ).
    ENDIF.

    " Build response
    DATA: BEGIN OF ls_chat_response,
            session_id TYPE char32,
            message TYPE string,
            status TYPE string,
          END OF ls_chat_response.

    ls_chat_response-session_id = lv_session_id.
    ls_chat_response-message = COND #( WHEN ls_choice IS NOT INITIAL
                                        THEN ls_choice-message-content
                                        ELSE 'No response' ).
    ls_chat_response-status = 'success'.

    /ui2/cl_json=>serialize(
      EXPORTING
        data        = ls_chat_response
        compress    = abap_true
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = lv_response_json
    ).

    send_json_response( io_server = io_server iv_json = lv_response_json ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->HANDLE_GET_CATEGORIES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SERVER                      TYPE REF TO IF_HTTP_SERVER
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_get_categories.
    DATA(lt_categories) = mo_tool_catalog->get_categories( ).

    DATA: lv_json TYPE string.
    /ui2/cl_json=>serialize(
      EXPORTING data = lt_categories
      RECEIVING r_json = lv_json
    ).

    send_json_response( io_server = io_server iv_json = lv_json ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->HANDLE_GET_INTENTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SERVER                      TYPE REF TO IF_HTTP_SERVER
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_get_intents.
    DATA: lv_category TYPE string,
          lt_intents TYPE yst_t_intent.

    lv_category = get_header_value( io_server = io_server iv_header_name = 'category' ).
    IF lv_category IS INITIAL.
      " Try from query parameters
      DATA(lv_query) = io_server->request->get_header_field( '~query_string' ).
      " Simplified query string parsing
      IF lv_query CS 'category='.
        DATA(lv_start) = sy-fdpos + 9.
        DATA(lv_end) = strlen( lv_query ).
        lv_category = lv_query+lv_start(lv_end).
        " Trim to first &
        FIND '&' IN lv_category.
        IF sy-subrc = 0.
          lv_category = lv_category(sy-fdpos).
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_category IS INITIAL.
      send_error_response(
        io_server = io_server
        iv_error_message = 'Category parameter is required'
        iv_status_code = 400
      ).
      RETURN.
    ENDIF.

    lt_intents = mo_tool_catalog->get_intents_by_category( lv_category ).

    DATA: lv_json TYPE string.
    /ui2/cl_json=>serialize(
      EXPORTING data = lt_intents
      RECEIVING r_json = lv_json
    ).

    send_json_response( io_server = io_server iv_json = lv_json ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->HANDLE_GET_TOOLS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SERVER                      TYPE REF TO IF_HTTP_SERVER
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_get_tools.
    DATA: lv_intent TYPE string,
          lt_tools TYPE yif_tool=>ytt_tools_definition.

    lv_intent = get_header_value( io_server = io_server iv_header_name = 'intent' ).
    IF lv_intent IS INITIAL.
      " Try from query parameters
      DATA(lv_query) = io_server->request->get_header_field( '~query_string' ).
      IF lv_query CS 'intent='.
        DATA(lv_start) = sy-fdpos + 7.
        DATA(lv_end) = strlen( lv_query ).
        lv_intent = lv_query+lv_start(lv_end).
        FIND '&' IN lv_intent.
        IF sy-subrc = 0.
          lv_intent = lv_intent(sy-fdpos).
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_intent IS INITIAL.
      send_error_response(
        io_server = io_server
        iv_error_message = 'Intent parameter is required'
        iv_status_code = 400
      ).
      RETURN.
    ENDIF.

    TRY.
        lt_tools = mo_tool_catalog->get_tools_for_intent( lv_intent ).

        DATA: lv_json TYPE string.
        /ui2/cl_json=>serialize(
          EXPORTING data = lt_tools
          RECEIVING r_json = lv_json
        ).

        send_json_response( io_server = io_server iv_json = lv_json ).

      CATCH cx_root INTO DATA(lo_error).
        send_error_response(
          io_server = io_server
          iv_error_message = |Error getting tools: { lo_error->get_text( ) }|
          iv_status_code = 500
        ).
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->HANDLE_SESSION_MANAGEMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SERVER                      TYPE REF TO IF_HTTP_SERVER
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_session_management.
    DATA: lv_request_body TYPE string,
          lv_command TYPE string,
          lv_session_id TYPE char32.

    lv_command = get_header_value( io_server = io_server iv_header_name = 'action' ).
    lv_request_body = get_request_body( io_server = io_server ).

    DATA: BEGIN OF ls_session_request,
            sap_logon TYPE username,
            nickname TYPE char30,
            session_id TYPE char32,
            session_title TYPE string,
          END OF ls_session_request.

    IF lv_request_body IS NOT INITIAL.
      /ui2/cl_json=>deserialize(
        EXPORTING json = lv_request_body
        CHANGING data = ls_session_request
      ).
    ENDIF.

    CASE lv_command.
      WHEN 'create' OR 'new'.
        " Create new session
        lv_session_id = create_session(
          iv_sap_logon = ls_session_request-sap_logon
          iv_nickname = ls_session_request-nickname
          iv_session_title = ls_session_request-session_title
        ).

        DATA: BEGIN OF ls_create_response,
                session_id TYPE char32,
                status TYPE string,
              END OF ls_create_response.

        ls_create_response-session_id = lv_session_id.
        ls_create_response-status = 'created'.

        DATA: lv_create_json TYPE string.
        /ui2/cl_json=>serialize(
          EXPORTING
            data        = ls_create_response
            compress    = abap_true
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
          RECEIVING
            r_json      = lv_create_json
        ).
        send_json_response( io_server = io_server iv_json = lv_create_json ).

      WHEN 'list' OR 'get'.
        " Get list of sessions
        DATA(lt_sessions) = get_user_sessions(
          iv_sap_logon = ls_session_request-sap_logon
          iv_nickname = ls_session_request-nickname
        ).

        DATA: lv_list_json TYPE string.
        /ui2/cl_json=>serialize(
          EXPORTING
            data        = lt_sessions
            compress    = abap_true
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
          RECEIVING
            r_json      = lv_list_json
        ).
        send_json_response( io_server = io_server iv_json = lv_list_json ).

      WHEN 'history' OR 'get_history'.
        " Get conversation history for session
        IF ls_session_request-session_id IS INITIAL.
          send_error_response(
            io_server = io_server
            iv_error_message = 'Session ID is required'
            iv_status_code = 400
          ).
          RETURN.
        ENDIF.

        DATA(lt_messages) = load_conversation_history( iv_session_id = ls_session_request-session_id ).

        DATA: lv_history_json TYPE string.
        /ui2/cl_json=>serialize(
          EXPORTING
            data        = lt_messages
            compress    = abap_true
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
          RECEIVING
            r_json      = lv_history_json
        ).
        send_json_response( io_server = io_server iv_json = lv_history_json ).

      WHEN OTHERS.
        send_error_response(
          io_server = io_server
          iv_error_message = |Unknown session action: { lv_command }|
          iv_status_code = 400
        ).
    ENDCASE.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YCL_LLM_CHAT_HANDLER->IF_HTTP_EXTENSION~HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_http_extension~handle_request.
    DATA: lv_command TYPE string.

    " Initialize services
    initialize_services( ).

    " Get command from header or query parameter
    lv_command = get_header_value( io_server = server iv_header_name = 'command' ).
    IF lv_command IS INITIAL.
      DATA: lt_form_fields TYPE tihttpnvp,
            ls_form_field TYPE LINE OF tihttpnvp.
      server->request->if_http_entity~get_form_fields( CHANGING fields = lt_form_fields ).
      READ TABLE lt_form_fields INTO ls_form_field WITH KEY name = 'command'.
      IF sy-subrc = 0.
        lv_command = ls_form_field-value.
      ENDIF.
    ENDIF.

    " If command not found, try from URL path
    IF lv_command IS INITIAL.
      DATA(lv_path) = server->request->get_header_field( '~request_uri' ).
      " Extract command from path (e.g., /chat/message, /session/create)
      IF lv_path CS '/chat/message'.
        lv_command = 'chat_message'.
      ELSEIF lv_path CS '/session'.
        lv_command = 'session'.
      ELSEIF lv_path CS '/categories'.
        lv_command = 'categories'.
      ELSEIF lv_path CS '/intents'.
        lv_command = 'intents'.
      ELSEIF lv_path CS '/tools'.
        lv_command = 'tools'.
      ENDIF.
    ENDIF.

    TRY.
        CASE lv_command.
          WHEN 'chat_message' OR 'message'.
            handle_chat_message( io_server = server ).

          WHEN 'session' OR 'create_session' OR 'get_sessions'.
            handle_session_management( io_server = server ).

          WHEN 'categories'.
            handle_get_categories( io_server = server ).

          WHEN 'intents'.
            handle_get_intents( io_server = server ).

          WHEN 'tools'.
            handle_get_tools( io_server = server ).

          WHEN OTHERS.
            send_error_response(
              io_server = server
              iv_error_message = |Unknown command: { lv_command }|
              iv_status_code = 400
            ).
        ENDCASE.

      CATCH cx_root INTO DATA(lo_error).
        DATA(lv_error_msg) = |Error: { lo_error->get_text( ) }|.
        send_error_response(
          io_server = server
          iv_error_message = lv_error_msg
          iv_status_code = 500
        ).
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->INITIALIZE_SERVICES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD initialize_services.
    IF mo_llm_service IS INITIAL.
      CREATE OBJECT mo_llm_service.
    ENDIF.

    IF mo_tool_catalog IS INITIAL.
      mo_tool_catalog = ycl_tool_catalog=>get_instance( ).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->LOAD_CONVERSATION_HISTORY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SESSION_ID                  TYPE        CHAR32
* | [<-()] RT_MESSAGES                    TYPE        YST_T_CHAT_MESSAGE
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_conversation_history.
    SELECT session_id,
           message_num,
           role,
           content,
           timestamp,
           tool_calls,
           tool_call_id
      FROM yllm_messages
      INTO CORRESPONDING FIELDS OF TABLE @rt_messages
      WHERE session_id = @iv_session_id
      ORDER BY message_num ASCENDING.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->LOAD_SESSION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SESSION_ID                  TYPE        CHAR32
* | [<-()] RS_SESSION                     TYPE        YST_SESSION
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_session.
    SELECT SINGLE session_id,
                  start_timestamp,
                  sap_logon,
                  nickname,
                  session_title
      FROM yllm_sessions
      INTO CORRESPONDING FIELDS OF @rs_session
      WHERE session_id = @iv_session_id.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_llm_error
        EXPORTING error_text = |Session not found: { iv_session_id }|.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->MERGE_TASK_RESULTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_RESULTS                     TYPE        STRING_TABLE
* | [<-()] RV_MERGED_RESULT               TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD merge_task_results.
    rv_merged_result = concat_lines_of( table = it_results sep = cl_abap_char_utilities=>newline ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->SAVE_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SESSION_ID                  TYPE        CHAR32
* | [--->] IV_ROLE                        TYPE        CHAR20
* | [--->] IV_CONTENT                     TYPE        STRING
* | [--->] IV_TOOL_CALLS                  TYPE        STRING(optional)
* | [--->] IV_TOOL_CALL_ID                TYPE        CHAR32(optional)
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save_message.
    DATA: ls_message TYPE yst_chat_message.
    DATA: lv_max_num TYPE int4.

    " Determine next message number
    SELECT MAX( message_num )
      FROM yllm_messages
      INTO @lv_max_num
      WHERE session_id = @iv_session_id.

    IF sy-subrc <> 0.
      lv_max_num = 0.
    ENDIF.

    " Fill message data
    ls_message-session_id = iv_session_id.
    ls_message-message_num = lv_max_num + 1.
    ls_message-role = iv_role.
    ls_message-content = iv_content.
    GET TIME STAMP FIELD ls_message-timestamp.
    ls_message-tool_calls = COND #( WHEN iv_tool_calls IS SUPPLIED THEN iv_tool_calls ELSE '' ).
    ls_message-tool_call_id = COND #( WHEN iv_tool_call_id IS SUPPLIED THEN iv_tool_call_id ELSE '' ).

    " Save to database
    INSERT yllm_messages FROM ls_message.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_llm_error
        EXPORTING error_text = 'Failed to save message'.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->SEND_ERROR_RESPONSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SERVER                      TYPE REF TO IF_HTTP_SERVER
* | [--->] IV_ERROR_MESSAGE               TYPE        STRING
* | [--->] IV_STATUS_CODE                 TYPE        I (default =500)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_error_response.
    DATA: BEGIN OF ls_error,
            error TYPE string,
            status TYPE string,
          END OF ls_error.

    ls_error-error = iv_error_message.
    ls_error-status = 'error'.

    DATA: lv_error_json TYPE string.
    /ui2/cl_json=>serialize(
      EXPORTING data = ls_error
      RECEIVING r_json = lv_error_json
    ).

    send_json_response(
      io_server = io_server
      iv_json = lv_error_json
      iv_status_code = iv_status_code
    ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->SEND_JSON_RESPONSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SERVER                      TYPE REF TO IF_HTTP_SERVER
* | [--->] IV_JSON                        TYPE        STRING
* | [--->] IV_STATUS_CODE                 TYPE        I (default =200)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_json_response.
    io_server->response->set_header_field(
      name  = 'Content-Type'
      value = 'application/json'
    ).

    io_server->response->set_status( code = iv_status_code reason = 'OK' ).

    io_server->response->set_cdata( data = iv_json ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_LLM_CHAT_HANDLER->UPDATE_SESSION_TITLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SESSION_ID                  TYPE        CHAR32
* | [--->] IV_TITLE                       TYPE        STRING
* | [!CX!] CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_session_title.
    UPDATE yllm_sessions
      SET session_title = @iv_title
      WHERE session_id = @iv_session_id.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_llm_error
        EXPORTING error_text = 'Failed to update session title'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.