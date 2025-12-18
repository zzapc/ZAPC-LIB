types: begin of control_tab,
         ctrl_name type SCRFNAME,
         parent    type i,
         ctrl      type ref  TO c_textedit_control,
         editor_container TYPE REF TO cl_gui_custom_container,
         ctrl_new  type ref to cl_gui_textedit,
      end of control_tab,
      t_control_tab type table of control_tab.

CLASS zcl_ap_control_texto DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA mostrado TYPE abap_bool.

    METHODS constructor
      IMPORTING controlname TYPE scrfname.

    METHODS set_editor
      IMPORTING  max_cols     TYPE i       DEFAULT 0
                 show_status  TYPE c       DEFAULT ''
                 status_text  TYPE sytitle DEFAULT ''
                 show_tool    TYPE c       DEFAULT ''
                 display_mode TYPE c       DEFAULT ''
                 !lines       TYPE table   OPTIONAL
                 !string      TYPE any     OPTIONAL
      PREFERRED PARAMETER max_cols
      EXPORTING  !message     TYPE bapi_msg
      EXCEPTIONS create_error
                 internal_error.

    METHODS get_editor
      IMPORTING  col_width TYPE i DEFAULT 0
      EXPORTING  !lines    TYPE table
                 changed   TYPE c
                 !string   TYPE any
                 !message  TYPE bapi_msg
      EXCEPTIONS internal_error.

    METHODS destroy
      EXPORTING !message TYPE bapi_msg.

    METHODS set_editable
      IMPORTING  !edit TYPE abap_bool
      EXCEPTIONS internal_error.

    METHODS get_control_tab
      IMPORTING  col_width TYPE i DEFAULT 0
      EXPORTING  !lines    TYPE table
                 changed   TYPE c
                 !string   TYPE any
      EXCEPTIONS internal_error.

    METHODS es_editable
      RETURNING  VALUE(edit) TYPE abap_bool
      EXCEPTIONS internal_error.


  PRIVATE SECTION.
    DATA controlname   TYPE scrfname.
    DATA i_control_tab TYPE t_control_tab.
    DATA control_tab   TYPE control_tab.
    DATA read_only     TYPE abap_bool.
endclass. "ZCL_AP_CONTROL_TEXTO definition
class ZCL_AP_CONTROL_TEXTO implementation.
  METHOD constructor.
    me->controlname = controlname.
  ENDMETHOD.
  METHOD destroy.
    DATA: local_parent  TYPE i,
          l_control_tab TYPE control_tab,
          l_tabix       TYPE i.

    CLEAR message.
* Get actual parent window

    CALL FUNCTION 'GUI_GET_PARENT_DYNPRO'
      IMPORTING
        parent = local_parent
      EXCEPTIONS
        OTHERS = 0.
* ist control schon da? Wenn nicht, dann erzeugen!
* Problem: andere REPID/DYNNR, dann relink?
    READ TABLE i_control_tab INTO l_control_tab WITH KEY ctrl_name = controlname
                                                         parent    = local_parent.
    IF sy-subrc <> 0.
      READ TABLE i_control_tab INTO l_control_tab
           WITH KEY ctrl_name = controlname.
    ENDIF.

    IF NOT l_control_tab IS INITIAL.
      l_tabix = sy-tabix.

      l_control_tab-ctrl_new->free(
        EXCEPTIONS
          OTHERS = 1 ).
      IF sy-subrc <> 0.
        message = 'Error freeing GUI text editor' ##NO_TEXT.
      ENDIF.

      l_control_tab-editor_container->free(
        EXCEPTIONS
          OTHERS = 1 ).
      IF sy-subrc <> 0.
        message = 'Error freeing GUI controls' ##NO_TEXT.
      ENDIF.

      DELETE i_control_tab INDEX l_tabix.

      cl_gui_cfw=>flush(
        EXCEPTIONS
          OTHERS = 1 ).
      IF sy-subrc <> 0.
        message = 'Error flushing GUI CFW' ##NO_TEXT.
      ENDIF.

      CLEAR mostrado.
    ENDIF.
  ENDMETHOD.
  METHOD es_editable.
    IF read_only IS INITIAL.
      edit = 'X'.
    ELSE.
      CLEAR edit.
    ENDIF.
  ENDMETHOD.
  METHOD get_control_tab.
    DATA local_parent TYPE i.

    CLEAR: lines, changed, string.
* Get actual parent window

    CALL FUNCTION 'GUI_GET_PARENT_DYNPRO'
      IMPORTING
        parent = local_parent
      EXCEPTIONS
        OTHERS = 0.
* ist control schon da? Wenn nicht, dann erzeugen!
* Problem: andere REPID/DYNNR, dann relink?
    CLEAR control_tab.
    READ TABLE i_control_tab INTO control_tab
         WITH KEY ctrl_name = controlname
                  parent    = local_parent.
    IF sy-subrc <> 0.
      READ TABLE i_control_tab INTO control_tab
           WITH KEY ctrl_name = controlname.
    ENDIF.
  ENDMETHOD.
  METHOD get_editor.
    DATA: l_string      TYPE string,
          local_modif   TYPE i,
          local_stream  TYPE TABLE OF text255,
          local_itf     TYPE TABLE OF tline,
          local_lnsize  TYPE itctk-tdlinesize,
          local_ascii   TYPE TABLE OF tline,
          l_local_ascii TYPE tline.

    CLEAR: string, lines, message, changed.
* Get actual parent window

    get_control_tab( ).
    IF NOT control_tab IS INITIAL.
      IF string IS REQUESTED.
        control_tab-ctrl_new->get_textstream(
          EXPORTING
            only_when_modified     = ''
          IMPORTING
            text                   = l_string
            is_modified            = local_modif
          EXCEPTIONS
            error_cntl_call_method = 1
            not_supported_by_gui   = 2
            OTHERS                 = 3 ).
        IF sy-subrc <> 0.
          message = 'Error getting text stream' ##NO_TEXT.
        ENDIF.

*   Flush is required for working on the string content
        cl_gui_cfw=>flush(
          EXCEPTIONS
            cntl_system_error = 1
            cntl_error        = 2
            OTHERS            = 3 ).
        IF sy-subrc <> 0.
          message = 'Error flushing GUI CFW' ##NO_TEXT.
          RETURN.
        ENDIF.
        string = l_string.
      ELSEIF lines IS REQUESTED.
        IF col_width = 0.
          control_tab-ctrl_new->get_text_as_r3table(
            IMPORTING
              is_modified = local_modif
              table       = lines[]
            EXCEPTIONS
              OTHERS      = 4 ).
          IF sy-subrc <> 0.
            message = 'Error getting text as R3 table' ##NO_TEXT.
            RETURN.
          ENDIF.
        ELSE.
          control_tab-ctrl_new->get_text_as_stream(
            IMPORTING
              is_modified = local_modif
              text        = local_stream[]
            EXCEPTIONS
              OTHERS      = 4 ).
          IF sy-subrc <> 0.
            message = 'Error getting text as stream' ##NO_TEXT.
            RETURN.
          ENDIF.
          CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
            TABLES
              text_stream = local_stream
              itf_text    = local_itf.

          local_lnsize = col_width + 1.
          CALL FUNCTION 'CONVERT_TEXT'
            EXPORTING
              direction   = 'EXPORT'
              format_type = 'ASCII'
              formatwidth = local_lnsize
            TABLES
              foreign     = local_ascii
              itf_lines   = local_itf.
          LOOP AT local_ascii INTO l_local_ascii.
            APPEND l_local_ascii TO lines.
          ENDLOOP.
        ENDIF.
      ELSE.
        control_tab-ctrl_new->get_textmodified_status(
          IMPORTING
            status = local_modif
          EXCEPTIONS
            OTHERS = 1 ).
        IF sy-subrc <> 0.
          message = 'Error getting text modified status' ##NO_TEXT.
          RETURN.
        ENDIF.
      ENDIF.
*    IF sy-subrc <> 0.
*      RAISE internal_error.
*    ENDIF.
      IF local_modif = cl_gui_textedit=>false.
        changed = space.
      ELSE.
        changed = 'X'.
      ENDIF.

      control_tab-ctrl_new->set_textmodified_status(
        EXCEPTIONS
          OTHERS = 1 ).
      IF sy-subrc <> 0.
        message = 'Error resetting modified status' ##NO_TEXT.
      ENDIF.
    ELSE.
      CLEAR: string, lines.
    ENDIF.
  ENDMETHOD.
  METHOD set_editable.
    DATA l_readonly TYPE i.

    get_control_tab( ).
    IF NOT control_tab IS INITIAL.
      IF edit IS INITIAL.
        l_readonly = cl_gui_textedit=>true.
        read_only = 'X'.
      ELSE.
        l_readonly = cl_gui_textedit=>false.
        CLEAR read_only.
      ENDIF.

      control_tab-ctrl_new->set_readonly_mode(
        EXPORTING
          readonly_mode          = l_readonly
        EXCEPTIONS
          error_cntl_call_method = 1
          invalid_parameter      = 2
          OTHERS                 = 3 ).
      IF sy-subrc <> 0.
        RAISE internal_error.
      ENDIF.

    ENDIF.
  ENDMETHOD.
  METHOD set_editor.
    DATA: local_parent TYPE i,
          local_wrapm  TYPE i,
          local_linbrk TYPE i.
    DATA: lv_max_cols LIKE max_cols, " Note 891627
          l_string    TYPE string.

    CLASS cl_gui_object DEFINITION LOAD.         " Note 891627

    CLEAR message.

    IF NOT cl_gui_object=>www_active IS INITIAL.
      lv_max_cols = 0.
    ELSE.
      lv_max_cols = max_cols.
    ENDIF.

* Determine wrap mode
    IF lv_max_cols = 0.
      local_wrapm  = cl_gui_textedit=>wordwrap_at_windowborder.
      local_linbrk = cl_gui_textedit=>false.
    ELSE.
      local_wrapm  = cl_gui_textedit=>wordwrap_at_fixed_position.
      local_linbrk = cl_gui_textedit=>true.
    ENDIF.

    get_control_tab( ).
    IF NOT control_tab IS INITIAL.
      destroy( ). " Si no no refresca!
    ENDIF.

    CLEAR control_tab.
    control_tab-ctrl_name = controlname.
    control_tab-parent    = local_parent.

*   create control container
    CREATE OBJECT control_tab-editor_container
      EXPORTING
        container_name              = controlname
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc <> 0.
      RAISE create_error.
    ENDIF.

*   create control editor
    CREATE OBJECT control_tab-ctrl_new
      EXPORTING
        wordwrap_mode              = local_wrapm
        wordwrap_position          = lv_max_cols
        wordwrap_to_linebreak_mode = local_linbrk
        parent                     = control_tab-editor_container
      EXCEPTIONS
        error_cntl_create          = 1
        error_cntl_init            = 2
        error_cntl_link            = 3
        error_dp_create            = 4
        gui_type_not_supported     = 5
        OTHERS                     = 6.
    IF sy-subrc = 0.
      APPEND control_tab TO i_control_tab.
    ELSE.
      RAISE create_error.
    ENDIF.

* Set Tool-/statusbar
    IF NOT show_status IS INITIAL.
      control_tab-ctrl_new->set_statusbar_mode(
        EXPORTING
          statusbar_mode = 1
        EXCEPTIONS
          OTHERS         = 0 ).
      IF NOT status_text IS INITIAL.
        control_tab-ctrl_new->set_status_text(
          EXPORTING
            status_text = status_text
          EXCEPTIONS
            OTHERS      = 0 ).
      ENDIF.
    ELSE.
      control_tab-ctrl_new->set_statusbar_mode(
        EXPORTING
          statusbar_mode = 0
        EXCEPTIONS
          OTHERS         = 0 ).
    ENDIF.
    IF NOT show_tool IS INITIAL.
      control_tab-ctrl_new->set_toolbar_mode(
        EXPORTING
          toolbar_mode = 1
        EXCEPTIONS
          OTHERS       = 0 ).
    ELSE.
      control_tab-ctrl_new->set_toolbar_mode(
        EXPORTING
          toolbar_mode = 0
        EXCEPTIONS
          OTHERS       = 0 ).
    ENDIF.

    IF NOT display_mode IS INITIAL.
      read_only = 'X'.
      control_tab-ctrl_new->set_readonly_mode(
        EXPORTING
          readonly_mode = 1
        EXCEPTIONS
          OTHERS        = 1 ).
      IF sy-subrc <> 0.
        RAISE internal_error.
      ENDIF.
    ELSE.
      read_only = ''.
      control_tab-ctrl_new->set_readonly_mode(
        EXPORTING
          readonly_mode = 0
        EXCEPTIONS
          OTHERS        = 1 ).
      IF sy-subrc <> 0.
        RAISE internal_error.
      ENDIF.
    ENDIF.

    IF string IS NOT INITIAL.
      l_string = string.
      control_tab-ctrl_new->set_textstream(
        EXPORTING
          text                   = l_string
        EXCEPTIONS
          error_cntl_call_method = 1
          not_supported_by_gui   = 2
          OTHERS                 = 3 ).
      IF sy-subrc <> 0.
        RAISE internal_error.
      ENDIF.

    ELSEIF lines IS NOT INITIAL.
      control_tab-ctrl_new->set_text_as_r3table(
        EXPORTING
          table           = lines[]
        EXCEPTIONS
          error_dp        = 1
          error_dp_create = 2 ).
      IF sy-subrc <> 0.
        RAISE internal_error.
      ENDIF.

    ENDIF.

    cl_gui_cfw=>flush(
      EXCEPTIONS
        OTHERS = 1 ).
    IF sy-subrc <> 0.
      message = 'Error flushing GUI CFW' ##NO_TEXT.
    ENDIF.
    mostrado = 'X'.
  ENDMETHOD.