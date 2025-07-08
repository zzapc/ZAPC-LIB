types: begin of control_tab,
         ctrl_name type SCRFNAME,
         parent    type i,
         ctrl      type ref  TO c_textedit_control,
         editor_container TYPE REF TO cl_gui_custom_container,
         ctrl_new  type ref to cl_gui_textedit,
      end of control_tab,
      t_control_tab type table of control_tab.
class ZCL_AP_CONTROL_TEXTO definition
  public
  final
  create public .

public section.

  data MOSTRADO type ABAP_BOOL .

  methods CONSTRUCTOR
    importing
      !CONTROLNAME type SCRFNAME .
  methods SET_EDITOR
    importing
      !MAX_COLS type I default 0
      !SHOW_STATUS type C default ''
      !STATUS_TEXT type SYTITLE default ''
      !SHOW_TOOL type C default ''
      !DISPLAY_MODE type C default ''
      !LINES type TABLE optional
      !STRING type ANY optional
    preferred parameter MAX_COLS
    exceptions
      CREATE_ERROR
      INTERNAL_ERROR .
  methods GET_EDITOR
    importing
      !COL_WIDTH type I default 0
    exporting
      !LINES type TABLE
      !CHANGED type C
      !STRING type ANY
    exceptions
      INTERNAL_ERROR .
  methods DESTROY .
  methods SET_EDITABLE
    importing
      !EDIT type ABAP_BOOL
    exceptions
      INTERNAL_ERROR .
  methods GET_CONTROL_TAB
    importing
      !COL_WIDTH type I default 0
    exporting
      !LINES type TABLE
      !CHANGED type C
      !STRING type ANY
    exceptions
      INTERNAL_ERROR .
  methods ES_EDITABLE
    returning
      value(EDIT) type ABAP_BOOL
    exceptions
      INTERNAL_ERROR .
protected section.
private section.

  data CONTROLNAME type SCRFNAME .
  data I_CONTROL_TAB type T_CONTROL_TAB .
  data CONTROL_TAB type CONTROL_TAB .
  data READ_ONLY type ABAP_BOOL .
endclass. "ZCL_AP_CONTROL_TEXTO definition
class ZCL_AP_CONTROL_TEXTO implementation.
method CONSTRUCTOR.

  me->controlname = controlname.

endmethod.
METHOD destroy.
  DATA: local_parent  TYPE i,
        l_control_tab TYPE control_tab,
        l_tabix       TYPE i.


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
  IF sy-subrc NE 0.
    READ TABLE i_control_tab INTO l_control_tab
                           WITH KEY ctrl_name = controlname.
  ENDIF.

  IF NOT l_control_tab IS INITIAL.
    l_tabix = sy-tabix.

    CALL METHOD l_control_tab-ctrl_new->free
      EXCEPTIONS
        OTHERS = 1.

    CALL METHOD l_control_tab-editor_container->free
      EXCEPTIONS
        OTHERS = 1.

    DELETE i_control_tab INDEX l_tabix.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        OTHERS = 1.

    CLEAR mostrado.
  ENDIF.

ENDMETHOD.
METHOD ES_EDITABLE.

  IF read_only IS INITIAL.
    edit = 'X'.
  ELSE.
    CLEAR edit.
  ENDIF.

ENDMETHOD.
METHOD get_control_tab.
  DATA: local_parent TYPE i.

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
  IF sy-subrc NE 0.
    READ TABLE i_control_tab INTO control_tab
                           WITH KEY ctrl_name = controlname.
  ENDIF.

ENDMETHOD.
METHOD get_editor.

  DATA: local_stream  TYPE TABLE OF text255,
        local_itf     TYPE TABLE OF tline,
        local_ascii   TYPE TABLE OF tline,
        l_local_ascii TYPE tline,
        local_parent  TYPE i,
        local_modif   TYPE i,
        local_lnsize  TYPE itctk-tdlinesize,
        l_string      TYPE string.

  CLEAR: string, lines.
* Get actual parent window

  get_control_tab( ).
  IF NOT control_tab IS INITIAL.
    IF string IS REQUESTED.
      CALL METHOD control_tab-ctrl_new->get_textstream
        EXPORTING
          only_when_modified     = ''
        IMPORTING
          text                   = l_string
          is_modified            = local_modif
        EXCEPTIONS
          error_cntl_call_method = 1
          not_supported_by_gui   = 2
          OTHERS                 = 3.

*   Flush is required for working on the string content
      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3.

      string = l_string.
    ELSEIF lines IS REQUESTED.
      IF col_width = 0.
        CALL METHOD control_tab-ctrl_new->get_text_as_r3table
          IMPORTING
            is_modified = local_modif
            table       = lines[]
          EXCEPTIONS
            OTHERS      = 4.
      ELSE.
        CALL METHOD control_tab-ctrl_new->get_text_as_stream
          IMPORTING
            is_modified = local_modif
            text        = local_stream[]
          EXCEPTIONS
            OTHERS      = 4.
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
      CALL METHOD control_tab-ctrl_new->get_textmodified_status
        IMPORTING
          status = local_modif
        EXCEPTIONS
          OTHERS = 1.
    ENDIF.
*    IF sy-subrc <> 0.
*      RAISE internal_error.
*    ENDIF.
    IF local_modif = cl_gui_textedit=>false.
      changed = space.
    ELSE.
      changed = 'X'.
    ENDIF.

    CALL METHOD control_tab-ctrl_new->set_textmodified_status
      EXCEPTIONS
        OTHERS = 1.
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

    CALL METHOD control_tab-ctrl_new->set_readonly_mode
      EXPORTING
        readonly_mode          = l_readonly
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDMETHOD.
METHOD set_editor.
  DATA: local_parent TYPE i,
        local_wrapm  TYPE i,
        local_linbrk TYPE i.
  DATA: lv_max_cols LIKE max_cols,              "Note 891627
        l_string    TYPE string.


  CLASS cl_gui_object DEFINITION LOAD.         "Note 891627
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
    destroy( ). "Si no no refresca!
  ENDIF.

  clear control_tab.
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
  IF sy-subrc NE 0.
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
    CALL METHOD control_tab-ctrl_new->set_statusbar_mode
      EXPORTING
        statusbar_mode = 1
      EXCEPTIONS
        OTHERS         = 0.
    IF NOT status_text IS INITIAL.
      CALL METHOD control_tab-ctrl_new->set_status_text
        EXPORTING
          status_text = status_text
        EXCEPTIONS
          OTHERS      = 0.
    ENDIF.
  ELSE.
    CALL METHOD control_tab-ctrl_new->set_statusbar_mode
      EXPORTING
        statusbar_mode = 0
      EXCEPTIONS
        OTHERS         = 0.
  ENDIF.
  IF NOT show_tool IS INITIAL.
    CALL METHOD control_tab-ctrl_new->set_toolbar_mode
      EXPORTING
        toolbar_mode = 1
      EXCEPTIONS
        OTHERS       = 0.
  ELSE.
    CALL METHOD control_tab-ctrl_new->set_toolbar_mode
      EXPORTING
        toolbar_mode = 0
      EXCEPTIONS
        OTHERS       = 0.
  ENDIF.

  IF NOT display_mode IS INITIAL.
    read_only = 'X'.
    CALL METHOD control_tab-ctrl_new->set_readonly_mode
      EXPORTING
        readonly_mode = 1
      EXCEPTIONS
        OTHERS        = 1.
  ELSE.
    read_only = ''.
    CALL METHOD control_tab-ctrl_new->set_readonly_mode
      EXPORTING
        readonly_mode = 0
      EXCEPTIONS
        OTHERS        = 1.
  ENDIF.

  IF string IS SUPPLIED.
    l_string = string.
    CALL METHOD control_tab-ctrl_new->set_textstream
      EXPORTING
        text                   = l_string
      EXCEPTIONS
        error_cntl_call_method = 1
        not_supported_by_gui   = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      RAISE internal_error.
    ENDIF.

  ELSEIF lines IS SUPPLIED.
    CALL METHOD control_tab-ctrl_new->set_text_as_r3table
      EXPORTING
        table           = lines[]
      EXCEPTIONS
        error_dp        = 1
        error_dp_create = 2.
    IF sy-subrc <> 0.
      RAISE internal_error.
    ENDIF.

  ENDIF.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      OTHERS = 1.

  mostrado = 'X'.

ENDMETHOD.
