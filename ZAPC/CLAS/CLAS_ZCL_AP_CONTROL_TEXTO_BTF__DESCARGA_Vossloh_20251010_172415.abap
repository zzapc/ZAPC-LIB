
class ZCL_AP_CONTROL_TEXTO_BTF definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF control_tab,
             ctrl_name        TYPE scrfname,
             parent           TYPE i,
             ctrl             TYPE REF  TO c_textedit_control,
             editor_container TYPE REF TO cl_gui_custom_container,
             ctrl_new         TYPE REF TO cl_gui_textedit,
           END OF control_tab .
  types:
    t_control_tab TYPE TABLE OF control_tab .

  methods CONSTRUCTOR
    importing
      !CONTROLNAME type SCRFNAME
      !TEXTO_SIMPLE type ABAP_BOOL default '' .
  methods SET_EDITOR
    importing
      !MAX_COLS type I default 0
      !SHOW_STATUS type C default ''
      !STATUS_TEXT type SYTITLE default ''
      !SHOW_TOOL type C default ''
      !DISPLAY_MODE type C default ''
      !LINES type TABLE optional
      !STRING type ANY optional
      !ENCODING type STRING default 'utf-8'
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
  methods SET_DISPLAY_MODE
    importing
      !DISPLAY_MODE type FLAG default 'X' .
  methods SET_CONTENT
    importing
      !TEXT type XSTRING
      !ENCODING type STRING default 'utf-8'
      !CLEAN type I default 0 .
  methods SET_CONTENT_AS_STRING
    importing
      !STRING type STRING
      !ENCODING type STRING default 'utf-8' .
  methods GET_CONTENT
    exporting
      !LANGUAGE type TDSPRAS
      !ENCODING type STRING
      !TEXT type XSTRING .
  methods GET_CONTENT_AS_STRING
    importing
      !QUITAR_GRAFICOS type ABAP_BOOL default ''
    returning
      value(STRING) type STRING .
protected section.
private section.

  data CONTROLNAME type SCRFNAME .
  data I_CONTROL_TAB type T_CONTROL_TAB .
  data BTF type ref to IF_BTF .
  data BTF_DOC type ref to IF_BTF_DOCUMENT .
  data EDITOR type ref to IF_BTF_EDITOR .
  data TITULO type VTEXT value 'Editor' ##NO_TEXT.
  data CONTAINER_EDITOR type ref to CL_GUI_CUSTOM_CONTAINER .
  data DISPLAY_MODE type FLAG value SPACE ##NO_TEXT.
  data O_TEXTO_SIMPLE type ref to ZCL_AP_CONTROL_TEXTO .
  data O_VISOR_HTML type ref to ZCL_AP_HTML .
endclass. "ZCL_AP_CONTROL_TEXTO_BTF definition
class ZCL_AP_CONTROL_TEXTO_BTF implementation.
  method CONSTRUCTOR.
  DATA: l_its, rc.


  me->controlname = controlname.

  CALL FUNCTION 'IS_INTEGRATED_ITS'
    IMPORTING
      return = rc
    EXCEPTIONS
      error  = 1
      OTHERS = 2.
  IF sy-subrc EQ 0 AND NOT rc IS INITIAL.
    l_its = 'X'.
  ENDIF.

  IF NOT texto_simple IS INITIAL
    OR l_its = 'X'.
    CREATE OBJECT o_texto_simple
      EXPORTING
        controlname = controlname.
  ENDIF.

  endmethod.
  method DESTROY.
  DATA: l_valid TYPE i.
*  me->btf->free( ).

  IF o_texto_simple IS INITIAL.
    me->container_editor->is_valid( IMPORTING result = l_valid ).
    IF NOT l_valid IS INITIAL.
      me->container_editor->free( ).
    ENDIF.

    CLEAR: me->btf,
           me->btf_doc,
           me->editor,
           me->container_editor.

    FREE:  me->btf,
           me->btf_doc,
           me->editor,
           me->container_editor.
  ELSE.
    o_texto_simple->destroy( ).
  ENDIF.

  endmethod.
  method GET_CONTENT.

  TRY.
      editor->get_content( ).
    CATCH cx_root.
  ENDTRY.
  btf_doc->get_content(
    IMPORTING text     = text
              encoding = encoding
              language = language ).

  endmethod.
  method GET_CONTENT_AS_STRING.
  DATA: l_xstring  TYPE xstring,
        l_encoding TYPE string,
        l_enc      TYPE abap_encod,
        l_exp      TYPE string,
        l_aux      TYPE string.

  get_content( IMPORTING text = l_xstring encoding = l_encoding ).
  CHECK NOT l_xstring IS INITIAL.
  l_enc = l_encoding.
*  CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
*    EXPORTING
*      im_xstring  = l_xstring
*      im_encoding = l_enc
*    IMPORTING
*      ex_string   = string.
  string = zcl_ap_string=>xstring2string( xstring = l_xstring  encoding = zcl_c=>codepage_unicode ).
  IF string CS '<BODY></BODY>' OR string CS '<BODY> </BODY>'.
    CLEAR string.
  ELSE.
    IF zcl_c=>codepage_unicode NE zcl_c=>codepage_html.
      string = zcl_ap_string=>convierte_codificacion( cadena = string conv_caract_in = '#' conv_caract_out = cl_abap_char_utilities=>cr_lf ).
    ENDIF.

    TRY.
* Limpieza código general
        REPLACE ALL OCCURRENCES OF '<HTML xmlns="http://www.w3.org/1999/xhtml">' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF '<META content="text/html; charset=utf-8" http-equiv=Content-Type>' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF '<META name=GENERATOR content="MSHTML 11.00.9600.17496">' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'BACKGROUND: url(https://www.evernote.com/redesign/sharing/SharedNoteViewAction/download.png) 50% 50%;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'BACKGROUND: url(https://www.evernote.com/redesign/sharing/SharedNoteViewAction/gallery.png) 50% 50%;' IN string WITH ''. "#EC *
*    REPLACE ALL OCCURRENCES OF REGEX '\s+class=[^ >]*|\s+align=[^ >]*|\s+width=[^ >]*|\s+valign=[^ >]*|\s+style="+[^"]*"|</?SPAN+\s+[^>]*>|</SPAN>|&nbsp;|<p></p>|\s+border=[^ >]*|\s+cellpadding=[^ >]*|\s+cellspacing=[^ >]*' IN  string WITH ''.
        REPLACE ALL OCCURRENCES OF REGEX '\s+class=[^ >]*|\s+align=[^ >]*|\s+width=[^ >]*|\s+valign=[^ >]*|</?SPAN+\s+[^>]*>|</SPAN>|<p></p>|\s+border=[^ >]*|\s+cellpadding=[^ >]*|\s+cellspacing=[^ >]*' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'BACKGROUND: url(https://www.evernote.com/redesign/sharing/SharedNoteViewAction/download.png) 50% 50%;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF '&nbsp;' IN string WITH ` `. "#EC *
        REPLACE ALL OCCURRENCES OF REGEX '<DIV role=button+\s+[^>]*>' IN string WITH ''. "#EC *

        REPLACE ALL OCCURRENCES OF REGEX 'BACKGROUND: url*newattachmentcards*;' IN string WITH ''. "#EC *

        REPLACE ALL OCCURRENCES OF 'BORDER-TOP: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'BORDER-RIGHT: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'WHITE-SPACE: normal;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'WORD-SPACING: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'BORDER-BOTTOM: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'TEXT-TRANSFORM: none;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'COLOR: rgb(34,34,34);' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'PADDING-BOTTOM: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'PADDING-TOP: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'PADDING-LEFT: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'MARGIN: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'BORDER-LEFT: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'LETTER-SPACING: normal;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'PADDING-RIGHT: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'BACKGROUND-COLOR: rgb(255,255,255);' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'TEXT-INDENT: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF '-webkit-text-stroke-width: 0px' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'MAX-WIDTH: 100%;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'HEIGHT: auto;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'BORDER-RIGHT: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'PADDING-BOTTOM: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'PADDING-RIGHT: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'WHITE-SPACE: normal;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'VERTICAL-ALIGN: middle;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'MARGIN: 0cm 0cm 0pt;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'BORDER-LEFT-WIDTH: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'BORDER-BOTTOM-WIDTH: 0px;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'WORD-BREAK: break-all;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'CURSOR: pointer;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'POSITION: relative;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'DISPLAY: inline-block;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'DISPLAY: inline;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'DISPLAY: inline-block' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'PADDING-RIGHT: 0px' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'alt="Inline image 1"' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'alt="Inline image 2"' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'POSITION: absolute;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'Z-INDEX: 100;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'Z-INDEX: 101;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'COLOR: rgb(0,0,0);' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'opacity: 0;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF 'pointer-events: none;' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF '<U></U>' IN string WITH ''. "#EC *
        REPLACE ALL OCCURRENCES OF '<P style="FONT: 11pt Calibri, sans-serif; WIDOWS: 1">' IN string WITH ''. "#EC *

        IF quitar_graficos = 'X'.
          REPLACE ALL OCCURRENCES OF REGEX '<IMG+\s+[^>]*>' IN string WITH ''. "#EC *
          REPLACE ALL OCCURRENCES OF REGEX '<DIV+\s+[^>]*>' IN string WITH ''. "#EC *
          REPLACE ALL OCCURRENCES OF REGEX '<BR>' IN string WITH ''. "#EC *
          REPLACE ALL OCCURRENCES OF REGEX '</BR>' IN string WITH ''. "#EC *
        ENDIF.

      CATCH cx_sy_regex_too_complex.
    ENDTRY.
  ENDIF.

  endmethod.
  method GET_EDITOR.

  IF o_texto_simple IS INITIAL.
    string = get_content_as_string( ).
  ELSE.
    CALL METHOD o_texto_simple->get_editor
      EXPORTING
        col_width      = col_width
      IMPORTING
        lines          = lines
        changed        = changed
        string         = string
      EXCEPTIONS
        internal_error = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RAISE internal_error.
    ENDIF.
  ENDIF.

  endmethod.
  METHOD set_content.

    btf_doc->set_content(
      EXPORTING text     = text
                encoding = encoding
                clean    = clean ).
    TRY.
        editor->set_content( ).
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.
  METHOD set_content_as_string.

    DATA: l_xstring TYPE xstring,
          l_len     TYPE i,
          l_string  TYPE string.

    IF NOT o_visor_html IS INITIAL.
      IF NOT string IS INITIAL.
        l_xstring = zcl_ap_string=>string2xstring( string ).
        cl_bcs_convert=>xstring_to_solix(
          EXPORTING
            iv_xstring   = l_xstring
          RECEIVING
            et_solix  = DATA(i_solix) ).
      ENDIF.

      CALL METHOD o_visor_html->o_html->load_data
        EXPORTING
          url        = 'index.html'
          encoding   = 'UTF-8'
          type       = 'html'
        CHANGING
          data_table = i_solix. " o_visor_html->i_html.

      o_visor_html->show( 'index.html' ).
    ELSE.
      CALL FUNCTION 'ECATT_CONV_STRING_TO_XSTRING'
        EXPORTING
          im_string   = string
          im_encoding = 'UTF-8'
        IMPORTING
          ex_xstring  = l_xstring
          ex_len      = l_len.

      set_content( EXPORTING text = l_xstring encoding = 'UTF-8' ).
    ENDIF.

  ENDMETHOD.
  method SET_DISPLAY_MODE.

  me->display_mode = display_mode.

  IF me->display_mode IS INITIAL .
    TRY.
        editor->set_design_mode( if_btf_editor_constants=>co_design_mode_on ).
      CATCH cx_root.
    ENDTRY.
  ELSE.
    TRY.
        editor->set_design_mode( if_btf_editor_constants=>co_design_mode_off ).
      CATCH cx_root.
    ENDTRY.
  ENDIF.

  endmethod.
  METHOD set_editor.
    DATA: o_conf   TYPE REF TO if_btf_configuration,
          font     TYPE tdfamily,
          l_string TYPE string.

    IF o_texto_simple IS INITIAL.
      me->btf = cl_btf=>get_reference( ).
      o_conf  = me->btf->create_configuration( sy-langu ).
*  font    = 'HELVE'.
*  o_conf->set_default_font( font = font ).
      TRY.
          me->btf_doc = me->btf->create_document( language = sy-langu configuration = o_conf ).
        CATCH cx_root.
      ENDTRY.
      me->editor  = me->btf->create_editor( me->btf_doc ).

      IF me->container_editor IS INITIAL.
        TRY.
            CREATE OBJECT me->container_editor
              EXPORTING
                container_name = me->controlname.
          CATCH cx_root.
        ENDTRY.
      ENDIF.

      TRY.
          IF display_mode = abap_false.
            CALL METHOD me->editor->initialize
              EXPORTING
                ctrl_parent = me->container_editor.
          ELSE.
            O_VISOR_HTML = NEW #( container = me->container_editor ).
          ENDIF.
        CATCH cx_root.
      ENDTRY.

      IF NOT status_text IS INITIAL.
        titulo =  status_text.
      ENDIF.

      set_display_mode( display_mode ).


      l_string = string.
      IF display_mode IS INITIAL.
        IF NOT string CS zcl_ap_html=>c_ini_html_btf.
          CONCATENATE '<pre>' string '</pre>' INTO l_string.
        ENDIF.
      ENDIF.

      IF NOT string IS INITIAL.
        set_content_as_string( string = l_string encoding = encoding ).
      ENDIF.
    ELSE.
      l_string = string.
      IF NOT string IS INITIAL.
        IF string CS zcl_ap_html=>c_ini_html_btf.
          l_string = zcl_ap_html=>remove_tags( l_string ).
        ENDIF.
      ENDIF.

      CALL METHOD o_texto_simple->set_editor
        EXPORTING
          max_cols       = max_cols
          show_status    = show_status
          status_text    = status_text
          show_tool      = show_tool
          display_mode   = display_mode
          lines          = lines
          string         = l_string
        EXCEPTIONS
          create_error   = 1
          internal_error = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        RAISE create_error.
      ENDIF.
    ENDIF.

  endmethod.