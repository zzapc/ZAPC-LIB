
CLASS zcl_ap_control_texto_btf DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF control_tab,
        ctrl_name        TYPE scrfname,
        parent           TYPE i,
        ctrl             TYPE REF TO c_textedit_control,
        editor_container TYPE REF TO cl_gui_custom_container,
        ctrl_new         TYPE REF TO cl_gui_textedit,
      END OF control_tab.
    TYPES t_control_tab TYPE TABLE OF control_tab.

    METHODS constructor
      IMPORTING controlname      TYPE scrfname
                texto_simple     TYPE abap_bool DEFAULT ''
                simplificar_html TYPE abap_bool DEFAULT ''.

    METHODS set_editor
      IMPORTING  max_cols     TYPE i       DEFAULT 0
                 show_status  TYPE c       DEFAULT ''
                 status_text  TYPE sytitle DEFAULT ''
                 show_tool    TYPE c       DEFAULT ''
                 display_mode TYPE c       DEFAULT ''
                 !lines       TYPE table   OPTIONAL
                 !string      TYPE any     OPTIONAL
                 !encoding    TYPE string  DEFAULT 'utf-8'
      PREFERRED PARAMETER max_cols
      EXCEPTIONS create_error
                 internal_error.

    METHODS get_editor
      IMPORTING  col_width TYPE i DEFAULT 0
      EXPORTING  !lines    TYPE table
                 changed   TYPE c
                 !string   TYPE any
      EXCEPTIONS internal_error.

    METHODS destroy.

    METHODS set_display_mode
      IMPORTING display_mode TYPE flag DEFAULT 'X'.

    METHODS set_content
      IMPORTING !text     TYPE xstring
                !encoding TYPE string DEFAULT 'utf-8'
                clean     TYPE i      DEFAULT 0.

    METHODS set_content_as_string
      IMPORTING !string   TYPE string
                !encoding TYPE string DEFAULT 'utf-8'.

    METHODS get_content
      EXPORTING !language TYPE tdspras
                !encoding TYPE string
                !text     TYPE xstring.

    METHODS get_content_as_string
      IMPORTING quitar_graficos TYPE abap_bool DEFAULT ''
      RETURNING VALUE(string)   TYPE string.


  PRIVATE SECTION.
    DATA controlname      TYPE scrfname.
    DATA i_control_tab    TYPE t_control_tab.
    DATA btf              TYPE REF TO if_btf.
    DATA btf_doc          TYPE REF TO if_btf_document.
    DATA editor           TYPE REF TO if_btf_editor.
    DATA titulo           TYPE vtext                          VALUE 'Editor' ##NO_TEXT.
    DATA container_editor TYPE REF TO cl_gui_custom_container.
    DATA display_mode     TYPE flag                           VALUE space ##NO_TEXT.
    DATA o_texto_simple   TYPE REF TO zcl_ap_control_texto.
    DATA o_visor_html     TYPE REF TO zcl_ap_html.
    DATA simplificar_html TYPE abap_bool                      VALUE '' ##NO_TEXT.
endclass. "ZCL_AP_CONTROL_TEXTO_BTF definition
class ZCL_AP_CONTROL_TEXTO_BTF implementation.
  METHOD constructor.
    DATA: rc    TYPE c LENGTH 1,
          l_its TYPE c LENGTH 1.

    me->controlname      = controlname.
    me->simplificar_html = simplificar_html.

    CALL FUNCTION 'IS_INTEGRATED_ITS'
      IMPORTING
        return = rc
      EXCEPTIONS
        error  = 1
        OTHERS = 2.
    IF sy-subrc = 0 AND NOT rc IS INITIAL.
      l_its = 'X'.
    ENDIF.

    IF    NOT texto_simple IS INITIAL
       OR     l_its         = 'X'.
      o_texto_simple = NEW #(
          controlname = controlname ).
    ENDIF.
  ENDMETHOD.
  METHOD destroy.
    DATA l_valid TYPE i.

*  me->btf->free( ).

    IF o_texto_simple IS INITIAL.
      container_editor->is_valid( IMPORTING result = l_valid ).
      IF NOT l_valid IS INITIAL.
        container_editor->free( ).
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
  ENDMETHOD.
  METHOD get_content.
    TRY.
        editor->get_content( ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
    btf_doc->get_content(
      IMPORTING text     = text
                encoding = encoding
                language = language ).
  ENDMETHOD.
  METHOD get_content_as_string.
    DATA: l_xstring  TYPE xstring,
          l_encoding TYPE string,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_enc      TYPE abap_encod.

    get_content( IMPORTING text = l_xstring encoding = l_encoding ).
    IF l_xstring IS INITIAL.
      RETURN.
    ENDIF.
    l_enc = l_encoding.

    string = zcl_ap_string=>xstring2string( xstring = l_xstring  encoding = zcl_c=>codepage_unicode ).
    IF string CS '<BODY></BODY>' OR string CS '<BODY> </BODY>'.
      CLEAR string.
    ELSE.
      IF zcl_c=>codepage_unicode <> zcl_c=>codepage_html ##BOOL_OK.
        string = zcl_ap_string=>convierte_codificacion( cadena = string conv_caract_in = '#' conv_caract_out = cl_abap_char_utilities=>cr_lf ).
      ENDIF.

      TRY.
* Limpieza código general
          REPLACE ALL OCCURRENCES OF '<HTML xmlns="http://www.w3.org/1999/xhtml">' IN string WITH '' ##NO_TEXT.
          REPLACE ALL OCCURRENCES OF '<META content="text/html; charset=utf-8" http-equiv=Content-Type>' IN string WITH '' ##NO_TEXT.
          REPLACE ALL OCCURRENCES OF '<META name=GENERATOR content="MSHTML 11.00.9600.17496">' IN string WITH '' ##NO_TEXT.
          REPLACE ALL OCCURRENCES OF 'BACKGROUND: url(https://www.evernote.com/redesign/sharing/SharedNoteViewAction/download.png) 50% 50%;' IN string WITH '' ##NO_TEXT.
          REPLACE ALL OCCURRENCES OF 'BACKGROUND: url(https://www.evernote.com/redesign/sharing/SharedNoteViewAction/gallery.png) 50% 50%;' IN string WITH '' ##NO_TEXT.
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
  ENDMETHOD.
  METHOD get_editor.
    IF o_texto_simple IS INITIAL.
      string = get_content_as_string( ).
    ELSE.
      o_texto_simple->get_editor(
        EXPORTING
          col_width      = col_width
        IMPORTING
          lines          = lines
          changed        = changed
          string         = string
        EXCEPTIONS
          internal_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        RAISE internal_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD set_content.
    btf_doc->set_content(
        text     = text
        encoding = encoding
        clean    = clean ).
    TRY.
        editor->set_content( ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.
  METHOD set_content_as_string.
    DATA: l_xstring TYPE xstring,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_len     TYPE i.

    IF NOT o_visor_html IS INITIAL.
      IF NOT string IS INITIAL.
        l_xstring = zcl_ap_string=>string2xstring( string ).
        DATA(i_solix) = cl_bcs_convert=>xstring_to_solix(
                            iv_xstring   = l_xstring ).
      ENDIF.

      o_visor_html->o_html->load_data(
        EXPORTING
          url        = 'index.html'
          encoding   = 'UTF-8'
          type       = 'html'
        CHANGING
          data_table = i_solix ) ##NO_TEXT.

      o_visor_html->show( 'index.html' ) ##NO_TEXT.
    ELSE.
      CALL FUNCTION 'ECATT_CONV_STRING_TO_XSTRING'
        EXPORTING
          im_string   = string
          im_encoding = 'UTF-8'
        IMPORTING
          ex_xstring  = l_xstring
          ex_len      = l_len.

      set_content( text = l_xstring encoding = 'UTF-8' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.
  METHOD set_display_mode.
    me->display_mode = display_mode.

    IF me->display_mode IS INITIAL.
      TRY.
          editor->set_design_mode( if_btf_editor_constants=>co_design_mode_on ).
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ELSE.
      TRY.
          editor->set_design_mode( if_btf_editor_constants=>co_design_mode_off ).
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD set_editor.
    DATA: o_conf   TYPE REF TO if_btf_configuration,
          l_string TYPE string.
*          font     TYPE tdfamily.

    IF o_texto_simple IS INITIAL.
      btf = cl_btf=>get_reference( ).
      o_conf  = btf->create_configuration( sy-langu ).
*  font    = 'HELVE'.
*  o_conf->set_default_font( font = font ).
      TRY.
          btf_doc = btf->create_document( language = sy-langu configuration = o_conf ).
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
      editor = btf->create_editor( me->btf_doc ).

      IF me->container_editor IS INITIAL.
        TRY.
            me->container_editor = NEW #(
                container_name = controlname ).
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.
      ENDIF.

      TRY.
          IF display_mode = abap_false.
            me->editor->initialize(
                ctrl_parent = container_editor ).
          ELSE.
            o_visor_html = NEW #( container = container_editor ).
          ENDIF.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

      IF NOT status_text IS INITIAL.
        titulo = status_text.
      ENDIF.

      set_display_mode( display_mode ).

      IF NOT string IS INITIAL.
        l_string = string.
      ELSE.
        l_string = zcl_ap_string=>tabla2string( lines ).
      ENDIF.

      IF NOT l_string CS zcl_ap_html=>c_ini_html_btf OR NOT l_string CS '<html>' OR NOT l_string CS '<html>'.
        CONCATENATE '<pre>' string '</pre>' INTO l_string.
      ENDIF.

      IF NOT l_string IS INITIAL.
        set_content_as_string( string = l_string encoding = encoding ).
      ENDIF.
    ELSE.
      l_string = string.
      IF NOT string IS INITIAL.
        IF string CS zcl_ap_html=>c_ini_html_btf AND simplificar_html = 'X'.
          l_string = zcl_ap_html=>remove_tags( l_string ).
        ENDIF.
      ENDIF.

      o_texto_simple->set_editor(
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
          OTHERS         = 3 ).
      IF sy-subrc <> 0.
        RAISE create_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.