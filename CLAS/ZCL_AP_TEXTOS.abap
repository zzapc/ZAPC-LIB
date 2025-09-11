class ZCL_AP_TEXTOS definition
  public
  create public .

public section.

  class-methods GET_TEXTO
    importing
      !ID type ANY
      !NAME type ANY
      !SPRAS type STXH-TDSPRAS optional
      !OBJECT type ANY
      !MEMORY type ABAP_BOOL default ''
      !ELIM_REGEXP type ANY default ''
    returning
      value(LINEAS) type TLINETAB .

    CLASS-METHODS save_texto
      IMPORTING !id             TYPE stxh-tdid
                !name           TYPE any
                spras           TYPE stxh-tdspras OPTIONAL
                !object         TYPE any
                lineas          TYPE tlinetab
                savemode_direct TYPE abap_bool    DEFAULT 'X'.

  class-methods GET_STXH
    importing
      !ID type ANY
      !NAME type ANY
      !SPRAS type STXH-TDSPRAS optional
      !OBJECT type ANY
    returning
      value(STXH) type STXH .
  class-methods GET_TEXTO_STRING
    importing
      !ID type ANY
      !NAME type ANY
      !SPRAS type STXH-TDSPRAS optional
      !OBJECT type ANY
      !SEPARAR_CON_ESPACIO type ABAP_BOOL default ''
      !MEMORY type ABAP_BOOL default ''
      !ELIM_REGEXP type ANY default ''
      !TODO_SALTOS type ABAP_BOOL default ''
    returning
      value(STRING) type STRING .
  class-methods EDITAR_TEXTO
    importing
      !ID type STXH-TDID
      !NAME type ANY
      !SPRAS type STXH-TDSPRAS optional
      !OBJECT type STXH-TDOBJECT
      value(DISPLAY) type ABAP_BOOL default 'X'
      !FORZAR_NUEVO type ABAP_BOOL default '' .
  class-methods SUSTITUIR_TEXTO
    importing
      !ID type STXH-TDID
      !NAME type STXH-TDNAME
      !SPRAS type STXH-TDSPRAS optional
      !OBJECT type STXH-TDOBJECT
      !TEXTO_ORIGINAL type ANY
      !TEXTO_REEMPLAZO type ANY
      !CASE type ABAP_BOOL default 'X'
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SAVE_TEXTO_STRING
    importing
      !ID type STXH-TDID
      !NAME type ANY
      !SPRAS type STXH-TDSPRAS optional
      !OBJECT type STXH-TDOBJECT
      !STRING type STRING
      !LONGITUD type I default 72
      !WORD_WRAP type ABAP_BOOL default 'X'
      !DELIMITER type C default ''
      !SAVEMODE_DIRECT type ABAP_BOOL default 'X'
      !INSERT type ABAP_BOOL default 'X' .
  class-methods GET_TEXTO_STRING_HTML
    importing
      !ID type ANY
      !NAME type ANY
      !SPRAS type STXH-TDSPRAS optional
      !OBJECT type ANY
    returning
      value(STRING) type STRING .
  class-methods SHOW_POPUP
    importing
      !TITULO type ANY optional
      !I_TEXTO type TLINETAB .
  class-methods GET_TEXTO_TABLE_STRING
    importing
      !ID type ANY
      !NAME type ANY
      !SPRAS type STXH-TDSPRAS optional
      !OBJECT type ANY
      !SEPARAR_CON_ESPACIO type ABAP_BOOL default ''
    returning
      value(T_STRING) type TABLE_OF_STRINGS .
  class-methods TLINE2STRING
    importing
      !LINEAS type TLINETAB
      !EMPEZAR_EN_LINEA type I default 1
      !SEPARAR_CON_ESPACIO type ABAP_BOOL default ''
      !TODO_SALTOS type ABAP_BOOL default ''
    returning
      value(STRING) type STRING .
  class-methods STRING2TLINE
    importing
      !LONGITUD type I default 72
      !STRING type STRING
      !WORD_WRAP type ABAP_BOOL default 'X'
      !DELIMITER type C default ''
    returning
      value(LINEAS) type TLINETAB .
  class-methods BORRAR_TEXTO
    importing
      !ID type STXH-TDID
      !NAME type STXH-TDNAME
      !SPRAS type STXH-TDSPRAS optional
      !OBJECT type STXH-TDOBJECT
    returning
      value(RETURN) type BOOLEAN .
  class-methods GET_CATALOG
    returning
      value(CATALOG) type CMS_TAB_TEXT_MEMORY .
protected section.
private section.
endclass. "ZCL_AP_TEXTOS definition
class ZCL_AP_TEXTOS implementation.
METHOD borrar_texto.

    CALL FUNCTION 'DELETE_TEXT'
      EXPORTING
*       CLIENT          = SY-MANDT
        id              = id
        language        = spras
        name            = name
        object          = object
        savemode_direct = 'X'
*       TEXTMEMORY_ONLY = ' '
*       LOCAL_CAT       = ' '
      EXCEPTIONS
        not_found       = 1
        OTHERS          = 2.
    IF sy-subrc NE 0.
      return = 0.
    ELSE.
      return = 1.
    ENDIF.


  ENDMETHOD.
method EDITAR_TEXTO.
  DATA: l_stxh TYPE stxh,
        lineas TYPE tlinetab,
        l_header TYPE  thead,
        l_name TYPE stxh-tdname,
        l_line TYPE tline.
  CLEAR lineas.

  l_name = name.
  l_stxh = get_stxh( id     = id
                     spras  = spras
                     name   = l_name
                     object = object ).
  IF l_stxh IS INITIAL AND NOT spras IS INITIAL.
    l_stxh = get_stxh( id     = id
                       name   = l_name
                       object = object ).
  ENDIF.

  CHECK NOT l_stxh IS INITIAL OR forzar_nuevo = 'X'.

  IF l_stxh IS INITIAL AND forzar_nuevo = 'X'.
    l_header-tdid = id.
    l_header-tdname = l_name.
    l_header-tdobject = object.
    IF spras IS INITIAL.
      l_header-tdspras = sy-langu.
    ELSE.
      l_header-tdspras = spras.
    ENDIF.
    MOVE-CORRESPONDING l_header TO l_stxh.
    l_line-tdformat = '*'.
    APPEND l_line TO lineas.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*       CLIENT                = SY-MANDT
        header                = l_header
        insert                = 'X'
*       SAVEMODE_DIRECT       = ' '
*       OWNER_SPECIFIED       = ' '
*       LOCAL_CAT             = ' '
*     IMPORTING
*       FUNCTION              =
*       NEWHEADER             =
      TABLES
        lines                 = lineas
     EXCEPTIONS
       id                    = 1
       language              = 2
       name                  = 3
       object                = 4
       OTHERS                = 5
              .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CLEAR lineas.
  ENDIF.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = l_stxh-tdid
      language                = l_stxh-tdspras
      name                    = l_stxh-tdname
      object                  = l_stxh-tdobject
    IMPORTING
      header                  = l_header
    TABLES
      lines                   = lineas
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  CALL FUNCTION 'EDIT_TEXT'
    EXPORTING
      display             = display
*   EDITOR_TITLE        = ' '
      header              = l_header
*   PAGE                = ' '
*   WINDOW              = ' '
*   SAVE                = 'X'
*   LINE_EDITOR         = ' '
*   CONTROL             = ' '
*   PROGRAM             = ' '
*   LOCAL_CAT           = ' '
* IMPORTING
*   FUNCTION            =
*   NEWHEADER           =
*   RESULT              =
    TABLES
      lines               = lineas
 EXCEPTIONS
   id                  = 1
   language            = 2
   linesize            = 3
   name                = 4
   object              = 5
   textformat          = 6
   communication       = 7
   OTHERS              = 8
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*       CLIENT                = SY-MANDT
        header                = l_header
        insert                = 'X'
        savemode_direct       = 'X'
*       OWNER_SPECIFIED       = ' '
*       LOCAL_CAT             = ' '
*     IMPORTING
*       FUNCTION              =
*       NEWHEADER             =
      TABLES
        lines                 = lineas
     EXCEPTIONS
       id                    = 1
       language              = 2
       name                  = 3
       object                = 4
       OTHERS                = 5
              .
  ENDIF.

endmethod.
METHOD get_catalog.
    IMPORT catalog TO catalog FROM MEMORY ID 'SAPLSTXD'.
  ENDMETHOD.
method GET_STXH.

  clear stxh.
  IF not spras IS SUPPLIED OR spras IS INITIAL.
    SELECT *
      FROM stxh
      WHERE tdid = @id
        AND tdname = @name
        AND tdobject = @object
      ORDER BY PRIMARY KEY
      INTO @stxh
      UP TO 1 ROWS.
    ENDSELECT.
  ELSE.
    SELECT SINGLE * FROM stxh
      INTO stxh
     WHERE tdid = id
       AND tdspras = spras
       AND tdname = name
       AND tdobject = object.
  ENDIF.

endmethod.
METHOD get_texto.
  DATA l_stxh TYPE stxh.

  CLEAR lineas.

  l_stxh = get_stxh( id     = id
                     spras  = spras
                     name   = name
                     object = object ).

  IF memory = 'X' AND l_stxh IS INITIAL.
    l_stxh-tdid = id.
    l_stxh-tdspras = spras.
    l_stxh-tdname = name.
    l_stxh-tdobject = object.
  ENDIF.

  CHECK NOT l_stxh IS INITIAL.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = l_stxh-tdid
      language                = l_stxh-tdspras
      name                    = l_stxh-tdname
      object                  = l_stxh-tdobject
    TABLES
      lines                   = lineas
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF NOT elim_regexp IS INITIAL.
    LOOP AT lineas ASSIGNING FIELD-SYMBOL(<linea>) WHERE tdformat = '>X' AND tdline(2) = '*'.
      IF <linea> CS '.' AND <linea> CS ':' AND <linea> CS '(' AND <linea> CS ')'.
        DELETE lineas.
      ELSE.
        <linea>-tdline = <linea>-tdline+2.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMETHOD.
METHOD get_texto_string.
  DATA: i_lineas TYPE tlinetab.

  i_lineas = get_texto( id     = id
                        name   = name
                        spras  = spras
                        object = object
                        memory = memory
                        elim_regexp = elim_regexp ).

  string = tline2string( lineas = i_lineas separar_con_espacio = separar_con_espacio todo_saltos = todo_saltos ).


ENDMETHOD.
method GET_TEXTO_STRING_HTML.
  DATA: i_lineas TYPE tlinetab,
        l_tline TYPE tline,
        i_tdline TYPE TABLE OF tdline,
        l_format_ant TYPE tdformat,
        l_first.

  i_lineas = get_texto( id     = id
                        name   = name
                        spras  = spras
                        object = object ).

  IF i_lineas IS INITIAL.
    CLEAR string.
  ELSE.
    string = '<P>'.
    LOOP AT i_lineas INTO l_tline.
      CLEAR l_first.
      AT FIRST.
        l_first = 'X'.
      ENDAT.
      IF l_first = ''.
        IF NOT l_tline-tdformat IS INITIAL.
          CONCATENATE string '</B>' INTO string.
        ENDIF.
      ENDIF.
      CONCATENATE string l_tline-tdline INTO string.
      l_format_ant = l_tline-tdformat.
    ENDLOOP.
    CONCATENATE string '</P>' INTO string.
  ENDIF.

endmethod.
method GET_TEXTO_TABLE_STRING.
  DATA: i_lineas TYPE tlinetab,
        l_tline TYPE tline,
        l_string TYPE string,
        l_first,
        l_last.

  clear t_string.

  i_lineas = get_texto( id     = id
                        name   = name
                        spras  = spras
                        object = object ).

  LOOP AT i_lineas INTO l_tline.
    CLEAR: l_first, l_last.
    AT FIRST.
      l_first = 'X'.
    ENDAT.
    AT LAST.
      l_last = 'X'.
    ENDAT.
    IF l_first = 'X'.
      l_string = l_tline-tdline.
    ELSE.
      IF l_tline-tdformat = '' OR l_tline-tdformat = '='.
        CONCATENATE l_string l_tline-tdline INTO l_string SEPARATED BY space.
      ELSE.
        IF l_first = ''.
          APPEND l_string TO t_string.
        ENDIF.
        l_string = l_tline-tdline.
      ENDIF.
    ENDIF.
    IF l_last = 'X'.
      APPEND l_string TO t_string.
    ENDIF.
  ENDLOOP.


endmethod.
method SAVE_TEXTO.
  DATA: l_header TYPE thead,
        l_stxh   TYPE stxh.

  l_stxh = get_stxh( id     = id
                     spras  = spras
                     name   = name
                     object = object ).

  CLEAR l_header.
  MOVE-CORRESPONDING l_stxh TO l_header.
  l_header-tdname    =  name.
  l_header-tdobject  = object.
  l_header-tdid    = id.
  IF spras IS INITIAL.
    l_header-tdspras  = sy-langu.
  ELSE.
    l_header-tdspras  = spras.
  ENDIF.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header          = l_header
      insert          = 'X'
      savemode_direct = savemode_direct
    TABLES
      lines           = lineas
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.

endmethod.
METHOD save_texto_string.
  DATA: l_header     TYPE thead,
        l_stxh       TYPE stxh,
        lineas       TYPE TABLE OF tline,
        i_lineas     TYPE TABLE OF tdline,
        i_lineas_aux TYPE TABLE OF tdline,
        i_strings    TYPE TABLE OF string,
        l_text132    TYPE tdline,
        l_tline      TYPE tline.

  l_stxh = get_stxh( id     = id
                     spras  = spras
                     name   = name
                     object = object ).

  CLEAR l_header.
  MOVE-CORRESPONDING l_stxh TO l_header.
  l_header-tdname    =  name.
  l_header-tdobject  = object.
  l_header-tdid    = id.
  IF spras IS INITIAL.
    l_header-tdspras  = sy-langu.
  ELSE.
    l_header-tdspras  = spras.
  ENDIF.

  IF word_wrap = 'X'.
    DATA: l_textline(5000),
          l_lin_aux        TYPE tdline,
          l_long           TYPE i,
          l_nlineas        TYPE i,
          l_index          TYPE i.

    zcl_ap_string=>string2tabla( EXPORTING string = string
                                           longitud = 99999
                                 CHANGING  tabla  = i_strings ).
    LOOP AT i_strings INTO l_textline.
      CLEAR i_lineas_aux.
      CALL FUNCTION 'RKD_WORD_WRAP'
        EXPORTING
          textline            = l_textline
          delimiter           = delimiter
          outputlen           = longitud
*   IMPORTING
*         OUT_LINE1           =
*         OUT_LINE2           =
*         OUT_LINE3           =
        TABLES
          out_lines           = i_lineas_aux
        EXCEPTIONS
          outputlen_too_large = 1
          OTHERS              = 2.

      IF lines( i_lineas_aux ) = 1.
        READ TABLE i_lineas_aux INTO DATA(l_lin) INDEX 1.
        CLEAR l_tline.
        l_tline-tdformat = '*'.
        l_tline-tdline = l_lin.
        APPEND l_tline TO lineas.
      ELSE.
* Buscamos posibles finales de líneas.
        DESCRIBE TABLE i_lineas_aux LINES l_nlineas.
        DO l_nlineas TIMES.
          READ TABLE i_lineas_aux INTO l_text132 INDEX sy-index.
          IF sy-subrc = 0.
            l_index = sy-tabix.
            l_long = strlen( l_text132 ).
            IF l_long < longitud.
              ADD 1 TO l_index.
              READ TABLE i_lineas_aux INTO l_text132 INDEX l_index.
              IF sy-subrc = 0.
                IF l_long < longitud.
                  l_text132+1 = l_text132.
                  CLEAR l_text132(1).
                  MODIFY i_lineas_aux FROM l_text132 INDEX l_index.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDDO.

        LOOP AT i_lineas_aux INTO l_text132.
          CLEAR l_tline.
          l_tline-tdformat = '='.
          AT FIRST.
            l_tline-tdformat = '*'.
          ENDAT.
          l_tline-tdline = l_text132.
          APPEND l_tline TO lineas.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ELSE.
    zcl_ap_string=>string2tabla( EXPORTING string = string
                                           longitud = longitud
                                 CHANGING  tabla  = i_lineas ).

    LOOP AT i_lineas INTO l_text132.
      CLEAR l_tline.
      l_tline-tdformat = '*'.
      l_tline-tdline = l_text132.
      APPEND l_tline TO lineas.
    ENDLOOP.
  ENDIF.

*  LOOP AT i_lineas INTO l_text132.
*    CLEAR l_tline.
*    l_tline-tdformat = '='.
*    AT FIRST.
*      l_tline-tdformat = '*'.
*    ENDAT.
*    l_tline-tdline = l_text132.
*    APPEND l_tline TO lineas.
*  ENDLOOP.

  IF string IS INITIAL.
    zcl_ap_textos=>borrar_texto( id = l_header-tdid
                                 object = l_header-tdobject
                                 name = l_header-tdname
                                 spras = l_header-tdspras ).
  ELSE.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = l_header
        insert          = insert
        savemode_direct = savemode_direct
      TABLES
        lines           = lineas
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
  ENDIF.

ENDMETHOD.
method SHOW_POPUP.

  CALL FUNCTION 'COPO_POPUP_TO_DISPLAY_TEXTLIST'
    EXPORTING
*   TASK             = 'DISPLAY'
      titel            = titulo
* IMPORTING
*   FUNCTION         =
    TABLES
      text_table       = i_texto.

endmethod.
METHOD string2tline.
  TYPES: BEGIN OF t_linea,
           linea(60000),
         END OF t_linea.
  DATA: i_lineas          TYPE TABLE OF tdline,
        l_text132         TYPE tdline,
        l_tline           TYPE tline,
        l_textline(60000),
        l_lin_aux         TYPE tdline,
        l_long            TYPE i,
        l_nlineas         TYPE i,
        l_index           TYPE i,
        l_linea           TYPE t_linea,
        tabla             TYPE TABLE OF t_linea,
        i_lineas_aux      TYPE TABLE OF tdline.

  IF word_wrap = 'X'.
    SPLIT string AT cl_abap_char_utilities=>cr_lf INTO TABLE tabla.
    LOOP AT tabla INTO l_linea.
      l_textline = l_linea.
      CLEAR i_lineas.
      CALL FUNCTION 'RKD_WORD_WRAP'
        EXPORTING
          textline            = l_textline
          delimiter           = delimiter
          outputlen           = longitud
*   IMPORTING
*         OUT_LINE1           =
*         OUT_LINE2           =
*         OUT_LINE3           =
        TABLES
          out_lines           = i_lineas
        EXCEPTIONS
          outputlen_too_large = 1
          OTHERS              = 2.

* Buscamos posibles finales de líneas.
      DESCRIBE TABLE i_lineas LINES l_nlineas.
      DO l_nlineas TIMES.
        READ TABLE i_lineas INTO l_text132 INDEX sy-index.
        IF sy-subrc = 0.
          l_index = sy-tabix.
          l_long = strlen( l_text132 ).
          IF l_long < longitud.
            ADD 1 TO l_index.
            READ TABLE i_lineas INTO l_text132 INDEX l_index.
            IF sy-subrc = 0.
              IF l_long < longitud.
                l_text132+1 = l_text132.
                CLEAR l_text132(1).
                MODIFY i_lineas FROM l_text132 INDEX l_index.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.

      LOOP AT i_lineas INTO l_text132.
        CLEAR l_tline.
        l_tline-tdformat = '='.
        l_tline-tdline = l_text132.
        AT FIRST.
          l_tline-tdformat = '*'.
        ENDAT.
        append l_tline to lineas.
      ENDLOOP.
    ENDLOOP.


  ELSEIF word_wrap = 'F'.
    l_textline = string.
    CALL FUNCTION 'C14W_STRING_TO_TLINE'
      EXPORTING
        i_string    = l_textline
      TABLES
        e_tline_tab = lineas.
  ELSE.
    zcl_ap_string=>string2tabla( EXPORTING string = string
                                           longitud = longitud
                                 CHANGING  tabla  = i_lineas ).

    LOOP AT i_lineas INTO l_text132.
      CLEAR l_tline.
      l_tline-tdformat = '='.
      AT FIRST.
        l_tline-tdformat = '*'.
      ENDAT.
      l_tline-tdline = l_text132.
      APPEND l_tline TO lineas.
    ENDLOOP.
  ENDIF.





ENDMETHOD.
method SUSTITUIR_TEXTO.
  DATA: l_stxh TYPE stxh,
        lineas TYPE tlinetab,
        l_header TYPE  thead,
        l_cambio.

  FIELD-SYMBOLS <linea> TYPE tline.

  CLEAR lineas.

  l_stxh = get_stxh( id     = id
                     spras  = spras
                     name   = name
                     object = object ).

  CHECK NOT l_stxh IS INITIAL.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = l_stxh-tdid
      language                = l_stxh-tdspras
      name                    = l_stxh-tdname
      object                  = l_stxh-tdobject
    IMPORTING
      header                  = l_header
    TABLES
      lines                   = lineas
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  LOOP AT lineas ASSIGNING <linea>.
    IF <linea>-tdline CS texto_original.
      IF case = 'X'.
    REPLACE ALL OCCURRENCES OF texto_original IN <linea>-tdline WITH texto_reemplazo
              IN CHARACTER MODE RESPECTING CASE.
      ELSE.
    REPLACE ALL OCCURRENCES OF texto_original IN <linea>-tdline WITH texto_reemplazo
              IN CHARACTER MODE IGNORING CASE.
      ENDIF.
      l_cambio = 'X'.
    ENDIF.
  ENDLOOP.

  CHECK l_cambio = 'X'.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header          = l_header
      insert          = ' '
      savemode_direct = 'X'
    TABLES
      lines           = lineas
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.

  RETURN = sy-subrc.

endmethod.
METHOD tline2string.
  DATA: l_tline  TYPE tline,
        i_lineas TYPE tlinetab,
        l_lin    TYPE i,
        i_tdline TYPE TABLE OF string,
        l_string TYPE string,
        l_first,
        l_last.

  i_lineas = lineas.
  IF empezar_en_linea > 1.
    l_lin = empezar_en_linea - 1.
    DO l_lin TIMES.
      DELETE i_lineas INDEX 1.
    ENDDO.
  ENDIF.

  LOOP AT i_lineas INTO l_tline.
    IF todo_saltos IS INITIAL.
      CLEAR: l_first, l_last.
      AT FIRST.
        l_first = 'X'.
      ENDAT.
      AT LAST.
        l_last = 'X'.
      ENDAT.
      IF l_first = 'X'.
        l_string = l_tline-tdline.
      ELSE.
        IF l_tline-tdformat = '' OR l_tline-tdformat = '=' OR l_tline-tdformat = '/'.
          CONCATENATE l_string l_tline-tdline INTO l_string SEPARATED BY space.
        ELSE.
          IF l_first = ''.
            APPEND l_string TO i_tdline.
          ENDIF.
          l_string = l_tline-tdline.
        ENDIF.
      ENDIF.
      IF l_last = 'X'.
        APPEND l_string TO i_tdline.
      ENDIF.
    ELSE.
      APPEND l_tline-tdline TO i_tdline.
    ENDIF.
  ENDLOOP.


  string = zcl_ap_string=>tabla2string( tabla = i_tdline separar_con_espacio = separar_con_espacio ).

ENDMETHOD.
