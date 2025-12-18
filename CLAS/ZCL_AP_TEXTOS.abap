CLASS zcl_ap_textos DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get_texto
      IMPORTING !id           TYPE any
                !name         TYPE any
                spras         TYPE stxh-tdspras OPTIONAL
                !object       TYPE any
                !memory       TYPE abap_bool    DEFAULT ''
                elim_regexp   TYPE any          DEFAULT ''
      EXPORTING !message      TYPE bapi_msg
      RETURNING VALUE(lineas) TYPE tlinetab.

    CLASS-METHODS save_texto
      IMPORTING !id             TYPE stxh-tdid
                !name           TYPE any
                spras           TYPE stxh-tdspras OPTIONAL
                !object         TYPE any
                lineas          TYPE tlinetab
                savemode_direct TYPE abap_bool    DEFAULT 'X'
      RETURNING VALUE(message)  TYPE bapi_msg.

    CLASS-METHODS get_stxh
      IMPORTING !id         TYPE any
                !name       TYPE any
                spras       TYPE stxh-tdspras OPTIONAL
                !object     TYPE any
      RETURNING VALUE(stxh) TYPE stxh.

    CLASS-METHODS get_texto_string
      IMPORTING !id                 TYPE any
                !name               TYPE any
                spras               TYPE stxh-tdspras OPTIONAL
                !object             TYPE any
                separar_con_espacio TYPE abap_bool    DEFAULT ''
                !memory             TYPE abap_bool    DEFAULT ''
                elim_regexp         TYPE any          DEFAULT ''
                todo_saltos         TYPE abap_bool    DEFAULT ''
      RETURNING VALUE(string)       TYPE string.

    CLASS-METHODS editar_texto
      IMPORTING !id            TYPE stxh-tdid
                !name          TYPE any
                spras          TYPE stxh-tdspras OPTIONAL
                !object        TYPE stxh-tdobject
                VALUE(display) TYPE abap_bool    DEFAULT 'X'
                forzar_nuevo   TYPE abap_bool    DEFAULT ''
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS sustituir_texto
      IMPORTING !id             TYPE stxh-tdid
                !name           TYPE stxh-tdname
                spras           TYPE stxh-tdspras OPTIONAL
                !object         TYPE stxh-tdobject
                texto_original  TYPE any
                texto_reemplazo TYPE any
                !case           TYPE abap_bool DEFAULT 'X'
      EXPORTING !message        TYPE bapi_msg
      RETURNING VALUE(return)   TYPE sy-subrc.

    CLASS-METHODS save_texto_string
      IMPORTING !id             TYPE stxh-tdid
                !name           TYPE any
                spras           TYPE stxh-tdspras OPTIONAL
                !object         TYPE stxh-tdobject
                !string         TYPE string
                longitud        TYPE i            DEFAULT 72
                word_wrap       TYPE abap_bool    DEFAULT 'X'
                delimiter       TYPE c            DEFAULT ''
                savemode_direct TYPE abap_bool    DEFAULT 'X'
                !insert         TYPE abap_bool    DEFAULT 'X'
      RETURNING VALUE(message)  TYPE bapi_msg.

    CLASS-METHODS get_texto_string_html
      IMPORTING !id           TYPE any
                !name         TYPE any
                spras         TYPE stxh-tdspras OPTIONAL
                !object       TYPE any
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS show_popup
      IMPORTING titulo  TYPE any OPTIONAL
                i_texto TYPE tlinetab.

    CLASS-METHODS get_texto_table_string
      IMPORTING !id                 TYPE any
                !name               TYPE any
                spras               TYPE stxh-tdspras OPTIONAL
                !object             TYPE any
                separar_con_espacio TYPE abap_bool    DEFAULT ''
      RETURNING VALUE(t_string)     TYPE table_of_strings.

    CLASS-METHODS tline2string
      IMPORTING lineas              TYPE tlinetab
                empezar_en_linea    TYPE i         DEFAULT 1
                separar_con_espacio TYPE abap_bool DEFAULT ''
                todo_saltos         TYPE abap_bool DEFAULT ''
      RETURNING VALUE(string)       TYPE string.

    CLASS-METHODS string2tline
      IMPORTING longitud      TYPE i         DEFAULT 72
                !string       TYPE string
                word_wrap     TYPE abap_bool DEFAULT 'X'
                delimiter     TYPE c         DEFAULT ''
      RETURNING VALUE(lineas) TYPE tlinetab.

    CLASS-METHODS borrar_texto
      IMPORTING !id           TYPE stxh-tdid
                !name         TYPE stxh-tdname
                spras         TYPE stxh-tdspras OPTIONAL
                !object       TYPE stxh-tdobject
      RETURNING VALUE(return) TYPE boolean.

    CLASS-METHODS get_catalog
      RETURNING VALUE(catalog) TYPE cms_tab_text_memory.

  PROTECTED SECTION.

  PRIVATE SECTION.
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
    IF sy-subrc <> 0.
      return = 0.
    ELSE.
      return = 1.
    ENDIF.
  ENDMETHOD.
  METHOD editar_texto.
    DATA: l_name   TYPE stxh-tdname,
          l_stxh   TYPE stxh,
          l_header TYPE thead,
          l_line   TYPE tline,
          lineas   TYPE tlinetab.

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

    IF l_stxh IS INITIAL AND forzar_nuevo <> 'X'.
      RETURN.
    ENDIF.

    IF l_stxh IS INITIAL AND forzar_nuevo = 'X'.
      l_header-tdid     = id.
      l_header-tdname   = l_name.
      l_header-tdobject = object.
      IF spras IS INITIAL.
        l_header-tdspras = sy-langu.
      ELSE.
        l_header-tdspras = spras.
      ENDIF.
      MOVE-CORRESPONDING l_header TO l_stxh.
      l_line-tdformat = '*'.
      APPEND l_line TO lineas.
      message = save_texto( id = l_header-tdid
                             object = l_header-tdobject
                             name = l_header-tdname
                             spras = l_header-tdspras
                             lineas          = lineas  ).
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
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
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
     OTHERS              = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      message = save_texto( id = l_header-tdid
                             object = l_header-tdobject
                             name = l_header-tdname
                             spras = l_header-tdspras
                             lineas          = lineas  ).
    ENDIF.
  ENDMETHOD.
  METHOD get_catalog.
    IMPORT catalog TO catalog FROM MEMORY ID 'SAPLSTXD'.
  ENDMETHOD.
  METHOD get_stxh.
    CLEAR stxh.
    IF NOT spras IS SUPPLIED OR spras IS INITIAL.
      SELECT *
        FROM stxh
        WHERE tdid     = @id
          AND tdname   = @name
          AND tdobject = @object
        ORDER BY PRIMARY KEY
        INTO @stxh
        UP TO 1 ROWS.
      ENDSELECT.
    ELSE.
      SELECT SINGLE * FROM stxh
        INTO stxh
       WHERE tdid     = id
         AND tdspras  = spras
         AND tdname   = name
         AND tdobject = object.
    ENDIF.
  ENDMETHOD.
  METHOD get_texto.
    DATA l_stxh TYPE stxh.

    CLEAR: lineas, message.

    l_stxh = get_stxh( id     = id
                       spras  = spras
                       name   = name
                       object = object ).

    IF memory = 'X' AND l_stxh IS INITIAL.
      l_stxh-tdid     = id.
      l_stxh-tdspras  = spras.
      l_stxh-tdname   = name.
      l_stxh-tdobject = object.
    ENDIF.

    IF l_stxh IS INITIAL.
      RETURN.
    ENDIF.

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

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ENDIF.

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
    DATA i_lineas TYPE tlinetab.

    i_lineas = get_texto( id     = id
                          name   = name
                          spras  = spras
                          object = object
                          memory = memory
                          elim_regexp = elim_regexp ).

    string = tline2string( lineas = i_lineas separar_con_espacio = separar_con_espacio todo_saltos = todo_saltos ).
  ENDMETHOD.
  METHOD get_texto_string_html.
    DATA: i_lineas     TYPE tlinetab,
          l_tline      TYPE tline,
          l_first      TYPE c LENGTH 1.

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
      ENDLOOP.
      CONCATENATE string '</P>' INTO string.
    ENDIF.
  ENDMETHOD.
  METHOD get_texto_table_string.
    " TODO: parameter SEPARAR_CON_ESPACIO is never used (ABAP cleaner)

    DATA: i_lineas TYPE tlinetab,
          l_tline  TYPE tline,
          l_first  TYPE c LENGTH 1,
          l_last   TYPE c LENGTH 1,
          l_string TYPE string.

    CLEAR t_string.

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
  ENDMETHOD.
  METHOD save_texto.
    DATA: l_stxh   TYPE stxh,
          l_header TYPE thead.

    CLEAR message.
    l_stxh = get_stxh( id     = id
                       spras  = spras
                       name   = name
                       object = object ).

    CLEAR l_header.
    MOVE-CORRESPONDING l_stxh TO l_header.
    l_header-tdname   = name.
    l_header-tdobject = object.
    l_header-tdid     = id.
    IF spras IS INITIAL.
      l_header-tdspras = sy-langu.
    ELSE.
      l_header-tdspras = spras.
    ENDIF.

    SELECT SINGLE tdname FROM stxh
      INTO l_header-tdname
      WHERE tdid     = l_header-tdid
        AND tdspras  = l_header-tdspras
        AND tdobject = l_header-tdobject
        AND tdname   = l_header-tdname.
    IF sy-subrc <> 0.
      DATA(l_insert) = 'X'.
    ENDIF.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = l_header
        insert          = l_insert
        savemode_direct = savemode_direct
      TABLES
        lines           = lineas
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ENDIF.
  ENDMETHOD.
  METHOD save_texto_string.
    DATA: l_stxh       TYPE stxh,
          l_header     TYPE thead,
          i_strings    TYPE TABLE OF string,
          i_lineas_aux TYPE TABLE OF tdline,
          l_tline      TYPE tline,
          lineas       TYPE TABLE OF tline,
          l_text132    TYPE tdline,
          i_lineas     TYPE TABLE OF tdline.
    DATA: l_textline TYPE c LENGTH 5000,
          l_nlineas  TYPE i,
          l_index    TYPE i,
          l_long     TYPE i.

    CLEAR message.
    l_stxh = get_stxh( id     = id
                       spras  = spras
                       name   = name
                       object = object ).

    CLEAR l_header.
    MOVE-CORRESPONDING l_stxh TO l_header.
    l_header-tdname   = name.
    l_header-tdobject = object.
    l_header-tdid     = id.
    IF spras IS INITIAL.
      l_header-tdspras = sy-langu.
    ELSE.
      l_header-tdspras = spras.
    ENDIF.

    IF word_wrap = 'X'.
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
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        IF lines( i_lineas_aux ) = 1.
          READ TABLE i_lineas_aux INTO DATA(l_lin) INDEX 1.
          CLEAR l_tline.
          l_tline-tdformat = '*'.
          l_tline-tdline   = l_lin.
          APPEND l_tline TO lineas.
        ELSE.
* Buscamos posibles finales de líneas.
          l_nlineas = lines( i_lineas_aux ).
          DO l_nlineas TIMES.
            READ TABLE i_lineas_aux INTO l_text132 INDEX sy-index.
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.

            l_index = sy-tabix.
            l_long = strlen( l_text132 ).
            IF l_long < longitud.
              l_index = l_index + 1.
              READ TABLE i_lineas_aux INTO l_text132 INDEX l_index.
              IF sy-subrc = 0.
                IF l_long < longitud.
                  l_text132+1 = l_text132.
                  CLEAR l_text132(1).
                  MODIFY i_lineas_aux FROM l_text132 INDEX l_index.
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
        l_tline-tdline   = l_text132.
        APPEND l_tline TO lineas.
      ENDLOOP.
    ENDIF.

    IF string IS INITIAL.
      zcl_ap_textos=>borrar_texto( id = l_header-tdid
                                   object = l_header-tdobject
                                   name = l_header-tdname
                                   spras = l_header-tdspras ).
    ELSE.
      message = save_texto( id = l_header-tdid
                            object = l_header-tdobject
                            name = l_header-tdname
                            spras = l_header-tdspras
                            lineas          = lineas  ).

    ENDIF.
  ENDMETHOD.
  METHOD show_popup.
    CALL FUNCTION 'COPO_POPUP_TO_DISPLAY_TEXTLIST'
      EXPORTING
*   TASK             = 'DISPLAY'
        titel            = titulo
* IMPORTING
*   FUNCTION         =
      TABLES
        text_table       = i_texto.
  ENDMETHOD.
  METHOD string2tline.
    TYPES: BEGIN OF t_linea,
             linea TYPE c LENGTH 60000,
           END OF t_linea.

    DATA: tabla      TYPE TABLE OF t_linea,
          l_linea    TYPE t_linea,
          l_textline TYPE c LENGTH 60000,
          i_lineas   TYPE TABLE OF tdline,
          l_nlineas  TYPE i,
          l_text132  TYPE tdline,
          l_index    TYPE i,
          l_long     TYPE i,
          l_tline    TYPE tline.

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
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
* Buscamos posibles finales de líneas.
        l_nlineas = lines( i_lineas ).
        DO l_nlineas TIMES.
          READ TABLE i_lineas INTO l_text132 INDEX sy-index.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          l_index = sy-tabix.
          l_long = strlen( l_text132 ).
          IF l_long < longitud.
            l_index = l_index + 1.
            READ TABLE i_lineas INTO l_text132 INDEX l_index.
            IF sy-subrc = 0.
              IF l_long < longitud.
                l_text132+1 = l_text132.
                CLEAR l_text132(1).
                MODIFY i_lineas FROM l_text132 INDEX l_index.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDDO.

        LOOP AT i_lineas INTO l_text132.
          CLEAR l_tline.
          l_tline-tdformat = '='.
          l_tline-tdline   = l_text132.
          AT FIRST.
            l_tline-tdformat = '*'.
          ENDAT.
          APPEND l_tline TO lineas.
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
  METHOD sustituir_texto.
    DATA: l_stxh   TYPE stxh,
          l_header TYPE thead,
          lineas   TYPE tlinetab,
          l_cambio TYPE c LENGTH 1.

    FIELD-SYMBOLS <linea> TYPE tline.

    CLEAR message.
    l_stxh = get_stxh( id     = id
                       spras  = spras
                       name   = name
                       object = object ).

    IF l_stxh IS INITIAL.
      RETURN.
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
    IF sy-subrc <> 0.
      return = sy-subrc.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ENDIF.

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

    IF l_cambio <> 'X'.
      RETURN.
    ENDIF.

    message = save_texto( id = l_header-tdid
                           object = l_header-tdobject
                           name = l_header-tdname
                           spras = l_header-tdspras
                           lineas          = lineas  ).

    IF NOT message IS INITIAL.
      return = 4.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD tline2string.
    DATA: i_lineas TYPE tlinetab,
          l_lin    TYPE i,
          l_tline  TYPE tline,
          l_first  TYPE c LENGTH 1,
          l_last   TYPE c LENGTH 1,
          l_string TYPE string,
          i_tdline TYPE TABLE OF string.

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
