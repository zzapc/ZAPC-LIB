*---------------------------------------------------------------------*
* Programa para editar cualquier report, sin control de SAP           *
* ¡Se cuidadoso utilizándolo!                                         *
*---------------------------------------------------------------------*
* http://www.sap4.com                                                 *
*---------------------------------------------------------------------*
REPORT zeditor MESSAGE-ID 0k NO STANDARD PAGE HEADING.

TABLES: trdir, ztemp.

DATA: content(255)     OCCURS 0 WITH HEADER LINE, msg(400), answer,
      content_ini(255) OCCURS 0 WITH HEADER LINE.

PARAMETERS: programm LIKE rs38m-programm.

PARAMETERS: clase   LIKE seoclass-clsname,
            metodo  LIKE rs38m-programm, "seocpdkey-cpdname,
            funcion LIKE tfdir-funcname.
SELECTION-SCREEN SKIP.
PARAMETERS rfc AS CHECKBOX.

AT SELECTION-SCREEN.

  IF NOT clase IS INITIAL.
    DATA: l_mtdkey TYPE seocpdkey.

    l_mtdkey-clsname = clase.
    l_mtdkey-cpdname = metodo.
    CALL FUNCTION 'SEO_METHOD_GET_SOURCE'
      EXPORTING
        mtdkey                        = l_mtdkey
*       STATE                         =
      IMPORTING
        incname                       = programm
      EXCEPTIONS
        _internal_method_not_existing = 1
        _internal_class_not_existing  = 2
        version_not_existing          = 3
        inactive_new                  = 4
        inactive_deleted              = 5
        OTHERS                        = 6.

    IF metodo IS INITIAL.
      programm+30 = 'CP'.
    ENDIF.

  ELSEIF NOT funcion IS INITIAL.
    DATA tfdir TYPE tfdir.
    SELECT SINGLE * FROM tfdir
     WHERE funcname = funcion.
    IF sy-subrc = 0.
      CONCATENATE tfdir-pname+3 'U' tfdir-include INTO programm.
    ENDIF.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR programm.
  DATA: o_popup TYPE REF TO zcl_ap_matchcode_z.

  CREATE OBJECT o_popup
    EXPORTING
      tabname = 'ZTEMP'.

  o_popup->add_field( field = 'TEXTO' selectflag = 'X' ).
  o_popup->add_field( 'VALOR1' ).
  o_popup->add_field( 'VALOR2' ).
  o_popup->add_field( 'VALOR3' ).
  o_popup->add_field( 'ERDAT' ).
  o_popup->add_field( 'ERZET' ).
  o_popup->add_field( 'ERNAM' ).

  SELECT * FROM ztemp
    INTO ztemp
    UP TO 50 ROWS
   WHERE clave = 'ZEDIT'
  ORDER BY erdat DESCENDING erzet DESCENDING.

    o_popup->add_valor( ztemp-texto ).
    o_popup->add_valor( ztemp-valor1 ).
    o_popup->add_valor( ztemp-valor2 ).
    o_popup->add_valor( ztemp-valor3 ).
    o_popup->add_valor( ztemp-erdat ).
    o_popup->add_valor( ztemp-erzet ).
    o_popup->add_valor( ztemp-ernam ).
  ENDSELECT.


  o_popup->matchcode( EXPORTING field   = 'TEXTO'
                      CHANGING  valor   = programm ).



START-OF-SELECTION.

  DATA l_fecha TYPE d.
  l_fecha = sy-datum - 100.
  DELETE FROM ztemp
   WHERE clave = 'ZEDIT'
     AND erdat < l_fecha.

  IF rfc IS INITIAL.
    READ REPORT programm INTO content.
    content_ini[] = content[].
    PERFORM editor.
    CLEAR msg.
  ELSE.
    CALL FUNCTION 'Z_RFC_GET_SOURCE'
*      DESTINATION zcl_c=>rfc_APICAZO_sc
      EXPORTING
        report    = programm
      TABLES
        i_content = content.
  ENDIF.
  INSERT REPORT programm FROM content.                      "#EC *
  DELETE FROM ztemp
   WHERE clave = 'ZEDIT'
     AND texto = programm.
  zcl_ap_temp=>set_st( clave = 'ZEDIT' subclave_auto = 'X' valor1 = funcion valor2 = clase valor3 = metodo texto = programm permanente = 'X' ).

  SELECT SINGLE * FROM trdir WHERE name = programm.
  IF sy-subrc NE 0.
    CLEAR trdir.
    trdir-name = programm.
    trdir-clas = 'TEMP'.
    trdir-dbna = ' '.
    trdir-fixpt = 'X'.
    trdir-rstat = 'P'.
    trdir-subc = '1'.
    trdir-rmand = sy-mandt.
    IF trdir-sqlx LT 'R'.
      trdir-sqlx = 'R'.
    ENDIF.
    MODIFY trdir.
  ENDIF.
  CALL FUNCTION 'DB_COMMIT'.
  msg = 'Línea:'.
  IF trdir-subc NE 'I'.
    GENERATE REPORT programm LINE msg+10(10) MESSAGE msg+50.
    IF sy-subrc <> 0.
      MESSAGE i000 WITH msg(50) msg+50(50) msg+100(50) msg+150(50).
      DATA l_hora LIKE sy-uzeit.
      l_hora = sy-uzeit - 60.
      DELETE FROM snap
       WHERE datum = sy-datum
         AND uzeit >= l_hora
         AND uname = sy-uname
         AND mandt = sy-mandt.
    ELSE.
      CALL FUNCTION 'POPUP_FOR_INTERACTION'
        EXPORTING
          headline       = 'Programa generado con éxito'
          text1          = 'Se ha generado el programa'
          text2          = programm
          text3          = '¿Desea ejecutarlo ahora?'
          ticon          = 'S'
          button_1       = 'Finalizar'
          button_2       = 'Ejecutar'
        IMPORTING
          button_pressed = answer
        EXCEPTIONS
          OTHERS         = 1.
      IF answer = '2'.
        SUBMIT (programm)
          AND RETURN
          VIA SELECTION-SCREEN.
      ENDIF.
    ENDIF.
  ENDIF.

*---------------------------------------------------------------------*
*       FORM editor                                                   *
*---------------------------------------------------------------------*
FORM editor.

  DATA: u-index        LIKE sy-index, u-txtindex(50), u-title(50).
  CONCATENATE 'Editando programa' programm
         INTO u-title SEPARATED BY ' '.
  EDITOR-CALL FOR content TITLE u-title.
  IF sy-subrc = 4. STOP. ENDIF.
  CALL FUNCTION 'EDITOR_SYNTAX_CHECK'
    EXPORTING
      i_program       = programm
    IMPORTING
      o_error_line    = u-index
      o_error_message = msg
    TABLES
      i_source        = content.
  IF NOT msg IS INITIAL.
    u-txtindex = u-index - 2.
    SHIFT u-txtindex LEFT DELETING LEADING space.
    CONCATENATE 'Error de sintaxis en línea:' u-txtindex INTO u-txtindex
     SEPARATED BY space.
    CALL FUNCTION 'POPUP_FOR_INTERACTION'
      EXPORTING
        headline       = u-txtindex
        text1          = msg(60)
        text2          = msg+60(60)
        text3          = msg+120(60)
        text4          = msg+180(60)
        text5          = msg+240(60)
        text6          = msg+300(60)
        ticon          = 'E'
        button_1       = 'Finalizar.'
        button_2       = 'Volver al editor'
      IMPORTING
        button_pressed = answer
      EXCEPTIONS
        OTHERS         = 1.
    ROLLBACK WORK.
    IF answer = '1'. LEAVE PROGRAM. ENDIF.
    IF answer = '2'. PERFORM editor. ENDIF.
  ELSE.
    IF lines( content ) > 3 AND lines( content_ini ) > 3.
      IF content[ 1 ] NE content_ini[ 1 ] OR
         content[ 2 ] NE content_ini[ 2 ] OR
         content[ 3 ] NE content_ini[ 3 ].
        CALL FUNCTION 'POPUP_FOR_INTERACTION'
          EXPORTING
            headline       = 'Verifique el programa es el mismo!'
            text1          = content[ 1 ]
            text2          = content[ 2 ]
            text3          = content[ 3 ]
            text4          = content_ini[ 1 ]
            text5          = content_ini[ 2 ]
            text6          = content_ini[ 3 ]
            ticon          = 'E'
            button_1       = 'Finalizar.'
            button_2       = 'Volver al editor'
          IMPORTING
            button_pressed = answer
          EXCEPTIONS
            OTHERS         = 1.
        IF answer = '2'. PERFORM editor. ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                               " EDITOR
