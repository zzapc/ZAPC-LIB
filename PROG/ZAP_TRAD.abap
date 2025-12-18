***********************************************************************
* TIPO : LISTADO
* TITULO : Traducciones de programas
*
* AUTOR: Andrés Picazo                                FECHA: 11/11/2025
*
***********************************************************************
REPORT zrbc0004.

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: trdir, dd04l.

*------TABLAS INTERNAS-------------------------------------------------*
*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM zcl_ap_alv_check FINAL.
  PUBLIC SECTION.
    METHODS handle_user_command REDEFINITION.
    METHODS visualizar_objeto   REDEFINITION.
ENDCLASS.

TYPES: BEGIN OF t_CARGA,
         name          TYPE trdir-name,
         tipo          TYPE srmelemtyp,
         lango         TYPE char1,
         id            TYPE string,             " textpoolid,
         key           TYPE string,             " textpoolky,
         campo_texto   TYPE fieldname,
         entry         TYPE textpooltx,
         length        TYPE textpoolln,
         langd         TYPE sy-langu,
         sobreescribir TYPE abadrflagoverwrite,
         entry_d       TYPE textpooltx,
       END OF t_CARGA.

DATA i_carga TYPE TABLE OF t_carga.

DATA: ls_e071 TYPE e071,
      lt_e071 TYPE tr_objects.


*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_listado,
             check         TYPE xfeld,
             lights        TYPE zico_estado_mensaje,
             name          TYPE trdir-name,
             tipo          TYPE srmelemtyp,
             lango         TYPE sy-langu,
             id            TYPE string,              " textpoolid,
             key           TYPE string,              " textpoolky,
             campo_texto   TYPE fieldname,
             entry         TYPE textpooltx,
             length        TYPE textpoolln,
             langd         TYPE sy-langu,
             sobreescribir TYPE abadrflagoverwrite,
             entry_d       TYPE textpooltx,
             message       TYPE bapi_msg,
           END OF t_listado,
           tt_listado TYPE STANDARD TABLE OF t_listado,
           BEGIN OF t_conf_tabla,
             name         TYPE trdir-name,
             campo_idioma TYPE dd03l-fieldname,
             long_clave   TYPE int4,
             pos_idioma   TYPE int4,
           END OF t_conf_tabla.

    DATA: i_listado    TYPE tt_listado,
          i_conf_tabla TYPE TABLE OF t_conf_tabla,
          o_alv        TYPE REF TO lcl_alv ##NEEDED.

    METHODS  main.

    METHODS: listado,
             seleccionar_datos,

      grabar_textos
        IMPORTING !name TYPE progname
                  !lang TYPE sy-langu,

      grabar_cuad
        IMPORTING !name TYPE progname
                  !lang TYPE sy-langu,

      grabar_dtel
        IMPORTING !name TYPE progname
                  !lang TYPE sy-langu,

      grabar_propuesta
        IMPORTING lango TYPE char1
                  texto TYPE textpooltx
                  langd TYPE syst_langu
                  textd TYPE textpooltx.

  PRIVATE SECTION.
    METHODS get_tabla
      IMPORTING tabla       TYPE string
                campo_clave TYPE string
                campo_texto TYPE string
                !name       TYPE progname.

    METHODS grabar_registro
      IMPORTING tabla       TYPE string
                clave       TYPE string
                idioma      TYPE any
                campo_texto TYPE fieldname
                texto       TYPE textpooltx
      RETURNING VALUE(ok)   TYPE abap_bool.

ENDCLASS.

*------VARIABLES-------------------------------------------------------*
DATA o_prog TYPE REF TO zcl_report.


*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-sel.
  PARAMETERS p_prog RADIOBUTTON GROUP g DEFAULT 'X' USER-COMMAND g.
  SELECT-OPTIONS s_name FOR trdir-name.
  PARAMETERS p_dtel RADIOBUTTON GROUP g.
  SELECT-OPTIONS s_dtel FOR dd04l-rollname.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS: p_lango TYPE sy-langu DEFAULT sy-langu OBLIGATORY,
              p_langd TYPE sy-langu OBLIGATORY.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS p_fiche TYPE text255.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b01.
__botones_plantilla.


************************************************************************
*
* LOGICA DEL PROGRAMA
*
************************************************************************

*----------------------------------------------------------------------*
* CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.
  METHOD visualizar_objeto.
    " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
    DATA l_list TYPE o_prog->t_listado.

    l_list = list.
    CASE column.
*      WHEN 'BUKRS'.
*        MESSAGE l_list-bukrs TYPE 'I'.
      WHEN OTHERS. message = '?'.
    ENDCASE.
  ENDMETHOD. " handle_double_click

  METHOD handle_user_command.
    DATA i_sel TYPE TABLE OF t_carga.

    check_ucomm_sel = 'PROP'.

    super->handle_user_command( e_salv_function ).

    CASE ucomm.
      WHEN 'EXCELC'.
        MOVE-CORRESPONDING o_prog->i_listado TO i_carga.
        DATA(o_alvl) = NEW zcl_ap_alv( tabla = 'I_CARGA' handle = 'CARG' ).

        IF p_fiche IS INITIAL.
          zcl_ap_abap2xls=>alv_2_xls( alv = o_alvl->o_alv refresh_metadata = 'X' tabla = i_carga abrir = 'X' nombre_fichero = 'Traducciones.xlsx' ).
        ELSE.
          zcl_ap_abap2xls=>alv_2_xls( alv = o_alvl->o_alv refresh_metadata = 'X' tabla = i_carga abrir = 'X' ruta = p_fiche ).
        ENDIF.
      WHEN 'CARGAR'.
        NEW zcl_ap_abap2xls( )->lee_fichero( EXPORTING popup_select_file = 'X'
                                                       get_datos         = 'X'
                                                       huge              = 'X'
                                                       mostrar_error     = 'X'
                                                       fichero           = p_fiche
                                             IMPORTING datos             = i_carga ).

        LOOP AT i_carga ASSIGNING FIELD-SYMBOL(<carga>) WHERE entry_d <> ''.
          IF <carga>-sobreescribir = abap_true.
            ASSIGN o_prog->i_listado[ name  = <carga>-name
                                      langd = <carga>-langd
                                      id    = <carga>-id
                                      key   = <carga>-key
                                      campo_Texto = <carga>-campo_Texto ] TO FIELD-SYMBOL(<listado>).
            IF sy-subrc = 0.
              IF <listado>-entry_d <> <carga>-entry_d.
                <listado>-entry_d = <carga>-entry_d.
                <listado>-lights  = icon_change.

                o_prog->grabar_propuesta( lango = <carga>-lango
                                          texto = <listado>-entry
                                          langd = <carga>-langd
                                          textd = <listado>-entry_d  ).
              ENDIF.
            ENDIF.
          ELSE.
            ASSIGN o_prog->i_listado[ name  = <carga>-name
                                      langd = <carga>-langd
                                      id    = <carga>-id
                                      key   = <carga>-key
                                      campo_Texto = <carga>-campo_Texto
                                      entry_d = '' ] TO <listado>.
            IF sy-subrc = 0.
              <listado>-entry_d = <carga>-entry_d.
              <listado>-lights  = icon_change.

              o_prog->grabar_propuesta( lango = <carga>-lango
                                        texto = <listado>-entry
                                        langd = <carga>-langd
                                        textd = <listado>-entry_d ).
            ENDIF.
          ENDIF.
        ENDLOOP.

        refresh( ).

      WHEN 'GRABAR'.
        CLEAR lt_e071.
        LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE lights = icon_change.
          IF NOT line_exists( i_sel[ name = <listado>-name langd = <listado>-langd tipo = 'Textos' ] ).
            APPEND VALUE #( name = <listado>-name langd = <listado>-langd tipo = 'Textos' ) TO i_sel.
            o_prog->grabar_textos(  name = <listado>-name
                                    lang = <listado>-langd ).
          ENDIF.

          IF NOT line_exists( i_sel[ name = <listado>-name langd = <listado>-langd tipo = 'Tabla' id = 'RSMPTEXTS' ] ).
            APPEND VALUE #( name = <listado>-name langd = <listado>-langd tipo = 'Tabla' id = 'RSMPTEXTS'  ) TO i_sel.
            o_prog->grabar_cuad(  name = <listado>-name
                                  lang = <listado>-langd ).
          ENDIF.

          IF NOT line_exists( i_sel[ name = <listado>-name langd = <listado>-langd tipo = 'Tabla' id = 'DD04T' ] ).
            APPEND VALUE #( name = <listado>-name langd = <listado>-langd tipo = 'Tabla' id = 'DD04T'  ) TO i_sel.

            o_prog->grabar_dtel(  name = <listado>-name
                                  lang = <listado>-langd ).
          ENDIF.
        ENDLOOP.

        IF NOT lt_e071 IS INITIAL.
          SORT lt_e071.
          DELETE ADJACENT DUPLICATES FROM lt_e071.
          CALL FUNCTION 'TR_REQUEST_CHOICE'
            EXPORTING
*         IV_REQUEST_TYPES     =
*         IV_CLI_DEP           = ' '
*         IV_REQUEST           = ' '
              it_e071  = lt_e071
*         IV_LOCK_OBJECTS      = ' '
*         IV_NO_OWNER_CHECK    = ' '
            EXCEPTIONS
              OTHERS   = 1.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.

        refresh( ).

      WHEN 'PROP'.
        LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X'.
          DATA(l_idiomas) = |{ <listado>-lango }-{ <listado>-langd }|.
          SELECT SINGLE string FROM ztemps
            INTO @DATA(texto)
            WHERE clave = 'TRAD'
              AND valor = @l_idiomas
              AND texto = @<listado>-entry.
          IF sy-subrc = 0 AND NOT texto IS INITIAL.
            IF <listado>-entry_d <> texto.
              <listado>-entry_d = texto.
              <listado>-lights  = icon_change.
            ENDIF.
          ENDIF.
        ENDLOOP.
        refresh( ).

      WHEN 'DESC_PROP'.
        SELECT * FROM ztemps
          INTO TABLE @DATA(i_trad)
          WHERE clave = 'TRAD'.
        zcl_ap_segw=>get_json( EXPORTING datos = i_trad IMPORTING json = DATA(l_json) ).
        IF NOT l_json IS INITIAL.
          zcl_ap_ficheros=>grabar_xstring( fichero = |propuesta traducciones.json| dialogo = 'X' string = l_json ).
        ENDIF.

      WHEN 'SUBIR_PROP'.
        zcl_ap_ficheros=>leer_xstring( EXPORTING popup_select_fichero = 'X' get_string = 'X'
                                       IMPORTING string = l_json ).
        zcl_ap_segw=>set_json( EXPORTING json = l_json
                                IMPORTING datos = i_trad ).

        MODIFY ztemps FROM TABLE i_trad.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS zcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report IMPLEMENTATION.
  METHOD main.
    seleccionar_datos( ).
    listado( ).
  ENDMETHOD.                    " REPORT

  METHOD seleccionar_datos.
    DATA: text_o TYPE TABLE OF textpool,
          text_d TYPE TABLE OF textpool.

    IF p_prog = 'X'.
      sgpi_texto( 'Seleccionando datos'(sda) ).
      SELECT name FROM trdir
        INTO TABLE @DATA(i_prog)
       WHERE name IN @s_name.

      o_prog->o_sgpi->get_filas_tabla( i_prog[] ).
      LOOP AT i_prog ASSIGNING FIELD-SYMBOL(<prog>).
        sgpi_texto( texto1 = 'Procesando datos'(pda) cant_porc = 100 ).

        READ TEXTPOOL <prog>-name INTO text_o LANGUAGE p_lango.
        READ TEXTPOOL <prog>-name INTO text_d LANGUAGE p_langd.
        LOOP AT text_o ASSIGNING FIELD-SYMBOL(<to>) WHERE entry <> 'D       .'.
          APPEND INITIAL LINE TO i_listado ASSIGNING FIELD-SYMBOL(<listado>).
          <listado>-name = <prog>-name.
          <listado>-tipo = 'Textos'.
          MOVE-CORRESPONDING <to> TO <listado>.
          <listado>-lango = p_lango.
          <listado>-langd = p_langd.
          ASSIGN text_d[ key = <to>-key
                         id = <to>-id ] TO FIELD-SYMBOL(<td>).
          IF sy-subrc = 0.
            <listado>-entry_d = <td>-entry.
            <listado>-lights  = icon_yellow_light.
          ENDIF.

          IF <listado>-id = 'S'.
            <listado>-entry   = <listado>-entry+8.
            <listado>-entry_d = <listado>-entry_d+8.
          ENDIF.
        ENDLOOP.

        get_tabla( tabla = 'RSMPTEXTS'
                   campo_clave = 'PROGNAME'
                   campo_texto = 'TEXT'
                   name = <prog>-name ).

      ENDLOOP.
    ELSEIF p_dtel = 'X'.
      SELECT rollname FROM dd04l
        INTO TABLE @DATA(i_dtel)
       WHERE rollname IN @s_dtel.

      o_prog->o_sgpi->get_filas_tabla( i_dtel[] ).
      LOOP AT i_dtel ASSIGNING FIELD-SYMBOL(<dtel>).
        sgpi_texto( texto1 = 'Procesando datos'(pda) cant_porc = 100 ).

        get_tabla( tabla = 'DD04T'
                   campo_clave = 'ROLLNAME'
                   campo_texto = 'DDTEXT'
                   name = CONV #( <dtel>-rollname ) ).

        get_tabla( tabla = 'DD04T'
                   campo_clave = 'ROLLNAME'
                   campo_texto = 'REPTEXT'
                   name = CONV #( <dtel>-rollname ) ).

        get_tabla( tabla = 'DD04T'
                   campo_clave = 'ROLLNAME'
                   campo_texto = 'SCRTEXT_M'
                   name = CONV #( <dtel>-rollname ) ).

        get_tabla( tabla = 'DD04T'
                   campo_clave = 'ROLLNAME'
                   campo_texto = 'SCRTEXT_L'
                   name = CONV #( <dtel>-rollname ) ).

        get_tabla( tabla = 'DD04T'
                   campo_clave = 'ROLLNAME'
                   campo_texto = 'SCRTEXT_S'
                   name = CONV #( <dtel>-rollname ) ).

      ENDLOOP.
    ENDIF.

    SORT i_listado BY name lango id key.
  ENDMETHOD.

  METHOD listado.
    sgpi_texto( 'Generando informe'(gin) ).

    o_alv->add_button( button = 'F01' text = 'Excel'  icon = icon_xls ucomm = 'EXCELC' ).
    o_alv->add_button( button = 'F02' text = 'Carga Traducción'  icon = icon_import ucomm = 'CARGAR' ).
    o_alv->add_button( button = 'F03' text = 'Grabar traducción'  icon = icon_system_save ucomm = 'GRABAR' ).
    o_alv->add_button( button = 'F04' text = 'Propuesta'  icon = icon_execute_object ucomm = 'PROP' ).
    o_alv->add_button( button = 'M01' text = 'Descarga propuestas'  ucomm = 'DESC_PROP' ).
    o_alv->add_button( button = 'M02' text = 'Importar propuestas'  ucomm = 'SUBIR_PROP' ).

    o_alv->set_layout( p_vari ).

    o_alv->set_top_of_page( ).

    o_alv->set_field( campo = 'LIGHTS' op = 'KEY' ).
    o_alv->set_field_quitar( 'CHECK' ).

    o_alv->set_orden( 'NAME,TIPO,LANGO,ID,KEY' ).
    o_alv->get_datos_layout( EXPORTING reordenar_tabla = 'X' tabla_ref = 'X' CHANGING t_tabla = i_listado ).
    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).

    o_alv->show( ).
  ENDMETHOD.

  METHOD grabar_textos.
    DATA text TYPE TABLE OF textpool.

    READ TEXTPOOL name INTO text LANGUAGE lang.

    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>)
         WHERE name = name AND tipo = 'Textos' AND langd = lang AND lights = icon_change AND entry_d IS NOT INITIAL.
      ASSIGN text[ id = <listado>-id key = <listado>-key ] TO FIELD-SYMBOL(<t>).
      IF sy-subrc = 0.
        <t>-entry = <listado>-entry_d.
      ELSE.
        APPEND INITIAL LINE TO text ASSIGNING <t>.
        MOVE-CORRESPONDING <listado> TO <t>.
        <t>-entry = <listado>-entry_d.
      ENDIF.
      <listado>-lights = icon_green_light.
    ENDLOOP.
    IF sy-subrc = 0.

      LOOP AT text ASSIGNING <t>.
        IF <listado>-id = 'S'.
          <t>-entry+8 = <t>-entry.
          CLEAR <t>-entry(8).
        ENDIF.

        IF strlen( <t>-entry ) > <t>-length.
          <t>-length = strlen( <t>-entry ).
        ENDIF.

      ENDLOOP.

      INSERT TEXTPOOL name FROM text LANGUAGE lang.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.

        ls_e071-as4pos   = 1.
        ls_e071-pgmid    = 'LIMU'.
        ls_e071-object   = 'REPT'.
        ls_e071-obj_name = name.
        ls_e071-lang     = sy-langu.
        INSERT ls_e071 INTO TABLE lt_e071.
      ENDIF.
    ENDIF.

* Grabamos la traducción estándar para que SAP refresque
    DATA(o_bi) = NEW zcl_ap_batch_input( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPLWBABAP' dynpro = '0100' okcode = '=TRAN' ).
    o_bi->campos( campo = 'RS38M-PROGRAMM' valor = name ). " Nombre de programa ABAP
    o_bi->campos( campo = 'RS38M-FUNC_EDIT' valor = '' ). " Editor
    o_bi->campos( campo = 'RS38M-FUNC_TEXT' valor = 'X' ). " Elementos de texto

* Idioma objetivo para la traducción
    o_bi->dynpro( program = 'SAPLSETX' dynpro = '0200' okcode = '=NEXT' ).
    WRITE p_lango TO aux1.
    WRITE p_langd TO aux2.
    o_bi->campos( campo = 'RSETX-MASTERLANG' valor = aux1 ). " Clave de idioma
    o_bi->campos( campo = 'RSETX-TARG_LANGU' valor = aux2 ). " Clave de idioma

    o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120' okcode = '=SAVE' ).

    o_bi->llamar_transaccion( tcode = 'SE38' modo = 'N' ).
  ENDMETHOD.

  METHOD grabar_cuad.
    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>)
         WHERE name = name AND tipo = 'Tabla' AND id = 'RSMPTEXTS' AND langd = lang AND lights = icon_change AND entry_d IS NOT INITIAL.

      IF grabar_registro( tabla = 'RSMPTEXTS'
                       clave = <listado>-key
                       idioma = p_langd
                       campo_texto = 'TEXT'
                       texto = <listado>-entry_d ) = abap_true.
        <listado>-lights = icon_green_light.
        DATA(ok) = abap_true.
      ELSE.
        <listado>-lights = icon_red_light.
      ENDIF.
    ENDLOOP.
    IF ok = abap_true.
      ls_e071-as4pos   = 1.
      ls_e071-pgmid    = 'LIMU'.
      ls_e071-object   = 'CUAD'.
      ls_e071-obj_name = name.
      ls_e071-lang     = sy-langu.
      INSERT ls_e071 INTO TABLE lt_e071.

* Grabamos la traducción estándar para que SAP refresque
      DATA(o_bi) = NEW zcl_ap_batch_input( ).

      o_bi->inicio( ).

      o_bi->dynpro( program = 'SAPMSMPE' dynpro = '0100' okcode = '=TRAN' ).
      o_bi->campos( campo = 'RSMPE-PROGRAM' valor = name ). " Nombre de programa ABAP

* Idioma objetivo para la traducción
      o_bi->dynpro( program = 'SAPLSETX' dynpro = '0200' okcode = '=NEXT' ).
      WRITE p_lango TO aux1.
      WRITE p_langd TO aux2.
      o_bi->campos( campo = 'RSETX-MASTERLANG' valor = aux1 ). " Clave de idioma
      o_bi->campos( campo = 'RSETX-TARG_LANGU' valor = aux2 ). " Clave de idioma

      o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120' okcode = '=SEQU' ).

      o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120' okcode = '=SAVE' ).

      o_bi->llamar_transaccion( tcode = 'SE41' modo = 'N' ).
    ENDIF.
  ENDMETHOD.

  METHOD grabar_dtel.
    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>)
         WHERE name = name AND tipo = 'Tabla' AND id = 'DD04T' AND langd = lang AND lights = icon_change AND entry_d IS NOT INITIAL.

      IF grabar_registro( tabla = 'DD04T'
                       clave = <listado>-key
                       idioma = p_langd
                       campo_texto = <listado>-campo_texto
                       texto = <listado>-entry_d ) = abap_true.
        <listado>-lights = icon_green_light.
        DATA(ok) = abap_true.
      ELSE.
        <listado>-lights = icon_red_light.
      ENDIF.
    ENDLOOP.
    IF ok = abap_true.
      ls_e071-as4pos   = 1.
      ls_e071-pgmid    = 'LIMU'.
      ls_e071-object   = 'DTED'.
      ls_e071-obj_name = name.
      ls_e071-lang     = sy-langu.
      INSERT ls_e071 INTO TABLE lt_e071.

* Grabamos la traducción estándar para que SAP refresque
      DATA(o_bi) = NEW zcl_ap_batch_input( ).

      o_bi->inicio( ).

      o_bi->dynpro( program = 'SAPLSD_ENTRY' dynpro = '1000' okcode = '=CHANGE_RADIO' ).
      o_bi->campos( campo = 'RSRD1-TBMA' valor = '' ).
      o_bi->campos( campo = 'RSRD1-DDTYPE' valor = 'X' ).

      o_bi->dynpro( program = 'SAPLSD_ENTRY' dynpro = '1000' okcode = '=WB_TRANSLATION' ).
      o_bi->campos( campo = 'RSRD1-DDTYPE_VAL' valor = name ).

* Idioma objetivo para la traducción
      o_bi->dynpro( program = 'SAPLSETX' dynpro = '0200' okcode = '=NEXT' ).
      WRITE p_lango TO aux1.
      WRITE p_langd TO aux2.
      o_bi->campos( campo = 'RSETX-MASTERLANG' valor = aux1 ). " Clave de idioma
      o_bi->campos( campo = 'RSETX-TARG_LANGU' valor = aux2 ). " Clave de idioma
      o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120' okcode = '=SAVE' ).

      o_bi->llamar_transaccion( tcode = 'SE11' modo = 'N' ).
    ENDIF.
  ENDMETHOD.

  METHOD get_tabla.
    DATA:
      lr_struct_descr TYPE REF TO cl_abap_structdescr,
      lr_table_descr  TYPE REF TO cl_abap_tabledescr,
      lr_data         TYPE REF TO data,
      where           TYPE string.

    FIELD-SYMBOLS <tabla> TYPE ANY TABLE.

    SELECT fieldname, keyflag, leng, position, languflag FROM dd03l
      INTO TABLE @DATA(i_campos_tabla)
     WHERE tabname = @tabla
     ORDER BY position.
    IF sy-subrc <> 0.
      MESSAGE |No existe tabla { tabla }| TYPE 'E'.
    ENDIF.

    APPEND INITIAL LINE TO i_conf_tabla ASSIGNING FIELD-SYMBOL(<conf>).
    <conf>-name = tabla.
    LOOP AT i_campos_tabla ASSIGNING FIELD-SYMBOL(<campo>) WHERE keyflag = 'X'.
      IF <campo>-languflag = 'X'.
        <conf>-campo_idioma = <campo>-fieldname.
        <conf>-pos_idioma   = <conf>-long_clave.
      ENDIF.
      add <campo>-leng to <conf>-long_clave.
    ENDLOOP.

    IF <conf>-campo_idioma IS INITIAL.
      MESSAGE |La tabla { tabla } no tiene campo de idioma en la clave| TYPE 'E'.
    ENDIF.

    TRY.
        lr_struct_descr ?= cl_abap_typedescr=>describe_by_name( tabla ).
        lr_table_descr = cl_abap_tabledescr=>create( p_line_type = lr_struct_descr ).
        CREATE DATA lr_data TYPE HANDLE lr_table_descr.
        ASSIGN lr_data->* TO <tabla>.
      CATCH cx_root INTO DATA(lx_type).
        MESSAGE lx_type->get_text( ) TYPE 'E'.
    ENDTRY.

    where = |{ <conf>-campo_idioma } = '{ p_lango }'|.
    IF NOT campo_clave IS INITIAL.
      where = |{ where } AND { campo_clave } = '{ name }'|.
    ENDIF.
    SELECT * FROM (tabla) INTO TABLE @<tabla> WHERE (where).
    LOOP AT <tabla> ASSIGNING FIELD-SYMBOL(<wa>).
      APPEND INITIAL LINE TO i_listado ASSIGNING FIELD-SYMBOL(<listado>).
      <listado>-name        = name.
      <listado>-lango       = p_lango.
      <listado>-langD       = p_langD.
      <listado>-tipo        = 'Tabla'.
      <listado>-campo_texto = campo_texto.
      <listado>-id          = tabla.
      <listado>-key         = <wa>(<conf>-long_clave).
      ASSIGN COMPONENT campo_Texto OF STRUCTURE <wa> TO FIELD-SYMBOL(<texto>).
      IF sy-subrc = 0.
        <listado>-entry = <texto>.
      ENDIF.

      CLEAR where.
      LOOP AT i_campos_tabla ASSIGNING <campo> WHERE keyflag = 'X'.
        ASSIGN COMPONENT <campo>-fieldname OF STRUCTURE <wa> TO FIELD-SYMBOL(<clave>).
        IF <campo>-fieldname = <conf>-campo_idioma.
          ASSIGN p_langd TO <clave>.
        ENDIF.
        IF where IS INITIAL.
          where = |{ <campo>-fieldname } = '{ <clave> }'|.
        ELSE.
          where = |{ where } and { <campo>-fieldname } = '{ <clave> }'|.
        ENDIF.
      ENDLOOP.

      SELECT SINGLE (campo_texto) FROM (tabla) INTO @<listado>-entry_d WHERE (where).
      IF NOT <listado>-entry_D IS INITIAL.
        <listado>-lights = icon_yellow_light.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD grabar_registro.
    DATA: lr_row     TYPE REF TO data,
          where      TYPE string,
          where_Dest TYPE string.

    FIELD-SYMBOLS <row> TYPE any.

    CLEAR ok.

    ASSIGN i_conf_tabla[ name = tabla ] TO FIELD-SYMBOL(<conf>).

    TRY.
        cl_abap_typedescr=>describe_by_name( tabla ).

        CREATE DATA lr_row TYPE (tabla).
        ASSIGN lr_row->* TO <row>.

        <row> = clave.

        SELECT fieldname, keyflag, leng, position, languflag FROM dd03l
          INTO TABLE @DATA(i_campos_tabla)
         WHERE tabname = @tabla
         ORDER BY position.
        IF sy-subrc <> 0.
          MESSAGE |No existe tabla { tabla }| TYPE 'E'.
        ENDIF.

        CLEAR: where, where_dest.
        LOOP AT i_campos_tabla ASSIGNING FIELD-SYMBOL(<campo>) WHERE keyflag = 'X'.
          ASSIGN COMPONENT <campo>-fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<clave>).
          IF where IS INITIAL.
            where = |{ <campo>-fieldname } = '{ <clave> }'|.
          ELSE.
            where = |{ where } and { <campo>-fieldname } = '{ <clave> }'|.
          ENDIF.

          IF <campo>-fieldname = <conf>-campo_idioma.
            ASSIGN idioma TO <clave>.
          ENDIF.
          IF where_Dest IS INITIAL.
            where_Dest = |{ <campo>-fieldname } = '{ <clave> }'|.
          ELSE.
            where_Dest = |{ where_Dest } and { <campo>-fieldname } = '{ <clave> }'|.
          ENDIF.
        ENDLOOP.

* Si existe en idioma destino, modificamos
        SELECT SINGLE * FROM (tabla) INTO <row> WHERE (where_dest).
        IF sy-subrc <> 0.
* Si no, copiamos del origen
          SELECT SINGLE * FROM (tabla) INTO <row> WHERE (where).
        ENDIF.
        IF sy-subrc = 0.
          ASSIGN COMPONENT <conf>-campo_idioma OF STRUCTURE <row> TO FIELD-SYMBOL(<idioma>).
          <idioma> = idioma.
          ASSIGN COMPONENT campo_texto OF STRUCTURE <row> TO FIELD-SYMBOL(<texto>).
          IF <texto> <> texto.
            <texto> = texto.

            MODIFY (tabla) FROM <row>.
            IF sy-subrc = 0.
              ok = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD grabar_propuesta.
    DATA ztemps TYPE ztemps.

    ztemps-clave      = 'TRAD'.
    ztemps-subclave   = |{ lango }-{ langd }-{ texto }|.
    ztemps-valor      = |{ lango }-{ langd }|.
    ztemps-texto      = textO.
    ztemps-string     = textD.
    ztemps-permanente = 'X'.
    ztemps-erdat      = sy-datum.
    ztemps-erzet      = sy-uzeit.
    ztemps-ernam      = sy-uname.
    MODIFY ztemps FROM ztemps.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  o_prog = NEW #( status       = 'INICIO_DYN'
                  status_prog  = 'ZAP_STATUS'
                  no_param     = 'X'
                  guardar_logz = '' ).

  PERFORM add_button IN PROGRAM zap_status USING 'M01' 'Log'(log) '' ''.

  o_prog->o_alv = NEW #( status             = 'STANDARD_ALV_DYN'
                         status_prog        = 'ZAP_STATUS'
                         top_of_page_auto   = 'X'
                         top_of_page_titulo = 'X'
                         o_dev              = o_prog ).

  p_vari = o_prog->o_alv->get_default_layout( ).

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).

AT SELECTION-SCREEN OUTPUT.
  o_prog->handle = ''.
  IF o_prog->o_alv IS INITIAL OR o_prog->handle <> o_prog->aux1.
    o_prog->aux1 = o_prog->handle.
    IF NOT o_prog->o_alv IS INITIAL.
      o_prog->o_alv->free( ).
      CLEAR o_prog->o_alv.
    ENDIF.
    o_prog->o_alv = NEW #( status             = 'STANDARD_ALV_DYN'
                           status_prog        = 'ZAP_STATUS'
                           top_of_page_auto   = 'X'
                           top_of_page_titulo = 'X'
                           handle             = o_prog->handle
                           o_dev              = o_prog ).
    p_vari = o_prog->o_alv->get_default_layout( ).
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  p_vari = o_prog->o_alv->get_f4_layout( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fiche.
  p_fiche = zcl_ap_ficheros=>popup_select_fichero( file_filter = zcl_ap_ficheros=>c_filtro_xlsx ).

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'ONLI'.
      IF p_prog = 'X' AND s_name[] IS INITIAL.
        MESSAGE 'Informe selección de programas' TYPE 'E'.
      ELSEIF p_dtel = 'X' AND s_dtel[] IS INITIAL.
        MESSAGE 'Informe selección de elementos de datos' TYPE 'E'.
      ENDIF.

      o_prog->validar_seleccion_obligatoria( campos_or = '*' msgty = 'W' ).
    WHEN OTHERS.
      o_prog->at_selection( ).
  ENDCASE.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  o_prog->at_selection( ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  o_prog->main( ).
