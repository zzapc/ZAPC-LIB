" ----------------------------------------------------------------------
" TIPO : LISTADO
" TITULO : Traducciones de programas
"
" AUTOR: Andrés Picazo                                FECHA: 11/11/2025
"
" ----------------------------------------------------------------------
REPORT zrbc0004.

" ------TABLAS/ESTRUCTURAS-----------------------------------------------
TABLES: trdir, dd04l, tadir.

" ------TABLAS INTERNAS--------------------------------------------------

CONSTANTS: c_textos TYPE string VALUE 'Textos' ##NO_TEXT,
           c_tabla  TYPE string VALUE 'Tabla' ##NO_TEXT.

CLASS lcl_event_grid DEFINITION INHERITING FROM zcl_ap_alv_grid_eventos FINAL.
  PUBLIC SECTION.
    METHODS data_changed REDEFINITION.
ENDCLASS.

TYPES: BEGIN OF t_carga,
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
         entry_d       TYPE textpool-entry,
       END OF t_carga.

DATA i_carga TYPE TABLE OF t_carga.

DATA ls_e071 TYPE e071.
DATA lt_e071 TYPE tr_objects.


" -----------------------------------------------------------------------
" CLASS zcl_report DEFINITION
" -----------------------------------------------------------------------
"
" -----------------------------------------------------------------------
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_listado,
             check         TYPE xfeld,
             lights        TYPE zico_estado_mensaje,
             name          TYPE trdir-name,
             tipo          TYPE srmelemtyp,
             lango         TYPE sy-langu,
             id            TYPE string, " textpoolid,
             key           TYPE string, " textpoolky,
             campo_texto   TYPE fieldname,
             entry         TYPE textpooltx,
             length        TYPE textpoolln,
             langd         TYPE sy-langu,
             sobreescribir TYPE tabadrsf-overwrite,
             entry_d       TYPE textpool-entry,
             message       TYPE bapi_msg,
             tabix         TYPE sy-tabix,
           END OF t_listado,
           tt_listado TYPE STANDARD TABLE OF t_listado.
    TYPES: BEGIN OF t_conf_tabla,
             name         TYPE trdir-name,
             campo_idioma TYPE dd03l-fieldname,
             long_clave   TYPE int4,
             pos_idioma   TYPE int4,
           END OF t_conf_tabla.

    DATA i_listado     TYPE tt_listado.
    DATA i_listado_ini TYPE tt_listado.
    DATA i_conf_tabla  TYPE TABLE OF t_conf_tabla.
    DATA o_alv         TYPE REF TO zcl_ap_alv_grid.
    DATA o_event       TYPE REF TO lcl_event_grid.

    METHODS main.

    METHODS seleccionar_datos.

    METHODS grabar_textos
      IMPORTING !name TYPE progname
                !lang TYPE sy-langu.

    METHODS grabar_cuad
      IMPORTING !name TYPE progname
                !lang TYPE sy-langu.

    METHODS grabar_dynpro
      IMPORTING !name TYPE progname
                !lang TYPE syst_langu
                !id   TYPE string.

    METHODS grabar_dtel
      IMPORTING !name TYPE progname
                !lang TYPE sy-langu.

    METHODS grabar_propuesta
      IMPORTING lango TYPE char1
                texto TYPE textpooltx
                langd TYPE syst_langu
                textd TYPE textpooltx.

    METHODS get_traduccion
      IMPORTING lango             TYPE char1
                langd             TYPE syst_langu
                texto             TYPE textpooltx
      RETURNING VALUE(traduccion) TYPE string.

    METHODS get_idioma_2 IMPORTING !lang           TYPE sy-langu
                         RETURNING VALUE(idioma_2) TYPE char2.

    METHODS status_dynpro_0100.
    METHODS command_dynpro_0100.

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

    METHODS propuesta.

ENDCLASS.

" ------VARIABLES--------------------------------------------------------
DATA o_prog TYPE REF TO zcl_report.


" ------PARAMETER/SELECT-OPTIONS EN PANTALLA-----------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-sel.
  PARAMETERS p_prog RADIOBUTTON GROUP g DEFAULT 'X' USER-COMMAND g.
  SELECT-OPTIONS: s_name FOR trdir-name,
                  s_devcl FOR tadir-devclass.
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


" -----------------------------------------------------------------------
"
" LOGICA DEL PROGRAMA
"
" -----------------------------------------------------------------------
CLASS lcl_event_grid IMPLEMENTATION.
  METHOD data_changed.
    ini_data_changed( cambios = er_data_changed->mt_good_cells ).
    LOOP AT i_cambios_celda INTO cambio_celda.
      AT NEW row_id.
        READ TABLE o_prog->i_listado INTO DATA(l_listado_ini) INDEX cambio_celda-row_id. "#EC CI_SUBRC
        DATA(l_listado) = l_listado_ini.
      ENDAT.

      set_valor_mod( CHANGING datos = l_listado ).

      AT END OF row_id.
        IF l_listado-entry_d <> ''.
          IF l_listado-entry_d = o_prog->i_listado_ini[ tabix = l_listado-tabix ]-entry_d.
            l_listado-lights = icon_equal_green.
          ELSE.
            l_listado-lights = icon_change.
          ENDIF.

          MODIFY o_prog->i_listado FROM l_listado INDEX cambio_celda-row_id.
          actualizar_fila( fila_ini        = l_listado_ini
                           fila_fin        = l_listado
                           er_data_changed = er_data_changed
                           fila            = cambio_celda-row_id ).
        ENDIF.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


" -----------------------------------------------------------------------
" CLASS zcl_report IMPLEMENTATION
" -----------------------------------------------------------------------
"
" -----------------------------------------------------------------------
CLASS zcl_report IMPLEMENTATION.
  METHOD main.
    seleccionar_datos( ).
    IF sy-batch IS INITIAL.
      CALL SCREEN 0100.
    ELSE.
      MESSAGE 'Este programa no se puede ejecutar en fondo'(pnf) TYPE 'E'.
    ENDIF.
  ENDMETHOD.                    " REPORT

  METHOD seleccionar_datos.
    DATA text_o TYPE TABLE OF textpool.
    DATA text_d TYPE TABLE OF textpool.

    IF p_prog = 'X'.
      sgpi_texto( 'Seleccionando datos'(sda) ).
      SELECT DISTINCT name
        FROM trdir
               JOIN
                 tadir ON trdir~name = tadir~obj_name  "#EC CI_BUFFJOIN
        INTO TABLE @DATA(i_prog)
        WHERE name     IN @s_name
          AND devclass IN @s_devcl.

      SELECT obj_name AS name FROM tadir                "#EC CI_GENBUFF
        INTO TABLE @DATA(i_prog_class)
        WHERE obj_name IN @s_name
          AND pgmid     = 'R3TR'
          AND object    = 'CLAS'
          AND devclass IN @s_devcl.
      LOOP AT i_prog_class ASSIGNING FIELD-SYMBOL(<cl>).
        DATA(l_cl) = |{ <cl>-name }%CP|.
        SELECT name FROM trdir
          INTO @DATA(l_name)
          UP TO 1 ROWS
          WHERE name LIKE @l_cl
          ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc = 0.
          APPEND l_name TO i_prog.                      "#EC CI_CONV_OK
        ENDIF.
      ENDLOOP.

      SORT i_prog BY name.
      DELETE ADJACENT DUPLICATES FROM i_prog COMPARING ALL FIELDS.

      o_prog->o_sgpi->get_filas_tabla( i_prog[] ).
      LOOP AT i_prog ASSIGNING FIELD-SYMBOL(<prog>).
        sgpi_texto( texto1    = 'Procesando datos'(pda)
                    cant_porc = 100 ).

        READ TEXTPOOL <prog>-name INTO text_o LANGUAGE p_lango.
        READ TEXTPOOL <prog>-name INTO text_d LANGUAGE p_langd.
        LOOP AT text_o ASSIGNING FIELD-SYMBOL(<to>) WHERE entry <> 'D       .'.
          APPEND INITIAL LINE TO i_listado ASSIGNING FIELD-SYMBOL(<listado>).
          <listado>-name = <prog>-name.
          <listado>-tipo = c_textos.
          MOVE-CORRESPONDING <to> TO <listado>.
          <listado>-lango = p_lango.
          <listado>-langd = p_langd.
          ASSIGN text_d[ key = <to>-key
                         id  = <to>-id ] TO FIELD-SYMBOL(<td>).
          IF sy-subrc = 0.
            IF <td>-entry <> 'ALV Template'.
              <listado>-entry_d = <td>-entry.
              <listado>-lights  = icon_yellow_light.
            ENDIF.
          ENDIF.

          IF <listado>-id = 'S'.
            <listado>-entry   = <listado>-entry+8.
            <listado>-entry_d = <listado>-entry_d+8.
          ENDIF.
        ENDLOOP.

        get_tabla( tabla       = 'RSMPTEXTS'
                   campo_clave = 'PROGNAME'
                   campo_texto = 'TEXT'
                   name        = <prog>-name ).

        get_tabla( tabla       = 'D021T'
                   campo_clave = 'PROG'
                   campo_texto = 'DTXT'
                   name        = <prog>-name ).

      ENDLOOP.

      DELETE i_listado WHERE tipo = c_tabla AND id = 'D021T-1000' AND key CS '%'.
    ELSEIF p_dtel = 'X'.
      SELECT rollname
        FROM dd04l
               JOIN
                 tadir ON  dd04l~rollname = tadir~obj_name "#EC CI_BUFFJOIN
                       AND tadir~object   = 'DTEL'
        INTO TABLE @DATA(i_dtel)
        WHERE rollname IN @s_dtel
          AND devclass IN @s_devcl.

      o_prog->o_sgpi->get_filas_tabla( i_dtel[] ).
      LOOP AT i_dtel ASSIGNING FIELD-SYMBOL(<dtel>).
        sgpi_texto( texto1    = 'Procesando datos'(pda)
                    cant_porc = 100 ).

        get_tabla( tabla       = 'DD04T'
                   campo_clave = 'ROLLNAME'
                   campo_texto = 'DDTEXT'
                   name        = CONV #( <dtel>-rollname ) ).

        get_tabla( tabla       = 'DD04T'
                   campo_clave = 'ROLLNAME'
                   campo_texto = 'REPTEXT'
                   name        = CONV #( <dtel>-rollname ) ).

        get_tabla( tabla       = 'DD04T'
                   campo_clave = 'ROLLNAME'
                   campo_texto = 'SCRTEXT_M'
                   name        = CONV #( <dtel>-rollname ) ).

        get_tabla( tabla       = 'DD04T'
                   campo_clave = 'ROLLNAME'
                   campo_texto = 'SCRTEXT_L'
                   name        = CONV #( <dtel>-rollname ) ).

        get_tabla( tabla       = 'DD04T'
                   campo_clave = 'ROLLNAME'
                   campo_texto = 'SCRTEXT_S'
                   name        = CONV #( <dtel>-rollname ) ).

      ENDLOOP.
    ENDIF.

    SORT i_listado BY name
                      lango
                      id
                      key.

    LOOP AT i_listado ASSIGNING <listado>.
      <listado>-tabix = sy-tabix.
    ENDLOOP.
    i_listado_ini = i_listado.
  ENDMETHOD.

  METHOD grabar_textos.
    DATA: text      TYPE TABLE OF textpool,
          text_orig TYPE TABLE OF textpool.

    READ TEXTPOOL name INTO text LANGUAGE lang.
    READ TEXTPOOL name INTO text_orig LANGUAGE p_lango.

    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>)
         WHERE name = name AND tipo = c_textos AND langd = lang AND lights = icon_change AND entry_d IS NOT INITIAL.
      ASSIGN text[ id  = <listado>-id
                   key = <listado>-key ] TO FIELD-SYMBOL(<t>).
      IF sy-subrc = 0.
        <t>-entry = <listado>-entry_d.
      ELSE.
        APPEND INITIAL LINE TO text ASSIGNING <t>.
        MOVE-CORRESPONDING <listado> TO <t>.
        <t>-entry = <listado>-entry_d.
      ENDIF.

      IF <listado>-entry <> ''.
        ASSIGN text_orig[ id  = <listado>-id
                          key = <listado>-key ] TO FIELD-SYMBOL(<to>).
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO text_orig ASSIGNING <to>.
          MOVE-CORRESPONDING <listado> TO <to>.
          DATA(l_new) = abap_true.
        ENDIF.
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
        IF l_new = abap_true.
          INSERT TEXTPOOL name FROM text_orig LANGUAGE p_lango.
        ENDIF.

        ls_e071-as4pos   = 1.
        ls_e071-pgmid    = 'LIMU'.
        ls_e071-object   = 'REPT'.
        ls_e071-obj_name = name.
        ls_e071-lang     = sy-langu.
        INSERT ls_e071 INTO TABLE lt_e071.
      ENDIF.
    ENDIF.

    " Grabamos la traducción estándar para que SAP refresque
    DATA(o_bi) = NEW zcl_ap_batch_input( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPLWBABAP'
                  dynpro  = '0100'
                  okcode  = '=TRAN' ).
    o_bi->campos( campo = 'RS38M-PROGRAMM'
                  valor = name ). " Nombre de programa ABAP
    o_bi->campos( campo = 'RS38M-FUNC_EDIT'
                  valor = '' ). " Editor
    o_bi->campos( campo = 'RS38M-FUNC_TEXT'
                  valor = 'X' ). " Elementos de texto

    " Idioma objetivo para la traducción
    o_bi->dynpro( program = 'SAPLSETX'
                  dynpro  = '0200'
                  okcode  = '=NEXT' ).
    WRITE p_lango TO aux1.
    WRITE p_langd TO aux2.
    o_bi->campos( campo = 'RSETX-MASTERLANG'
                  valor = aux1 ). " Clave de idioma
    o_bi->campos( campo = 'RSETX-TARG_LANGU'
                  valor = aux2 ). " Clave de idioma

    o_bi->dynpro( program = 'SAPMSSY0'
                  dynpro  = '0120'
                  okcode  = '=SAVE' ).

    o_bi->llamar_transaccion( tcode = 'SE38'
                              modo  = 'N' ).
  ENDMETHOD.

  METHOD grabar_cuad.
    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>)
         WHERE name = name AND tipo = c_tabla AND id = 'RSMPTEXTS' AND langd = lang AND lights = icon_change AND entry_d IS NOT INITIAL.

      IF grabar_registro( tabla       = 'RSMPTEXTS'
                          clave       = <listado>-key
                          idioma      = p_langd
                          campo_texto = 'TEXT'
                          texto       = <listado>-entry_d ) = abap_true.
        <listado>-lights = icon_green_light.
        DATA(ok) = abap_true.
      ELSE.
        <listado>-lights = icon_red_light.
      ENDIF.
    ENDLOOP.
    IF ok = abap_false.
      RETURN.
    ENDIF.

    ls_e071-as4pos   = 1.
    ls_e071-pgmid    = 'LIMU'.
    ls_e071-object   = 'CUAD'.
    ls_e071-obj_name = name.
    ls_e071-lang     = sy-langu.
    INSERT ls_e071 INTO TABLE lt_e071.

    " Grabamos la traducción estándar para que SAP refresque
    DATA(o_bi) = NEW zcl_ap_batch_input( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPMSMPE'
                  dynpro  = '0100'
                  okcode  = '=TRAN' ).
    o_bi->campos( campo = 'RSMPE-PROGRAM'
                  valor = name ). " Nombre de programa ABAP

    " Idioma objetivo para la traducción
    o_bi->dynpro( program = 'SAPLSETX'
                  dynpro  = '0200'
                  okcode  = '=NEXT' ).
    WRITE p_lango TO aux1.
    WRITE p_langd TO aux2.
    o_bi->campos( campo = 'RSETX-MASTERLANG'
                  valor = aux1 ). " Clave de idioma
    o_bi->campos( campo = 'RSETX-TARG_LANGU'
                  valor = aux2 ). " Clave de idioma

    o_bi->dynpro( program = 'SAPMSSY0'
                  dynpro  = '0120'
                  okcode  = '=SEQU' ).

    o_bi->dynpro( program = 'SAPMSSY0'
                  dynpro  = '0120'
                  okcode  = '=SAVE' ).

    o_bi->llamar_transaccion( tcode = 'SE41'
                              modo  = 'N' ).
  ENDMETHOD.

  METHOD grabar_dtel.
    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>)
         WHERE name = name AND tipo = c_tabla AND id = 'DD04T' AND langd = lang AND lights = icon_change AND entry_d IS NOT INITIAL.

      IF grabar_registro( tabla       = 'DD04T'
                          clave       = <listado>-key
                          idioma      = p_langd
                          campo_texto = <listado>-campo_texto
                          texto       = <listado>-entry_d ) = abap_true.
        <listado>-lights = icon_green_light.
        DATA(ok) = abap_true.
      ELSE.
        <listado>-lights = icon_red_light.
      ENDIF.
    ENDLOOP.
    IF ok = abap_false.
      RETURN.
    ENDIF.

    ls_e071-as4pos   = 1.
    ls_e071-pgmid    = 'LIMU'.
    ls_e071-object   = 'DTED'.
    ls_e071-obj_name = name.
    ls_e071-lang     = sy-langu.
    INSERT ls_e071 INTO TABLE lt_e071.

    " Grabamos la traducción estándar para que SAP refresque
    DATA(o_bi) = NEW zcl_ap_batch_input( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPLSD_ENTRY'
                  dynpro  = '1000'
                  okcode  = '=CHANGE_RADIO' ).
    o_bi->campos( campo = 'RSRD1-TBMA'
                  valor = '' ).
    o_bi->campos( campo = 'RSRD1-DDTYPE'
                  valor = 'X' ).

    o_bi->dynpro( program = 'SAPLSD_ENTRY'
                  dynpro  = '1000'
                  okcode  = '=WB_TRANSLATION' ).
    o_bi->campos( campo = 'RSRD1-DDTYPE_VAL'
                  valor = name ).

    " Idioma objetivo para la traducción
    o_bi->dynpro( program = 'SAPLSETX'
                  dynpro  = '0200'
                  okcode  = '=NEXT' ).
    WRITE p_lango TO aux1.
    WRITE p_langd TO aux2.
    o_bi->campos( campo = 'RSETX-MASTERLANG'
                  valor = aux1 ). " Clave de idioma
    o_bi->campos( campo = 'RSETX-TARG_LANGU'
                  valor = aux2 ). " Clave de idioma
    o_bi->dynpro( program = 'SAPMSSY0'
                  dynpro  = '0120'
                  okcode  = '=SAVE' ).

    o_bi->llamar_transaccion( tcode = 'SE11'
                              modo  = 'N' ).
  ENDMETHOD.

  METHOD get_tabla.
    DATA lr_struct_descr TYPE REF TO cl_abap_structdescr.
    DATA lr_table_descr  TYPE REF TO cl_abap_tabledescr.
    DATA lr_data         TYPE REF TO data.
    DATA where           TYPE string.

    FIELD-SYMBOLS <tabla> TYPE ANY TABLE.

    SELECT fieldname, keyflag, leng, position, languflag
      FROM dd03l
      INTO TABLE @DATA(i_campos_tabla)
      WHERE tabname = @tabla
      ORDER BY position.
    IF sy-subrc <> 0.
      MESSAGE |{ 'No existe tabla'(net) } { tabla }| TYPE 'E'.
    ENDIF.

    APPEND INITIAL LINE TO i_conf_tabla ASSIGNING FIELD-SYMBOL(<conf>).
    <conf>-name = tabla.
    LOOP AT i_campos_tabla ASSIGNING FIELD-SYMBOL(<campo>) WHERE keyflag = 'X'.
      IF <campo>-languflag = 'X'.
        <conf>-campo_idioma = <campo>-fieldname.
        <conf>-pos_idioma   = <conf>-long_clave.
      ENDIF.
      <conf>-long_clave = <conf>-long_clave + <campo>-leng. "#EC CI_CONV_OK
    ENDLOOP.

    IF <conf>-campo_idioma IS INITIAL.
      MESSAGE |{ 'La tabla'(ltb) } { tabla } { 'no tiene campo de idioma en la clave'(nic) }| TYPE 'E'.
    ENDIF.

    TRY.
        lr_struct_descr ?= cl_abap_typedescr=>describe_by_name( tabla ).
        lr_table_descr = cl_abap_tabledescr=>create( p_line_type = lr_struct_descr ).
        CREATE DATA lr_data TYPE HANDLE lr_table_descr.
        ASSIGN lr_data->* TO <tabla>.
      CATCH cx_root INTO DATA(lx_type) ##CATCH_ALL.
        MESSAGE lx_type->get_text( ) TYPE 'E'.
    ENDTRY.

    where = |{ <conf>-campo_idioma } = '{ p_lango }'|.
    IF campo_clave IS NOT INITIAL.
      where = |{ where } AND { campo_clave } = '{ name }'|.
    ENDIF.
    SELECT * FROM (tabla) INTO TABLE @<tabla> WHERE (where).
    LOOP AT <tabla> ASSIGNING FIELD-SYMBOL(<wa>).
      APPEND INITIAL LINE TO i_listado ASSIGNING FIELD-SYMBOL(<listado>).
      <listado>-name        = name.
      <listado>-lango       = p_lango.
      <listado>-langd       = p_langd.
      <listado>-tipo        = c_tabla.
      <listado>-campo_texto = campo_texto.
      <listado>-id          = tabla.
      IF tabla = 'D021T'.
        ASSIGN COMPONENT 'DYNR' OF STRUCTURE <wa> TO FIELD-SYMBOL(<dynr>).
        IF sy-subrc = 0.
          <listado>-id = |{ tabla }-{ <dynr> }|.
        ENDIF.
      ENDIF.
      <listado>-key = <wa>(<conf>-long_clave).
      ASSIGN COMPONENT campo_texto OF STRUCTURE <wa> TO FIELD-SYMBOL(<texto>).
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
      IF <listado>-entry_d IS NOT INITIAL.
        <listado>-lights = icon_yellow_light.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD grabar_registro.
    DATA lr_row     TYPE REF TO data.
    DATA where      TYPE string.
    DATA where_dest TYPE string.

    FIELD-SYMBOLS <row> TYPE any.

    CLEAR ok.

    ASSIGN i_conf_tabla[ name = tabla ] TO FIELD-SYMBOL(<conf>).

    TRY.
        cl_abap_typedescr=>describe_by_name( tabla ).

        CREATE DATA lr_row TYPE (tabla).
        ASSIGN lr_row->* TO <row>.

        <row> = clave.

        SELECT fieldname, keyflag, leng, position, languflag
          FROM dd03l
          INTO TABLE @DATA(i_campos_tabla)
          WHERE tabname = @tabla
          ORDER BY position.
        IF sy-subrc <> 0.
          MESSAGE |{ 'No existe tabla'(net) } { tabla }| TYPE 'E'.
        ENDIF.

        CLEAR: where,
               where_dest.
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
          IF where_dest IS INITIAL.
            where_dest = |{ <campo>-fieldname } = '{ <clave> }'|.
          ELSE.
            where_dest = |{ where_dest } and { <campo>-fieldname } = '{ <clave> }'|.
          ENDIF.
        ENDLOOP.

        " Si existe en idioma destino, modificamos
        SELECT SINGLE * FROM (tabla) INTO <row> WHERE (where_dest).
        IF sy-subrc = 0.
          DATA(l_mod) = abap_true.
        ELSE.
          " Si no, copiamos del origen
          SELECT SINGLE * FROM (tabla) INTO <row> WHERE (where).
        ENDIF.
        IF <row> IS NOT INITIAL.
          ASSIGN COMPONENT <conf>-campo_idioma OF STRUCTURE <row> TO FIELD-SYMBOL(<idioma>).
          <idioma> = idioma.
          ASSIGN COMPONENT campo_texto OF STRUCTURE <row> TO FIELD-SYMBOL(<texto>).
          IF <texto> <> texto OR l_mod = abap_false.
            <texto> = texto.

            MODIFY (tabla) FROM <row>.
            IF sy-subrc = 0.
              ok = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH cx_root INTO DATA(lx_root) ##CATCH_ALL.
        MESSAGE lx_root->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD grabar_propuesta.
    DATA ztemps TYPE ztemps.

    ztemps-clave    = 'TRAD'.
    ztemps-subclave = |{ lango }-{ langd }-{ texto }|.
    ztemps-valor    = |{ lango }-{ langd }|.
    ztemps-texto    = texto.
    ztemps-string   = textd.

    SELECT string FROM ztemps
      INTO @DATA(texto_bd)
      UP TO 1 ROWS
      WHERE clave = 'TRAD' AND valor = @ztemps-valor AND subclave = @ztemps-subclave
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0 OR texto_bd <> textd.
      ztemps-permanente = 'X'.
      ztemps-erdat      = sy-datum.
      ztemps-erzet      = sy-uzeit.
      ztemps-ernam      = sy-uname.
      MODIFY ztemps FROM ztemps.
    ENDIF.
  ENDMETHOD.

  METHOD status_dynpro_0100.
    status_dynpro( EXPORTING cprog     = 'ZAP_STATUS'
                             status    = 'ST_DYN'
                   CHANGING  i_listado = i_listado ).
    IF inicio IS NOT INITIAL.
      RETURN.
    ENDIF.

    inicio = 'X'.
    o_alv->add_button( button = 'F01'
                       text   = 'Excel'
                       icon   = icon_xls
                       ucomm  = 'EXCELC' ) ##NO_TEXT.
    o_alv->add_button( button = 'F02'
                       text   = 'Carga Traducción'(ctr)
                       icon   = icon_import
                       ucomm  = 'CARGAR' ).
    o_alv->add_button( button = 'F03'
                       text   = 'Grabar traducción'(gtr)
                       icon   = icon_system_save
                       ucomm  = 'GRABAR' ).
    o_alv->add_button( button = 'F04'
                       text   = 'Propuesta'(pro)
                       icon   = icon_execute_object
                       ucomm  = 'PROP' ).
    o_alv->add_button( button = 'F05'
                       text   = 'Traducir'(tra)
                       icon   = icon_url
                       ucomm  = 'TRADUCIR' ).
    o_alv->add_button( button = 'M01'
                       text   = 'Descarga propuestas'(dcp)
                       ucomm  = 'DESC_PROP' ).
    o_alv->add_button( button = 'M02'
                       text   = 'Importar propuestas'(imp)
                       ucomm  = 'SUBIR_PROP' ).

    o_alv->add_button( button = 'M03'
                       text   = 'Buscar textos sin elemento'(bts)
                       ucomm  = 'TEXT_ELM' ).
    o_alv->add_button( button = 'M04'
                       text   = 'Proponer elementos de texto'(pte)
                       ucomm  = 'PROP_TEXT_ELM' ).
    o_alv->add_button( button = 'M05'
                       text   = 'Buscar mensajes'(msg)
                       ucomm  = 'MESSAGES' ).

    o_alv->variant-variant = p_vari.
    o_alv->registrar_mod( ).
    o_alv->set_layout( no_rowmove = 'X'
                       no_rowins  = 'X' ).
    o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_refresh ).
    o_alv->set_campos_tabint( i_listado[] ).
    o_alv->set_field_quitar( 'CHECK,TABIX' ).
    o_alv->set_field( campo = 'SOBREESCRIBIR'
                      op    = 'CHECKBOX' ).
    o_alv->set_field_input( 'SOBREESCRIBIR,ENTRY_D' ).
    o_alv->set_orden( 'NAME,TIPO,LANGO,ID,KEY' ).
    o_alv->set_field_text( campo = 'KEY'
                           valor = 'Clave'(cla) ).
    o_alv->set_field_text( campo = 'ID'
                           valor = 'Tabla'(tab) ).
    o_alv->set_field_text( campo = 'ENTRY'
                           valor = 'Texto Origen'(txo) ).
    o_alv->set_field_text( campo  = 'LANGO'
                           valor  = 'IOrig'
                           valor2 = 'Idioma Origen'(ido) ).
    o_alv->set_field_text( campo = 'ENTRY_D'
                           valor = 'Texto Destino'(txd) ).
    o_alv->set_field_text( campo  = 'LANGD'
                           valor  = 'IDest'
                           valor2 = 'Idioma Destino'(idd) ).

    sgpi_texto( 'Generando informe'(gin) ).
    o_alv->show( CHANGING tabla = i_listado ).

    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).
  ENDMETHOD.

  METHOD command_dynpro_0100.
    TYPES: BEGIN OF t_elm,
             check    TYPE xfeld,
             linea    TYPE sy-tabix,
             key      TYPE rs38m-itex132,
             text     TYPE rs38m-itex132,
             text_sap TYPE rs38m-itex132,
           END OF t_elm.

    DATA l_comm_sel  TYPE string VALUE 'PROP,TRADUCIR,TEXT_ELM,PROP_TEXT_ELM,MESSAGES'.
    DATA l_hay_sel   TYPE c LENGTH 1.
    DATA i_sel       TYPE TABLE OF t_carga.
    DATA i_list_proc TYPE TABLE OF t_listado.
    DATA i_content   TYPE TABLE OF text255.
    DATA i_elm       TYPE TABLE OF t_elm.
    DATA: l_linea_completa TYPE string,
          l_original       TYPE string,
          l_nuevo          TYPE string,
          l_msg            TYPE bapi_msg.
    DATA l_n1 TYPE numc1.
    DATA: l_var_msg TYPE string,
          lv_inner  TYPE string.
    DATA: lt_parts TYPE TABLE OF string,
          lv_part  TYPE string.

    command_dynpro( EXPORTING o_alv         = o_alv
                              seleccion     = l_comm_sel
                    CHANGING  i_listado     = i_listado
                              i_listado_ini = i_listado_ini
                              hay_sel       = l_hay_sel ).

    CASE ucomm.
      WHEN 'EXCELC'.
        MOVE-CORRESPONDING o_prog->i_listado TO i_carga.
        DATA(o_alvl) = NEW zcl_ap_alv( tabla  = 'I_CARGA'
                                       handle = 'CARG' ).

        IF p_fiche IS INITIAL.
          zcl_ap_abap2xls=>alv_2_xls( alv              = o_alvl->o_alv
                                      refresh_metadata = 'X'
                                      tabla            = i_carga
                                      abrir            = 'X'
                                      nombre_fichero   = 'Traducciones.xlsx' ) ##NO_TEXT.
        ELSE.
          zcl_ap_abap2xls=>alv_2_xls( alv              = o_alvl->o_alv
                                      refresh_metadata = 'X'
                                      tabla            = i_carga
                                      abrir            = 'X'
                                      ruta             = p_fiche ).
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
            ASSIGN o_prog->i_listado[ name        = <carga>-name
                                      langd       = <carga>-langd
                                      id          = <carga>-id
                                      key         = <carga>-key
                                      campo_texto = <carga>-campo_texto ] TO FIELD-SYMBOL(<listado>).
            IF sy-subrc = 0.
              IF <listado>-entry_d <> <carga>-entry_d.
                <listado>-entry_d = <carga>-entry_d.
                <listado>-lights  = icon_change.
              ENDIF.
            ENDIF.
          ELSE.
            ASSIGN o_prog->i_listado[ name        = <carga>-name
                                      langd       = <carga>-langd
                                      id          = <carga>-id
                                      key         = <carga>-key
                                      campo_texto = <carga>-campo_texto
                                      entry_d     = '' ] TO <listado>.
            IF sy-subrc = 0.
              <listado>-entry_d = <carga>-entry_d.
              <listado>-lights  = icon_change.
            ENDIF.
          ENDIF.
        ENDLOOP.

        o_alv->refrescar_grid( ).

      WHEN 'GRABAR'.
        CLEAR lt_e071.
        LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE lights = icon_change.

          IF <listado>-tipo = c_textos.
            IF NOT line_exists( i_sel[ name  = <listado>-name
                                       langd = <listado>-langd
                                       tipo  = c_textos ] ).
              APPEND VALUE #( name  = <listado>-name
                              langd = <listado>-langd
                              tipo  = c_textos ) TO i_sel.
              o_prog->grabar_textos( name = <listado>-name
                                     lang = <listado>-langd ).
            ENDIF.
          ENDIF.

          IF <listado>-tipo = c_tabla AND <listado>-id = 'RSMPTEXTS'.
            IF NOT line_exists( i_sel[ name  = <listado>-name
                                       langd = <listado>-langd
                                       tipo  = c_tabla
                                       id    = 'RSMPTEXTS' ] ).
              APPEND VALUE #( name  = <listado>-name
                              langd = <listado>-langd
                              tipo  = c_tabla
                              id    = 'RSMPTEXTS' ) TO i_sel.
              o_prog->grabar_cuad( name = <listado>-name
                                   lang = <listado>-langd ).
            ENDIF.
          ENDIF.

          IF <listado>-id CS 'D021T'.
            IF NOT line_exists( i_sel[ name  = <listado>-name
                                       langd = <listado>-langd
                                       tipo  = c_tabla
                                       id    = <listado>-id ] ).
              APPEND VALUE #( name  = <listado>-name
                              langd = <listado>-langd
                              tipo  = c_tabla
                              id    = <listado>-id ) TO i_sel.
              o_prog->grabar_dynpro( name = <listado>-name
                                     lang = <listado>-langd
                                     id   = <listado>-id ).
            ENDIF.
          ENDIF.

          IF <listado>-tipo = c_tabla AND <listado>-id = 'DD04T'.
            IF NOT line_exists( i_sel[ name  = <listado>-name
                                       langd = <listado>-langd
                                       tipo  = c_tabla
                                       id    = 'DD04T' ] ).
              APPEND VALUE #( name  = <listado>-name
                              langd = <listado>-langd
                              tipo  = c_tabla
                              id    = 'DD04T' ) TO i_sel.

              o_prog->grabar_dtel( name = <listado>-name
                                   lang = <listado>-langd ).
            ENDIF.
          ENDIF.

          IF NOT <listado>-entry_d IS INITIAL.
            o_prog->grabar_propuesta( lango = p_lango
                                      texto = <listado>-entry
                                      langd = p_langd
                                      textd = <listado>-entry_d ).
          ENDIF.

        ENDLOOP.
        i_listado_ini = i_listado.

        IF lt_e071 IS NOT INITIAL.
          SORT lt_e071.
          DELETE ADJACENT DUPLICATES FROM lt_e071.
          CALL FUNCTION 'TR_REQUEST_CHOICE'
            EXPORTING
*             IV_REQUEST_TYPES =
*             IV_CLI_DEP = ' '
*             IV_REQUEST = ' '
              it_e071 = lt_e071
*             IV_LOCK_OBJECTS = ' '
*             IV_NO_OWNER_CHECK = ' '
            EXCEPTIONS
              OTHERS  = 1.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.

        o_alv->refrescar_grid( ).

      WHEN 'PROP'.
        propuesta( ).

      WHEN 'TRADUCIR'.
        LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X' AND ( sobreescribir = abap_true OR entry_d IS INITIAL ).
          DATA(l_trad) = o_prog->get_traduccion( lango = <listado>-lango
                                                 langd = <listado>-langd
                                                 texto = <listado>-entry ).
          IF l_trad <> ''.
            IF <listado>-entry_d <> l_trad.
              <listado>-entry_d = l_trad.
              <listado>-lights  = icon_change.
            ENDIF.
          ENDIF.
        ENDLOOP.
        o_alv->refrescar_grid( ).

      WHEN 'DESC_PROP'.
        SELECT * FROM ztemps                  "#EC CI_ALL_FIELDS_NEEDED
          INTO TABLE @DATA(i_trad)
          WHERE clave = 'TRAD'.
        zcl_ap_segw=>get_json( EXPORTING datos = i_trad
                               IMPORTING json  = DATA(l_json) ).
        IF l_json IS NOT INITIAL.
          zcl_ap_ficheros=>grabar_xstring( fichero = 'propuesta traducciones.json'
                                           dialogo = 'X'
                                           string  = l_json ) ##NO_TEXT.
        ENDIF.

      WHEN 'SUBIR_PROP'.
        zcl_ap_ficheros=>leer_xstring( EXPORTING popup_select_fichero = 'X'
                                                 get_string           = 'X'
                                       IMPORTING string               = l_json ).
        zcl_ap_segw=>set_json( EXPORTING json  = l_json
                               IMPORTING datos = i_trad ).

        MODIFY ztemps FROM TABLE i_trad.

      WHEN 'TEXT_ELM' OR 'PROP_TEXT_ELM' OR 'MESSAGES'.
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X' AND tipo = c_textos.
          IF NOT line_exists( i_list_proc[ name = <listado>-name ] ).
            APPEND VALUE #( name = <listado>-name
                            tipo = c_textos ) TO i_list_proc.
            DATA(l_clase) = ''.
            IF <listado>-name CS '='.
              l_clase = 'X'.
              SPLIT <listado>-name AT '=' INTO DATA(obj_name) aux1.
              i_content = zcl_ap_dev=>get_objeto_como_texto( tipo   = 'CLAS'
                                                             nombre = obj_name ).
            ELSE.
              READ REPORT <listado>-name INTO i_content.
            ENDIF.
            IF NOT i_content IS INITIAL.
              CLEAR i_elm.
              LOOP AT i_content ASSIGNING FIELD-SYMBOL(<linea>).
                DATA(l_tabix) = sy-tabix.

                DATA(l_lineam) = to_upper( <linea> ).
                IF ucomm = 'TEXT_ELM'.
                  DATA(matcher) = cl_abap_matcher=>create_pcre( pattern = '''([^'']*)''\(([A-Za-z0-9]{3})\)' text = <linea> ) ##NO_TEXT.
                  WHILE matcher->find_next( ) = abap_true.
                    DATA(l_id) = to_upper( matcher->get_submatch( 2 ) ).
                    DATA(l_text) = matcher->get_submatch( 1 ).
                    ASSIGN i_listado[ name = <listado>-name
                                      tipo = c_textos
                                      key  = l_id ] TO FIELD-SYMBOL(<sap>).
                    IF sy-subrc <> 0.
                      APPEND VALUE #( text  = l_text
                                      key   = l_id
                                      check = 'X' ) TO i_elm.
                    ELSEIF <sap>-entry <> l_text.
                      APPEND VALUE #( text     = l_text
                                      text_sap = <sap>-entry
                                      key      = l_id
                                      check    = '' ) TO i_elm.
                    ENDIF.
                  ENDWHILE.
                ELSEIF ucomm = 'PROP_TEXT_ELM'.
*                  IF l_tabix = 233.
*                    BREAK-POINT.
*                  ENDIF.
                  IF    <linea>  CS 'NO_TEXT' OR l_lineam CS ` AND `
                     OR l_lineam CS 'WHEN'    OR l_lineam CS 'CALL FUNCTION'.
                    CONTINUE.
                  ENDIF.
*                  matcher = cl_abap_matcher=>create_pcre( pattern = |'([^']*)'(?!\\()| text = <linea> ).
                  matcher = cl_abap_matcher=>create_pcre( pattern = '''([^'']*)''(?!\()' text = <linea> ) ##NO_TEXT.
                  WHILE matcher->find_next( ) = abap_true.
                    l_text = matcher->get_submatch( 1 ).
                    IF    strlen( l_text )  = 1   OR l_text  = to_upper( l_text ) OR l_text CS '=' OR l_text(1)  = '('
                       OR l_text           CS '-' OR l_text CS '{'                OR l_text CS '|' OR l_text    CS '@'.
                      CONTINUE.
                    ENDIF.

                    CLEAR l_linea_completa.
                    l_linea_completa = <linea>.

                    DATA(l_prev) = l_tabix.
                    DO 3 TIMES.
                      l_prev = l_prev - 1.
                      ASSIGN i_content[ l_prev ] TO FIELD-SYMBOL(<prev_line>).
                      IF sy-subrc = 0.
                        IF    zcl_ap_string=>ultimo_caracter( <prev_line> ) = '.'
                           OR <prev_line>(1) = '*'.
                          EXIT.
                        ELSE.
                          l_linea_completa = condense( |{ <prev_line> }#{ l_linea_completa }| ).
                        ENDIF.
                      ELSE.
                        EXIT.
                      ENDIF.
                    ENDDO.

                    IF zcl_ap_string=>ultimo_caracter( <linea> ) <> '.'.
                      DATA(l_next) = l_tabix.
                      DO 3 TIMES.
                        l_next = l_next + 1.
                        ASSIGN i_content[ l_next ] TO FIELD-SYMBOL(<next_line>).
                        IF sy-subrc = 0.
                          l_linea_completa = condense( |{ l_linea_completa }#{ <next_line> }| ).
                          IF    zcl_ap_string=>ultimo_caracter( <next_line> ) = '.'
                             OR <next_line>(1) = '*'.
                            EXIT.
                          ENDIF.

                        ELSE.
                          EXIT.
                        ENDIF.
                      ENDDO.
                    ENDIF.

                    DATA(l_texto_may) = to_upper( l_linea_completa ).
                    IF    l_texto_may CS 'NO_TEXT' OR l_texto_may CS ` AND ` OR l_texto_may CS '~'
                       OR l_texto_may CS 'WHEN'    OR l_texto_may CS 'CALL FUNCTION'
                       OR l_texto_may CS 'REPLACE'.
                      CONTINUE.
                    ENDIF.

                    CLEAR string.
                    DATA(l_nuevo_id) = abap_false.
                    ASSIGN i_listado[ name = <listado>-name
                                      tipo = c_textos
                                      entry = l_text ] TO <sap>.
                    IF sy-subrc = 0.
                      string = <sap>-key.
                    ELSE.
                      ASSIGN i_elm[ text = l_text ] TO FIELD-SYMBOL(<elm>).
                      IF sy-subrc = 0.
                        string = <elm>-key.
                      ELSE.
                        l_nuevo_id = abap_true.
                        SPLIT l_text AT ' ' INTO TABLE DATA(i_palabras).
                        IF lines( i_palabras ) = 1.
                          string = i_palabras[ 1 ].
                        ELSE.
                          LOOP AT i_palabras ASSIGNING FIELD-SYMBOL(<palabra>).
                            IF <palabra>(1) <> '.' AND <palabra>(1) <> ` `.
                              IF string IS INITIAL.
                                string = <palabra>(1).
                              ELSE.
                                string = |{ string }{ <palabra>(1) }|.
                              ENDIF.
                            ENDIF.
                          ENDLOOP.
                        ENDIF.
                        IF strlen( string ) = 1.
                          string = |{ string }01|.
                        ELSEIF strlen( string ) = 2.
                          string = |{ string }1|.
                        ENDIF.
                      ENDIF.
                    ENDIF.

                    aux1 = to_upper( string ).
                    aux1 = aux1(3).
                    REPLACE ALL OCCURRENCES OF '.' IN aux1 WITH 'P'.
                    REPLACE ALL OCCURRENCES OF '%' IN aux1 WITH 'P'.
                    REPLACE ALL OCCURRENCES OF '?' IN aux1 WITH 'Q'.
                    REPLACE ALL OCCURRENCES OF '¿' IN aux1 WITH 'Q'.
                    REPLACE ALL OCCURRENCES OF '/' IN aux1 WITH 'S'.
                    REPLACE ALL OCCURRENCES OF 'º' IN aux1 WITH 'O'.

                    string = aux1.
                    zcl_ap_string=>quitar_caracteres_extranos( CHANGING string = string ).
                    aux1 = string.

                    IF l_nuevo_id = abap_true.
                      DO 9 TIMES.
                        l_n1 = sy-index.
                        DATA(l_existe) = abap_false.
                        ASSIGN i_listado[ name = <listado>-name
                                          tipo = c_textos
                                          key  = aux1 ] TO <sap>.
                        IF sy-subrc = 0.
                          l_existe = abap_true.
                        ELSE.
                          ASSIGN i_elm[ key = aux1 ] TO <elm>.
                          IF sy-subrc = 0.
                            l_existe = abap_true.
                          ENDIF.
                        ENDIF.
                        IF l_existe = abap_false.
                          EXIT.
                        ELSE.
                          aux1+2(1) = l_n1.
                        ENDIF.
                      ENDDO.
                    ENDIF.

                    APPEND VALUE #( text     = l_text
                                    linea    = l_tabix
                                    key      = aux1(3)
                                    text_sap = l_linea_completa ) TO i_elm.
                  ENDWHILE.
                ELSEIF ucomm = 'MESSAGES'.
                  IF l_lineam CS 'MESSAGE' AND l_lineam CS '|' AND l_lineam CS 'TYPE' AND NOT l_lineam CS '('.
                    " 1) Extraer entre pipes
                    FIND REGEX '\|([^|]+)\|' IN <linea> SUBMATCHES l_var_msg.
                    " lv_inner = 'INICIO { VARIABLE } FINAL'
                    lv_inner = l_var_msg.

                    " 2) Reemplazar todos los bloques { ... } por un separador literal
                    REPLACE ALL OCCURRENCES OF REGEX '\{[^}]*\}' IN lv_inner WITH '<<<SEP>>>'.
                    " ahora lv_inner = 'INICIO <<<SEP>>> FINAL'

                    IF NOT lv_inner CS '<<<SEP>>>'.
                      string = lv_inner.
                    ELSE.
                      " 3) Separar por el separador (SPLIT por texto funciona bien)
                      SPLIT lv_inner AT '<<<SEP>>>' INTO TABLE lt_parts.

                      CLEAR string.
                      LOOP AT lt_parts INTO lv_part.
                        CONDENSE lv_part. " quita espacios repetidos y bordes
                        lv_part = |{ lv_part } &_|.
                        IF sy-tabix = 1.
                          string = lv_part.
                        ELSE.
                          string = |{ string }%{ lv_part }|.
                        ENDIF.
                      ENDLOOP.
                      string = |{ string }%|.
                    ENDIF.
                    IF string IS NOT INITIAL.
                      SELECT arbgb, msgnr, text FROM t100
                        INTO TABLE @DATA(i_msg)
                        WHERE sprsl    = @p_lango
                          AND text  LIKE @string.
                      IF sy-subrc <> 0.
                        APPEND VALUE #( text     = l_var_msg
                                        linea    = l_tabix
                                        key      = 'ERROR'
                                        text_sap = 'NADA' ) TO i_elm.
                      ELSE.
                        LOOP AT i_msg ASSIGNING FIELD-SYMBOL(<msg>).
                          SPLIT l_lineam AT `TYPE '` INTO aux1 aux2.

                          APPEND VALUE #( text     = l_var_msg
                                          linea    = l_tabix
                                          key      = |{ aux2(1) }{ <msg>-msgnr }({ <msg>-arbgb })|
                                          text_sap = <msg>-text ) TO i_elm.
                        ENDLOOP.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDLOOP.
              IF i_elm IS NOT INITIAL.
                zcl_ap_popup=>popup_alv_seleccion( EXPORTING campos_texto = 'TEXT=Código&&TEXT_SAP=Actual'
                                                   IMPORTING cancelado    = DATA(l_cancelado)
                                                   CHANGING  tabla        = i_elm ).
                IF l_cancelado = abap_false.
                  IF ucomm = 'PROP_TEXT_ELM' OR ucomm = 'MESSAGES'.
                    DELETE i_elm WHERE check = '' OR key = 'ERROR'.
                    DELETE ADJACENT DUPLICATES FROM i_elm COMPARING linea key.
                  ENDIF.
                  LOOP AT i_elm ASSIGNING <elm> WHERE check = 'X'.
                    IF ucomm = 'PROP_TEXT_ELM' OR ucomm = 'MESSAGES'.
                      IF l_clase = 'X'.
                        MESSAGE 'No es posible modificar clases' TYPE 'I'.
                        RETURN.
                      ENDIF.
                      DATA(l_mod_report) = abap_true.
                      ASSIGN i_content[ <elm>-linea ] TO FIELD-SYMBOL(<linea_mod>).
                      IF sy-subrc = 0.
                        IF ucomm = 'PROP_TEXT_ELM'.
                          l_original = |'{ <elm>-text }'|.
                          l_nuevo = |'{ <elm>-text }'({ <elm>-key })|.
                          REPLACE l_original IN <linea_mod> WITH l_nuevo.
                          IF NOT <linea_mod> CS l_nuevo.
                            MESSAGE |No se ha podido modificar la línea { <elm>-linea } { <elm>-text } | TYPE 'E'.
                          ENDIF.
                        ELSEIF ucomm = 'MESSAGES'.
                          <linea_mod> = |{ <linea_mod> } "TODO { <elm>-key } { <elm>-text_sap }|.
                        ENDIF.
                      ENDIF.
                    ELSEIF ucomm = 'TEXT_ELM'.
                      APPEND INITIAL LINE TO i_listado ASSIGNING FIELD-SYMBOL(<listado2>).
                      <listado2> = <listado>.
                      <listado2>-check  = ''.
                      <listado2>-id     = 'I'.
                      <listado2>-key    = <elm>-key.
                      <listado2>-entry  = <elm>-text.
                      <listado2>-length = strlen( <elm>-text ).
                      CLEAR <listado2>-entry_d.
                    ENDIF.
                  ENDLOOP.

                  IF l_mod_report = abap_true.
                    CALL FUNCTION 'EDITOR_SYNTAX_CHECK'
                      EXPORTING
                        i_program       = <listado>-name
                      IMPORTING
                        o_error_message = l_msg
                        o_error_line    = l_tabix
                      TABLES
                        i_source        = i_content.

                    IF NOT l_msg IS INITIAL.
                      MESSAGE |Linea { l_tabix } { l_msg }| TYPE 'I'.
                    ELSE.
                      INSERT REPORT <listado>-name FROM i_content.
                    ENDIF.
                  ELSE.
                    o_alv->refrescar_grid( ).
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD grabar_dynpro.
    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>)
         WHERE name = name AND tipo = c_tabla AND id = id AND langd = lang AND lights = icon_change AND entry_d IS NOT INITIAL.

      IF grabar_registro( tabla       = 'D021T'
                          clave       = <listado>-key
                          idioma      = p_langd
                          campo_texto = 'DTXT'
                          texto       = <listado>-entry_d ) = abap_true.
        <listado>-lights = icon_green_light.
        DATA(ok) = abap_true.
      ELSE.
        <listado>-lights = icon_red_light.
      ENDIF.
    ENDLOOP.
    IF ok = abap_false.
      RETURN.
    ENDIF.

    ls_e071-as4pos = 1.
    ls_e071-pgmid  = 'LIMU'.
    ls_e071-object = 'DYNP'.

    DATA(l_dynpro) = id+6.
    ls_e071-obj_name = |{ name } { l_dynpro }|.
    ls_e071-lang     = sy-langu.
    INSERT ls_e071 INTO TABLE lt_e071.

    " Grabamos la traducción estándar para que SAP refresque
    DATA(o_bi) = NEW zcl_ap_batch_input( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPLWBSCREEN'
                  dynpro  = '0010'
                  okcode  = '=TRAN' ).
    o_bi->campos( campo = 'RS37A-DYNPROG'
                  valor = name ). " Nombre de programa ABAP
    o_bi->campos( campo = 'FELD-DYNNR'
                  valor = l_dynpro ).

    " Idioma objetivo para la traducción
    o_bi->dynpro( program = 'SAPLSETX'
                  dynpro  = '0200'
                  okcode  = '=NEXT' ).
    WRITE p_lango TO aux1.
    WRITE p_langd TO aux2.
    o_bi->campos( campo = 'RSETX-MASTERLANG'
                  valor = aux1 ). " Clave de idioma
    o_bi->campos( campo = 'RSETX-TARG_LANGU'
                  valor = aux2 ). " Clave de idioma

    o_bi->dynpro( program = 'SAPMSSY0'
                  dynpro  = '0120'
                  okcode  = '=SEQU' ).

    o_bi->dynpro( program = 'SAPMSSY0'
                  dynpro  = '0120'
                  okcode  = '=SAVE' ).

    o_bi->llamar_transaccion( tcode = 'SE51'
                              modo  = 'N' ).
  ENDMETHOD.

  METHOD get_traduccion.
    TYPES: BEGIN OF ty_response_data,
             translatedtext TYPE string,
             match          TYPE i,
           END OF ty_response_data.

    TYPES: BEGIN OF ty_match,
             id               TYPE string,
             segment          TYPE string,
             translation      TYPE string,
             source           TYPE string,
             target           TYPE string,
             quality          TYPE i,
             reference        TYPE string,
             usage_count      TYPE i,
             subject          TYPE string,
             created_by       TYPE string,
             last_updated_by  TYPE string,
             create_date      TYPE string,
             last_update_date TYPE string,
             match            TYPE i,
             penalty          TYPE i,
           END OF ty_match.

    TYPES ty_matches_table TYPE STANDARD TABLE OF ty_match WITH EMPTY KEY.

    TYPES: BEGIN OF ty_root,
             responsedata    TYPE ty_response_data,
             quotafinished   TYPE abap_bool,
             mtlangsupported TYPE string,
             responsedetails TYPE string,
             responsestatus  TYPE i,
             responderid     TYPE string,
             exception_code  TYPE string,
             matches         TYPE ty_matches_table,
           END OF ty_root.

    DATA l_valor TYPE ty_root.

    CLEAR traduccion.
    IF texto IS INITIAL.
      RETURN.
    ENDIF.

    DATA(l_texto) = cl_http_utility=>escape_url( CONV #( texto ) ).
    DATA(l_peticion) = |q={ l_texto }&langpair={ get_idioma_2( lango ) }%&%{ get_idioma_2( langd ) }|.
    REPLACE '%&%' IN l_peticion WITH '|'.

    NEW zcl_ap_odata( host = 'https://api.mymemory.translated.net/' )->get_odata( ##NO_TEXT
      EXPORTING
        servicio  = |get?{ l_peticion }| ##NO_TEXT
*       popup_respuesta = 'X'
        get_datos = 'X'
      IMPORTING
        datos     = l_valor ).

    IF l_valor-responsestatus = '200'.
      IF l_valor-responsedata-translatedtext <> '' AND l_valor-responsedata-match >= 1.
        traduccion = l_valor-responsedata-translatedtext.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_idioma_2.
    WRITE lang TO idioma_2.
  ENDMETHOD.

  METHOD propuesta.
    DATA i_tradsap TYPE STANDARD TABLE OF lxe_pcx_s1.

    FIELD-SYMBOLS <listado> TYPE zcl_report=>t_listado.

    LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X' AND ( sobreescribir = abap_true OR entry_d IS INITIAL ) AND entry IS NOT INITIAL.

* Intento ver si hay alguna traducción en la tabla ZTEMPS primero
      DATA(l_idiomas) = |{ <listado>-lango }-{ <listado>-langd }|.
      SELECT string FROM ztemps
        INTO @DATA(texto)
        UP TO 1 ROWS
        WHERE clave = 'TRAD' AND valor = @l_idiomas AND texto = @<listado>-entry
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0 AND texto IS NOT INITIAL.
        IF <listado>-entry_d <> texto.
          <listado>-entry_d = texto.
          <listado>-lights  = icon_change.
        ENDIF.
      ELSE.
* Si no, busco si hay algo confirmado en el módulo de traducciones estándar
        DATA(l_src_lang) = COND lxeisolang( WHEN <listado>-lango = 'E' THEN 'enUS'
                                            WHEN <listado>-lango = 'S' THEN 'esES'
                                            WHEN <listado>-lango = 'F' THEN 'frFR' ).
        DATA(l_dst_lang) = COND lxeisolang( WHEN <listado>-langd = 'E' THEN 'enUS'
                                            WHEN <listado>-langd = 'S' THEN 'esES'
                                            WHEN <listado>-langd = 'F' THEN 'frFR' ).
        DATA(l_src_text) = <listado>-entry.
        CALL FUNCTION 'LXE_PP1_SEARCH_ENTRY'
          EXPORTING
            s_lang    = l_src_lang
            t_lang    = l_dst_lang
            s_text    = l_src_text
          TABLES
            ex_pcx_s1 = i_tradsap.
        ASSIGN i_tradsap[ s_text = l_src_text ] TO FIELD-SYMBOL(<trad>).
        IF sy-subrc = 0.
          IF <listado>-entry_d <> <trad>-t_text AND <trad>-t_text <> ''.
            <listado>-entry_d = <trad>-t_text.
            <listado>-lights  = icon_change.
          ENDIF.
        ENDIF.
      ENDIF.

* Si no, miro lo puedo sacar a partir de valores similares en elementos de datos
      IF strlen( <listado>-entry ) < 55.
        IF <listado>-entry_d IS INITIAL AND <listado>-lights <> icon_change.
          SELECT rollname FROM dd04t
            INTO @DATA(l_dominio)
            UP TO 1 ROWS
            WHERE ddlanguage = @<listado>-lango
              AND ddtext     = @<listado>-entry
            ORDER BY PRIMARY KEY.
          ENDSELECT.
          IF sy-subrc = 0.
            SELECT SINGLE ddtext FROM dd04t
              INTO <listado>-entry_d
              WHERE rollname   = l_dominio
                AND ddlanguage = <listado>-langd.
            IF <listado>-entry_d <> ''.
              <listado>-lights = icon_change.
            ENDIF.
          ENDIF.

* Si no, miro lo puedo sacar a partir de valores similares de dominios
          IF <listado>-entry_d IS INITIAL AND <listado>-lights <> icon_change.
            SELECT domname, valpos FROM dd07t
              INTO (@l_dominio, @DATA(l_valpos))
              UP TO 1 ROWS
              WHERE ddlanguage = @<listado>-lango
                AND ddtext     = @<listado>-entry
              ORDER BY PRIMARY KEY.
            ENDSELECT.
            IF sy-subrc = 0.
              SELECT SINGLE ddtext FROM dd07t
                INTO <listado>-entry_d
                WHERE domname    = l_dominio
                  AND valpos     = l_valpos
                  AND ddlanguage = <listado>-langd.
              IF <listado>-entry_d <> ''.
                <listado>-lights = icon_change.
              ENDIF.
            ENDIF.
          ENDIF.

* Si no, miro lo puedo sacar a partir de valores similares demensajes
          IF <listado>-entry_d IS INITIAL AND <listado>-lights <> icon_change.
            SELECT * FROM t100
              INTO @DATA(l_t100)
              UP TO 1 ROWS
              WHERE sprsl = @<listado>-lango
                AND text     = @<listado>-entry
              ORDER BY PRIMARY KEY.
            ENDSELECT.
            IF sy-subrc = 0.
              SELECT SINGLE text FROM t100
                INTO <listado>-entry_d
                WHERE sprsl = <listado>-langd
                  AND arbgb = l_t100-arbgb
                  AND msgnr = l_t100-msgnr.
              IF <listado>-entry_d <> ''.
                <listado>-lights = icon_change.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    o_alv->refrescar_grid( ).
  ENDMETHOD.
ENDCLASS.

" -----------------------------------------------------------------------
" INITIALIZATION
" -----------------------------------------------------------------------

INITIALIZATION.
  o_prog = NEW #( status       = 'INICIO_DYN'
                  status_prog  = 'ZAP_STATUS'
                  no_param     = 'X'
                  guardar_logz = '' ).

  PERFORM add_button IN PROGRAM zap_status
          USING 'M01'
                'Log'(log)
                ''
                ''.

  IF sy-batch IS INITIAL.
    o_prog->o_event = NEW #( boton_refrescar = 'X'
                             boton_excel     = 'Y'
                             o_prog          = o_prog ).

    o_prog->o_alv = NEW #( estructura = ''
                           o_event    = o_prog->o_event ).

    p_vari = o_prog->o_alv->get_default_layout( ).
  ENDIF.

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  p_vari = o_prog->o_alv->get_f4_layout( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fiche.
  p_fiche = zcl_ap_ficheros=>popup_select_fichero( file_filter = zcl_ap_ficheros=>c_filtro_xlsx ).

  " -----------------------------------------------------------------------
  " AT SELECTION-SCREEN.
  " -----------------------------------------------------------------------

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'ONLI'.
      IF p_prog = 'X' AND s_name[] IS INITIAL.
        MESSAGE 'Informe selección de programas'(isp) TYPE 'E'.
      ELSEIF p_dtel = 'X' AND s_dtel[] IS INITIAL.
        MESSAGE 'Informe selección de elementos de datos'(ise) TYPE 'E'.
      ENDIF.

      o_prog->validar_seleccion_obligatoria( campos_or = '*'
                                             msgty     = 'W' ).
    WHEN OTHERS.
      o_prog->at_selection( ).
  ENDCASE.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  o_prog->at_selection( ).

  " ----------------------------------------------------------------------
  " START-OF-SELECTION.
  " -----------------------------------------------------------------------

START-OF-SELECTION.
  o_prog->main( ).

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.

  o_prog->status_dynpro_0100( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  o_prog->command_dynpro_0100( ).


ENDMODULE.