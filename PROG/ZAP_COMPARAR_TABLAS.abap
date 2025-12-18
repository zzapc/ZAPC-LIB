***********************************************************************
* TIPO : LISTADO
* TITULO : ??
* DESCRIPCION : Adaptación report UGMD_TABLE_DELTA_TO_TRANSPORT
*
* AUTOR: Andrés Picazo                                FECHA: 04/03/2019
* ANALISTA: ??
*
* Doc. Tecnica: http://sap4.com/tareas?=&cliente=CCC&objeto=OOO
**->MANUAL:,http://sap4.com
**->PLANTILLA:,http://sap4.com
*
* MODIFICACIONES
* 18/04/2017 Tarea inicial: http://sap4.com/tareas?&ntask=TTT
*
***********************************************************************
REPORT zap_comparar_tablas.


*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: dd03l.


*------TABLAS INTERNAS-------------------------------------------------*
TYPES: BEGIN OF t_base,
         check      TYPE xfeld,
         lights     TYPE zico_estado_mensaje,
         tabla      TYPE tabname,
         key        TYPE string,
         sistema    TYPE string,
         campos_dif TYPE string,
         message    TYPE bapi_msg,
         color      TYPE lvc_t_scol,
       END OF t_base.
__data_set_var base.


*------VARIABLES-------------------------------------------------------*
FIELD-SYMBOLS: <i_listado> TYPE table,
               <lineal>    TYPE any.
DATA: i_listado TYPE REF TO data,
      lineal    TYPE REF TO data.

*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*

CLASS lcl_event_grid DEFINITION INHERITING FROM zcl_ap_alv_grid_eventos FINAL.
  PUBLIC SECTION.
    METHODS: data_changed REDEFINITION,
      data_changed_finished REDEFINITION,
      toolbar      REDEFINITION,
      user_command REDEFINITION.
ENDCLASS.                    "lcl_event_grid DEFINITION

*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev FINAL.
  PUBLIC SECTION.
    DATA: o_alv            TYPE REF TO zcl_ap_alv_grid,
          o_event          TYPE REF TO lcl_event_grid,
          i_campos_alv     TYPE lvc_t_fcat,
          l_campos_alv     TYPE lvc_s_fcat,
          num_tablas       TYPE i,
          no_key,
          campos_excluidos,
          key_len          TYPE i,
          lt_dfies_s       TYPE STANDARD TABLE OF dfies.

    METHODS:  buscar_datos REDEFINITION,
      comparar_tablas IMPORTING p_table  TYPE tabname
                                crear_ot TYPE abap_bool DEFAULT '',
      status_dynpro_0100,
      command_dynpro_0100.
  PRIVATE SECTION.

    METHODS get_key
      IMPORTING
        datos      TYPE any
      RETURNING
        VALUE(key) TYPE string.

ENDCLASS.                    "REPORT DEFINITION

DATA: o_prog  TYPE REF TO zcl_report.                       "#EC NEEDED

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME.
PARAMETERS: p_target TYPE rfcdest OBLIGATORY,
            p_source TYPE rfcdest DEFAULT space             "H1489976
                                        NO-DISPLAY.
SELECT-OPTIONS: s_table  FOR dd03l-tabname OBLIGATORY,
                s_field  FOR dd03l-fieldname.
SELECTION-SCREEN SKIP.
PARAMETERS: p_soldif AS CHECKBOX DEFAULT 'X',
            p_novac  AS CHECKBOX DEFAULT 'X',
            p_ignerr AS CHECKBOX,
            p_fstd   AS CHECKBOX DEFAULT '' USER-COMMAND fuz.
SELECTION-SCREEN SKIP.
PARAMETERS: p_clave  TYPE text255 MODIF ID fuz,
            p_clave2 TYPE text255 MODIF ID fuz.
SELECTION-SCREEN SKIP.
PARAMETERS: p_where1 TYPE text255 MODIF ID whe,
            p_where2 TYPE text255 MODIF ID whe,
            p_where3 TYPE text255 MODIF ID whe,
            p_where4 TYPE text255 MODIF ID whe,
            p_where5 TYPE text255 MODIF ID whe.
SELECTION-SCREEN END OF BLOCK b01.
__botones_plantilla.

************************************************************************
*
* LOGICA DEL PROGRAMA
*
************************************************************************
CLASS lcl_event_grid IMPLEMENTATION.

  METHOD toolbar.

    super->toolbar( e_object = e_object e_interactive = e_interactive ).

  ENDMETHOD.                                               "toolbar

  METHOD user_command.

    CASE e_ucomm.
      WHEN OTHERS.
        super->user_command( e_ucomm = e_ucomm ).
    ENDCASE.

  ENDMETHOD.                                               "USER_COMMAND


  METHOD data_changed.

  ENDMETHOD.                                               "data_changed

  METHOD data_changed_finished.

    IF NOT tabla_data_changed IS INITIAL.
      o_alv->refrescar_grid( ).
      CLEAR tabla_data_changed.
    ENDIF.

  ENDMETHOD.

ENDCLASS.                    "lcl_event_grid IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS zcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report IMPLEMENTATION.

  METHOD buscar_datos.

    i_campos_alv  = zcl_ap_dev=>get_fieldcatalog_tabla_alv( i_base ).

    CLEAR l_campos_alv.
    l_campos_alv-fieldname = 'COLOR'.
    l_campos_alv-ref_table = 'CALENDAR_TYPE'.
    l_campos_alv-ref_field = 'COLTAB'.

    APPEND l_campos_alv TO i_campos_alv.

    SELECT tabname FROM dd02l
    INTO TABLE @DATA(i_tablas)
    WHERE tabname IN @s_table.

    num_tablas = lines( i_tablas ).

    LOOP AT i_tablas ASSIGNING FIELD-SYMBOL(<tabla>).
      DATA(i_campos) = zcl_ap_dev=>get_fieldcatalog_tabla( <tabla>-tabname ).
      LOOP AT i_campos ASSIGNING FIELD-SYMBOL(<campos>) WHERE fieldname IN s_field.
        IF NOT line_exists( i_campos_alv[ fieldname = <campos>-fieldname ] ).
          IF <campos>-inttype = 'y'.
            campos_excluidos = 'X'.
          ELSE.
            CLEAR l_campos_alv.
            MOVE-CORRESPONDING <campos> TO l_campos_alv.
            APPEND l_campos_alv TO i_campos_alv.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = i_campos_alv
      IMPORTING
        ep_table                  = i_listado
      EXCEPTIONS
        generate_subpool_dir_full = 1.
    IF sy-subrc = 0.
      ASSIGN i_listado->* TO <i_listado>.
      CLEAR <i_listado>.
    ENDIF.

    LOOP AT i_tablas ASSIGNING <tabla>.
      comparar_tablas( <tabla>-tabname ).
    ENDLOOP.

  ENDMETHOD.                                               "seleccionar_datos

  METHOD status_dynpro_0100.


    status_dynpro( EXPORTING cprog = 'ZAP_STATUS' status = 'ST_DYN' CHANGING i_listado = <i_listado> ).
    IF inicio IS INITIAL.
      inicio = 'X'.
      IF sy-sysid = zcl_c=>entorno_desarrollo AND no_key IS INITIAL.
        o_alv->add_button( button = 'F01' text = 'Crear OT'  icon = icon_execute_object ).
      ENDIF.
      IF s_field[] IS INITIAL AND campos_excluidos IS INITIAL.
        o_alv->add_button( button = 'F02' text = 'Copiar a sistema local'  icon = icon_system_copy ).
      ENDIF.
      IF no_key IS INITIAL.
        o_alv->add_button( button = 'F03' text = 'Borrar de sistema local'  icon = icon_delete ).
      ENDIF.
      o_alv->registrar_mod( ).
      o_alv->set_layout( no_rowmove = 'X' no_rowins = 'X' style = 'STYLE' colort = 'COLOR' ).
      o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_refresh ).
      o_alv->set_campos_tabint( <i_listado> ).
      o_alv->set_field_quitar( 'CHECK' ).
      o_alv->set_orden( 'TABLA,KEY,SISTEMA' ).
      o_alv->set_field_noout( 'MESSAGE,KEY,CAMPOS_DIF' ).
      IF line_exists( i_campos_alv[ fieldname = 'MANDT' ] ).
        o_alv->set_field_noout( 'MANDT' ).
      ENDIF.
      IF line_exists( i_campos_alv[ fieldname = 'DOCUMENTACION' ] ).
        o_alv->set_field_hotspot( campo = 'DOCUMENTACION' valor = 'TEXT' ).
      ENDIF.


      LOOP AT i_campos_alv INTO l_campos_alv WHERE fieldname NE 'COLOR'.
        o_alv->set_field_text( campo = l_campos_alv-fieldname valor = l_campos_alv-reptext ).
      ENDLOOP.

      o_alv->ocultar_columnas_vacias( <i_listado> ).

      sgpi_texto( 'Generando informe' ).
      o_alv->show( CHANGING tabla = <i_listado> ).
    ENDIF.

  ENDMETHOD.


  METHOD command_dynpro_0100.
    DATA: l_hay_sel,
          l_comm_sel TYPE string VALUE 'F01,F02,F03'.

    command_dynpro( EXPORTING o_alv = o_alv seleccion = l_comm_sel
                            CHANGING i_listado = <i_listado> i_listado_ini = <i_listado> hay_sel = l_hay_sel ).

    IF zcl_ap_lista=>es_elemento( lista = l_comm_sel elemento = ucomm ).
      IF l_hay_sel IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    CASE ucomm.
      WHEN 'F01'.
        DATA: ls_e071  TYPE e071,
              lt_e071  TYPE tr_objects,
              ls_e071k TYPE e071k,
              lt_e071k TYPE tr_keys,
              l_tabla  TYPE tabname.
        FIELD-SYMBOLS: <tabla>   TYPE any,
                       <key>     TYPE any,
                       <check>   TYPE any,
                       <sistema> TYPE any,
                       <lineal>  TYPE any,
                       <linea>   TYPE any.

        LOOP AT <i_listado> ASSIGNING FIELD-SYMBOL(<list>).
          ASSIGN COMPONENT 'CHECK' OF STRUCTURE <list> TO <check>.
          IF <check> = 'X'.
            ASSIGN COMPONENT 'SISTEMA' OF STRUCTURE <list> TO <sistema>.
            IF <sistema> = 'Local'.
              ASSIGN COMPONENT 'TABLA' OF STRUCTURE <list> TO <tabla>.
              ASSIGN COMPONENT 'KEY' OF STRUCTURE <list> TO <key>.
              IF <tabla> NE l_tabla.
                l_tabla = <tabla>.
                ls_e071-as4pos   = 1.
                ls_e071-pgmid    = 'R3TR'.
                ls_e071-object   = 'TABU'.
                ls_e071-obj_name = <tabla>.
                ls_e071-objfunc  = 'K'.
                ls_e071-lang     = sy-langu.
                INSERT ls_e071 INTO TABLE lt_e071.
              ENDIF.
              ls_e071k-pgmid      = 'R3TR'.
              ls_e071k-object     = 'TABU'.
              ls_e071k-objname    = <tabla>.
              ls_e071k-mastertype = 'TABU'.
              ls_e071k-mastername = <tabla>.
              ls_e071k-lang       = sy-langu.

              ls_e071k-tabkey = <key>.

              INSERT ls_e071k INTO TABLE lt_e071k.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF lt_e071k[] IS INITIAL.
          MESSAGE 'No ha seleccionado ninguna entrada local' TYPE 'I'.
        ELSE.
          CALL FUNCTION 'TR_REQUEST_CHOICE'
            EXPORTING
*             IV_REQUEST_TYPES     =
*             IV_CLI_DEP           = ' '
*             IV_REQUEST           = ' '
              it_e071  = lt_e071
              it_e071k = lt_e071k
*             IV_LOCK_OBJECTS      = ' '
*             IV_NO_OWNER_CHECK    = ' '
            EXCEPTIONS
              OTHERS   = 1.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      WHEN 'F02'.
        DATA tabla TYPE REF TO data.
        CLEAR cont.
        IF sy-sysid = zcl_c=>entorno_produccion.
          IF zcl_ap_popup=>confirmar( texto = '¿Está seguro de querer modificar registros en producción?' ) = ''.
            RETURN.
          ENDIF.
        ENDIF.
        LOOP AT <i_listado> ASSIGNING <list>.
          ASSIGN COMPONENT 'CHECK' OF STRUCTURE <list> TO <check>.
          IF <check> = 'X'.
            ASSIGN COMPONENT 'SISTEMA' OF STRUCTURE <list> TO <sistema>.
            IF <sistema> = 'Remoto'.
              ASSIGN COMPONENT 'TABLA' OF STRUCTURE <list> TO <tabla>.

              CREATE DATA tabla TYPE (<tabla>).
              ASSIGN tabla->* TO <lineal>.
              MOVE-CORRESPONDING <list> TO <lineal>.
              MODIFY (<tabla>) FROM <lineal>.
              IF sy-subrc NE 0.
                MESSAGE |Error modificando { <tabla> } { <key> }| TYPE 'E'.
              ELSE.
                ADD 1 TO cont.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF cont = 0.
          MESSAGE 'No se ha modificado ningún registro. Debe seleccionar registros del sistema remoto' TYPE 'I'.
        ELSE.
          MESSAGE |Se han actualizado { cont } registros| TYPE 'S'.
        ENDIF.

      WHEN 'F03'.
        IF sy-sysid = zcl_c=>entorno_produccion.
          IF zcl_ap_popup=>confirmar( texto = '¿Está seguro de querer borrar registros en producción?' ) = ''.
            RETURN.
          ENDIF.
        ENDIF.
        CLEAR cont.
        LOOP AT <i_listado> ASSIGNING <list>.
          ASSIGN COMPONENT 'CHECK' OF STRUCTURE <list> TO <check>.
          IF <check> = 'X'.
            ASSIGN COMPONENT 'SISTEMA' OF STRUCTURE <list> TO <sistema>.
            IF <sistema> = 'Local'.
              ASSIGN COMPONENT 'TABLA' OF STRUCTURE <list> TO <tabla>.

              CREATE DATA tabla TYPE (<tabla>).
              ASSIGN tabla->* TO <lineal>.
              MOVE-CORRESPONDING <list> TO <lineal>.
              DELETE (<tabla>) FROM <lineal>.
              IF sy-subrc NE 0.
                MESSAGE |Error modificando { <tabla> } { <key> }| TYPE 'E'.
              ELSE.
                ADD 1 TO cont.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF cont = 0.
          MESSAGE 'No se ha modificado ningún registro. Debe seleccionar registros del sistema local' TYPE 'I'.
        ELSE.
          MESSAGE |Se han actualizado { cont } registros| TYPE 'S'.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD comparar_tablas.
    DATA: lt_dfies_t  TYPE STANDARD TABLE OF dfies,
          lt_data     TYPE STANDARD TABLE OF tab512,
          lr_s_data   TYPE REF TO data,
          lr_t_data_s TYPE REF TO data,
          lr_t_data_t TYPE REF TO data,
          lr_t_data_p TYPE REF TO data,
          lr_t_data_m TYPE REF TO data,
          lt_keyfield TYPE ugmd_t_fieldname,
          l_keylen    TYPE i,
          l_i         TYPE i,
          lt_option   TYPE TABLE OF rfc_db_opt,             "H1489976
          ls_field    TYPE rfc_db_fld,
          lt_field    TYPE TABLE OF rfc_db_fld,
          l_message   TYPE bapi_msg,
          where       TYPE string,
          clave1      TYPE string,
          clave2      TYPE string.

    FIELD-SYMBOLS: <ls_data>    TYPE any,
                   <lt_data_s>  TYPE ANY TABLE,
                   <lt_data_t>  TYPE ANY TABLE,
                   <lt_data_p>  TYPE ANY TABLE,
                   <lt_data_m>  TYPE ANY TABLE,
                   <any>        TYPE any,
                   <ls_dfies>   TYPE dfies,
                   <fs>         TYPE any,
                   <sistema>    TYPE any,
                   <tabla>      TYPE any,
                   <key>        TYPE any,
                   <lights>     TYPE any,
                   <campos_dif> TYPE any.

    IF NOT p_where1 IS INITIAL OR NOT p_where2 IS INITIAL.
      CONCATENATE p_where1 p_where2 p_where3 p_where4 p_where5 INTO where SEPARATED BY space.
    ENDIF.

    DATA(l_dest) = SWITCH  string( sy-sysid WHEN zcl_c=>entorno_desarrollo THEN  'CLAVEP' ELSE 'CLAVED' ).
    string = zcl_ap_temp=>get_st_valor1( clave = 'ZAP_OTS' subclave = sy-uname && l_dest ).
    IF NOT string IS INITIAL.
      zcl_ap_string=>to_clipboard( string = string ).
      MESSAGE |Se copia clave en portapapeles| TYPE 'S'.
    ENDIF.

    CLEAR sy-msgid.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      DESTINATION p_target
      EXPORTING
        tabname        = p_table
      TABLES
        dfies_tab      = lt_dfies_t
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      IF sy-msgty IS INITIAL.
        LEAVE LIST-PROCESSING.
      ELSE.
        IF sy-msgid IS INITIAL.
          MESSAGE |Error conectando con destino { p_target }| TYPE 'E'.
        ELSE.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = p_table
      TABLES
        dfies_tab      = lt_dfies_s
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lt_dfies_t[] NE lt_dfies_s[].
      DATA(lt_dfies_t_copy) = lt_dfies_t.
      DATA(lt_dfies_s_copy) = lt_dfies_s.
      LOOP AT lt_dfies_t_copy ASSIGNING FIELD-SYMBOL(<copy_t>).
        READ TABLE lt_dfies_s_copy ASSIGNING FIELD-SYMBOL(<copy_s>) WITH KEY fieldname = <copy_t>-fieldname.
        IF sy-subrc = 0.
          IF <copy_s>-fieldname = <copy_t>-fieldname AND <copy_s>-offset = <copy_t>-offset.
            DELETE lt_dfies_s_copy INDEX sy-tabix.
            DELETE lt_dfies_t_copy.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF NOT lt_dfies_s_copy IS INITIAL OR NOT lt_dfies_t_copy IS INITIAL.
        MESSAGE 'Los campos en sistema origen y destino no coinciden!' TYPE 'I'.
        IF p_ignerr IS INITIAL.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

* get the data
    LOOP AT lt_dfies_s ASSIGNING <ls_dfies>.
      IF <ls_dfies>-keyflag NE space.
        INSERT <ls_dfies>-fieldname INTO TABLE lt_keyfield.
        ADD <ls_dfies>-leng TO l_keylen.
        IF NOT <ls_dfies>-fieldname IN s_field.
          no_key = 'X'.
        ENDIF.
      ENDIF.
      l_i = <ls_dfies>-offset + <ls_dfies>-leng.
      IF l_i GT 512.
        EXIT.
      ENDIF.
      ls_field-fieldname = <ls_dfies>-fieldname.
      ls_field-offset    = <ls_dfies>-offset.
      ls_field-length    = <ls_dfies>-leng.
*  ls_field-TYPE      = <ls_dfies>-DATATYPE.
      ls_field-type      = <ls_dfies>-inttype.
      ls_field-fieldtext = <ls_dfies>-fieldtext.
      INSERT ls_field INTO TABLE lt_field.
    ENDLOOP.

    key_len = l_keylen.

    CREATE DATA lr_t_data_s TYPE SORTED TABLE OF (p_table)
                            WITH UNIQUE KEY (lt_keyfield).
    ASSIGN lr_t_data_s->* TO <lt_data_s>.
    CREATE DATA lr_t_data_t LIKE <lt_data_s>.
    ASSIGN lr_t_data_t->* TO <lt_data_t>.
    CREATE DATA lr_s_data LIKE LINE OF <lt_data_s>.
    ASSIGN lr_s_data->* TO <ls_data>.

    CREATE DATA lineal LIKE LINE OF <i_listado>.
    ASSIGN lineal->* TO <lineal>.


    sgpi_texto( 'Seleccionando datos del entorno actual' ).
    IF p_fstd = 'X'.
      zcl_ap_string=>string2tabla( EXPORTING string = where longitud = 72
                                   CHANGING tabla = lt_option[] ).
      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION p_source
        EXPORTING
          query_table          = p_table
*         NO_DATA              = ' '
*         ROWSKIPS             = 0
*         ROWCOUNT             = 0
        TABLES
          options              = lt_option
          fields               = lt_field
          data                 = lt_data
        EXCEPTIONS
          table_not_available  = 1
          table_without_data   = 2
          option_not_valid     = 3
          field_not_valid      = 4
          not_authorized       = 5
          data_buffer_exceeded = 6
          OTHERS               = 7.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        EXIT.
      ENDIF.

      LOOP AT lt_data ASSIGNING <any>.
        CLEAR <ls_data>.
        <ls_data> = <any>.
        INSERT <ls_data> INTO TABLE <lt_data_s>.

        MOVE-CORRESPONDING <ls_data> TO <lineal>.
        ASSIGN COMPONENT 'SISTEMA' OF STRUCTURE <lineal> TO <sistema>.
        <sistema> = 'Local'.
        ASSIGN COMPONENT 'KEY' OF STRUCTURE <lineal> TO <key>.
        <key> = get_key( <ls_data> ).
        ASSIGN COMPONENT 'TABLA' OF STRUCTURE <lineal> TO <tabla>.
        <tabla> = p_table.
        APPEND <lineal> TO <i_listado>.
      ENDLOOP.

    ELSE.
      clave1 = p_clave.
      clave2 = p_clave2.
      DATA l_cont TYPE xstring.
      CALL FUNCTION 'Z_RFC_SET_TABLA'
        EXPORTING
          tabla     = p_table
          clave     = clave1
          clave2    = clave2
          where     = where
        IMPORTING
          contenido = l_cont
          message   = l_message.

      CALL TRANSFORMATION id
        SOURCE XML l_cont
        RESULT <g_wa_tabla> = <lt_data_s>.

      IF NOT l_message IS INITIAL.
        MESSAGE l_message TYPE 'E'.
      ENDIF.

      LOOP AT <lt_data_s> ASSIGNING <ls_data>.
        MOVE-CORRESPONDING <ls_data> TO <lineal>.
        ASSIGN COMPONENT 'SISTEMA' OF STRUCTURE <lineal> TO <sistema>.
        <sistema> = 'Local'.
        ASSIGN COMPONENT 'KEY' OF STRUCTURE <lineal> TO <key>.
        <key> = get_key( <ls_data> ).


        ASSIGN COMPONENT 'TABLA' OF STRUCTURE <lineal> TO <tabla>.
        <tabla> = p_table.
        APPEND <lineal> TO <i_listado>.
      ENDLOOP.
    ENDIF.

    sgpi_texto( 'Seleccionando datos del entorno destino' ).
    REFRESH lt_data.
    IF p_fstd = 'X'.
      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION p_target
        EXPORTING
          query_table          = p_table
*         NO_DATA              = ' '
*         ROWSKIPS             = 0
*         ROWCOUNT             = 0
        TABLES
          options              = lt_option                    "H1489976
          fields               = lt_field
          data                 = lt_data
        EXCEPTIONS
          table_not_available  = 1
          table_without_data   = 2
          option_not_valid     = 3
          field_not_valid      = 4
          not_authorized       = 5
          data_buffer_exceeded = 6
          OTHERS               = 7.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        EXIT.
      ENDIF.
      LOOP AT lt_data ASSIGNING <any>.
        CLEAR <ls_data>.
        <ls_data> = <any>.
        INSERT <ls_data> INTO TABLE <lt_data_t>.

        MOVE-CORRESPONDING <ls_data> TO <lineal>.
        ASSIGN COMPONENT 'SISTEMA' OF STRUCTURE <lineal> TO <sistema>.
        <sistema> = 'Remoto'.
        ASSIGN COMPONENT 'KEY' OF STRUCTURE <lineal> TO <key>.
        <key> = get_key( <ls_data> ).

        ASSIGN COMPONENT 'TABLA' OF STRUCTURE <lineal> TO <tabla>.
        <tabla> = p_table.
        APPEND <lineal> TO <i_listado>.
      ENDLOOP.

    ELSE.
      CALL FUNCTION 'Z_RFC_SET_TABLA'
        DESTINATION p_target
        EXPORTING
          tabla     = p_table
          clave     = clave1
          clave2    = clave2
          where     = where
        IMPORTING
          contenido = l_cont
          message   = l_message.

      CALL TRANSFORMATION id
        SOURCE XML l_cont
        RESULT <g_wa_tabla> = <lt_data_t>.

      IF NOT l_message IS INITIAL.
        MESSAGE l_message TYPE 'E'.
      ENDIF.

      LOOP AT <lt_data_t> ASSIGNING <ls_data>.
        TRY.
            MOVE-CORRESPONDING <ls_data> TO <lineal>.
          CATCH cx_root into data(o_root).
            message |Error moviendo datos { o_root->get_text( ) }| type 'S'.
        ENDTRY.
        ASSIGN COMPONENT 'SISTEMA' OF STRUCTURE <lineal> TO <sistema>.
        <sistema> = 'Remoto'.
        ASSIGN COMPONENT 'KEY' OF STRUCTURE <lineal> TO <key>.
        <key> = get_key( <ls_data> ).
        ASSIGN COMPONENT 'TABLA' OF STRUCTURE <lineal> TO <tabla>.
        <tabla> = p_table.
        APPEND <lineal> TO <i_listado>.
      ENDLOOP.
    ENDIF.



* compare the data
    CREATE DATA lr_t_data_p LIKE <lt_data_s>.
    ASSIGN lr_t_data_p->* TO <lt_data_p>.
    CREATE DATA lr_t_data_m LIKE <lt_data_s>.
    ASSIGN lr_t_data_m->* TO <lt_data_m>.
* data missing in source system
    LOOP AT <lt_data_t> ASSIGNING <ls_data>.
      READ TABLE <lt_data_s> FROM <ls_data> TRANSPORTING NO FIELDS.
      IF sy-subrc NE space.
        INSERT <ls_data> INTO TABLE <lt_data_p>.
      ENDIF.
    ENDLOOP.
* data missing in target system
    LOOP AT <lt_data_s> ASSIGNING <ls_data>.
      READ TABLE <lt_data_t> FROM <ls_data> TRANSPORTING NO FIELDS.
      IF sy-subrc NE space.
        INSERT <ls_data> INTO TABLE <lt_data_m>.
      ENDIF.
    ENDLOOP.


    ASSIGN lineal->* TO <lineal>.
    LOOP AT <i_listado> ASSIGNING FIELD-SYMBOL(<list>).

      ASSIGN COMPONENT 'TABLA' OF STRUCTURE <list> TO <tabla>.
      IF <tabla> = p_table.
        ASSIGN COMPONENT 'KEY' OF STRUCTURE <list> TO <key>.
        ASSIGN COMPONENT 'SISTEMA' OF STRUCTURE <list> TO <sistema>.
        ASSIGN COMPONENT 'LIGHTS' OF STRUCTURE <list> TO <lights>.
        ASSIGN COMPONENT 'CAMPOS_DIF' OF STRUCTURE <list> TO <campos_dif>.
        IF <sistema> = 'Local'.
          LOOP AT <lt_data_m> ASSIGNING <ls_data>.
            IF <key> = get_key( <ls_data> ).
              set_status_list( EXPORTING message = 'Registro no existe en sistema destino' icono = icon_delete color = 'N' int = 1 CHANGING list = <list> ).
            ENDIF.
          ENDLOOP.
        ELSE.
          LOOP AT <lt_data_p> ASSIGNING <ls_data>.
            IF <key> = get_key( <ls_data> ).
              set_status_list( EXPORTING message = 'Registro no existe en sistema local' icono = icon_create color = 'N' CHANGING list = <list> ).
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF <lights> IS INITIAL.
          UNASSIGN: <ls_data>.
          DATA(l_found) = ''.
          IF <sistema> = 'Local'.
            LOOP AT <lt_data_t> ASSIGNING <ls_data>.
              IF <key> = get_key( <ls_data> ).
                l_found = 'X'.
                EXIT.
              ENDIF.
            ENDLOOP.
          ELSE.
            LOOP AT <lt_data_s> ASSIGNING <ls_data>.
              IF <key> = get_key( <ls_data> ).
                l_found = 'X'.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.
          IF l_found = 'X'.
            MOVE-CORRESPONDING <ls_data> TO <lineal>.
            LOOP AT lt_dfies_s ASSIGNING <ls_dfies> WHERE fieldname IN s_field.
              ASSIGN COMPONENT <ls_dfies>-fieldname OF STRUCTURE <list> TO <fs>.
              ASSIGN COMPONENT <ls_dfies>-fieldname OF STRUCTURE <lineal> TO <any>.
              IF <fs> NE <any>.
                __add_lista <campos_dif> <ls_dfies>-fieldname.
              ENDIF.
            ENDLOOP.
            IF NOT <campos_dif> IS INITIAL.
              set_status_list( EXPORTING message = 'Existen diferencias en campos' icono = icon_reject campos_color = <campos_dif> color = 'R' CHANGING list = <list> ).
            ENDIF.
          ELSE.
            <lights> = 'Error buscando'.
          ENDIF.
        ENDIF.

        IF p_soldif = 'X' AND <lights> IS INITIAL.
          DELETE <i_listado>.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_key.
    DATA l_text TYPE text255.
    FIELD-SYMBOLS <any> TYPE any.

    TRY.
        key = datos(key_len).
      CATCH  cx_sy_range_out_of_bounds.
        MESSAGE 'Imposible recuperar indice' TYPE 'E'.
      CATCH cx_sy_offset_not_allowed.
        CLEAR: cont, l_text.
        LOOP AT lt_dfies_s ASSIGNING FIELD-SYMBOL(<ls_dfies>) WHERE keyflag = 'X'.
          ASSIGN COMPONENT <ls_dfies>-fieldname OF STRUCTURE  datos TO <any>.
          l_text+cont(<ls_dfies>-leng) = <any>.
          ADD <ls_dfies>-leng TO cont.
        ENDLOOP.
        key = l_text.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.                    "REPORT IMPLEMENTATION
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  o_prog = NEW #( status       = 'INICIO'
                  guardar_logz = 'X'
                  status_prog  = 'ZAP_STATUS' ).

  IF sy-sysid = zcl_c=>entorno_produccion.
    p_target = zcl_c=>rfc_desarrollo.
  ELSE.
    p_target = zcl_c=>rfc_produccion.
  ENDIF.

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).

  IF sy-batch IS INITIAL.
    o_prog->o_event = NEW #( boton_refrescar = 'X'
                             boton_excel     = 'Y'
                             o_prog          = o_prog ).

    o_prog->o_alv = NEW #( estructura = ''
                           o_event    = o_prog->o_event ).

  ENDIF.



************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'ONLI'.
      o_prog->validar_seleccion_obligatoria( campos_or = '*' msgty = 'W' ).
    WHEN OTHERS.
      o_prog->at_selection( ).
  ENDCASE.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  o_prog->at_selection( ).

AT SELECTION-SCREEN OUTPUT.
  zcl_ap_dynpro=>screen_visible( group1 = 'FUZ' variable = p_fstd variable_inv = 'X' ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  o_prog->buscar_datos( ).

  IF sy-batch IS INITIAL.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE 'Este programa no se puede ejecutar en fondo'(pnf) TYPE 'E'.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.

  o_prog->status_dynpro_0100( ).

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  o_prog->command_dynpro_0100( ).


ENDMODULE.                 " USER_COMMAND_0100  INPUT
