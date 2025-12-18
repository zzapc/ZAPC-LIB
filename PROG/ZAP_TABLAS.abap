***********************************************************************
* TIPO : LISTADO
* TITULO : Gestión tablas AP
*
* AUTOR: Andrés Picazo                                FECHA: 01/12/2016
*
***********************************************************************
REPORT zap_tablas.

*------INCLUDES--------------------------------------------------------*

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: ztemp, rsrd1, tadir.

*------TABLAS INTERNAS-------------------------------------------------*
TYPES: BEGIN OF t_ztemp,
         check.
    INCLUDE TYPE ztemp.
TYPES  END OF t_ztemp.
__data_set_var ztemp.

TYPES: BEGIN OF t_ztemps,
         check.
    INCLUDE TYPE ztemps.
TYPES  END OF t_ztemps.
__data_set_var ztemps.

TYPES: BEGIN OF t_zcache,
         check,
         color TYPE lvc_t_scol.
    INCLUDE TYPE zcache.
TYPES  END OF t_zcache.
__data_set_var zcache.

TYPES: BEGIN OF t_cont,
         check,
         tabname TYPE dd02l-tabname,
         ddtext  TYPE dd02t-ddtext,
         cont    TYPE pay_numrec_alv,
       END OF t_cont.
__data_set_var cont.

DATA: v_tabla TYPE string.

*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM zcl_ap_alv_check.
  PUBLIC SECTION.
    METHODS: handle_double_click REDEFINITION.
    METHODS: handle_user_command REDEFINITION.
ENDCLASS.                    "lcl_alv DEFINITION

*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev.
  PUBLIC SECTION.
    METHODS: main.

    METHODS:  listado,
      seleccionar_datos.

ENDCLASS.                    "REPORT DEFINITION

*------VARIABLES-------------------------------------------------------*
DATA: o_prog TYPE REF TO zcl_report,
      o_alv  TYPE REF TO lcl_alv.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE TEXT-sel.
PARAMETERS: p_temp  RADIOBUTTON GROUP g DEFAULT 'X' USER-COMMAND ggg,
            p_temps RADIOBUTTON GROUP g,
            p_cache RADIOBUTTON GROUP g.
SELECT-OPTIONS: s_clave FOR ztemp-clave,
                s_report FOR sy-cprog MODIF ID zca,
                s_erdat FOR ztemp-erdat.
PARAMETERS: p_cont  RADIOBUTTON GROUP g.
SELECT-OPTIONS: s_tablas FOR rsrd1-tbma_val MODIF ID con,
                s_devcl FOR tadir-devclass MODIF ID con.
PARAMETERS: p_maxreg TYPE i DEFAULT 10000,
            p_mail   TYPE text1024.
SELECTION-SCREEN END OF BLOCK 001.
PARAMETERS: p_defpa DEFAULT 'X' NO-DISPLAY.
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
  METHOD handle_double_click.
    DATA: zcache TYPE zcache.
    CASE v_tabla.
      WHEN 'I_ZCACHE'.
        READ TABLE i_zcache ASSIGNING FIELD-SYMBOL(<cache>) INDEX row.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING <cache> TO zcache.
          zcl_ap_cache=>visualizar( cache = zcache campo = column tipo_visualizacion = 'P' ).
        ENDIF.
    ENDCASE.
  ENDMETHOD. "handle_double_click
  METHOD handle_user_command.
    DATA: l_row TYPE i.

    CASE e_salv_function.
      WHEN 'EXCEL'.
        exportar_excel( ).
      WHEN 'F01'.

        get_seleccion( ).
        refresh( ).
        CASE v_tabla.
          WHEN 'I_ZTEMP'.
            LOOP AT i_ztemp ASSIGNING <ztemp> WHERE check = 'X'.
              DELETE FROM  ztemp
                     WHERE  clave     = <ztemp>-clave
                     AND    subclave  = <ztemp>-subclave
                     AND    indice    = <ztemp>-indice.
              DELETE i_ztemp.
            ENDLOOP.
          WHEN 'I_ZTEMPS'.
            LOOP AT i_ztemps ASSIGNING <ztemps> WHERE check = 'X'.
              DELETE FROM  ztemps
                     WHERE  clave     = <ztemps>-clave
                     AND    subclave  = <ztemps>-subclave
                     AND    indice    = <ztemps>-indice.
              DELETE i_ztemps.
            ENDLOOP.
          WHEN 'I_ZCACHE'.
            LOOP AT i_zcache ASSIGNING <zcache> WHERE check = 'X'.
              DELETE FROM  zcache
                     WHERE  report    = <zcache>-report
                     AND    clave     = <zcache>-clave
                     AND    subclave  = <zcache>-subclave.

              DELETE i_zcache.
            ENDLOOP.
        ENDCASE.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD. "handle_USER_COMMAND
ENDCLASS. "lcl_alv IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS zcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report IMPLEMENTATION.
  METHOD main.

    IF p_temp = 'X'.
      v_tabla = 'I_ZTEMP'.
    ELSEIF p_temps = 'X'.
      v_tabla = 'I_ZTEMPS'.
    ELSEIF p_cache = 'X'.
      v_tabla = 'I_ZCACHE'.
    ELSEIF p_cont = 'X'.
      v_tabla = 'I_CONT'.
    ENDIF.

    seleccionar_datos( ).
    listado( ).
  ENDMETHOD.                    "REPORT

  METHOD seleccionar_datos.
    __data_set_vart tadir.
    DATA zcache TYPE zcache.

    sgpi_texto( 'Seleccionando datos'(SD1) ).

    IF p_temp = 'X'.
      SELECT * FROM ztemp
        INTO CORRESPONDING FIELDS OF TABLE i_ztemp
       WHERE clave IN s_clave
         AND erdat IN s_erdat.
    ELSEIF p_temps = 'X'.
      SELECT * FROM ztemps
        INTO CORRESPONDING FIELDS OF TABLE i_ztemps
       WHERE clave IN s_clave
         AND erdat IN s_erdat.
    ELSEIF p_cache = 'X'.
      SELECT * FROM zcache                              "#EC CI_GENBUFF
        INTO CORRESPONDING FIELDS OF TABLE i_zcache
       WHERE report IN s_report
         AND clave IN s_clave
         AND fecha IN s_erdat.
      LOOP AT i_zcache ASSIGNING FIELD-SYMBOL(<cache>).
        IF NOT <cache>-variables IS INITIAL.
          MOVE-CORRESPONDING <cache> TO zcache.
          IF zcl_ap_cache=>visualizar( cache = zcache campo = 'VARIABLES' tipo_visualizacion = 'C' ).
            zcl_ap_alv_grid=>append_color( EXPORTING campo = 'VARIABLES' colorc = 'V' CHANGING tabla_color = <cache>-color ).
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSEIF p_cont = 'X'.
      CHECK NOT s_tablas[] IS INITIAL OR NOT s_devcl[] IS INITIAL.
      CLEAR l_cont.
      SELECT obj_name FROM tadir                        "#EC CI_GENBUFF
        INTO CORRESPONDING FIELDS OF TABLE i_tadir
       WHERE pgmid = 'R3TR'
         AND object = 'TABL'
         AND obj_name IN s_tablas
         AND devclass IN s_devcl.
      IF sy-subrc = 0.
        LOOP AT i_tadir INTO l_tadir.
          CLEAR l_cont.

          SELECT tabname FROM dd02l
            INTO l_cont-tabname
            UP TO 1 ROWS
           WHERE tabname = l_tadir-obj_name
             AND tabclass = 'TRANSP'
           ORDER BY PRIMARY KEY.
          ENDSELECT.
          IF sy-subrc = 0.
            SELECT ddtext FROM dd02t
              INTO l_cont-ddtext
              UP TO 1 ROWS
             WHERE tabname = l_cont-tabname
             ORDER BY PRIMARY KEY.
            ENDSELECT.

            SELECT COUNT( * ) FROM (l_cont-tabname)
              INTO l_cont-cont.
            APPEND l_cont TO i_cont.

            cont = l_cont-cont.
            IF l_cont-tabname(4) = 'ZLOG' OR l_cont-tabname = 'ZESTADISTICAS' or l_cont-tabname = 'ZAP_MAIL_LOG'.
              cont  = l_cont-cont / 200. "La tabla de LOG o estadísticas, es normal que se llene mas.
            elseIF l_cont-tabname(5) = 'ZTEMP'.
              cont  = l_cont-cont / 10. "La tabla de LOG o estadísticas, es normal que se llene mas.
            ELSEIF l_cont-tabname = 'ZCACHE'.
              cont  = l_cont-cont * 10. "La tabla de cache no queremos que crezca demasiado.
            ENDIF.

            IF l_cont-tabname = 'ZCACHE' AND ( zcl_c=>cliente_tasks = 'GRE' OR zcl_c=>cliente_tasks = 'JGC' ) AND l_cont-cont < 100000.
            ELSEIF l_cont-tabname = 'ZDOCUMENTOS' AND zcl_c=>cliente_tasks = 'FGV' AND l_cont-cont < 900000.
            ELSEIF l_cont-tabname = 'ZTEMP' AND ( zcl_c=>cliente_tasks = 'FGV' OR zcl_c=>cliente_tasks = 'JGC' ) AND l_cont-cont < 100000.
            ELSE.
              IF cont > p_maxreg AND NOT p_mail IS INITIAL.
                string = zcl_ap_utils=>concat( p1 = 'La tabla'(tab) p2 = l_cont-tabname p3 = 'contiene'(con) p4 = l_cont-cont p5 = 'registros en'(reg) p6 = sy-sysid ).
                zcl_ap_envio_mail=>mail( subject = string direccion = p_mail texto = string ).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "seleccionar_datos


  METHOD listado.

    sgpi_texto( 'Generando informe'(GI1) ).

    DATA(l_campo_color) = COND lvc_fname( WHEN p_cache = 'X' THEN 'COLOR' ).

    CREATE OBJECT o_alv
      EXPORTING
        status             = 'STANDARD_ALV_DYN'
        status_prog        = 'ZAP_STATUS'
*       lights             = 'LIGHTS'
        color              = l_campo_color
        top_of_page_auto   = 'X'
        top_of_page_titulo = 'X'
        tabla              = v_tabla.

    o_alv->add_button( button = 'F01' text = 'Borrar'(BOR)  icon = icon_delete ).

    o_alv->set_top_of_page( ).

    o_alv->set_field_noout( 'CHECK' ).

    o_alv->set_titulo_col_sin_nombre( ).

    CASE v_tabla.
      WHEN 'ZTEMP'.
        o_alv->set_seleccion( CHANGING t_tabla = i_ztemp ).
      WHEN 'ZTEMPS'.
        o_alv->set_seleccion( CHANGING t_tabla = i_ztemps ).
      WHEN 'ZCACHE'.
        o_alv->set_seleccion( CHANGING t_tabla = i_zcache ).
    ENDCASE.

    o_alv->show( ).


  ENDMETHOD.                    "

ENDCLASS.                    "REPORT IMPLEMENTATION

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  CREATE OBJECT o_prog
    EXPORTING
      status        = 'INICIO_DYN'
      status_prog   = 'ZAP_STATUS'
      get_nombre_pc = 'X'
      no_param      = 'X'.


  APPEND VALUE #( option = 'EQ' sign = 'I' low = 'ZAPC' ) TO s_devcl.
  APPEND VALUE #( option = 'CP' sign = 'I' low = 'YAPC*' ) TO s_devcl.

  o_prog->initialization_i( EXPORTING get_default_param = p_defpa CHANGING sscrfields = sscrfields ).

  zcl_ap_dynpro=>set_primer_radiobutton( campos = 'P_TEMP,P_TEMPS,P_CACHE' ).

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.
  o_prog->at_selection( ).

AT SELECTION-SCREEN OUTPUT.
  zcl_ap_dynpro=>screen_visible( group1 = 'ZCA' variable = p_cache ).
  zcl_ap_dynpro=>screen_visible( group1 = 'CON' variable = p_cont ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  o_prog->at_selection( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_clave-low.
  DATA: o_popup TYPE REF TO zcl_ap_matchcode_z.

  CREATE OBJECT o_popup
    EXPORTING
      tabname = 'ZTEMP'.

  IF p_cache = 'X'.
    o_popup->add_field( field = 'SUBCLAVE' ).
  ENDIF.
  o_popup->add_field( field = 'CLAVE' selectflag = 'X' ).
  o_popup->add_field( 'ERDAT' ).
  o_popup->add_field( 'VALOR1' ).

  IF p_temp = 'X'.
    SELECT clave COUNT( * ) FROM ztemp
      INTO (ztemp-clave, sy-tfill )
     WHERE clave IN s_clave
       AND erdat IN s_erdat
    GROUP BY clave
    ORDER BY clave.
      ztemp-valor1 = sy-tfill.

      o_popup->add_valor( ztemp-clave ).
      SELECT MAX( erdat ) FROM ztemp
        INTO ztemp-erdat
       WHERE clave = ztemp-clave.
      o_popup->add_valor( ztemp-erdat ).
      o_popup->add_valor( ztemp-valor1 ).
    ENDSELECT.
  ELSEIF p_temps = 'X'.
    SELECT clave COUNT( * ) FROM ztemps
      INTO (ztemp-clave, sy-tfill )
   WHERE clave IN s_clave
     AND erdat IN s_erdat
      GROUP BY clave
      ORDER BY clave.
      ztemp-valor1 = sy-tfill.

      o_popup->add_valor( ztemp-clave ).
      SELECT MAX( erdat ) FROM ztemps
        INTO ztemp-erdat
       WHERE clave = ztemp-clave.
      o_popup->add_valor( ztemp-erdat ).
      o_popup->add_valor( ztemp-valor1 ).
    ENDSELECT.
  ELSEIF p_cache = 'X'.
    RANGES r_report FOR sy-cprog.
    SELECT report clave COUNT( * ) FROM zcache "#EC CI_BYPASS "#EC CI_GENBUFF
      INTO (ztemp-subclave, ztemp-clave, sy-tfill )
     WHERE report IN r_report
       AND clave IN s_clave
       AND fecha IN s_erdat
      GROUP BY report clave
      ORDER BY report clave.
      ztemp-valor1 = sy-tfill.

      o_popup->add_valor( ztemp-subclave ).
      o_popup->add_valor( ztemp-clave ).
      SELECT MAX( fecha ) FROM zcache    "#EC CI_BYPASS "#EC CI_GENBUFF
        INTO ztemp-erdat
       WHERE report = ztemp-subclave
         AND clave = ztemp-clave.
      o_popup->add_valor( ztemp-erdat ).
      o_popup->add_valor( ztemp-valor1 ).
    ENDSELECT.
  ENDIF.

  o_popup->matchcode( EXPORTING field   = 'CLAVE'
                      CHANGING  valor   = s_clave-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_report-low.
  DATA: o_popup TYPE REF TO zcl_ap_matchcode_z.
  CREATE OBJECT o_popup
    EXPORTING
      tabname = 'ZCACHE'.

  o_popup->add_field( field = 'REPORT' selectflag = 'X' ).
  o_popup->add_field( 'FECHA' ).
  o_popup->add_field( 'AUX1' ).

  DATA zcache TYPE zcache.
  RANGES r_report FOR sy-cprog.
  SELECT report COUNT( * ) FROM zcache   "#EC CI_BYPASS "#EC CI_GENBUFF
    INTO (zcache-report, sy-tfill )
   WHERE report IN r_report
     AND clave IN s_clave
     AND fecha IN s_erdat
    GROUP BY report
    ORDER BY report.
    ztemp-valor1 = sy-tfill.

    o_popup->add_valor( zcache-report ).
    SELECT MAX( fecha ) FROM zcache      "#EC CI_BYPASS "#EC CI_GENBUFF
      INTO zcache-fecha
     WHERE report = zcache-report.
    o_popup->add_valor( zcache-fecha ).
    zcache-aux1 = sy-tfill.
    o_popup->add_valor( zcache-aux1 ).
  ENDSELECT.

  o_popup->matchcode( EXPORTING field   = 'REPORT'
                      CHANGING  valor   = s_report-low ).


*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  o_prog->main( ).