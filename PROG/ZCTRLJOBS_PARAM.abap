************************************************************************
* APLICACION  : BC                                                     *
* TIPO        : Listado
* TITULO      : Parametrización control de jobs
* DESCRIPCION :
*                                                                      *
* AUTOR: Andrés Picazo                          FECHA: 30/07/2007      *
* MODIFICACIONES                                                       *
* -------------                                                        *
* FECHA        NOMBRE            TAREA              ORDEN              *
* -------------------------------------------------------------------- *
* dd.mm.yyyy   username          APPXXX             PO1K90XXXXX        *
************************************************************************
REPORT zctrljobs_param
      NO STANDARD PAGE HEADING
      LINE-COUNT 90
      LINE-SIZE 120.

*------INCLUDES--------------------------------------------------------*

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES : zctrljobs_doc,
         zctrljobs_resp,
         zebc20,
         user_addr,
         zctrljobs_param.

*------TABLAS INTERNAS-------------------------------------------------*
TYPES: BEGIN OF t_listado,
*        check,
         tipocontrol   LIKE zctrljobs_param-tipocontrol,
         jobname       LIKE zctrljobs_param-jobname,
         report        LIKE zctrljobs_param-report,
         textobreve    LIKE zctrljobs_param-textobreve,
         casoaviso     LIKE zctrljobs_param-casoaviso,
         msgid         LIKE zctrljobs_param-msgid,
         msgno         LIKE zctrljobs_param-msgno,
         msgv1         LIKE zctrljobs_param-msgv1,
         distinto      LIKE zctrljobs_param-distinto,
         no_enviar     TYPE zstring,
         solo_usuarios TYPE zstring,
         no_usuarios   TYPE zstring,
         creado_por    TYPE zstring,
         ejec_report   LIKE zctrljobs_param-ejec_report,
       END OF t_listado.
DATA: i_listado TYPE TABLE OF t_listado,
      l_listado TYPE t_listado.

FIELD-SYMBOLS <listado> TYPE t_listado.


DATA: i_zctrljobs_doc       LIKE zctrljobs_doc OCCURS 0 WITH HEADER LINE,
      i_zctrljobs_resp      LIKE zctrljobs_resp OCCURS 0 WITH HEADER LINE,
      i_zebc20              LIKE zebc20 OCCURS 0 WITH HEADER LINE,
      o_texto               TYPE REF TO zcl_ap_control_texto,
      v_hreditor_visualizar,
      i_hreditor_texto      TYPE TABLE OF text72,
      l_text72              TYPE zctrljobs_doc-comentarios.

*----------------------------------------------------------------------*
*       CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM zcl_ap_alv. "_check.
  PUBLIC SECTION.
    METHODS: handle_double_click REDEFINITION.
    METHODS: handle_user_command REDEFINITION.
ENDCLASS. "lcl_alv DEFINITION


*------VARIABLES-------------------------------------------------------*
DATA: v_inicio,
      o_alv TYPE REF TO lcl_alv.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TCU' ITSELF
CONTROLS: tcu TYPE TABLEVIEW USING SCREEN 0100.

*&SPWIZARD: LINES OF TABLECONTROL 'TCU'
DATA:     g_tcu_lines  LIKE sy-loopc.

DATA:     ok_code LIKE sy-ucomm.


*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk_par WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: s_tipoc  FOR zctrljobs_param-tipocontrol,
                s_job    FOR zctrljobs_param-jobname,
                s_report FOR zctrljobs_param-report,
                s_texto  FOR zctrljobs_param-textobreve,
                s_caso   FOR zctrljobs_param-casoaviso.
SELECTION-SCREEN END OF BLOCK blk_par.

*----------------------------------------------------------------------*
*       CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.
  METHOD handle_double_click.
    READ TABLE i_listado INDEX row INTO l_listado.
    IF sy-subrc = 0.
      CLEAR zctrljobs_param.
      SELECT SINGLE * FROM  zctrljobs_param
        INTO zctrljobs_param
             WHERE  tipocontrol  = l_listado-tipocontrol
             AND    jobname      = l_listado-jobname
             AND    report       = l_listado-report.

      v_inicio = 'X'.
      v_hreditor_visualizar = 'X'.
      CALL SCREEN 100.
    ENDIF.
  ENDMETHOD. "handle_double_click
  METHOD handle_user_command.
    DATA: l_row TYPE i.

    CASE e_salv_function.
      WHEN 'EXCEL'.
        exportar_excel( ).
      WHEN 'NUEVO'.
        CLEAR zctrljobs_param.

        v_inicio = 'X'.
        CLEAR v_hreditor_visualizar.
        CALL SCREEN 100.
        PERFORM seleccionar_datos.
        refresh( ).

      WHEN 'MODIFICAR'.
        l_row = get_fila_activa( ).
        READ TABLE i_listado INTO l_listado INDEX l_row.
        IF sy-subrc = 0.
          CLEAR zctrljobs_param.
          MOVE-CORRESPONDING l_listado TO zctrljobs_param.
          v_inicio = 'X'.
          CLEAR v_hreditor_visualizar.
          CALL SCREEN 100.
          PERFORM seleccionar_datos.
          refresh( ).
        ENDIF.


      WHEN 'BORRAR'.
        l_row = get_fila_activa( ).
        READ TABLE i_listado INTO l_listado INDEX l_row.
        IF sy-subrc = 0.
          DELETE FROM zctrljobs_param
           WHERE tipocontrol = l_listado-tipocontrol
             AND jobname     = l_listado-jobname
             AND report      = l_listado-report.

          PERFORM seleccionar_datos.
          refresh( ).
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD. "handle_USER_COMMAND
ENDCLASS. "lcl_alv IMPLEMENTATION

AT SELECTION-SCREEN.

************************************************************************
*
*                  LOGICA DEL PROGRAMA
*
************************************************************************

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.


  PERFORM seleccionar_datos.

  PERFORM listado.

*----------------------------------------------------------------------
* AT USER-COMMAND.
*----------------------------------------------------------------------*
AT USER-COMMAND.

*----------------------------------------------------------------------
* TOP-OF-PAGE.
*----------------------------------------------------------------------*
TOP-OF-PAGE.

************************************************************************
*
*                  FORMS ADICIONALES
*
************************************************************************


*&---------------------------------------------------------------------*
*&      Form  SELECCIONAR_DATOS
*&---------------------------------------------------------------------*
FORM seleccionar_datos.

  REFRESH i_listado.
  SELECT * FROM zctrljobs_param               "#EC CI_ALL_FIELDS_NEEDED
    INTO CORRESPONDING FIELDS OF TABLE i_listado
   WHERE tipocontrol IN s_tipoc
     AND jobname     IN s_job
     AND report      IN s_report
     AND textobreve  IN s_texto
     AND casoaviso   IN s_caso.

  LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
    SELECT  tipo, string FROM  zctrljobs_paramt
      INTO TABLE @DATA(i_t)
     WHERE tipocontrol  = @<listado>-tipocontrol
       AND jobname      = @<listado>-jobname
       AND report       = @<listado>-report.
    LOOP AT i_t ASSIGNING FIELD-SYMBOL(<t>).
      ASSIGN COMPONENT <t>-tipo OF STRUCTURE <listado> TO FIELD-SYMBOL(<fs>).
      IF sy-subrc = 0.
        <fs> = <t>-string.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " SELECCIONAR_DATOS

*&---------------------------------------------------------------------*
*&      Form  listado_alv
*&---------------------------------------------------------------------*
FORM listado.

  CREATE OBJECT o_alv
    EXPORTING
      status = 'STANDARD'.

  o_alv->set_field_text( 'NO_ENVIAR,SOLO_USUARIOS,NO_USUARIOS,CREADO_POR' ).
  o_alv->show( ).

ENDFORM.                    " listado


******************** DYNPRO ********************************************

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS'.
  SET TITLEBAR '001'.

  IF v_inicio = 'X'.
    IF NOT zctrljobs_param IS INITIAL.
      SELECT comentarios FROM zctrljobs_doc
        INTO CORRESPONDING FIELDS OF TABLE i_zctrljobs_doc
       WHERE tipocontrol = zctrljobs_param-tipocontrol
         AND jobname     = zctrljobs_param-jobname
         AND report      = zctrljobs_param-report.
      CLEAR i_hreditor_texto.
      LOOP AT i_zctrljobs_doc.
        APPEND i_zctrljobs_doc-comentarios TO i_hreditor_texto.
      ENDLOOP.

      REFRESH i_zebc20.
      SELECT * FROM zctrljobs_resp
        INTO CORRESPONDING FIELDS OF TABLE i_zebc20
       WHERE tipocontrol = zctrljobs_param-tipocontrol
         AND jobname     = zctrljobs_param-jobname
         AND report      = zctrljobs_param-report.
    ELSE.
      REFRESH: i_zctrljobs_doc, i_zebc20, i_hreditor_texto.
    ENDIF.

    IF o_texto IS INITIAL.
      CREATE OBJECT o_texto
        EXPORTING
          controlname = 'CDOC'.

    ENDIF.
    o_texto->set_editor( lines = i_hreditor_texto[]
                         display_mode = v_hreditor_visualizar ).
  ENDIF.

  IF i_zebc20[] IS INITIAL.
    DO 10 TIMES.
      CLEAR i_zebc20.
      APPEND i_zebc20.
    ENDDO.
  ENDIF.

  LOOP AT i_zebc20.
    IF  NOT i_zebc20-usuario IS INITIAL.
      CLEAR user_addr.
      SELECT SINGLE name_first name_last FROM  user_addr
        INTO CORRESPONDING FIELDS OF user_addr
             WHERE  bname  = i_zebc20-usuario.
      CONCATENATE user_addr-name_first user_addr-name_last
            INTO i_zebc20-nombre SEPARATED BY space.
    ELSE.
      CLEAR i_zebc20-nombre.
    ENDIF.
    MODIFY i_zebc20.
  ENDLOOP.

  DESCRIBE TABLE i_zebc20 LINES tcu-lines.

  CLEAR v_inicio.

  LOOP AT SCREEN.
    IF v_hreditor_visualizar = 'X'.
      IF screen-name(6) = 'ZCTRLJOBS_PARAM'.
        screen-input = 0.
        IF screen-name(10) = 'ZCTRLJOBS_PARAM-MSG' OR
           screen-name = 'ZCTRLJOBS_PARAM-DISTINTO'.
          IF zctrljobs_param-casoaviso = 'D'.
            screen-invisible = 0.
          ELSE.
            screen-invisible = 1.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      IF screen-name(10) = 'ZCTRLJOBS_PARAM-MSG' OR
         screen-name = 'ZCTRLJOBS_PARAM-DISTINTO'.
        IF zctrljobs_param-casoaviso = 'D'.
          screen-invisible = 0.
          screen-input = 1.
        ELSE.
          screen-invisible = 1.
          screen-input = 0.
        ENDIF.
      ELSEIF screen-name(6) = 'ZCTRLJOBS_PARAM'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " STATUS_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA zctrljobs_paramt TYPE zctrljobs_paramt.
  CASE sy-ucomm.
    WHEN 'GRABAR'.
      MODIFY zctrljobs_param FROM zctrljobs_param.
      MOVE-CORRESPONDING zctrljobs_param TO zctrljobs_paramt.
      zctrljobs_paramt-tipo = 'NO_ENVIAR'.
      IF NOT l_listado-no_enviar IS INITIAL.
        zctrljobs_paramt-string = l_listado-no_enviar.
        MODIFY zctrljobs_paramt FROM zctrljobs_paramt.
      ELSE.
        DELETE zctrljobs_paramt FROM zctrljobs_paramt.
      ENDIF.
      zctrljobs_paramt-tipo = 'SOLO_USUARIOS'.
      IF NOT l_listado-solo_usuarios IS INITIAL.
        zctrljobs_paramt-string = l_listado-solo_usuarios.
        MODIFY zctrljobs_paramt FROM zctrljobs_paramt.
      ELSE.
        DELETE zctrljobs_paramt FROM zctrljobs_paramt.
      ENDIF.
      zctrljobs_paramt-tipo = 'NO_USUARIOS'.
      IF NOT l_listado-no_usuarios IS INITIAL.
        zctrljobs_paramt-string = l_listado-no_usuarios.
        MODIFY zctrljobs_paramt FROM zctrljobs_paramt.
      ELSE.
        DELETE zctrljobs_paramt FROM zctrljobs_paramt.
      ENDIF.
      zctrljobs_paramt-tipo = 'CREADO_POR'.
      IF NOT l_listado-creado_por IS INITIAL.
        zctrljobs_paramt-string = l_listado-creado_por.
        MODIFY zctrljobs_paramt FROM zctrljobs_paramt.
      ELSE.
        DELETE zctrljobs_paramt FROM zctrljobs_paramt.
      ENDIF.

      o_texto->get_editor( EXPORTING col_width = 120 IMPORTING lines = i_hreditor_texto[] ).
      DELETE FROM zctrljobs_doc
       WHERE tipocontrol = zctrljobs_param-tipocontrol
         AND jobname     = zctrljobs_param-jobname
         AND report      = zctrljobs_param-report.

      DELETE ADJACENT DUPLICATES FROM i_hreditor_texto.
      LOOP AT i_hreditor_texto INTO l_text72.
        CLEAR zctrljobs_doc.
        MOVE-CORRESPONDING zctrljobs_param TO zctrljobs_doc.
        zctrljobs_doc-linea = sy-tabix.
        zctrljobs_doc-comentarios = l_text72.
        MODIFY zctrljobs_doc.
      ENDLOOP.


      DELETE FROM zctrljobs_resp
       WHERE tipocontrol = zctrljobs_param-tipocontrol
         AND jobname     = zctrljobs_param-jobname
         AND report      = zctrljobs_param-report.

      LOOP AT i_zebc20 WHERE NOT usuario IS INITIAL
                          OR NOT email IS INITIAL.
        CLEAR zctrljobs_resp.
        MOVE-CORRESPONDING i_zebc20 TO zctrljobs_resp.
        MOVE-CORRESPONDING zctrljobs_param TO zctrljobs_resp.
        INSERT zctrljobs_resp.
      ENDLOOP.

      MESSAGE s398(00) WITH 'Se han grabado los datos'.
    WHEN 'PICK'.
      DATA: l_campo       TYPE string,
            l_texto(4098),
            l_mod.
      GET CURSOR FIELD l_campo.
      CASE l_campo.
        WHEN 'L_LISTADO-NO_ENVIAR'.
          CLEAR l_mod.
          l_texto = l_listado-no_enviar.
*          l_string = zcl_ap_string=>editor_popup_string( string = l_string titulo = 'No enviar si...' ).
          CALL FUNCTION 'Z_POPUP_EDIT_TEXT'
            EXPORTING
              titulo     = 'No enviar si...'
              texto      = ''
              ini_x      = 1
*             INI_Y      = 3
              fin_x      = 202
*             FIN_Y      = 24
              max_cols   = 200
*             DISPLAY_MODE       = ''
            IMPORTING
              modificado = l_mod
*             CANCELADO  =
            CHANGING
              string     = l_texto.
          IF l_mod = 'X'.
            l_listado-no_enviar = l_texto.
          ENDIF.

        WHEN 'L_LISTADO-NO_USUARIOS'.
          CLEAR l_mod.
          l_texto = l_listado-no_usuarios.
          CALL FUNCTION 'Z_POPUP_EDIT_TEXT'
            EXPORTING
              titulo     = 'No enviar a usuarios...'
              texto      = ''
              ini_x      = 1
*             INI_Y      = 3
              fin_x      = 202
*             FIN_Y      = 24
              max_cols   = 200
*             DISPLAY_MODE       = ''
            IMPORTING
              modificado = l_mod
*             CANCELADO  =
            CHANGING
              string     = l_texto.
          IF l_mod = 'X'.
            l_listado-no_usuarios = l_texto.
          ENDIF.

      ENDCASE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
      o_texto->destroy( ).
      CLEAR o_texto.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  validar_campos_obligatorios  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validar_campos_obligatorios INPUT.

  CHECK sy-ucomm = 'GRABAR'.
  IF zctrljobs_param-tipocontrol IS INITIAL OR
     zctrljobs_param-jobname IS INITIAL OR
     zctrljobs_param-casoaviso IS INITIAL.
    MESSAGE e398(00) WITH 'Informe campos obligatorios'.
  ENDIF.

ENDMODULE.                 " validar_campos_obligatorios  INPUT





************************** GENERADO TC ****************************

*&SPWIZARD: OUTPUT MODULE FOR TC 'TCU'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tcu_change_tc_attr OUTPUT.
  DESCRIBE TABLE i_zebc20 LINES tcu-lines.
ENDMODULE.                    "TCU_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TCU'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tcu_get_lines OUTPUT.
  g_tcu_lines = sy-loopc.

  LOOP AT SCREEN.
    IF v_hreditor_visualizar = 'X'.
      IF screen-name = 'ZEBC20-USUARIO' OR
         screen-name = 'ZEBC20-EMAIL'.
        screen-input = 0.
      ENDIF.
    ELSE.
      IF screen-name = 'ZEBC20-USUARIO' OR
         screen-name = 'ZEBC20-EMAIL'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                    "TCU_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TCU'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tcu_modify INPUT.
  MODIFY i_zebc20
    FROM zebc20
    INDEX tcu-current_line.
ENDMODULE.                    "TCU_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TCU'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tcu_mark INPUT.
  DATA: g_tcu_wa2 LIKE LINE OF i_zebc20.
  IF tcu-line_sel_mode = 1
  AND zebc20-marca = 'X'.
    LOOP AT i_zebc20 INTO g_tcu_wa2
      WHERE marca = 'X'.
      g_tcu_wa2-marca = ''.
      MODIFY i_zebc20
        FROM g_tcu_wa2
        TRANSPORTING marca.
    ENDLOOP.
  ENDIF.
  MODIFY i_zebc20
    FROM zebc20
    INDEX tcu-current_line
    TRANSPORTING marca.
ENDMODULE.                    "TCU_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TCU'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tcu_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TCU'
                              'I_ZEBC20'
                              'MARCA'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TCU_USER_COMMAND INPUT

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
