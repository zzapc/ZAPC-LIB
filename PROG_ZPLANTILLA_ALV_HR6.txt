***********************************************************************
* TIPO : LISTADO
* TITULO : ??
* DESCRIPCION : ??
*
* AUTOR: Andrés Picazo                                FECHA: 02/11/2015
* ANALISTA: ??
*
* Doc. Tecnica: http://sap4.com/tareas?=&cliente=CCC&objeto=OOO
*->MANUAL:http://sap4.com
*
* MODIFICACIONES
* 02/11/2015 Tarea inicial: http://sap4.com/tareas?&ntask=TTT
*
***********************************************************************
REPORT zplantilla_alv_hr6.

*------INCLUDES--------------------------------------------------------*

*------TABLAS/ESTRUCTURAS----------------------------------------------*
tables: pernr, zest_empleado.

infotypes: 0000,
           0001.


*------TABLAS INTERNAS-------------------------------------------------*
TYPES: BEGIN OF t_listado,
         check,
         lights,
         pernr type persno,
         ename type pernr-ename,
         begda type p0001-begda,
         endda type p0001-endda,
END OF t_listado.

__data_set_var listado.

data: l_listado_ini type t_listado,
      i_listado_ini type tt_listado.

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
      o_alv TYPE REF TO lcl_alv.

ranges: r_pnppernr for pa0001-pernr.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE text-sel.
select-options: s_orgeh for pernr-orgeh.
SELECTION-SCREEN: SKIP 1.
PARAMETERS: p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK 001.
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
    READ TABLE i_listado INTO l_listado INDEX row.
    IF sy-subrc = 0.
      zcl_ap_empleado=>visualizar_st( pernr = l_listado-pernr
                                      begda = l_listado-begda
                                      endda = l_listado-endda
                                      infty = '0001' ).
    ENDIF.
  ENDMETHOD. "handle_double_click
  METHOD handle_user_command.
    DATA: l_row TYPE i.

    CASE e_salv_function.
      WHEN 'EXCEL'.
        exportar_excel( ).
      WHEN 'BLOQUEAR'.
        get_seleccion( ).
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'.
          <listado>-lights = zcl_ap_alv=>c_sem_rojo.
        ENDLOOP.
        refresh( ).

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
    seleccionar_datos( ).
    listado( ).
  ENDMETHOD.                    "REPORT

  METHOD seleccionar_datos.

    sgpi_texto( 'Seleccionando datos' ).

  ENDMETHOD.                    "seleccionar_datos


  METHOD listado.

    sgpi_texto( 'Generando informe' ).

    o_alv->set_layout( p_vari ).

    o_alv->set_top_of_page( ).

    o_alv->set_orden( 'PERNR,ENAME,BEGDA,ENDDA' ).

    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).

    o_alv->show( ).


  ENDMETHOD.                    "

ENDCLASS.                    "REPORT IMPLEMENTATION

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  CREATE OBJECT o_prog
    EXPORTING
      status        = 'INICIO'
      get_nombre_pc = 'X'
      no_param      = 'X'.


  pnpstat2-option = 'EQ'.
  pnpstat2-sign   = 'I'.
  pnpstat2-low    = '3'.
  append pnpstat2.


  concatenate sy-datum(4) '0101' into pnpbegda.
  concatenate sy-datum(4) '1231' into pnpendda.

  CREATE OBJECT o_alv
    EXPORTING
      status             = 'STANDARD'
      lights             = 'LIGHTS'
      top_of_page_auto   = 'X'
      top_of_page_titulo = 'X'.

  p_vari = o_alv->get_default_layout( ).

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.

  p_vari = o_alv->get_f4_layout( ).

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.
  o_prog->at_selection( ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  o_prog->at_selection( ).

at selection-screen output.
  PNPTIMR1 = 'X'.
PNPTIMR6 = ''.
pnpbegda = pnpendda = sy-datum.
* Ocultar selección de personas
*  zcl_ap_dynpro=>screen_visible( campo = 'PNPBEGPS,PNPENDPS,%FDPS117,%FBIS120,%PBLP125' variable = '' option = 'CP' ).
* Otros campos no usados
*  zcl_ap_dynpro=>screen_visible( campo = 'PNPXBWBK,PNPABKRS,PNPXPGPK' variable = '' option = 'CP' ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  pn-begps = pn-begda.
  pn-endps = pn-endda.

  r_pnppernr[] = pnppernr[].
  refresh pnppernr.

  clear pnppernr.
  pnppernr-option = 'EQ'.
  pnppernr-sign   = 'I'.

* Filtro previo de empleados para optimizar la BDL

  select pernr from pa0001
    into pnppernr-low
   where pernr in r_pnppernr
     and begda <= pn-endda
     and endda >= pn-begda.
    collect pnppernr.
  endselect.


get pernr.

  o_prog->sgpi_texto( texto1 = 'Leyendo empleado' texto2 = pernr-pernr quitar_ceros = 'X').
  rp-provide-from-last p0000 space pn-begda pn-endda.
  check p0000-stat2 in pnpstat2.

  rp-provide-from-last p0001 space pn-begda pn-endda.

  zest_empleado = zcl_ap_empleado=>get_datos_basicos( pernr = pernr-pernr
                                                      begda = p0001-begda
                                                      endda = p0001-endda ).

  clear l_listado.
  move-corresponding pernr to l_listado.
  move-corresponding zest_empleado to l_listado_ini.

  l_listado = l_listado_ini.
  append l_listado to i_listado.


end-of-selection.

  pnppernr[] = r_pnppernr[].

  o_prog->main( ).
