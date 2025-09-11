CLASS zcl_ap_alv_form DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA o_grid     TYPE REF TO cl_salv_form_layout_grid.
    DATA i_filtros  TYPE ztab_parm_sel.
    DATA o_grid_sel TYPE REF TO cl_salv_form_layout_grid.

    METHODS constructor.

    METHODS set_titulo
      IMPORTING !row    TYPE i   OPTIONAL
                !column TYPE i   DEFAULT 1
                !text   TYPE any OPTIONAL
                tooltip TYPE any OPTIONAL
                colspan TYPE i   OPTIONAL
      PREFERRED PARAMETER text.

    METHODS get_grid
      RETURNING VALUE(grid) TYPE REF TO cl_salv_form_layout_grid.

    METHODS add_rango
      IMPORTING texto        TYPE any
                rango        TYPE table     OPTIONAL
                tabla        TYPE any       DEFAULT ''
                nombre_campo TYPE any       DEFAULT ''
                valor        TYPE any       DEFAULT ''
                valor_hasta  TYPE any       DEFAULT ''
                quitar_ceros TYPE abap_bool DEFAULT ''
                campo_clave  TYPE any       DEFAULT ''
                campo_idioma TYPE any       DEFAULT ''.

    METHODS crea_info_seleccion
      IMPORTING maximo_lineas TYPE i DEFAULT 6.

    METHODS set_linea
      IMPORTING texto TYPE any.

    METHODS set_accion
      IMPORTING !row    TYPE i   OPTIONAL
                !column TYPE i   DEFAULT 1
                !text   TYPE any OPTIONAL
                tooltip TYPE any OPTIONAL
      PREFERRED PARAMETER text.

    METHODS add_fechas
      IMPORTING texto TYPE any   DEFAULT ''
                begda TYPE begda OPTIONAL
                endda TYPE endda OPTIONAL
      PREFERRED PARAMETER begda.

    METHODS add_parametro
      IMPORTING titulo TYPE any
                valor  TYPE any.

    METHODS add_rango_auto
      IMPORTING !report TYPE sy-cprog                   DEFAULT sy-cprog
                excluir TYPE any                        DEFAULT ''
                i_param TYPE zcl_ap_alv=>tt_top_of_page OPTIONAL
      PREFERRED PARAMETER report.

    METHODS add_grafico
      IMPORTING grafico TYPE any
                !row    TYPE i
                !column TYPE i.


  PRIVATE SECTION.
    DATA fila TYPE i.
endclass. "ZCL_AP_ALV_FORM definition
class ZCL_AP_ALV_FORM implementation.
  METHOD add_fechas.
    DATA: l_fila TYPE zest_parm_sel,
          l_f1   TYPE c LENGTH 10,
          l_f2   TYPE c LENGTH 10.

    l_fila-titulo = texto.

    IF endda IS INITIAL OR begda = endda.
      WRITE begda TO l_fila-valor.
      IF texto IS INITIAL.
        l_fila-titulo = 'Fecha de selección'.
      ENDIF.
    ELSE.
      WRITE begda TO l_f1.
      WRITE endda TO l_f2.

      CONCATENATE l_f1 'a' l_f2 INTO l_fila-valor SEPARATED BY space.
      IF texto IS INITIAL.
        l_fila-titulo = 'Fechas de selección'.
      ENDIF.
    ENDIF.

    APPEND l_fila TO i_filtros.
  ENDMETHOD.
  METHOD add_grafico.
    DATA lr_picture TYPE REF TO cl_salv_form_picture.

    lr_picture = NEW #(
        picture_id = grafico ).

    o_grid->set_element(
        row       = row
        column    = column
        r_element = lr_picture ).
  ENDMETHOD.
  METHOD add_parametro.
    DATA l_fila TYPE zest_parm_sel.

    l_fila-titulo = titulo.
    WRITE valor TO l_fila-valor.  "#EC *
    APPEND l_fila TO i_filtros.
  ENDMETHOD.
  METHOD add_rango.
    DATA: l_fila               TYPE zest_parm_sel,
          l_descripcion        TYPE c LENGTH 40,
          l_hasta              TYPE c LENGTH 20,
          l_hay_hasta          TYPE c LENGTH 1,
          l_valor              TYPE c LENGTH 40,
          l_buscar_descripcion TYPE c LENGTH 1,
          l_option             TYPE c LENGTH 2,
          l_campo              TYPE c LENGTH 40,
          l_tipo               TYPE c LENGTH 1.

    FIELD-SYMBOLS: <fs>     TYPE any,
                   <low>    TYPE any,
                   <sign>   TYPE any,
                   <high>   TYPE any,
                   <option> TYPE any.

    IF NOT valor IS INITIAL.
      l_fila-titulo = texto.
      WRITE valor TO l_fila-valor.  "#EC *
      IF NOT tabla IS INITIAL.
        l_descripcion = zcl_ap_dev=>get_descripcion( tabla = tabla valor = valor nombre_campo = nombre_campo campo_clave = campo_clave campo_idioma = campo_idioma ).
        IF NOT l_descripcion IS INITIAL.
          CONCATENATE l_fila-valor l_descripcion INTO l_fila-valor SEPARATED BY space.
        ENDIF.
      ENDIF.

      IF NOT valor_hasta IS INITIAL.
        WRITE valor_hasta TO l_hasta.  "#EC *
        CONCATENATE 'Desde'(des) l_fila-valor 'hasta'(has) l_hasta INTO l_fila-valor
                    SEPARATED BY space.
      ENDIF.

      APPEND l_fila TO i_filtros.
    ENDIF.

    IF rango IS INITIAL.
      RETURN.
    ENDIF.

    l_fila-titulo = texto.
    LOOP AT rango ASSIGNING <fs>.
      CLEAR l_hay_hasta.
      ASSIGN ('<FS>-LOW') TO <low>.
      IF sy-subrc = 0.
        WRITE <low> TO l_valor.  "#EC *

        ASSIGN ('<FS>-SIGN') TO <sign>.
        IF sy-subrc = 0.
          IF <sign> = 'E'.
            CONCATENATE '<>' l_valor INTO l_valor.
          ENDIF.
        ENDIF.
        IF quitar_ceros = 'X'.
          zcl_ap_string=>quitar_ceros_c( CHANGING  cadena = l_valor ).
        ENDIF.

        IF NOT tabla IS INITIAL.
          ASSIGN ('<FS>-HIGH') TO <high>.
          IF sy-subrc = 0.
            IF <high> IS INITIAL.
              l_buscar_descripcion = 'X'.
            ENDIF.
          ELSE.
            l_buscar_descripcion = 'X'.
          ENDIF.

          IF l_buscar_descripcion = 'X'.
            l_descripcion = zcl_ap_dev=>get_descripcion( tabla = tabla valor = <low> nombre_campo = nombre_campo campo_clave = campo_clave campo_idioma = campo_idioma ).
            IF NOT l_descripcion IS INITIAL.
              CONCATENATE l_valor l_descripcion INTO l_valor SEPARATED BY space.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      ASSIGN ('<FS>-HIGH') TO <high>.
      IF sy-subrc = 0.
        IF NOT <high> IS INITIAL.
          WRITE <high> TO l_hasta.  "#EC *

          IF quitar_ceros = 'X'.
            zcl_ap_string=>quitar_ceros_c( CHANGING  cadena = l_hasta ).
          ENDIF.

          CONCATENATE 'Desde'(des) l_valor 'hasta'(has) l_hasta INTO l_fila-valor
                      SEPARATED BY space.
          l_hay_hasta = 'X'.
        ENDIF.
      ENDIF.

      IF l_hay_hasta IS INITIAL.
        ASSIGN ('<FS>-OPTION') TO <option>.
        IF sy-subrc = 0.
          IF <option> <> 'EQ'.
            l_option = <option>.
            IF l_fila-valor IS INITIAL.
              CASE <option>.
                WHEN 'GT'. l_option = '>'.
                WHEN 'GE'. l_option = '>='.
                WHEN 'LT'. l_option = '<'.
                WHEN 'LE'. l_option = '<='.
                WHEN 'NE'. l_option = '<>'.
              ENDCASE.
              CONCATENATE l_option l_valor INTO l_valor SEPARATED BY space.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF l_hay_hasta = 'X'.
        APPEND l_fila TO i_filtros.
      ELSE.
        IF l_fila-valor IS INITIAL.
          l_fila-valor = l_valor.
        ELSE.
          CONCATENATE l_fila-valor ',' l_valor INTO l_fila-valor.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF l_hay_hasta IS INITIAL.
      APPEND l_fila TO i_filtros.
    ENDIF.
  ENDMETHOD.
  METHOD add_rango_auto.
    DATA: r_excluir TYPE RANGE OF string,
          l_report  TYPE string,
          l_campo   TYPE string.

    FIELD-SYMBOLS: <valor>  TYPE any,
                   <valor2> TYPE any.

    r_excluir = zcl_ap_lista=>lista2rango( excluir ).

    CONCATENATE '(' report ')' INTO l_report.

    DEFINE add_parametro.
      IF NOT &1 IN r_excluir.
        CONCATENATE l_report &1 INTO l_campo.
        ASSIGN (l_campo) TO <valor>.
        IF sy-subrc = 0.
          IF NOT <valor> IS INITIAL.
            add_rango( texto = &2 valor = <valor> tabla = &3 nombre_campo = &4 quitar_ceros = &5 ).
          ENDIF.
        ENDIF.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE add_parametro_2.
      IF NOT &1 IN r_excluir.
        CONCATENATE l_report &1 INTO l_campo.
        ASSIGN (l_campo) TO <valor>.
        IF sy-subrc = 0.
          IF NOT <valor> IS INITIAL.
            CONCATENATE l_report &2 INTO l_campo.
            ASSIGN (l_campo) TO <valor2>.
            IF sy-subrc = 0.
              IF NOT <valor2> IS INITIAL.
                add_rango( texto = &3 valor = <valor> valor_hasta = <valor2> tabla = &4 nombre_campo = &5 quitar_ceros = &6 ).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE add_rango.
      IF NOT &1 IN r_excluir.
        CONCATENATE l_report &1 '[]' INTO l_campo.
        ASSIGN (l_campo) TO <valor>.
        IF sy-subrc = 0.
          IF NOT <valor> IS INITIAL.
            add_rango( texto = &2 rango = <valor> tabla = &3 nombre_campo = &4 quitar_ceros = &5 ).
          ENDIF.
        ENDIF.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE add_rango_c.
      IF NOT &1 IN r_excluir.
        CONCATENATE l_report &1 '[]' INTO l_campo.
        ASSIGN (l_campo) TO <valor>.
        IF sy-subrc = 0.
          IF NOT <valor> IS INITIAL.
            add_rango( texto = &2 rango = <valor> tabla = &3 nombre_campo = &4 quitar_ceros = &5 campo_clave = &6 campo_idioma = 'NO' ).
          ENDIF.
        ENDIF.
      ENDIF.
    END-OF-DEFINITION.

    LOOP AT i_param ASSIGNING FIELD-SYMBOL(<param>).
      IF <param>-tipo = 'P'.
        add_parametro <param>-param <param>-texto <param>-tabla <param>-campo <param>-no_ceros.
      ELSEIF <param>-tipo = 'R'.
        add_rango <param>-param <param>-texto <param>-tabla <param>-campo <param>-no_ceros.
      ELSEIF <param>-tipo = ''.
        add_rango( texto = <param>-texto valor = <param>-valor ).
      ENDIF.
    ENDLOOP.

    add_parametro: 'P_BUKRS' 'Sociedad'(soc) 'T001' 'BUTXT' '',
                   'P_GJAHR' 'Ejercicio'(eje) '' '' '',
                   'P_MONAT' 'Periodo'(per) '' '' '',
                   'P_SERNR' 'Nº de serie'(nse) '' '' '',
                   'P_PERIO' 'Periodo'(per) '' '' '',
                   'P_WERKS' 'Centro'(cen)  'T001W' 'NAME1' '',
                   'P_KUNNR' 'Cliente'(cli) 'KNA1' 'NAME1' 'X',
                   'P_LIFNR' 'Proveedor'(pro) 'LFA1' 'NAME1' 'X',
                   'P_FILE'  'Fichero'(fic) '' '' '',
                   'P_BUDAT' 'F.Contabilización'(fct) '' '' '',
                   'P_LGNUM' 'Número almacén'(nal) 'T300T' 'LNUMT' '',
                   'P_MATNR' 'Material'(mat) 'MAKT' 'MAKTX' '',
                   'P_VKORG' 'Organización ventas'(ove) 'TVKOT' 'VTEXT' '',
                   'P_FECHA' 'Fecha'(fec) '' '' ''.

    add_rango: 'hasta'(has) 'Cuentas'(cue) '' '' 'X',
               'S_GJAHR' 'Ejercicio'(eje) '' '' '',
               'S_MONAT' 'Mes'(mes) '' '' '',
               'S_BUDAT' 'F.Contabilización'(fct) '' '' '',
               'S_CPUDT' 'F.Registro'(frg) '' '' '',
               'S_BUKRS' 'Sociedad'(soc) 'T001' 'BUTXT' '',
               'S_BELNR' 'Nº Documento'(ndt) '' '' 'X',
               'S_MWSKZ' 'Indicador IVA'(iva) '' '' '',
               'S_WERKS' 'Centros'(cen) 'T001W' 'NAME1' '',
               'S_LGORT' 'Almacenes'(alm) '' '' '',
               'S_ORGEH' 'Unidad Org.'(uor) 'T527X' 'ORGTX' 'X',
               'S_PERNR' 'Empleados'(emp) 'PA0001' 'ENAME' 'X',
               'S_LGNUM' 'Nº Almacén'(nal) 'T300T' 'LNUMT' '',
               'S_LIFNR' 'Proveedor'(pro) 'LFA1' 'NAME1' '',
               'S_LAUFD' 'Fecha prop.pago'(fpp) '' '' '',
               'S_LAUFI' 'ID. de pago'(idp) '' '' '',
               'S_MATNR' 'Material'(mat) 'MAKT' 'MAKTX' '',
               'S_CHARG' 'Lotes'(lot) '' '' '',
               'S_VSTEL' 'Puesto expedición'(pex) 'TVSTT' 'VTEXT' '',
               'S_LFDAT' 'Fecha de entrega'(fen) '' '' '',
               'S_WADAT' 'Fecha salida mercancías'(fsm) '' '' '',
               'S_LAND1' 'País'(pai) 'T005T' 'LANDX' '',
               'S_MATKL' 'Grupo artículos'(gar) 'T023T' 'WGBEZ' '',
               'S_BWART' 'Clase movimiento'(clm) 'T156T' 'BTEXT' '',
               'S_AUFNR' 'Orden'(ord) '' '' '',
               'S_FECHA' 'Fecha'(fec) '' '' '',
               'S_GSTRS' 'Fecha ini.prog.'(fip) '' '' '',
               'S_AUDAT' 'Fecha pedido'(fep) '' '' '',
               'S_HORA'  'Hora'(hor) '' '' '',
               'S_MTART' 'Tipo material'(tpm) 'T134T' 'MTBEZ' '',
               'S_SERNR' 'Nº de serie'(nse) '' '' '',
               'S_QMNUM' 'Nº aviso'(nav) '' '' 'X',
               'S_ERDAT' 'Fecha creación'(fcr) '' '' '',
               'S_ERSDA' 'Fecha creación'(fcr) '' '' '',
               'S_EQUNR' 'Nº equipo'(neq) 'EQKT' 'EQKTX' 'X',
               'S_BLART' 'Clase documento'(cld) 'T003T' 'LTEXT' '',
               'S_SPWOC' 'Semana'(sem) '' '' '',
               'S_DISPO' 'Planificador necesidades'(pne) 'T024D' 'DSNAM' '',
               'S_EBELN' 'Documento compras'(dco) '' '' '',
               'S_DOKNR' 'Documento'(doc) '' '' '',
               'S_DOKST' 'Status documento'(std) '' '' '',
               'S_PERIO' 'Periodo'(per) '' '' '',
               'S_KUNNR' 'Cliente'(cli) 'KNA1' 'NAME1' 'X',
               'S_KUNAG' 'Solicitante'(sol) 'KNA1' 'NAME1' 'X',
               'S_KUNWE' 'Dest.Mercancía'(dsm) 'KNA1' 'NAME1' 'X',
               'S_VDATU' 'Fecha pref.entrega'(fpe) '' '' '',
               'S_GSTRP' 'Fecha inicio extrema'(fie) '' '' '',
               'S_AUARTO' 'Clase de orden'(cor) 'T003P' 'TXT' '',
               'S_FRGGR' 'Grupo de liberación'(glb) 'T16FH' 'FRGGT' '',
               'S_FRGSX' 'Estrategia liberación'(elb) 'T16FT' 'FRGXT' '',
               'S_BSARK' 'Clase de pedido'(cpe) 'T176T' 'VTEXT' '',
               'S_HKONT' 'Cuenta contable'(ccc) '' '' '',
               'S_KSTAR' 'Clase de coste'(cco) 'CSKU' 'KTEXT' '',
               'S_VKORG' 'Organización ventas'(ove) 'TVKOT' 'VTEXT' '',
               'S_VTWEG' 'Canal'(can) 'TVTWT' 'VTEXT' '',
               'S_FKART' 'Clase factura'(cfa) 'TVFKT' 'VTEXT' '',
               'S_FKDAT' 'Fecha factura'(ffa) '' '' '',
               'S_FAEDT' 'Fecha vencimiento'(fve) '' '' '',
               'S_ZLSCH' 'Vía de pago'(vpa) '' '' '',
               'S_QMART' 'Clase de aviso'(cla) 'TQ80_T' 'QMARTX' '',
               'S_ARBPL' 'Puesto de trabajo'(ptt) '' '' '',
               'S_STAWN' 'NºEstad.Mercancía'(nem) '' '' '',
               'S_EKGRP' 'Grupo de compras'(gco) 'T024' 'EKNAM' '',
               'S_BSART' 'Clase de pedido'(clp) '' '' '',
               'S_EXTWG' 'Grupo artículos externo'(gae) 'TWEWT' 'EWBEZ' ''.

    add_rango_c 'S_FRGCO' 'Código de liberación'(cli) 'T16FD' 'FRGCT' '' 'FRGCO'.

    add_rango: 'S_FRGOT' 'Tipo del objeto a liberar'(tlb) '' '' '',
               'S_KZEAR' 'Operación'(ope) '' '' '',
               'S_LGTKZ' 'In.Tipo Almacén'(ita) 'T305T' 'LTKZT' '',
               'S_LGTYP' 'Tipo almacén'(tal) 'T301T' 'LTYPT' '',
               'S_LGBKZ' 'In.área almacén'(ial) '' '' '',
               'S_LGBER' 'Área almacenamiento'(aal) '' '' '',
               'S_EXIDV' 'Unidad manipulación'(uma) '' '' '',
               'S_EINDT' 'Fecha de entrega'(fen) '' '' '',
               'S_EDATU' 'Fecha Reparto'(fre) '' '' ''.

    add_rango_c 'S_PSPID' 'Proyecto'(pry) 'PROJ' 'POST1' '' 'PSPID'.

    CONCATENATE l_report 'PN-BEGDA' INTO l_campo.
    ASSIGN (l_campo) TO <valor>.
    IF sy-subrc = 0.
      add_rango: 'R_PNPPERNR' 'Empleados'(emp) 'PA0001' 'ENAME' 'X',
                 'PNPABKRS' 'Área de nómina'(ano) '' '' '',
                 'PNPPERSK' 'Área de personal'(ape) 'T503T' 'PTEXT' '',
                 'PNPWERKS' 'División de personal'(dvp) 'T500P' 'NAME1' '',
                 'PNPBTRTL' 'Subdivisión de personal'(sdp) '' '' ''.

      add_parametro_2 'PN-BEGDA' 'PN-ENDDA' 'Fechas selección'(fse) '' '' ''.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    o_grid = NEW #( ).

    CLEAR fila.
  ENDMETHOD.
  METHOD crea_info_seleccion.
    DATA: l_filas  TYPE i,
          l_filtro TYPE zest_parm_sel,
          l_fila   TYPE zest_parm_sel,
          l_tabix  TYPE i,
          " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
          lr_label TYPE REF TO cl_salv_form_label.
*        l_string TYPE string,
*        o_grid_txt TYPE REF TO cl_salv_form_layout_grid.

    IF maximo_lineas <> 0.
      DELETE i_filtros FROM maximo_lineas. " Cómo máximo 6 filas de selección
    ENDIF.
    l_filas = LINES( i_filtros ).
    IF l_filas <= 0.
      RETURN.
    ENDIF.

    IF l_filas = 1.
      READ TABLE i_filtros INTO l_filtro-titulo INDEX 1.
      IF l_filtro = 'Filtros para ejecución:'.
        DELETE i_filtros INDEX 1.
        EXIT.
      ENDIF.
    ENDIF.

    l_filas = l_filas + 1.
    o_grid_sel = o_grid->create_grid( row = l_filas column = 1 colspan = 0 ).
*    o_grid_txt = o_grid->create_grid( row = l_filas column = 2 ).
    LOOP AT i_filtros INTO l_fila.
      l_tabix = sy-tabix.
*      CONCATENATE l_fila-titulo l_fila-valor INTO l_string SEPARATED BY space.
*      lr_label = o_grid_sel->create_label(
*        row     = sy-tabix
*        column  = 1
*        text    = l_string
*        tooltip = l_fila-titulo ).
      lr_label = o_grid_sel->create_label(
        row     = l_tabix
        column  = 1
        text    = l_fila-titulo
        tooltip = l_fila-titulo ).
      o_grid_sel->create_text(
        row     = l_tabix
        column  = 2
        text    = l_fila-valor
        tooltip = l_fila-titulo ).
    ENDLOOP.
  ENDMETHOD.
  METHOD get_grid.
    grid = o_grid.
  ENDMETHOD.
  METHOD set_accion.
    DATA l_tooltip TYPE string.

    IF tooltip IS INITIAL.
      l_tooltip = text.
    ELSE.
      l_tooltip = tooltip.
    ENDIF.

    IF row IS SUPPLIED.
      fila = row.
    ELSE.
      fila = fila + 1.
    ENDIF.

    o_grid->create_action_information( row = fila column = column text = text tooltip = l_tooltip ).
    o_grid->add_row( ).
  ENDMETHOD.
  METHOD set_linea.
    DATA: l_string TYPE string,
          lr_grid  TYPE REF TO cl_salv_form_layout_grid,
          " TODO: variable is assigned but never used (ABAP cleaner)
          lr_label TYPE REF TO cl_salv_form_label.

    l_string = texto.
    lr_grid = o_grid->create_grid( row = 1 column = 1 ).

    lr_label = lr_grid->create_label(
      row     = 1
      column  = 1
      text    = l_string
      tooltip = l_string ).
  ENDMETHOD.
  METHOD set_titulo.
    DATA l_tooltip TYPE string.

    IF tooltip IS INITIAL.
      l_tooltip = text.
    ELSE.
      l_tooltip = tooltip.
    ENDIF.

    IF row IS SUPPLIED.
      fila = row.
    ELSE.
      fila = fila + 1.
    ENDIF.

    o_grid->create_header_information( row = fila column = column text = text tooltip = l_tooltip colspan = colspan ).
    o_grid->add_row( ).
  ENDMETHOD.
