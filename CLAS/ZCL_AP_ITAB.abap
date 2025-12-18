class ZCL_AP_ITAB definition
  public
  create public .

public section.

  types: BEGIN OF zrnrangefieldname,
             sign   TYPE  ddsign,
             option TYPE  ddoption,
             low    TYPE  lvc_fname,
             high   TYPE  lvc_fname,
           END OF zrnrangefieldname .
  types zt_rangefieldname TYPE STANDARD TABLE OF zrnrangefieldname WITH key sign option low high .
  types:
    BEGIN OF t_dif,
        clave TYPE string,
        updkz TYPE updkz,
        t1    TYPE string,
        t2    TYPE string,
      END OF t_dif .
  types:
    tt_dif TYPE TABLE OF t_dif .

  class-methods GET_MAX_LINE
    importing
      !TABLE type TABLE
      !COLUMN type NAME_FELD
    exporting
      !LINE type ANY
      !MESSAGE type ANY .
  class-methods GET_MIN_LINE
    importing
      !TABLE type TABLE
      !COLUMN type NAME_FELD
    exporting
      !LINE type ANY
      !MESSAGE type ANY .
  class-methods GET_MAX_VAL
    importing
      !TABLE type TABLE
      !COLUMN type NAME_FELD
    exporting
      !VAL type ANY
      !MESSAGE type ANY .
  class-methods GET_MIN_VAL
    importing
      !TABLE type TABLE
      !COLUMN type NAME_FELD
    exporting
      !VAL type ANY
      !MESSAGE type ANY .
  class-methods COMPARA_TABLAS
    importing
      !T1 type TABLE
      !T2 type TABLE
      !QUITAR_CAMPOS type ANY default ''
      !SOLO_CAMPOS type ANY default ''
      !LONG_CLAVE type I default 0
    exporting
      value(DIF) type TT_DIF .
  class-methods CREA_TABLA_DINAMICA
    importing
      !TABLA type TABLE
      !QUITAR_CAMPOS type ANY default ''
      !SOLO_CAMPOS type ANY default ''
      !NUEVOS_CAMPOS type LVC_T_FCAT optional
    exporting
      !NUEVA_TABLA type TABLE
      !NUEVA_TABLA_DATA type ref to DATA .
  class-methods GET_LISTA_CAMPO
    importing
      !LISTA type ANY
    returning
      value(CAMPOS) type TABLE_OF_STRINGS .
  class-methods GET_RANGO_CAMPOS
    importing
      !LISTA type ANY
    returning
      value(RANGO) type ZT_RANGEFIELDNAME .
  class-methods COPIAR_TABLA
    importing
      !TABLA type TABLE
      !QUITAR_CAMPOS type ANY default ''
      !SOLO_CAMPOS type ANY default ''
    exporting
      !NUEVA_TABLA type TABLE
      !NUEVA_TABLA_DATA type ref to DATA .
  class-methods TABLAS_IGUALES
    importing
      !T1 type TABLE
      !T2 type TABLE
      !QUITAR_CAMPOS type ANY default ''
      !SOLO_CAMPOS type ANY default ''
      !ORDENAR_POR type ANY default ''
    returning
      value(IGUALES) type ABAP_BOOL .
  class-methods COMPARA_TABLAS_C
    importing
      !LONG_CLAVE type I default 0
      !NO_CAMPOS type ANY
    changing
      !DIF type TABLE
      !T1 type TABLE
      !T2 type TABLE .
  class-methods MOVER_CAMPOS_NO_VACIOS
    importing
      !ORIGEN type ANY
    changing
      !DESTINO type ANY .
  class-methods LIBERAR_MEMORIA
    changing
      !CH_T_TAB type TABLE .
protected section.
private section.
endclass. "ZCL_AP_ITAB definition
class ZCL_AP_ITAB implementation.
method COMPARA_TABLAS.
  DATA: t1_data TYPE REF TO data,
        l1_data TYPE REF TO data,
        t2_data TYPE REF TO data,
        l2_data TYPE REF TO data,
        l_dif type t_dif.

  FIELD-SYMBOLS: <n_tt1> TYPE table,
                 <t1> TYPE ANY,
                 <n_t1> TYPE ANY,
                 <n_tt2> TYPE table,
                 <t2> TYPE ANY,
                 <n_t2> TYPE ANY.

  IF t1 = t2.
    EXIT.
  ELSE.
    copiar_tabla( EXPORTING tabla = t1
                            quitar_campos    = quitar_campos
                            solo_campos      = solo_campos
                  IMPORTING nueva_tabla_data = t1_data ).

    ASSIGN t1_data->* TO <n_tt1>.
    CHECK SY-SUBRC = 0.

    copiar_tabla( EXPORTING tabla = t2
                            quitar_campos    = quitar_campos
                            solo_campos      = solo_campos
                  IMPORTING nueva_tabla_data = t2_data ).

    ASSIGN t2_data->* TO <n_tt2>.
    CHECK SY-SUBRC = 0.

    SORT: <n_tt1>, <n_tt2>.

    IF <n_tt1> = <n_tt2>.
      EXIT.
    ELSE.
      SORT: <n_tt1>, <n_tt2>.
      LOOP AT <n_tt1> ASSIGNING <n_t1>.
        LOOP AT <n_tt2> ASSIGNING <n_t2>.
          IF <n_t1> = <n_t2>.
            DELETE: <n_tt1>, <n_tt2>.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      CLEAR dif.
      LOOP AT <n_tt1> ASSIGNING <n_t1>.
        l_dif-t1 = <n_t1>.
        append l_dif to dif.
      ENDLOOP.
      LOOP AT <n_tt2> ASSIGNING <n_t2>.
        l_dif-t2 = <n_t2>.
        append l_dif to dif.
      ENDLOOP.

    ENDIF.
  ENDIF.


*¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿¿???????????????
*  DATA: t1_data TYPE REF TO data,
*        l1_data TYPE REF TO data,
*        t2_data TYPE REF TO data,
*        l2_data TYPE REF TO data.
*
*  FIELD-SYMBOLS: <n_tt1> TYPE table,
*                 <t1> TYPE ANY,
*                 <n_t1> TYPE ANY,
*                 <n_tt2> TYPE table,
*                 <t2> TYPE ANY,
*                 <n_t2> TYPE ANY.
*
*  IF t1 = t2.
*    EXIT.
*  ELSE.
*    copiar_tabla( EXPORTING tabla = t1
*                            quitar_campos    = quitar_campos
*                            solo_campos      = solo_campos
*                  IMPORTING nueva_tabla_data = t1_data ).
*
*    ASSIGN t1_data->* TO <n_tt1>.
*
*    CREATE DATA l1_data LIKE LINE OF <n_tt1>.
*
*    ASSIGN l1_data->* TO <n_t1>.
*
*    LOOP AT t1 ASSIGNING <t1>.
*      MOVE-CORRESPONDING <t1> TO <n_t1>.
*      APPEND <n_t1> TO <n_tt1>.
*    ENDLOOP.
*
*    copiar_tabla( EXPORTING tabla = t2
*                            quitar_campos    = quitar_campos
*                            solo_campos      = solo_campos
*                  IMPORTING nueva_tabla_data = t2_data ).
*
*    ASSIGN t2_data->* TO <n_tt2>.
*
*    CREATE DATA l2_data LIKE LINE OF <n_tt2>.
*
*    ASSIGN l2_data->* TO <n_t1>.
*
*    LOOP AT t2 ASSIGNING <t2>.
*      MOVE-CORRESPONDING <t2> TO <n_t2>.
*      APPEND <n_t2> TO <n_tt2>.
*    ENDLOOP.
*
*    SORT: <n_tt1>, <n_tt2>.
*
*    IF <n_tt1> = <n_tt2>.
*      EXIT.
*    ELSE.
*      LOOP AT <n_tt1> ASSIGNING <n_t1>.
*        LOOP AT <n_tt2> ASSIGNING <n_t2>.
*          IF <n_t1> = <n_t2>.
*            DELETE: <n_tt1>, <n_tt2>.
*          ENDIF.
*        ENDLOOP.
*      ENDLOOP.
*
*      clear dif.
*
*    ENDIF.
*  ENDIF.
endmethod.
method COMPARA_TABLAS_C.
  DATA: i_campos TYPE TABLE OF string,
        l_campo TYPE string,
        l_valor type string.

  FIELD-SYMBOLS: <t1> TYPE ANY,
                 <t2> TYPE ANY,
                 <fs> TYPE ANY.

  i_campos = get_lista_campo( no_campos ).
  LOOP AT i_campos INTO l_campo.
    LOOP AT t1 ASSIGNING <t1>.
      CONCATENATE '<T1>-' l_campo INTO l_valor.
      ASSIGN (l_valor) TO <fs>.
      IF sy-subrc = 0.
        CLEAR <fs>.
      ENDIF.
    ENDLOOP.
    LOOP AT t2 ASSIGNING <t2>.
      CONCATENATE '<T2>-' l_campo INTO l_valor.
      ASSIGN (l_valor) TO <fs>.
      IF sy-subrc = 0.
        CLEAR <fs>.
      ENDIF.
    ENDLOOP.
  ENDLOOP.


  SORT: t1, t2.
  IF t1 = t2.
    REFRESH: t1, t2, dif.
    EXIT.
  ELSE.
    LOOP AT t1 ASSIGNING <t1>.
      LOOP AT t2 ASSIGNING <t2>.
        IF <t1> = <t2>.
          DELETE: t1, t2.
          EXIT.
        ELSEIF NOT long_clave IS INITIAL.
          IF <t1>(long_clave) = <t2>(long_clave).
            APPEND <t1> TO dif.
            DELETE: t1, t2.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

endmethod.
METHOD copiar_tabla.
  DATA: nueva_linea TYPE REF TO data,
        l_campos    TYPE string.

  FIELD-SYMBOLS: <linea>       TYPE any,
                 <nueva_linea> TYPE any,
                 <nueva_tabla> TYPE table,
                 <fs>          TYPE any.


  IF solo_campos = '!SOLO_INFORMADOS'.
    DATA(it_fieldcat)  = zcl_ap_dev=>get_fieldcatalog_tabla_alv( tabla ).

    LOOP AT it_fieldcat ASSIGNING FIELD-SYMBOL(<comp>) WHERE fieldname NE ''.
      DATA(l_valor) = ''.
      LOOP AT tabla ASSIGNING FIELD-SYMBOL(<tabla>).
        ASSIGN COMPONENT <comp>-fieldname OF STRUCTURE <tabla> TO <fs>.
        IF sy-subrc = 0.
          IF NOT <fs> IS INITIAL.
            l_valor = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF l_valor = 'X'.
        IF l_campos IS INITIAL.
          l_campos = <comp>-fieldname.
        ELSE.
          CONCATENATE l_campos <comp>-fieldname INTO l_campos SEPARATED BY ','.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    l_campos = solo_campos.
  ENDIF.

  crea_tabla_dinamica( EXPORTING tabla = tabla
                                 quitar_campos = quitar_campos
                                 solo_campos   = l_campos
                       IMPORTING nueva_tabla = nueva_tabla
                                 nueva_tabla_data = nueva_tabla_data ).

  ASSIGN nueva_tabla_data->* TO <nueva_tabla> .
  CHECK sy-subrc = 0.

  CREATE DATA nueva_linea LIKE LINE OF <nueva_tabla>.
  ASSIGN nueva_linea->* TO <nueva_linea>.
  CHECK sy-subrc = 0.


  LOOP AT tabla ASSIGNING <linea>.
    MOVE-CORRESPONDING <linea> TO <nueva_linea>.
    APPEND <nueva_linea> TO <nueva_tabla>.
  ENDLOOP.

ENDMETHOD.
METHOD crea_tabla_dinamica.
  DATA: i_campos        TYPE ddfields,
        l_campos        TYPE dfies,
        wa_fieldcat     TYPE lvc_s_fcat,
        it_fieldcat     TYPE lvc_t_fcat,
        r_campos_quitar TYPE zt_rangefieldname, "INI 20220908 POST Ajustes UPGRADE
        r_solo_campos   TYPE zt_rangefieldname. "INI 20220908 POST Ajustes UPGRADE

  FIELD-SYMBOLS <tabla> TYPE table.
*  i_campos = zcl_ap_dev=>get_fieldcatalog_ddic( tabla ).

  it_fieldcat  = zcl_ap_dev=>get_fieldcatalog_tabla_alv( tabla ).

  r_campos_quitar = get_rango_campos( quitar_campos ).
  IF NOT solo_campos IS INITIAL.
    r_solo_campos = get_rango_campos( solo_campos ).
  ENDIF.


*  LOOP AT i_campos INTO l_campos WHERE NOT fieldname IN r_campos_quitar
*                                   AND fieldname IN r_solo_campos
*                                   AND inttype NE 'h'. "No podemos crear tabla con campos tipo tabla
*    CLEAR wa_fieldcat.
*    MOVE-CORRESPONDING l_campos TO wa_fieldcat.
*    APPEND wa_fieldcat TO it_fieldcat.
*  ENDLOOP.

  DELETE it_fieldcat WHERE fieldname IN r_campos_quitar.
  DELETE it_fieldcat WHERE NOT fieldname IN r_solo_campos.

  DELETE it_fieldcat WHERE inttype = 'y'. "Los binarios dan problemas

  CHECK NOT it_fieldcat[] IS INITIAL.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = it_fieldcat
    IMPORTING
      ep_table                  = nueva_tabla_data
    EXCEPTIONS
      generate_subpool_dir_full = 1.
  IF sy-subrc = 0.
    ASSIGN nueva_tabla_data->* TO <tabla>.
  ENDIF.

*  TRY.
*      nueva_tabla = <tabla>.
*    CATCH cx_root.
*  ENDTRY.


ENDMETHOD.
method GET_LISTA_CAMPO.
  FIELD-SYMBOLS <campo> TYPE string.

  SPLIT lista AT ',' INTO table campos.
  LOOP AT campos ASSIGNING <campo>.
    TRANSLATE <campo> TO UPPER CASE.
    CONDENSE <campo> NO-GAPS.
  ENDLOOP.

endmethod.
method GET_MAX_LINE.
**********************************************************************
* Author: Bruno Esperança
* Description: Get the line with the maximum value of a certain field
**********************************************************************

  FIELD-SYMBOLS:
        <lf_table> TYPE STANDARD TABLE,
        <lf_line> TYPE ANY,
        <lf_field> TYPE ANY,
        <lf_max> TYPE ANY.

  DATA:
        lo_descr  TYPE REF TO cl_abap_structdescr,
        lv_col    TYPE name_feld,
        lo_error  TYPE REF TO cx_root,
        lv_max    TYPE p.

  CLEAR message.

  "Make sure the field is in upper case
  lv_col = column.
  TRANSLATE lv_col TO UPPER CASE.

  LOOP AT table ASSIGNING <lf_line>.

    "I would like a more elegant way to get the line
    IF lo_descr IS NOT BOUND.
      lo_descr ?= cl_abap_typedescr=>describe_by_data( <lf_line> ).
    ENDIF.

    "Find the field we are interested in
    READ TABLE lo_descr->components TRANSPORTING NO FIELDS
      WITH KEY name = lv_col.
    IF sy-subrc <> 0.
      "Raise column not found
      CONCATENATE 'No existe la columna' lv_col INTO message SEPARATED BY space.
    ENDIF.

    ASSIGN COMPONENT sy-tabix
      OF STRUCTURE <lf_line> TO <lf_field>.
    IF sy-subrc <> 0.
      "This should never happen
      EXIT.
    ELSE.

      TRY.
          IF lv_max IS INITIAL
            OR <lf_field> > lv_max.

            "Save max value and line
            line = <lf_line>.
            lv_max = <lf_field>.

          ENDIF.

        CATCH cx_sy_conversion_no_number INTO lo_error.
          message = 'Error en conversión'.
      ENDTRY.
    ENDIF.

  ENDLOOP.

endmethod.
method GET_MAX_VAL.
**********************************************************************
* Author: Bruno Esperança
* Description: Get the maximum value of a certain field
**********************************************************************

  FIELD-SYMBOLS:
        <lf_table> TYPE STANDARD TABLE,
        <lf_line> TYPE any,
        <lf_field> TYPE any,
        <lf_max> TYPE any.

  DATA:
        lo_descr  TYPE REF TO cl_abap_structdescr,
        lv_col    TYPE name_feld,
        lo_error  TYPE REF TO cx_root.

  "Make sure the field is in upper case
  lv_col = column.
  TRANSLATE lv_col TO UPPER CASE.

  LOOP AT table ASSIGNING <lf_line>.

    "I would like a more elegant way to get the line
    IF lo_descr IS NOT BOUND.
      lo_descr ?= cl_abap_typedescr=>describe_by_data( <lf_line> ).
    ENDIF.

    "Find the field we are interested in
    READ TABLE lo_descr->components TRANSPORTING NO FIELDS
      WITH KEY name = lv_col.
    IF sy-subrc <> 0.
      "Raise column not found
      CONCATENATE 'No existe la columna' lv_col into message SEPARATED BY space.
    ENDIF.

    ASSIGN COMPONENT sy-tabix
      OF STRUCTURE <lf_line> TO <lf_field>.
    IF sy-subrc <> 0.
      "This should never happen
      EXIT.
    ELSE.

      TRY.
          IF val IS INITIAL
            OR <lf_field> > val.

            "Save max value
            val = <lf_field>.

          ENDIF.

        CATCH cx_sy_conversion_no_number INTO lo_error.

          message = 'Error en conversion'.

      ENDTRY.
    ENDIF.

  ENDLOOP.

endmethod.
method GET_MIN_LINE.
**********************************************************************
* Author: Bruno Esperança
* Description: Get the line with the minimum value of a certain field
**********************************************************************

  FIELD-SYMBOLS:
        <lf_table> TYPE STANDARD TABLE,
        <lf_line> TYPE any,
        <lf_field> TYPE any,
        <lf_max> TYPE any.

  DATA:
        lo_descr  TYPE REF TO cl_abap_structdescr,
        lv_col    TYPE name_feld,
        lo_error  TYPE REF TO cx_root,
        lv_max    TYPE p.

  "Make sure the field is in upper case
  lv_col = column.
  TRANSLATE lv_col TO UPPER CASE.

  LOOP AT table ASSIGNING <lf_line>.

    "I would like a more elegant way to get the line struct
    IF lo_descr IS NOT BOUND.
      lo_descr ?= cl_abap_typedescr=>describe_by_data( <lf_line> ).
    ENDIF.

    "Find the field we are interested in
    READ TABLE lo_descr->components TRANSPORTING NO FIELDS
      WITH KEY name = lv_col.
    IF sy-subrc <> 0.
      "Raise column not found
      CONCATENATE 'No existe la columna' lv_col into message SEPARATED BY space.
    ENDIF.

    ASSIGN COMPONENT sy-tabix
      OF STRUCTURE <lf_line> TO <lf_field>.
    IF sy-subrc <> 0.
      "This should never happen
      EXIT.
    ELSE.

      TRY.
          IF lv_max IS INITIAL
            OR <lf_field> < lv_max.

            "Save min value and line
            line = <lf_line>.
            lv_max = <lf_field>.

          ENDIF.

        CATCH cx_sy_conversion_no_number INTO lo_error.
          message = 'Error en conversion'.
      ENDTRY.
    ENDIF.

  ENDLOOP.

endmethod.
method GET_MIN_VAL.
**********************************************************************
* Author: Bruno Esperança
* Description: Get the minimum value of a certain field
**********************************************************************

  FIELD-SYMBOLS:
        <lf_table> TYPE STANDARD TABLE,
        <lf_line> TYPE any,
        <lf_field> TYPE any,
        <lf_max> TYPE any.

  DATA:
        lo_descr  TYPE REF TO cl_abap_structdescr,
        lv_col    TYPE name_feld,
        lo_error  TYPE REF TO cx_root.

  "Make sure the field is in upper case
  lv_col = column.
  TRANSLATE lv_col TO UPPER CASE.

  LOOP AT table ASSIGNING <lf_line>.

    "I would like a more elegant way to get the line
    IF lo_descr IS NOT BOUND.
      lo_descr ?= cl_abap_typedescr=>describe_by_data( <lf_line> ).
    ENDIF.

    "Find the field we are interested in
    READ TABLE lo_descr->components TRANSPORTING NO FIELDS
      WITH KEY name = lv_col.
    IF sy-subrc <> 0.
      "Raise column not found
      CONCATENATE 'No existe la columna' lv_col into message SEPARATED BY space.
    ENDIF.

    ASSIGN COMPONENT sy-tabix
      OF STRUCTURE <lf_line> TO <lf_field>.
    IF sy-subrc <> 0.
      "This should never happen
      EXIT.
    ELSE.

      TRY.
          IF val IS INITIAL
            OR <lf_field> < val.

            "Save min value
            val = <lf_field>.

          ENDIF.

        CATCH cx_sy_conversion_no_number INTO lo_error.

          message = 'Error en conversion'.

      ENDTRY.
    ENDIF.

  ENDLOOP.

endmethod.
method GET_RANGO_CAMPOS.
  DATA: lr_field TYPE zrnrangefieldname,
        i_campos TYPE table_of_strings.
  FIELD-SYMBOLS <string> TYPE string.



  CLEAR lr_field.
  lr_field-option = 'EQ'.
  lr_field-sign   = 'I'.
  IF lista IS INITIAL.
    lr_field-low    = '?'.
    APPEND lr_field TO rango.
  ELSE.
    i_campos = get_lista_campo( lista ).
    LOOP AT i_campos ASSIGNING <string>.
      lr_field-low    = <string>.
      APPEND lr_field TO rango.
    ENDLOOP.
  ENDIF.

endmethod.
METHOD liberar_memoria.
    FIELD-SYMBOLS: <fs_table>    TYPE STANDARD TABLE,
                   <fs_line>     TYPE any,
                   <fs_tab_line> TYPE any.
    DATA: lo_struct_typ   TYPE REF TO cl_abap_structdescr,
          lo_dyntable_typ TYPE REF TO cl_abap_tabledescr,
          lt_dyntable     TYPE REF TO data,
          ls_dyntable     TYPE REF TO data.
    READ TABLE ch_t_tab INDEX 1
      ASSIGNING <fs_tab_line>.
    lo_struct_typ ?= cl_abap_typedescr=>describe_by_data( <fs_tab_line> ).
    lo_dyntable_typ = cl_abap_tabledescr=>create( p_line_type = lo_struct_typ ).
    CREATE DATA: lt_dyntable TYPE HANDLE lo_dyntable_typ,
                 ls_dyntable TYPE HANDLE lo_struct_typ.
    ASSIGN: ls_dyntable->* TO <fs_line>,
            lt_dyntable->* TO <fs_table>.
    LOOP AT ch_t_tab ASSIGNING <fs_tab_line>.
      MOVE-CORRESPONDING <fs_tab_line> TO <fs_line>.
      APPEND <fs_line> TO <fs_table>.
    ENDLOOP.
    FREE ch_t_tab.
    ch_t_tab[] = <fs_table>.

  ENDMETHOD.
METHOD mover_campos_no_vacios.
  DATA: o_campos_origen TYPE REF TO cl_abap_structdescr,
        o_campos_destino TYPE REF TO cl_abap_structdescr,
        i_campos_origen TYPE cl_abap_structdescr=>component_table,
        i_campos_destino TYPE cl_abap_structdescr=>component_table.

  FIELD-SYMBOLS: <campos_origen> TYPE cl_abap_structdescr=>component,
                 <campos_destino> TYPE cl_abap_structdescr=>component,
                 <origen> TYPE ANY,
                 <destino> TYPE ANY.

  o_campos_origen ?= cl_abap_structdescr=>describe_by_data( origen ).
  i_campos_origen = o_campos_origen->get_components( ).

  o_campos_destino ?= cl_abap_structdescr=>describe_by_data( destino ).
  i_campos_destino = o_campos_destino->get_components( ).

  LOOP AT i_campos_origen ASSIGNING <campos_origen>.
    ASSIGN COMPONENT <campos_origen>-name OF STRUCTURE origen TO <origen>.
    IF sy-subrc = 0.
      IF NOT <origen> IS INITIAL.
        READ TABLE i_campos_destino TRANSPORTING NO FIELDS WITH KEY name = <campos_origen>-name.
        IF sy-subrc = 0.
          ASSIGN COMPONENT <campos_origen>-name OF STRUCTURE destino TO <destino>.
          IF sy-subrc = 0.
            IF <destino> IS INITIAL.
              <destino> = <origen>.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMETHOD.
METHOD tablas_iguales.
  DATA: t1_data TYPE REF TO data,
        l1_data TYPE REF TO data,
        t2_data TYPE REF TO data,
        l2_data TYPE REF TO data,
        lines1  TYPE i,
        lines2  TYPE i.

  FIELD-SYMBOLS: <n_tt1> TYPE table,
                 <t1>    TYPE any,
                 <n_t1>  TYPE any,
                 <n_tt2> TYPE table,
                 <t2>    TYPE any,
                 <n_t2>  TYPE any.

  DESCRIBE TABLE t1 LINES lines1.
  DESCRIBE TABLE t2 LINES lines2.

  IF lines1 NE lines2.
    EXIT.
  ENDIF.

  IF t1 = t2.
    iguales = 'X'.
    EXIT.
  ELSE.
    copiar_tabla( EXPORTING tabla = t1
                            quitar_campos    = quitar_campos
                            solo_campos      = solo_campos
                  IMPORTING nueva_tabla_data = t1_data ).

    ASSIGN t1_data->* TO <n_tt1>.
    CHECK sy-subrc = 0.

    CREATE DATA l1_data LIKE LINE OF <n_tt1>.

    ASSIGN l1_data->* TO <n_t1>.
    CHECK sy-subrc = 0.

    copiar_tabla( EXPORTING tabla = t2
                            quitar_campos    = quitar_campos
                            solo_campos      = solo_campos
                  IMPORTING nueva_tabla_data = t2_data ).

    ASSIGN t2_data->* TO <n_tt2>.
    CHECK sy-subrc = 0.

    CREATE DATA l2_data LIKE LINE OF <n_tt2>.

    ASSIGN l2_data->* TO <n_t1>.
    CHECK sy-subrc = 0.

    IF ordenar_por IS INITIAL.
      SORT: <n_tt1>, <n_tt2>.
    ELSE.
      SORT: <n_tt1> BY (ordenar_por).
      SORT: <n_tt2> BY (ordenar_por).
    ENDIF.

    IF <n_tt1> = <n_tt2>.
      iguales = 'X'.
    ENDIF.
  ENDIF.

ENDMETHOD.
