CLASS zcl_ap_rango DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF range_c3,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE char3,
        high   TYPE char3,
      END OF range_c3.
    TYPES tab_range_c3 TYPE STANDARD TABLE OF range_c3 WITH KEY sign option low high.
    TYPES:
      BEGIN OF range_c4,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE char4,
        high   TYPE char4,
      END OF range_c4.
    TYPES tab_range_c4 TYPE STANDARD TABLE OF range_c4.
    TYPES:
      BEGIN OF range_c10,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE char10,
        high   TYPE char10,
      END OF range_c10.
    TYPES tab_range_c10 TYPE STANDARD TABLE OF range_c10.
    TYPES:
      BEGIN OF range_c20,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE char20,
        high   TYPE char20,
      END OF range_c20.
    TYPES tab_range_c20 TYPE STANDARD TABLE OF range_c20.
    TYPES:
      BEGIN OF range_c12,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE char12,
        high   TYPE char12,
      END OF range_c12.
    TYPES tab_range_c12 TYPE STANDARD TABLE OF range_c12.
    TYPES:
      BEGIN OF range_c30,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE char30,
        high   TYPE char30,
      END OF range_c30.
    TYPES tab_range_c30 TYPE STANDARD TABLE OF range_c30.

    TYPES:
      BEGIN OF range_n10,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE numc10,
        high   TYPE numc10,
      END OF range_n10.
    TYPES tab_range_n10 TYPE STANDARD TABLE OF range_n10.

    DATA rango        TYPE rstt_t_range_string.
    DATA rango_fechas TYPE tpmy_r_date.

    METHODS constructor
      IMPORTING rango_inicial             TYPE table     OPTIONAL
                valor_inicial             TYPE any       OPTIONAL
                excluir                   TYPE abap_bool DEFAULT ''
                filtrar_por_valor_inicial TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER rango_inicial.

    CLASS-METHODS set_eq_st
      IMPORTING valor      TYPE any
                !option    TYPE tvarv_opti DEFAULT 'EQ'
                !sign      TYPE tvarv_sign DEFAULT 'I'
                no_initial TYPE abap_bool  DEFAULT ''
      CHANGING  t_rango    TYPE rstt_t_range_string.

    METHODS set_eq
      IMPORTING valor     TYPE any
                !option   TYPE tvarv_opti DEFAULT 'EQ'
                !sign     TYPE tvarv_sign DEFAULT 'I'
                es_lista  TYPE abap_bool  DEFAULT ''
                separador TYPE any        DEFAULT ','.

    CLASS-METHODS set_bt_st
      IMPORTING !low    TYPE any
                !high   TYPE any        OPTIONAL
                !option TYPE tvarv_opti DEFAULT 'BT'
                !sign   TYPE tvarv_sign DEFAULT 'I'
      CHANGING  t_rango TYPE rstt_t_range_string.

    METHODS set_bt
      IMPORTING !low    TYPE any
                !high   TYPE any        OPTIONAL
                !option TYPE tvarv_opti DEFAULT 'BT'
                !sign   TYPE tvarv_sign DEFAULT 'I'.

    METHODS transform
      IMPORTING campo TYPE any.

    METHODS free.

    METHODS assign
      EXPORTING t_rango TYPE table.

    CLASS-METHODS set_eq_str
      IMPORTING valor          TYPE any        OPTIONAL
                !option        TYPE tvarv_opti DEFAULT 'EQ'
                !sign          TYPE tvarv_sign DEFAULT 'I'
                no_initial     TYPE abap_bool  DEFAULT 'X'
      PREFERRED PARAMETER valor
      RETURNING VALUE(t_rango) TYPE rstt_t_range_string.

    CLASS-METHODS set_eq_n10
      IMPORTING valor      TYPE any
                !option    TYPE tvarv_opti DEFAULT 'EQ'
                !sign      TYPE tvarv_sign DEFAULT 'I'
                no_initial TYPE abap_bool  DEFAULT ''
      CHANGING  t_rango    TYPE lxhme_range_n10_t.

    CLASS-METHODS set_eq_n10c
      IMPORTING valor          TYPE any        OPTIONAL
                !option        TYPE tvarv_opti DEFAULT 'EQ'
                !sign          TYPE tvarv_sign DEFAULT 'I'
                no_initial     TYPE abap_bool  DEFAULT ''
      PREFERRED PARAMETER valor
      RETURNING VALUE(t_rango) TYPE lxhme_range_n10_t.

    METHODS get
      IMPORTING t_rango TYPE table.

    CLASS-METHODS get_tabla_campo
      IMPORTING campo       TYPE any
                campo_check TYPE any        DEFAULT 'CHECK'
                tabla       TYPE table
                valor_check TYPE any        DEFAULT 'X'
                !option     TYPE tvarv_opti DEFAULT 'EQ'
                !sign       TYPE tvarv_sign DEFAULT 'I'
      CHANGING  rango       TYPE table.

    CLASS-METHODS get_descripcion_rango
      IMPORTING rango         TYPE table     OPTIONAL
                quitar_ceros  TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER rango
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS get_valor
      IMPORTING rango           TYPE table
                popup           TYPE abap_bool DEFAULT ''
                error_si_varios TYPE abap_bool DEFAULT ''
      RETURNING VALUE(valor)    TYPE string.

    CLASS-METHODS rango_string_to_n10
      IMPORTING rango            TYPE rstt_t_range_string
      RETURNING VALUE(rango_n10) TYPE lxhme_range_n10_t.

    CLASS-METHODS interseccion
      IMPORTING rango1             TYPE table
                rango2             TYPE table
      EXPORTING exclusion          TYPE abap_bool
                cumple             TYPE abap_bool
                rango_interseccion TYPE rsdd_t_range
                !intersect         TYPE cl_rsdrc_seldr_srvs=>tn_intersect.

    CLASS-METHODS tabla_n10_to_rango_cond
      IMPORTING tabla        TYPE any
      RETURNING VALUE(rango) TYPE lxhme_range_n10_t.

  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_RANGO definition
class ZCL_AP_RANGO implementation.
  METHOD assign.
    DATA: lr_data TYPE REF TO data,
          l_rango TYPE rstt_s_range_string.

    FIELD-SYMBOLS <fs> TYPE any.

    CREATE DATA lr_data LIKE LINE OF t_rango.

    ASSIGN lr_data->* TO <fs>.

    CLEAR t_rango.
    LOOP AT rango INTO l_rango.
      MOVE-CORRESPONDING l_rango TO <fs>.
      APPEND <fs> TO t_rango.
    ENDLOOP.
  ENDMETHOD.
  METHOD constructor.
    IF NOT rango_inicial IS INITIAL.
      get( rango_inicial ).
    ENDIF.

    IF excluir IS INITIAL.
      IF    NOT valor_inicial             IS INITIAL
         OR     filtrar_por_valor_inicial  = 'X'.
        set_eq( valor_inicial ).
      ENDIF.
    ELSE.
      set_eq( valor = valor_inicial sign = 'E' ).
    ENDIF.
  ENDMETHOD.
  METHOD free.
    CLEAR: rango,
           rango_fechas.
  ENDMETHOD.
  METHOD get.
    DATA l_rango TYPE rstt_s_range_string.

    FIELD-SYMBOLS <rango> TYPE any.

    LOOP AT t_rango ASSIGNING <rango>.
      MOVE-CORRESPONDING <rango> TO l_rango.
      APPEND l_rango TO rango.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_descripcion_rango.
    DATA: l_hay_hasta TYPE c LENGTH 1,
          l_valor     TYPE c LENGTH 40,
          l_hasta     TYPE c LENGTH 20.

    FIELD-SYMBOLS: <fs>   TYPE any,
                   <low>  TYPE any,
                   <high> TYPE any.

    LOOP AT rango ASSIGNING <fs>.
      CLEAR l_hay_hasta.
      ASSIGN ('<FS>-LOW') TO <low>.
      IF sy-subrc = 0.
        WRITE <low> TO l_valor. "#EC *
        IF quitar_ceros = 'X'.
          zcl_ap_string=>quitar_ceros_c( CHANGING  cadena = l_valor ).
        ENDIF.
      ENDIF.

      ASSIGN ('<FS>-HIGH') TO <high>.
      IF sy-subrc = 0.
        IF NOT <high> IS INITIAL.
          WRITE <high> TO l_hasta. "#EC *

          IF quitar_ceros = 'X'.
            zcl_ap_string=>quitar_ceros_c( CHANGING  cadena = l_hasta ).
          ENDIF.

          CONCATENATE 'Desde'(des) l_valor 'hasta'(has) l_hasta INTO string
                      SEPARATED BY space.
          l_hay_hasta = 'X'.
        ENDIF.
      ENDIF.

      IF l_hay_hasta = 'X'.
        EXIT.
      ELSE.
        IF string IS INITIAL.
          string = l_valor.
        ELSE.
          CONCATENATE string ',' l_valor INTO string.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_tabla_campo.
    DATA l_rango TYPE REF TO data.

    FIELD-SYMBOLS: <rango>  TYPE any,
                   <opcion> TYPE any,
                   <sign>   TYPE any,
                   <linea>  TYPE any,
                   <check>  TYPE any,
                   <campo>  TYPE any,
                   <low>    TYPE any.

    CREATE DATA l_rango LIKE LINE OF rango.

    ASSIGN l_rango->* TO <rango>.

    ASSIGN ('<RANGO>-OPTION') TO <opcion>.
    IF sy-subrc = 0.
      <opcion> = option.
    ENDIF.

    ASSIGN ('<RANGO>-SIGN') TO <sign>.
    IF sy-subrc = 0.
      <sign> = sign.
    ENDIF.

    LOOP AT tabla ASSIGNING <linea>.

      IF NOT campo_check IS INITIAL.
        ASSIGN COMPONENT campo_check OF STRUCTURE <linea> TO <check>.
        IF sy-subrc = 0.
          IF <check> <> valor_check.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT campo OF STRUCTURE <linea> TO <campo>.
      IF sy-subrc = 0.
        ASSIGN ('<RANGO>-LOW') TO <low>.
        IF sy-subrc = 0.
          <low> = <campo>.
          COLLECT <rango> INTO rango.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
  METHOD get_valor.
    DATA: l_lista TYPE spopli,
          i_lista TYPE ccseq_t_values.

    FIELD-SYMBOLS: <rango> TYPE any,
                   <valor> TYPE any.

    CLEAR valor.

    IF popup IS INITIAL AND error_si_varios IS INITIAL.
      ASSIGN rango[ 1 ] TO <rango>.
      IF sy-subrc = 0.
        ASSIGN ('<RANGO>-LOW') TO <valor>.
        IF sy-subrc = 0.
          valor = <valor>.
        ENDIF.
      ENDIF.
    ELSE.
      LOOP AT rango ASSIGNING <rango>.
        ASSIGN ('<RANGO>-LOW') TO <valor>.
        IF sy-subrc = 0.
          IF NOT <valor> IS INITIAL.
            l_lista-varoption = <valor>.
            COLLECT l_lista INTO i_lista.
          ENDIF.
        ENDIF.
        ASSIGN ('<RANGO>-HIGH') TO <valor>.
        IF sy-subrc = 0.
          IF NOT <valor> IS INITIAL.
            l_lista-varoption = <valor>.
            COLLECT l_lista INTO i_lista.
          ENDIF.
        ENDIF.
      ENDLOOP.
      DESCRIBE TABLE i_lista LINES sy-tfill.
      IF sy-tfill = 1.
        READ TABLE i_lista INTO l_lista INDEX 1.
        valor = l_lista-varoption.
      ELSEIF sy-tfill > 1.
        IF popup = 'X'.
          valor = zcl_ap_popup=>lista_opciones( titulo = 'Seleccione valor'(slv) i_lista = i_lista ).
        ELSE.
          valor = '!ERROR!'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD interseccion.
    DATA: l_rango        TYPE rrrangesid,
          ld_i_t_range_a TYPE rsdd_t_range,
          ld_i_t_range_b TYPE rsdd_t_range,
          ld_i_chanm     TYPE rsd_chanm,
          " TODO: variable is assigned but never used (ABAP cleaner)
          ld_e_keyfl1    TYPE rs_bool.

    FIELD-SYMBOLS: <option_r1> TYPE any,
                   <option_r2> TYPE any.

    CLEAR: cumple,
           rango_interseccion.
    IF rango1 IS INITIAL OR rango2 IS INITIAL.
      cumple = 'X'.
      RETURN.
    ENDIF.

    LOOP AT rango1 ASSIGNING FIELD-SYMBOL(<r1>).
      MOVE-CORRESPONDING <r1> TO l_rango.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <r1> TO <option_r1>.
      l_rango-opt   = <option_r1>.
      l_rango-keyfl = 'X'.
      APPEND l_rango TO ld_i_t_range_a.
    ENDLOOP.

    LOOP AT rango2 ASSIGNING FIELD-SYMBOL(<r2>).
      MOVE-CORRESPONDING <r2> TO l_rango.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <r1> TO <option_r2>.
      l_rango-opt   = <option_r2>.
      l_rango-keyfl = 'X'.
      APPEND l_rango TO ld_i_t_range_b.
    ENDLOOP.

    CALL FUNCTION 'RRK_RANGE_INTERSECT'
      EXPORTING
        i_chanm      = ld_i_chanm
        i_check_is   = 'X'
*       i_keyfl1_a   = ld_i_keyfl1_a
        i_t_range_a  = ld_i_t_range_a
        i_t_range_b  = ld_i_t_range_b
      IMPORTING
        e_t_range    = rango_interseccion
        e_keyfl1     = ld_e_keyfl1
        e_intersect  = intersect
      EXCEPTIONS
        not_possible = 1
        empty        = 2
        x_message    = 3.
    IF sy-subrc = 0.
      LOOP AT rango_interseccion ASSIGNING FIELD-SYMBOL(<rango>) WHERE sign = 'E'. "#EC *
        IF line_exists( rango_interseccion[ sign = 'I' opt = <rango>-opt low = <rango>-low high = <rango>-high ] ). "#EC *
          exclusion = 'X'.
          RETURN.
        ENDIF.
      ENDLOOP.

      IF NOT rango_interseccion IS INITIAL.
        cumple = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD rango_string_to_n10.
    DATA l_rangon10 TYPE lxhme_range_n10.

    FIELD-SYMBOLS <rango> TYPE rstt_s_range_string.

    LOOP AT rango ASSIGNING <rango>.
      CLEAR l_rangon10.
      MOVE-CORRESPONDING <rango> TO l_rangon10.
      APPEND l_rangon10 TO rango_n10.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_bt.
    set_bt_st( EXPORTING low = low high = high option = option sign = sign
               CHANGING  t_rango = rango ).

    transform( low ).
  ENDMETHOD.
  METHOD set_bt_st.
    DATA l_linea TYPE rstt_s_range_string.

    CLEAR l_linea.
    l_linea-option = option.
    l_linea-sign   = sign.
    l_linea-low    = low.
    IF high IS INITIAL.
      l_linea-high = low.
    ELSE.
      l_linea-high = high.
    ENDIF.
    COLLECT l_linea INTO t_rango.
  ENDMETHOD.
  METHOD set_eq.
    DATA: i_lista  TYPE TABLE OF string,
          l_string TYPE string.

    IF es_lista IS INITIAL.
      set_eq_st( EXPORTING valor = valor option = option sign = sign
                 CHANGING  t_rango = rango ).

      transform( valor ).
    ELSE.
      SPLIT valor AT separador INTO TABLE i_lista.
      LOOP AT i_lista INTO l_string.
        set_eq_st( EXPORTING valor = l_string option = option sign = sign
                   CHANGING  t_rango = rango ).

        transform( l_string ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD set_eq_n10.
    DATA l_linea TYPE lxhme_range_n10.

    IF no_initial = '' OR NOT valor IS INITIAL.
      CLEAR l_linea.
      l_linea-option = option.
      l_linea-sign   = sign.
      l_linea-low    = valor.
      COLLECT l_linea INTO t_rango.
    ENDIF.
  ENDMETHOD.
  METHOD set_eq_n10c.
    DATA l_linea TYPE lxhme_range_n10.

    IF no_initial = '' OR NOT valor IS INITIAL.
      CLEAR l_linea.
      l_linea-option = option.
      l_linea-sign   = sign.
      l_linea-low    = valor.
      COLLECT l_linea INTO t_rango.
    ENDIF.
  ENDMETHOD.
  METHOD set_eq_st.
    DATA l_linea TYPE rstt_s_range_string.

    IF no_initial = '' OR NOT valor IS INITIAL.
      CLEAR l_linea.
      l_linea-option = option.
      l_linea-sign   = sign.
      l_linea-low    = valor.
      COLLECT l_linea INTO t_rango.
    ENDIF.
  ENDMETHOD.
  METHOD set_eq_str.
    DATA l_linea TYPE rstt_s_range_string.

    IF no_initial = '' OR NOT valor IS INITIAL.
      CLEAR l_linea.
      l_linea-option = option.
      l_linea-sign   = sign.
      l_linea-low    = valor.
      COLLECT l_linea INTO t_rango.
    ENDIF.
  ENDMETHOD.
  METHOD tabla_n10_to_rango_cond.
    DATA: i_tabla_n10 TYPE vbeln_vl_t,
          l_rango     TYPE lxhme_range_n10,
          l_next      TYPE vbeln_vl,
          l_append    TYPE c LENGTH 1.

    i_tabla_n10 = tabla.
    SORT i_tabla_n10.
    LOOP AT i_tabla_n10 ASSIGNING FIELD-SYMBOL(<n10>).
      DATA(l_tabix) = sy-tabix.
      IF NOT <n10> CO '0123456789'.
        IF l_rango IS INITIAL.
          l_rango-option = 'EQ'.
          l_rango-sign   = 'I'.
          l_rango-low    = <n10>.
          APPEND l_rango TO rango.
          CLEAR l_rango.
        ELSE.
          l_rango-option = 'BT'.
          l_rango-high   = <n10>.
          APPEND l_rango TO rango.
          CLEAR l_rango.
        ENDIF.
      ELSE.
        l_next = <n10> + 1.
        __poner_ceros l_next.
        l_tabix = l_tabix + 1.
        ASSIGN i_tabla_n10[ l_tabix ] TO FIELD-SYMBOL(<n10_next>).
        IF sy-subrc <> 0.
* No hay ninguno siguiente, es el último
          IF l_rango IS INITIAL.
            l_rango-option = 'EQ'.
            l_rango-sign   = 'I'.
            l_rango-low    = <n10>.
            APPEND l_rango TO rango.
            CLEAR l_rango.
          ELSE.
            l_rango-option = 'BT'.
            l_rango-high   = <n10>.
            APPEND l_rango TO rango.
            CLEAR l_rango.
          ENDIF.
        ELSE.
* Hay un consecutivo.
* Si el siguiente es el que esperamos, dejamos abierto.
          IF <n10_next> = l_next.
            IF l_rango IS INITIAL.
              l_rango-option = 'BT'.
              l_rango-sign   = 'I'.
              l_rango-low    = <n10>.
            ENDIF.
          ELSE.
* Si no, cerramos.
            IF l_rango IS INITIAL.
              l_rango-option = 'EQ'.
              l_rango-sign   = 'I'.
              l_rango-low    = <n10>.
              APPEND l_rango TO rango.
              CLEAR l_rango.
            ELSE.
              l_rango-option = 'BT'.
              l_rango-high   = <n10>.
              APPEND l_rango TO rango.
              CLEAR l_rango.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
  METHOD transform.
    DATA: l_tipo   TYPE c LENGTH 1,
          l_rango  TYPE rstt_s_range_string,
          l_fechas TYPE tpms_r_date.

    CLEAR rango_fechas.
    DESCRIBE FIELD campo TYPE l_tipo.
    CASE l_tipo.
      WHEN 'D'.
        LOOP AT rango INTO l_rango.
          MOVE-CORRESPONDING l_rango TO l_fechas.
          APPEND l_fechas TO rango_fechas.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.
