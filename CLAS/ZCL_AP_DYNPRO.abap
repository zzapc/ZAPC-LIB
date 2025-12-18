CLASS zcl_ap_dynpro DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_screen TYPE STANDARD TABLE OF screen WITH KEY name.

    CLASS-METHODS screen_visible
      IMPORTING campo        TYPE any OPTIONAL
                variable     TYPE any
                !input       TYPE c   DEFAULT '1'
                group1       TYPE any OPTIONAL
                group2       TYPE any OPTIONAL
                group3       TYPE any OPTIONAL
                group4       TYPE any OPTIONAL
                variable_inv TYPE c   DEFAULT ''
                !option      TYPE any DEFAULT 'EQ'
                group2_input TYPE any DEFAULT ''.

    CLASS-METHODS screen_input
      IMPORTING campo           TYPE any       OPTIONAL
                variable        TYPE any
                group1          TYPE any       OPTIONAL
                group2          TYPE any       OPTIONAL
                !option         TYPE any       DEFAULT 'EQ'
                group3          TYPE any       OPTIONAL
                group4          TYPE any       OPTIONAL
                variable_inv    TYPE c         DEFAULT ''
                solo_si_visible TYPE abap_bool DEFAULT ''
                todos           TYPE abap_bool DEFAULT ''
                !no             TYPE any       DEFAULT ''
                !output         TYPE any       DEFAULT ''
                display_3d      TYPE any       DEFAULT ''.

    CLASS-METHODS screen_obligatorio
      IMPORTING campo        TYPE any OPTIONAL
                variable     TYPE any
                group1       TYPE any OPTIONAL
                group2       TYPE any OPTIONAL
                group3       TYPE any OPTIONAL
                group4       TYPE any OPTIONAL
                variable_inv TYPE c   DEFAULT ''
                !option      TYPE any DEFAULT 'EQ'
                invisible    TYPE c   DEFAULT '0'.

    CLASS-METHODS set_cursor_campo_vacio
      IMPORTING campos     TYPE any DEFAULT ''
                estructura TYPE any DEFAULT ''
      PREFERRED PARAMETER campos.

    CLASS-METHODS campos_vacios_no_visibles
      IMPORTING excluir_campos TYPE string DEFAULT ''
                excluir_grupos TYPE string DEFAULT ''.

    CLASS-METHODS write_screen
      IMPORTING !write          TYPE abap_bool DEFAULT ''
                !break          TYPE abap_bool
      RETURNING VALUE(i_screen) TYPE zcl_ap_dynpro=>tt_screen.

    CLASS-METHODS set_primer_radiobutton
      IMPORTING campos TYPE any DEFAULT ''
                group1 TYPE any DEFAULT ''
      PREFERRED PARAMETER campos.

    CLASS-METHODS modificar_texto
      IMPORTING campo TYPE any
                texto TYPE any
                repid TYPE sy-repid DEFAULT sy-repid
                dynnr TYPE sy-dynnr DEFAULT sy-dynnr
                ancho TYPE i        DEFAULT -1.

    CLASS-METHODS modificar_texto_sel_screen
      IMPORTING campo TYPE any
                texto TYPE any
                cprog TYPE sy-cprog   DEFAULT sy-cprog
                !kind TYPE rsscr_kind DEFAULT 'P'.

    CLASS-METHODS sscrfields_add
      IMPORTING boton       TYPE i
                texto       TYPE any       OPTIONAL
                icono       TYPE any       OPTIONAL
                texto_icono TYPE any       OPTIONAL
                !quickinfo  TYPE any       OPTIONAL
                !visible    TYPE abap_bool DEFAULT 'X'.

    CLASS-METHODS sscrfields_view
      IMPORTING boton    TYPE i
                !visible TYPE abap_bool.

    CLASS-METHODS deshabilitar_boton_variante.

    CLASS-METHODS deshabilitar_boton
      IMPORTING ucomm TYPE sy-ucomm.


endclass. "ZCL_AP_DYNPRO definition
class ZCL_AP_DYNPRO implementation.
  METHOD campos_vacios_no_visibles.
    LOOP AT SCREEN.
      IF NOT excluir_grupos IS INITIAL AND NOT screen-group1 IS INITIAL.
        IF zcl_ap_lista=>es_elemento( elemento = screen-group1 lista = excluir_grupos ).
          CONTINUE.
        ENDIF.
      ENDIF.
      IF NOT excluir_campos IS INITIAL.
        IF zcl_ap_lista=>es_elemento( elemento = screen-name lista = excluir_campos ).
          CONTINUE.
        ENDIF.
      ENDIF.
      IF screen-invisible = 0 AND screen-input = 0.
        IF zcl_ap_fs=>es_inicial( screen-name ) = 'X'.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD deshabilitar_boton.
    DATA it_ucomm TYPE TABLE OF sy-ucomm.

    APPEND ucomm TO it_ucomm.
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING p_status  = sy-pfkey
                p_program = ''
      TABLES    p_exclude = it_ucomm.
  ENDMETHOD.
  METHOD deshabilitar_boton_variante.
    deshabilitar_boton( 'GET' ).
  ENDMETHOD.
  METHOD modificar_texto.
    DATA : dynp_header  TYPE d020s,
           dynp_fields  TYPE d021s,
           " TODO: variable is assigned but never used (ABAP cleaner)
           dynp_logic   TYPE d022s,
           " TODO: variable is assigned but never used (ABAP cleaner)
           dynp_matchc  TYPE d023s,
           tdynp_fields TYPE TABLE OF d021s,
           tdynp_logic  TYPE TABLE OF d022s,
           tdynp_matchc TYPE TABLE OF d023s.
    DATA l_f1 TYPE trmsg.

    DATA: BEGIN OF dynproname,
            prog TYPE d020s-prog,
            dnum TYPE d020s-dnum,
          END OF dynproname.

    CLEAR: dynp_header,
           dynp_fields,
           dynp_logic,
           dynp_matchc.
    REFRESH: tdynp_fields, tdynp_logic, tdynp_matchc.

    dynproname-prog = repid.
    dynproname-dnum = dynnr.

    IMPORT DYNPRO dynp_header tdynp_fields
                  tdynp_logic tdynp_matchc ID dynproname.

    LOOP AT tdynp_fields INTO dynp_fields                  "#EC CI_STDSEQ
         WHERE fnam = campo.
      dynp_fields-stxt = texto.
      IF ancho > 0.
        IF ancho = 0.
          dynp_fields-leng = strlen( texto ) + 1.
        ELSE.
          dynp_fields-leng = ancho.
        ENDIF.
      ENDIF.
      MODIFY tdynp_fields FROM dynp_fields.
    ENDLOOP.
    IF sy-subrc = 0.
      EXPORT DYNPRO dynp_header tdynp_fields                  "#EC *
                    tdynp_logic tdynp_matchc ID dynproname.  "#EC *

      GENERATE DYNPRO dynp_header tdynp_fields                "#EC *
                    tdynp_logic tdynp_matchc ID dynproname MESSAGE l_f1 LINE l_f1 WORD l_f1.
    ENDIF.
  ENDMETHOD.
  METHOD modificar_texto_sel_screen.
    DATA: l_text TYPE rsseltexts,
          i_text TYPE TABLE OF rsseltexts.

    l_text-name = campo.
    l_text-kind = kind.
    l_text-text = texto.
    APPEND l_text TO i_text.

    CALL FUNCTION 'SELECTION_TEXTS_MODIFY'
      EXPORTING program  = cprog
      TABLES    seltexts = i_text.
  ENDMETHOD.
  METHOD screen_input.
    DATA: r_rango_string  TYPE RANGE OF string,
          r_rango         TYPE RANGE OF screen-name,
          lr_rango_string LIKE LINE OF r_rango_string,
          lr_rango        LIKE LINE OF r_rango.

    IF NOT campo IS INITIAL.
      r_rango_string = zcl_ap_string=>lista2rango( lista = campo option = option ).
      LOOP AT r_rango_string INTO lr_rango_string.
        MOVE-CORRESPONDING lr_rango_string TO lr_rango.
        APPEND lr_rango TO r_rango.
      ENDLOOP.
    ENDIF.

    LOOP AT SCREEN.
      IF     ( campo  = '' OR screen-name   NOT IN r_rango )
         AND ( group1 = '' OR screen-group1     <> group1 )
         AND ( group2 = '' OR screen-group2     <> group2 )
         AND ( group3 = '' OR screen-group3     <> group3 )
         AND ( group4 = '' OR screen-group4     <> group4 )
         AND todos <> 'X'.
        CONTINUE.
      ENDIF.

      IF todos = 'X'.
        IF NOT no IS INITIAL.
          IF no CS screen-name.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      IF screen-name CS 'OPTI_PUSH' OR screen-name CS 'VALU_PUSH'.
* No hacemos nada en los pulsadores de select-options
      ELSE.
        IF variable_inv IS INITIAL.
          IF variable IS INITIAL.
            screen-input = 0.
          ELSE.
            IF solo_si_visible = '' OR ( solo_si_visible = 'X' AND screen-invisible = '0' ).
              screen-input = 1.
            ENDIF.
          ENDIF.
        ELSE.
          IF variable IS INITIAL.
            screen-input = 1.
          ELSE.
            IF solo_si_visible = '' OR ( solo_si_visible = 'X' AND screen-invisible = '0' ).
              screen-input = 0.
            ENDIF.
          ENDIF.
        ENDIF.

        IF output = '0' OR output = '1'.
          screen-output = output.
        ENDIF.
        IF display_3d = '0' OR display_3d = '1'.
          screen-display_3d = display_3d.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.
  METHOD screen_obligatorio.
    DATA: r_rango_string  TYPE RANGE OF string,
          r_rango         TYPE RANGE OF fieldname,
          lr_rango_string LIKE LINE OF r_rango_string,
          lr_rango        LIKE LINE OF r_rango.

    IF NOT campo IS INITIAL.
      r_rango_string = zcl_ap_string=>lista2rango( lista = campo option = option ).
      LOOP AT r_rango_string INTO lr_rango_string.
        MOVE-CORRESPONDING lr_rango_string TO lr_rango.
        APPEND lr_rango TO r_rango.
      ENDLOOP.
    ENDIF.

    LOOP AT SCREEN.
      IF    ( campo  <> '' AND screen-name   IN r_rango )
         OR ( group1 <> '' AND screen-group1  = group1 )
         OR ( group2 <> '' AND screen-group2  = group2 )
         OR ( group3 <> '' AND screen-group3  = group3 )
         OR ( group4 <> '' AND screen-group4  = group4 ).

        IF variable_inv IS INITIAL.
          IF variable IS INITIAL.
            screen-required = 0.
          ELSE.
            screen-input     = 1.
            screen-required  = 1.
            screen-invisible = invisible.
          ENDIF.
        ELSE.
          IF variable IS INITIAL.
            screen-required = 0.
          ELSE.
            screen-input     = 1.
            screen-required  = 1.
            screen-invisible = invisible.
          ENDIF.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.
  METHOD screen_visible.
    DATA: r_rango_string  TYPE RANGE OF string,
          r_rango         TYPE RANGE OF screen-name,
          lr_rango_string LIKE LINE OF r_rango_string,
          lr_rango        LIKE LINE OF r_rango,
          r_group1        TYPE RANGE OF screen-name.

    IF NOT campo IS INITIAL.
      r_rango_string = zcl_ap_string=>lista2rango( lista = campo option = option ).
      LOOP AT r_rango_string INTO lr_rango_string.
        MOVE-CORRESPONDING lr_rango_string TO lr_rango.
        APPEND lr_rango TO r_rango.
      ENDLOOP.
    ENDIF.

    IF NOT group1 IS INITIAL.
      r_rango_string = zcl_ap_string=>lista2rango( lista = group1 option = option ).
      LOOP AT r_rango_string INTO lr_rango_string.
        MOVE-CORRESPONDING lr_rango_string TO lr_rango.
        APPEND lr_rango TO r_group1.
      ENDLOOP.
    ENDIF.

    LOOP AT SCREEN.
      IF    ( campo  <> '' AND screen-name   IN r_rango )
         OR ( group1 <> '' AND screen-group1 IN r_group1 )
         OR ( group2 <> '' AND screen-group2  = group2 )
         OR ( group3 <> '' AND screen-group3  = group3 )
         OR ( group4 <> '' AND screen-group4  = group4 ).

        IF variable_inv IS INITIAL.
          IF variable IS INITIAL.
            screen-input     = 0.
            screen-invisible = 1.
          ELSE.
            IF screen-name CS 'OPTI_PUSH' OR screen-name CS 'VALU_PUSH'.
            ELSE.
              IF group2_input IS INITIAL.
                screen-input = input.
              ELSE.
                IF group2_input = screen-group2.
                  screen-input = 1.
                ELSE.
                  screen-input = 0.
                ENDIF.
              ENDIF.
            ENDIF.
            screen-invisible = 0.
          ENDIF.
        ELSE.
          IF NOT variable IS INITIAL.
            screen-input     = 0.
            screen-invisible = 1.
          ELSE.
            IF screen-name CS 'OPTI_PUSH'.
            ELSE.
              IF group2_input IS INITIAL.
                screen-input = input.
              ELSE.
                IF group2_input = screen-group2.
                  screen-input = 1.
                ELSE.
                  screen-input = 0.
                ENDIF.
              ENDIF.
            ENDIF.
            screen-invisible = 0.
          ENDIF.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_cursor_campo_vacio.
*APC20151026 Nos posicionamos siempre en el primer campo EDITABLE y VACIO

    DATA: i_campos TYPE TABLE OF string,
          l_campo  TYPE string,
          l_salir  TYPE c LENGTH 1.

    FIELD-SYMBOLS <fs> TYPE any.

    SPLIT campos AT ',' INTO TABLE i_campos.
    LOOP AT i_campos INTO l_campo.
      IF NOT estructura IS INITIAL.
        CONCATENATE estructura l_campo INTO l_campo SEPARATED BY '-'.
      ENDIF.
      LOOP AT SCREEN.
        IF screen-invisible = 0 AND screen-input = 1 AND screen-name = l_campo.
          CONCATENATE '(' sy-cprog ')' screen-name INTO l_campo.
          ASSIGN (l_campo) TO <fs>.
          IF sy-subrc = 0.
            IF <fs> IS INITIAL.
              SET CURSOR FIELD screen-name.
              l_salir = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF l_salir = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF l_salir IS INITIAL.
      LOOP AT SCREEN.
        IF screen-invisible = 0 AND screen-input = 1.
          CONCATENATE '(' sy-cprog ')' screen-name INTO l_campo.
          ASSIGN (l_campo) TO <fs>.
          IF sy-subrc = 0.
            IF <fs> IS INITIAL.
              SET CURSOR FIELD screen-name.
              l_salir = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD set_primer_radiobutton.
    DATA: i_campos        TYPE TABLE OF string,
          l_campo         TYPE string,
          l_marcados      TYPE i,
          l_primero_hecho TYPE c LENGTH 1.

    FIELD-SYMBOLS <fs> TYPE any.

    IF NOT campos IS INITIAL.
      SPLIT campos AT ',' INTO TABLE i_campos.
    ELSEIF NOT group1 IS INITIAL.
      LOOP AT SCREEN.
        IF screen-group1 = group1.
          APPEND screen-name TO i_campos.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT i_campos INTO l_campo.
      CONCATENATE '(' sy-cprog ')' l_campo INTO l_campo.
      ASSIGN (l_campo) TO <fs>.
      IF sy-subrc = 0.
        IF <fs> = 'X'.
          l_marcados = l_marcados + 1.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF l_marcados = 1.
      RETURN.
    ENDIF.

    LOOP AT i_campos INTO l_campo.
      LOOP AT SCREEN.
        IF screen-name <> l_campo.
          CONTINUE.
        ENDIF.

        CONCATENATE '(' sy-cprog ')' screen-name INTO l_campo.
        ASSIGN (l_campo) TO <fs>.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF screen-invisible = 0 AND screen-input = 1.
          IF l_primero_hecho IS INITIAL.
            <fs> = 'X'.
            l_primero_hecho = 'X'.
          ELSE.
            CLEAR <fs>.
          ENDIF.
        ELSE.
          CLEAR <fs>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD sscrfields_add.
    DATA screenfields_name TYPE string.

    FIELD-SYMBOLS <sscrfields> TYPE sscrfields.

    CHECK     boton BETWEEN 1 AND 5
          AND (    NOT texto IS INITIAL
                OR NOT icono IS INITIAL ).

    screenfields_name = |({ sy-cprog })SSCRFIELDS|.
    ASSIGN (screenfields_name) TO <sscrfields>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT |FUNCTXT_0{ boton }| OF STRUCTURE <sscrfields> TO FIELD-SYMBOL(<button_text>).
      IF sy-subrc = 0.
        <button_text> = VALUE smp_dyntxt( icon_id   = icono
                                          icon_text = texto_icono
                                          text      = texto
                                          quickinfo = quickinfo ).
        sscrfields_view( boton = boton visible = visible ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD sscrfields_view.
    DATA current_screen TYPE string VALUE '(RSDBRUNT)CURRENT_SCREEN'.
    DATA current_scr    TYPE string VALUE'(RSDBRUNT)CURRENT_SCR'.

    FIELD-SYMBOLS <current_screen> TYPE sydb0_screen.
    FIELD-SYMBOLS <current_scr>    TYPE sydb0_scr_stack_line.

    ASSIGN (current_screen) TO <current_screen>.
    IF sy-subrc = 0 AND boton BETWEEN 1 AND 5.
      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA(offset) = boton - 1.
      <current_screen>-func_keys+offset(1) = visible.
    ENDIF.

    ASSIGN (current_scr) TO <current_scr>.
    IF sy-subrc = 0 AND boton BETWEEN 1 AND 5.
      DATA(button) = |FC0{ boton }|.
      IF visible = abap_true.
        DELETE <current_scr>-excl WHERE fcode = button.
      ELSE.
        IF NOT line_exists(  <current_scr>-excl[ fcode = button ] ).
          APPEND VALUE #( fcode = button ) TO <current_scr>-excl.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD write_screen.
    CLEAR i_screen.
    LOOP AT SCREEN.
      IF write = 'X'.
        WRITE: / screen-name,
                 screen-group1,
                 screen-group2,
                 screen-group3,
                 screen-group4,
                 screen-required,
                 screen-input,
                 screen-output,
                 screen-intensified,
                 screen-invisible,
                 screen-length,
                 screen-active,
                 screen-display_3d,
                 screen-value_help,
                 screen-request,
                 screen-values_in_combo,
                 screen-color.
      ENDIF.

      APPEND screen TO i_screen.
    ENDLOOP.

    IF break = 'X'.
      BREAK-POINT. "#EC *
    ENDIF.
  ENDMETHOD.
