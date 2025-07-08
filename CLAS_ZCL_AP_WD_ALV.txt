class ZCL_AP_WD_ALV definition
  public
  create public .

public section.

  data O_USAGE type ref to IF_WD_COMPONENT_USAGE .
  data O_CONFIG type ref to CL_SALV_WD_CONFIG_TABLE .
  data O_ALV type ref to IWCI_SALV_WD_TABLE .
  data O_FIELD type ref to CL_SALV_WD_FIELD .
  data O_SETTINGS type ref to IF_SALV_WD_TABLE_SETTINGS .
  data O_HEADER type ref to CL_SALV_WD_HEADER .
  data O_FUNCTION type ref to CL_SALV_WD_FUNCTION .
  data O_CONTEXT type ref to IF_WD_CONTEXT_NODE .

  methods CONSTRUCTOR
    importing
      !O_USAGE type ref to IF_WD_COMPONENT_USAGE
      !O_CONTEXT type ref to IF_WD_CONTEXT_NODE optional .
  type-pools ABAP .
  methods SET_ALV
    importing
      !O_ALV type ref to IWCI_SALV_WD_TABLE
      !FILAS type I default 0
      !DESIGN type WDY_UIE_LIBRARY_ENUM_TYPE default CL_WD_TABLE=>E_DESIGN-ALTERNATING
      !TITULO type STRING default ''
      !READ_ONLY type ABAP_BOOL default 'X'
      !EDIT_INSERT_ROW type ABAP_BOOL default '' .
  methods SET_FIELD_NOOUT
    importing
      !CAMPO type ANY .
  methods SET_FIELD
    importing
      !OP type TEXT10
      !CAMPO type ANY
      !VALOR type ANY optional
      !VALOR2 type ANY optional .
  methods SET_ORDEN
    importing
      !CAMPO type STRING
      !UP type CHAR1 default 'X'
      !DOWN type CHAR1 default ''
      !GROUP type SALV_DE_SORT_GROUP default IF_SALV_C_SORT=>GROUP_NONE
      !SUBTOT type CHAR1 default '' .
  methods SET_FIELD_LINK
    importing
      !CAMPO type ANY .
  methods ADD_FUNCTION
    importing
      !CAMPO type STRING
      !ID type STRING default ''
      !TEXTO type STRING default  ''
      !TIPO type ANY .
  methods SET_FIELD_INPUT
    importing
      !CAMPO type ANY .
protected section.
private section.
endclass. "ZCL_AP_WD_ALV definition
class ZCL_AP_WD_ALV implementation.
method ADD_FUNCTION.
  DATA: l_id    TYPE string,
        l_texto TYPE string.

  IF id IS INITIAL.
    l_id = campo.
  ELSE.
    l_id = id.
  ENDIF.

  IF texto IS INITIAL.
    l_texto = campo.
  ELSE.
    l_texto = texto.
  ENDIF.

  o_function = o_config->if_salv_wd_function_settings~create_function( id = l_id ).

  CASE tipo.
    WHEN 'DROPDOWN_BY_KEY'.
      DATA: lr_dropdown_by_key TYPE REF TO cl_salv_wd_fe_dropdown_by_key.
      CREATE OBJECT lr_dropdown_by_key
        EXPORTING
          selected_key_elementname = campo.
      lr_dropdown_by_key->set_label_text( l_texto ).
      o_function->set_editor( lr_dropdown_by_key ).
    WHEN 'TOGGLE_BUTTON'.
      DATA: lr_toggle_button TYPE REF TO cl_salv_wd_fe_toggle_button.
      CREATE OBJECT lr_toggle_button
        EXPORTING
          checked_elementname = campo.
      lr_toggle_button->set_text( l_texto ).
      o_function->set_editor( lr_toggle_button ).
  ENDCASE.

endmethod.
method CONSTRUCTOR.

  IF o_usage->has_active_component( ) IS INITIAL.
    o_usage->create_component( ).
  ENDIF.

  me->o_usage = o_usage.

  IF NOT o_context IS INITIAL.
    me->o_context = o_context.
  ENDIF.


endmethod.
method SET_ALV.

  me->o_alv   = o_alv.

  o_config    = o_alv->get_model( ).

  o_config->if_salv_wd_table_settings~set_design( design ).
  IF NOT filas IS INITIAL.
    o_config->if_salv_wd_table_settings~set_visible_row_count( filas ).
  ENDIF.
  o_config->if_salv_wd_table_settings~set_read_only( read_only ).

  IF edit_insert_row = 'X'.
    o_config->if_salv_wd_std_functions~set_edit_insert_row_allowed( ).
    o_config->if_salv_wd_table_settings~set_data_check( if_salv_wd_c_table_settings=>data_check_on_cell_event ).
  ENDIF.

  o_settings ?= o_config.

  IF NOT titulo IS INITIAL.
    o_header = o_settings->get_header( ).
    o_header->set_text( titulo ).
  ENDIF.

endmethod.
method SET_FIELD.
  DATA: l_column    TYPE string,
        i_column    TYPE TABLE OF string,
        l_error     TYPE string,
        l_text(100),
        l_texto     TYPE scrtext_s,
        l_texto2    TYPE scrtext_l,
        l_texto3    TYPE scrtext_m.

  DATA: o_link     TYPE REF TO cl_salv_wd_uie_link_to_action,
        o_column   TYPE REF TO cl_salv_wd_column,
        o_input    TYPE REF TO cl_salv_wd_uie_input_field,
        o_dropdown TYPE REF TO cl_salv_wd_uie_dropdown_by_key.

  SPLIT campo AT ',' INTO TABLE i_column.
  LOOP AT i_column INTO l_column.
    TRANSLATE l_column TO UPPER CASE.
    TRY.
      o_column    = o_config->if_salv_wd_column_settings~get_column( l_column ).

      CASE op.
        WHEN 'NOOUT' OR 'NO_OUT'.
          o_config->if_salv_wd_column_settings~delete_column( l_column ).
        WHEN  'LINK'.
          CREATE OBJECT o_link.
          o_link->set_text_fieldname( l_column ).
          o_column->set_cell_editor( o_link ).
        WHEN 'EDIT' OR 'INPUT'.
          CREATE OBJECT o_input
            EXPORTING
              value_fieldname = l_column.
          "   To set the drop down to the field REQUEST_TYPE.
          o_column->set_cell_editor( o_input ).
*        WHEN 'DROPDOWN'.
*          CREATE OBJECT o_dropdown
*            EXPORTING
*              value_fieldname = l_column.
*          o_column->set_cell_editor( o_dropdown ).
*
*          wd_assist->set_attribute_value_set( nodo = 'TASK' campo = 'PROYECTO' ).
      ENDCASE.
    ENDTRY.
  ENDLOOP.


endmethod.
method SET_FIELD_INPUT.

  set_field( op = 'INPUT' campo = campo ).

endmethod.
method SET_FIELD_LINK.

  set_field( op = 'LINK' campo = campo ).

endmethod.
method SET_FIELD_NOOUT.

  set_field( op = 'NO_OUT' campo = campo ).

endmethod.
method SET_ORDEN.
  DATA: l_columname TYPE string,
        i_columnas  TYPE TABLE OF string,
        l_sequence  TYPE salv_de_sort_sequence,
        l_error(80).

  IF down = 'X'.
    l_sequence = 2.
  ELSE.
    l_sequence = 1.
  ENDIF.

  l_columname = campo.
  SPLIT campo AT ',' INTO TABLE i_columnas.
  LOOP AT i_columnas INTO l_columname.
    TRY.
      o_field = o_config->if_salv_wd_field_settings~get_field( l_columname ).
      o_field->if_salv_wd_sort~create_sort_rule( ).
    ENDTRY.
  ENDLOOP.



endmethod.
