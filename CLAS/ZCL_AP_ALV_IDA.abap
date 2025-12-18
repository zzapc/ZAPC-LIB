class ZCL_AP_ALV_IDA definition
  public
  create public .

public section.

  data O_SALV_IVA type ref to IF_SALV_GUI_TABLE_IDA .
  data NOMBRE_TABLA type STRING .
  data TIPO_DOBLE_CLICK type STRING .
  data O_COLLECTOR type ref to CL_SALV_RANGE_TAB_COLLECTOR .

  methods CONSTRUCTOR
    importing
      !TABLA type ANY optional
      !SOLO_CAMPOS type ANY optional
      !QUITAR_CAMPOS type ANY optional
      !CAMPOS_SUMA type ANY optional
      !CDS type ANY optional
      !TIPO_DOBLE_CLICK type ANY optional
      !FILTROS type ANY optional .
  methods DOUBLE_CLICK
    for event DOUBLE_CLICK of IF_SALV_GUI_TABLE_DISPLAY_OPT
    importing
      !EV_FIELD_NAME
      !EO_ROW_DATA .
  methods FULLSCREEN_DISPLAY .
  methods ADD_RANGO
    importing
      !RANGO type TABLE optional
      !CAMPO type ANY
      !OPTION type ANY default 'EQ'
      !SIGN type ANY default 'I'
      !LOW type ANY optional
      !HIGH type ANY optional .
protected section.
private section.
endclass. "ZCL_AP_ALV_IDA definition
class ZCL_AP_ALV_IDA implementation.
METHOD add_rango.

    IF o_collector IS INITIAL.
      o_collector = NEW cl_salv_range_tab_collector( ).
    ENDIF.

    IF NOT rango IS INITIAL.
      o_collector->add_ranges_for_name( iv_name = campo it_ranges = rango ).
    ELSE.
      DATA r_rango TYPE rstt_t_range_string.
      r_rango = VALUE #( option = option sign = sign ( low = low high = high ) ).
      o_collector->add_ranges_for_name( iv_name = campo it_ranges = r_rango ).
    ENDIF.

  ENDMETHOD.
METHOD constructor.
  DATA: lts_field_names TYPE if_salv_gui_types_ida=>yts_field_name,
        ls_aggr_rule    TYPE if_salv_gui_types_ida=>ys_aggregation_rule,
        lt_aggr_rules   TYPE if_salv_gui_types_ida=>yt_aggregation_rule.

  IF NOT tabla IS INITIAL.
    o_salv_iva = cl_salv_gui_table_ida=>create( iv_table_name = CONV #( tabla ) ).
    nombre_tabla = tabla.
  ELSEIF NOT cds IS INITIAL.
    o_salv_iva = cl_salv_gui_table_ida=>create_for_cds_view( CONV #( cds ) ).
    nombre_tabla = cds.
  ENDIF.

  me->tipo_doble_click = tipo_doble_click.

  o_salv_iva->display_options( )->enable_double_click( ).
  SET HANDLER double_click FOR o_salv_iva->display_options( ).

  SPLIT solo_campos AT ',' INTO TABLE DATA(i_campos).
  SORT i_campos.
  LOOP AT i_campos ASSIGNING FIELD-SYMBOL(<campo>).
    APPEND <campo> TO lts_field_names.
  ENDLOOP.

  IF NOT quitar_campos IS INITIAL AND solo_campos IS INITIAL.
    o_salv_iva->field_catalog( )->get_all_fields( IMPORTING ets_field_names = lts_field_names ).
    DATA(r_rango_quitar) = zcl_ap_lista=>lista2rango( quitar_campos ).
    DELETE lts_field_names WHERE table_line IN r_rango_quitar.  "#EC CI_SORTSEQ
  ENDIF.

  IF NOT lts_field_names IS INITIAL.
    o_salv_iva->field_catalog( )->set_available_fields( lts_field_names ).
  ENDIF.

  SPLIT campos_suma AT ',' INTO TABLE i_campos.
  LOOP AT i_campos ASSIGNING <campo>.
    ls_aggr_rule-field_name = <campo>.
    ls_aggr_rule-function = if_salv_service_types=>cs_function_code-sum.
    APPEND ls_aggr_rule TO lt_aggr_rules.
  ENDLOOP.
  IF NOT lt_aggr_rules IS INITIAL.
    TRY.
        o_salv_iva->default_layout( )->set_aggregations( lt_aggr_rules ).
      CATCH cx_salv_ida_contract_violation.
    ENDTRY.
  ENDIF.

  SPLIT filtros AT ',' INTO TABLE i_campos.
  LOOP AT i_campos ASSIGNING <campo>.
    DATA(l_sign) = 'I'.
    IF <campo> CS '<='.
      DATA(l_option) = 'LE'. DATA(l_split) = '<='.
    ELSEIF <campo> CS '>='.
      l_option = 'GE'. l_split = '>='.
    ELSEIF <campo> CS '<>'.
      l_sign = 'E'.
      l_option = 'EQ'. l_split = '<>'.
    ELSEIF <campo> CS '='.
      l_option = 'EQ'. l_split = '='.
    ELSEIF <campo> CS '<'.
      l_option = 'LT'. l_split = '<'.
    ELSEIF <campo> CS '>'.
      l_option = 'GT'. l_split = '>'.
    ELSE.
      CONTINUE.
    ENDIF.
    DATA(l_long) = strlen( l_split ).
    SPLIT <campo> AT l_split(l_long) INTO DATA(campo) DATA(filtro).
    IF sy-subrc = 0.
      IF filtro = `''`.
        CLEAR filtro.
      ENDIF.
      add_rango( campo = campo low = filtro option = l_option sign = l_sign ).
    ENDIF.
  ENDLOOP.

ENDMETHOD.
METHOD double_click.
  DATA: lr_line TYPE REF TO data,
        l_ok,
        l_error.
  FIELD-SYMBOLS: <bukrs> TYPE bukrs, <belnr> TYPE belnr_d, <gjahr> TYPE gjahr, <kukey> TYPE kukey_eb,
                 <zeile> TYPE mseg-zeile, <buzei> TYPE bseg-buzei.

  DEFINE assign_campo.
    ASSIGN COMPONENT '&1' OF STRUCTURE <fila> TO <&1>.
    IF sy-subrc NE 0.
      l_error = 'X'.
    ENDIF.
  END-OF-DEFINITION.

  CREATE DATA lr_line TYPE (nombre_tabla).
  ASSIGN lr_line->* TO FIELD-SYMBOL(<fila>).

  eo_row_data->get_row_data( EXPORTING iv_request_type = if_salv_gui_selection_ida=>cs_request_type-all_fields
                             IMPORTING es_row =  <fila> ).

  CASE tipo_doble_click.
    WHEN 'EKBE'.
      assign_campo: belnr, gjahr.
      IF l_error IS INITIAL.
        ASSIGN COMPONENT 'BUZEI' OF STRUCTURE <fila> TO <zeile>.
        IF sy-subrc = 0.
          zcl_ap_docmat=>visualizar( mblnr = <belnr> mjahr = <gjahr> zeile = <zeile> ).
          l_ok = 'X'.
        ENDIF.
      ENDIF.

    WHEN 'FB03'.
      assign_campo: bukrs, belnr, gjahr.
      IF l_error IS INITIAL.
        zcl_ap_doc_fi=>fb03( bukrs = <bukrs> belnr = <belnr> gjahr = <gjahr> ).
        l_ok = 'X'.
      ENDIF.
    WHEN 'FF_6'.
      assign_campo: kukey.
      IF l_error IS INITIAL.
        SUBMIT rfebkap0
          AND RETURN
         WITH r_kukey = <kukey>.
        l_ok = 'X'.
      ENDIF.
  ENDCASE.

  IF l_ok IS INITIAL.
    cl_salv_ida_show_data_row=>display( iv_text = 'DoubleClickEvent for field'(001) && `  ` && ev_field_name
                                        is_data = <fila> ).
  ENDIF.

ENDMETHOD.
METHOD fullscreen_display.

  IF NOT o_collector IS INITIAL.
    o_collector->get_collected_ranges( IMPORTING et_named_ranges = DATA(lt_name_range_pairs) ).
    o_salv_iva->set_select_options( it_ranges = lt_name_range_pairs ).
  ENDIF.

  o_salv_iva->fullscreen( )->display( ).

ENDMETHOD.
