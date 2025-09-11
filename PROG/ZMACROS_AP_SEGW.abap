*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE get_rango_may_ceros.
  DATA: &1 TYPE /iwbep/t_cod_select_options.
  &1 = o_segw->get_filter( log = o_segw->v_log
                           name = &2
                           it_filter_select_options = it_filter_select_options
                           it_key_tab = it_key_tab
                           mayusculas = &3
                           poner_ceros = &4  ).
  __add_lista o_segw->v_no_campos &2.
END-OF-DEFINITION.

DEFINE get_rango_may_ceros_lt.
  DATA: &1 TYPE /iwbep/t_cod_select_options.
  &1 = o_segw->get_filter( log = o_segw->v_log
                           name = &2
                           it_filter_select_options = lt_filter_select_options
                           it_key_tab = it_key_tab
                           mayusculas = &3
                           poner_ceros = &4  ).
  __add_lista o_segw->v_no_campos &2.
END-OF-DEFINITION.

DEFINE get_rango.
  get_rango_may_ceros &1 &2 '' ''.
END-OF-DEFINITION.

DEFINE get_rango_ceros.
  get_rango_may_ceros &1 &2 '' 'X'.
END-OF-DEFINITION.

DEFINE get_rango_mayusculas.
  get_rango_may_ceros &1 &2 'X' ''.
END-OF-DEFINITION.

DEFINE get_key_may_ceros.
  o_segw->get_key( EXPORTING log         = o_segw->v_log
                             name        = &2
                             it_key_tab  = it_key_tab
                             mayusculas  = &3
                             poner_ceros = &4
                   IMPORTING key         = &1  ).
  __add_lista o_segw->v_no_campos &2.
END-OF-DEFINITION.

DEFINE get_rango_so.
  DATA: r_&1 TYPE /iwbep/t_cod_select_options.
  r_&1 = o_segw->get_filter( log = o_segw->v_log
                             name = '&1'
                             it_filter_select_options = l_so
                             it_key_tab = it_key_tab
                             mayusculas = 'A'
                             poner_ceros = ''  ).
  __add_lista o_segw->v_no_campos '&1'.
END-OF-DEFINITION.

DEFINE get_rango_so_Ceros.
  DATA: r_&1 TYPE /iwbep/t_cod_select_options.
  r_&1 = o_segw->get_filter( log = o_segw->v_log
                             name = '&1'
                             it_filter_select_options = l_so
                             it_key_tab = it_key_tab
                             mayusculas = 'A'
                             poner_ceros = &2  ).
  __add_lista o_segw->v_no_campos '&1'.
END-OF-DEFINITION.

DEFINE get_key.
  get_key_may_ceros &1 &2 '' ''.
END-OF-DEFINITION.

DEFINE get_key_ceros.
  get_key_may_ceros &1 &2 '' 'X'.
END-OF-DEFINITION.

DEFINE get_key_mayusculas.
  get_key_may_ceros &1 &2 'X' ''.
END-OF-DEFINITION.

DEFINE set_header.
  v_header-name = &1.
  v_header-value = &2.
  /iwbep/if_mgw_conv_srv_runtime~set_header( v_header ).
END-OF-DEFINITION.

DEFINE filter_order.
  IF NOT it_order IS INITIAL.
    o_segw->get_sort_itab( EXPORTING it_order = it_order CHANGING et_entityset = et_entityset ).
  ENDIF.

  o_segw->filter_itab( EXPORTING it_filter_select_options = it_filter_select_options
                                 is_paging                = is_paging
                                 it_key_tab               = it_key_tab
                                 no_campos                = o_segw->v_no_campos
                       CHANGING  et_entityset             = et_entityset ).

END-OF-DEFINITION.

DEFINE ini_metodo.
  o_segw->ini_metodo( entidad = &1 metodo = &2 ).
END-OF-DEFINITION.

DEFINE ini_metodo_set.
  DATA: l_entity LIKE LINE OF et_entityset,
        l_metodo TYPE string,
        l_sql TYPE string.

  CONCATENATE iv_entity_set_name '_GET_ENTITYSET' INTO l_metodo.
  TRANSLATE l_metodo TO UPPER CASE.
  o_segw->ini_metodo( entidad = iv_entity_name metodo = l_metodo ).
  get_is_count.
  IF NOT iv_filter_string IS INITIAL OR NOT iv_search_string IS INITIAL OR NOT it_filter_select_options IS INITIAL.
    TRY.
      l_sql = io_tech_request_context->get_osql_where_clause_convert( ).
      IF o_segw->grabar_filtros_no = 'D' or o_segw->grabar_todo = 'X'.
        o_segw->set_log( p1 = 'SQL=' p2 = l_sql type = 'I' ).
      ENDIF.
      CATCH cx_root INTO data(o_root_l).
        o_segw->set_log( p1 = 'Error GET_OSQL' p2 = o_root_l->get_text( ) type = 'E' ).
    ENDTRY.
  ENDIF.

END-OF-DEFINITION.

DEFINE ini_metodo_entity.
  DATA: l_metodo TYPE string.
  CONCATENATE iv_entity_set_name '_GET_ENTITY' INTO l_metodo.
  TRANSLATE l_metodo TO UPPER CASE.
  o_segw->ini_metodo( entidad = iv_entity_name metodo = l_metodo ).
END-OF-DEFINITION.

DEFINE key_2_rango.
  DATA: &1 TYPE /iwbep/t_cod_select_options.
  &1 = o_segw->key_2_rango( &2 ).
END-OF-DEFINITION.

DEFINE get_valor_parametro.
  READ TABLE it_parameter INTO ls_parameter WITH KEY name = &1.
  IF sy-subrc = 0.
    &2 = ls_parameter-value.
    o_segw->set_log( p1 = 'Parámetro' p2 = &1 p3 = &2 type = 'S' ).
  ELSE.
    __concat2 l_message 'No ha informado parámetro' &1.
    o_segw->set_log( p1 = l_message ).
  ENDIF.
END-OF-DEFINITION.

DEFINE get_primer_valor_rango.
  DATA: l&2 TYPE /iwbep/s_cod_select_option,
        &1 TYPE string.

  READ TABLE &2 INTO l&2 INDEX 1.
  IF sy-subrc = 0.
    &1 = l&2-low.
  ENDIF.
END-OF-DEFINITION.

DEFINE get_is_count.
  DATA l_is_count TYPE abap_bool.
  IF NOT io_tech_request_context IS INITIAL.
  l_is_count = io_tech_request_context->has_count( ).
  ENDIF.
END-OF-DEFINITION.

DEFINE get_rango_id.
  v_id = get_field_from_abap( &1 ).
  IF NOT v_id IS INITIAL.
    get_rango: &2    v_id.
  ENDIF.
END-OF-DEFINITION.

DEFINE get_rango_ceros_id.
  v_id = get_field_from_abap( &1 ).
  IF NOT v_id IS INITIAL.
    get_rango_ceros: &2    v_id &3.
  ENDIF.
END-OF-DEFINITION.

DEFINE change_rango_cp.
  LOOP AT &1 ASSIGNING FIELD-SYMBOL(<&1>) WHERE option = 'EQ' AND sign = 'I'.
    DATA(l_cp_&1) = <&1>-low.
    IF &2 = 10.
      __quitar_ceros l_cp_&1.
      CONDENSE l_cp_&1.
    ENDIF.
    IF strlen( l_cp_&1 ) < ( &2 - 1 ).
      <&1>-option = 'CP'.
      <&1>-low = '*' && l_cp_&1 && '*'.
    ENDIF.
  ENDLOOP.
END-OF-DEFINITION.
