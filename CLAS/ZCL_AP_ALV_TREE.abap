class zcl_ap_alv_tree definition
  public
  create public .

  public section.

    data o_alv type ref to cl_salv_tree .
    data o_functions type ref to cl_salv_functions_tree .
    data o_columns type ref to cl_salv_columns_tree .
    data o_column type ref to cl_salv_column .
    data o_dspset type ref to cl_salv_display_settings .
    data o_layout type ref to cl_salv_layout .
    data o_selections type ref to cl_salv_selections_tree .
    data o_events type ref to cl_salv_events_tree .
    data i_columns type salv_t_column_ref .
    data o_content type ref to cl_salv_form_element .
    data i_rows type salv_t_row .
    data campo_check type fieldname .
    data o_functions_list type ref to cl_salv_functions_list .
    data status type string read-only .

    methods constructor
      importing
        !tabla            type string default 'I_LISTADO'
        !list_display     type abap_bool default ''
        !cprog            type sy-cprog default sy-cprog
        !optimize         type abap_bool default 'X'
        !botones_standard type abap_bool default 'X'
        !color            type lvc_fname default ' '
        !status           type sypfkey default ' '
        !sel              type c default ''
        !campo_check      type fieldname default ''
        !lights           type string default ''
        !container_name   type c default '' .
    methods show .
    methods handle_double_click
        for event if_salv_events_tree~double_click of cl_salv_events_tree
      importing
        !node_key
        !columnname .
    methods handle_user_command
        for event if_salv_events_functions~added_function of cl_salv_events_tree
      importing
        !e_salv_function .
    methods set_field
      importing
        !op     type text10
        !campo  type any
        !valor  type any optional
        !valor2 type any optional .
    methods handle_top_of_page
        for event if_salv_events_tree~double_click of cl_salv_events_tree .
    methods handle_end_of_page
        for event end_of_page of cl_salv_events_table .
    methods top_of_page .
    methods end_of_page .
    methods show_popup
      importing
        !start_column type i default 1
        !end_column   type i default 100
        !start_line   type i default 1
        !end_line     type i default 20
        !sel          type abap_bool default 'X' .
    methods set_color
      importing
        !campo type string
        !color type i .
    methods set_status
      importing
        !status type sypfkey .
    methods get_seleccion .
    methods set_orden
      importing
        !campo  type string optional
        !up     type char1 default 'X'
        !down   type char1 default ''
        !group  type salv_de_sort_group default if_salv_c_sort=>group_none
        !subtot type char1 default ''
          preferred parameter campo .
    methods set_agregacion
      importing
        !campo type string
        !AGGREGATION type SALV_DE_AGGREGATION default IF_SALV_C_AGGREGATION=>TOTAL.
    methods refresh .
    methods set_top_of_page .
    methods set_end_of_page .
    methods get_f4_layout
      returning
        value(layout) type disvariant-variant .
    methods set_layout
      importing
        !layout type disvariant-variant .
    methods get_fila_activa
      returning
        value(key) type salv_de_node_key .
    methods constructor_tabla
      importing
        !list_display     type abap_bool default ''
        !optimize         type abap_bool default 'X'
        !botones_standard type abap_bool default 'X'
        !color            type lvc_fname default ' '
        !status           type sypfkey default ' '
        !sel              type c default ''
        !campo_check      type fieldname default ''
        !lights           type string default ''
        !container_name   type c default ''
        !tabla            type string default 'I_LISTADO'
        !cprog            type sy-cprog default sy-cprog
        !handle           type slis_handl default ''
      changing
        !t_tabla          type table .
    methods add_nodo
      importing
        !registro     type any
        !key          type lvc_nkey
        !text         type any default ''
      returning
        value(salida) type lvc_nkey .
    methods expand_all .
    methods free .
    methods convertir_a_tabla_plana
      importing
        !show           type abap_bool default ''
        !generar_excel  type any default ''
      returning
        value(lr_table) type ref to cl_salv_table .
protected section.

    data nombre_tabla type string .
private section.

    data o_sorts type ref to cl_salv_sorts .
    data o_aggregation type ref to cl_salv_aggregations .
    data o_container type ref to cl_gui_custom_container .
    data key_layout type salv_s_layout_key .

    methods inicializa_objetos
      importing
        !list_display     type abap_bool default ''
        !optimize         type abap_bool default 'X'
        !botones_standard type abap_bool default 'X'
        !color            type lvc_fname default ' '
        !status           type sypfkey default ' '
        !sel              type c default ''
        !campo_check      type fieldname default ''
        !lights           type string default ''
        !tabla            type string default 'I_LISTADO'
        !cprog            type sy-cprog default sy-cprog
        !handle           type slis_handl default '' .
endclass. "ZCL_AP_ALV_TREE definition
class ZCL_AP_ALV_TREE implementation.
method add_nodo.
    data: nodes  type ref to cl_salv_nodes,
          node   type ref to cl_salv_node,
          l_text type lvc_value.

    check not o_alv is initial.

    nodes = o_alv->get_nodes( ).

    try.
        node = nodes->add_node( related_node = key
                                data_row     = registro
                                relationship =
    cl_gui_column_tree=>relat_last_child ).

        if not text is initial.
          l_text = text.
          node->set_text( l_text ).
        endif.

        salida = node->get_key( ).
      catch cx_salv_msg.
    endtry.

  endmethod.
method constructor.
  endmethod.
method constructor_tabla.
    data: l_error  type string,
          l_string type string.

    try.
        if not container_name is initial.
          create object o_container
            exporting
              container_name = container_name.

          l_string = container_name.


          try.
              call method cl_salv_tree=>factory
                exporting
                  r_container = o_container
*                 hide_header =
                importing
                  r_salv_tree = o_alv
                changing
                  t_table     = t_tabla.
            catch cx_salv_error .
              clear o_alv.
              message 'ALV display not possible'(anp) type 'I' display like 'E'.
          endtry.

        else.
          try.
              cl_salv_tree=>factory(
                importing r_salv_tree = o_alv
                changing  t_table = t_tabla ).
            catch cx_root.
              message 'ALV display not possible'(anp) type 'I' display like 'E'.
              return.
          endtry.
        endif.


        inicializa_objetos( list_display     = list_display
                            optimize         = optimize
                            botones_standard = botones_standard
                            color            = color
                            status           = status
                            sel              = sel
                            campo_check      = campo_check
                            lights           = lights
                            tabla            = tabla
                            cprog            = cprog
                            handle           = handle ).

      catch cx_salv_msg.
        message 'ALV display not possible'(anp) type 'I' display like 'E'.
    endtry.

  endmethod.
method convertir_a_tabla_plana.
*https://blogs.sap.com/2015/07/24/salv-tree-to-excel-xlsx/
    constants:lc_xlspace     type c value ' '. "Hexa value for this field should be 0030

    data: lv_level type i,
          lv_xlsx  type xstring,
          lt_table type ref to data,
          lr_data  type ref to data,
          lt_nodes type salv_t_nodes,
          ls_node  like line of lt_nodes,
          lr_node  type ref to cl_salv_node.

    field-symbols: <data>  type any,
                   <table> type standard table,
                   <str>   type any.

    try.
        lt_nodes = o_alv->get_nodes( )->get_all_nodes( ).
      catch cx_root.
        message 'Error recuperando nodos'(ern) type 'E'.
    endtry.

    loop at lt_nodes into ls_node.
      lr_node = ls_node-node.
      clear lv_level.
      do.
        try.
            lr_node = lr_node->get_parent( ).
            add 1 to lv_level.
          catch cx_salv_msg.
            exit.
        endtry.
      enddo.

      lr_data = ls_node-node->get_data_row( ).

      assign lr_data->* to <data>.
      if <table> is not assigned.
        create data lt_table like standard table of <data>.
        assign lt_table->* to <table>.
      endif.

      assign component 1 of structure <data> to <str>.
      subtract 1 from lv_level.
      do lv_level times.
        concatenate lc_xlspace <str> into <str>.
      enddo.

      append <data> to <table>.
    endloop.

    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = lr_table
          changing
            t_table = <table> ).
      catch cx_root.
        message 'Error generando ALV'(ega) type 'E'.
    endtry.

    if show = 'X'.
      lr_table->display( ).
    endif.


    lv_xlsx = lr_table->to_xml( if_salv_bs_xml=>c_type_xlsx ).


    if not generar_excel is initial.
      data: lr_zip         type ref to cl_abap_zip,
            lr_xlnode      type ref to if_ixml_node,
            lr_xldimension type ref to if_ixml_node,
            lr_xlsheetpr   type ref to if_ixml_element,
            lr_xloutlinepr type ref to if_ixml_element,
            lv_file        type xstring,
            lr_file        type ref to cl_xml_document,
            lr_xlrows      type ref to if_ixml_node_list,
            lr_xlrow       type ref to if_ixml_element,
            lr_xlformat    type ref to if_ixml_element,
            lr_xlworksheet type ref to if_ixml_element,
            lv_tabix       type i,
            lv_maxlevel    type i,
            lv_levels      type string.

      create object lr_zip.

      lr_zip->load( lv_xlsx ).

*Get Worksheet XML file

      lr_zip->get( exporting name = 'xl/worksheets/sheet1.xml'
                   importing content = lv_file ).

      create object lr_file.
      lr_file->parse_xstring( lv_file ).
*Row elements are under SheetData
      lr_xlnode = lr_file->find_node( 'sheetData' ).
      lr_xlrows = lr_xlnode->get_children( ).
      do lr_xlrows->get_length( ) times.
        lv_tabix = sy-index - 1.
        lr_xlrow ?= lr_xlrows->get_item( lv_tabix ).
*Find the same node in the SALV Tree object
        read table lt_nodes into ls_node index lv_tabix.
        if sy-subrc eq 0.
          lr_node = ls_node-node.
*Find the level of the node
          clear lv_level.
          do.
            try.
                lr_node = lr_node->get_parent( ).
                add 1 to lv_level.
              catch cx_salv_msg.
                exit.
            endtry.
          enddo.

          subtract 1 from lv_level.

          if lv_level ne 0.
            lv_levels = lv_level.
            if lv_level > lv_maxlevel.
              lv_maxlevel = lv_level.
            endif.

            condense lv_levels.
*Assign the level to row
            lr_xlrow->set_attribute( name = 'outlineLevel' value = lv_levels ).
            lr_xlrow->set_attribute( name = 'hidden' value = 'true' ).
          endif.
        endif.
      enddo.
*Set maximum levels used in the sheet

      lv_levels = lv_maxlevel.

      condense lv_levels.

      lr_xlformat ?= lr_file->find_node( 'sheetFormatPr' ).

      lr_xlformat->set_attribute( name = 'outlineLevelRow' value = lv_levels ).

*Create new element in the XML file
      lr_xlworksheet ?= lr_file->find_node( 'worksheet' ).
      lr_xldimension ?= lr_file->find_node( 'dimension' ).
      lr_xlsheetpr = cl_ixml=>create( )->create_document( )->create_element( name = 'sheetPr' ).
      lr_xloutlinepr = cl_ixml=>create( )->create_document( )->create_element( name = 'outlinePr' ).
      lr_xlsheetpr->if_ixml_node~append_child( lr_xloutlinepr ).
      lr_xloutlinepr->set_attribute( name = 'summaryBelow' value = 'false' ).
      lr_xlworksheet->if_ixml_node~insert_child( new_child = lr_xlsheetpr ref_child = lr_xldimension ).

*Create Xstring file for the XML, and add it to Excel Zip file
      lr_file->render_2_xstring( importing stream = lv_file ).
      lr_zip->delete( exporting name = 'xl/worksheets/sheet1.xml' ).
      lr_zip->add( exporting name = 'xl/worksheets/sheet1.xml'
      content = lv_file ).

      lv_xlsx = lr_zip->save( ).

      zcl_ap_ficheros=>grabar_xstring( fichero = generar_excel xstring = lv_xlsx  ).
    endif.

  endmethod.
method end_of_page.

    clear o_content.
*  DATA: lr_header TYPE REF TO cl_salv_form_header_info,
*        l_text    TYPE string.
*  l_text = 'end of page for the report'.
*  CREATE OBJECT lr_header
*    EXPORTING
*      text    = l_text
*      tooltip = l_text.
*  o_content = lr_header.
  endmethod.
method expand_all.
    data: nodes type ref to cl_salv_nodes.

    nodes = o_alv->get_nodes( ).
    nodes->expand_all( ).

  endmethod.
method free.

    cl_gui_cfw=>flush( ).
    if not o_container is initial.
      o_container->free( ).
      clear o_container.
      cl_gui_cfw=>flush( ).
    endif.

    if not o_alv is initial.
      clear o_alv.
    endif.

  endmethod.
method get_f4_layout.
    data: ls_layout type salv_s_layout_info.

    ls_layout = cl_salv_layout_service=>f4_layouts(
                             s_key    = key_layout
                             restrict = cl_salv_layout=>restrict_none ).

    layout = ls_layout-layout.

  endmethod.
method get_fila_activa.

    clear: key.
*  i_rows = o_selections->get_selected_rows( ).
*  READ TABLE i_rows INTO fila INDEX 1.

*... §5.2 get the currently selected item
    data: lr_item type ref to cl_salv_item,
          ls_node type salv_s_nodes.

    o_selections = o_alv->get_selections( ).
    lr_item      = o_selections->get_selected_item( ).

    if lr_item is bound.
      ls_node-node = lr_item->get_node( ).
      ls_node-key  = ls_node-node->get_key( ).
      key         = ls_node-key.
    endif.

  endmethod.
method get_seleccion.
*  DATA: l_campo TYPE string,
*        l_row TYPE int4.
*
*  FIELD-SYMBOLS: <tabla> TYPE table,
*                 <check> TYPE ANY,
*                 <campo> TYPE c.
*
*  CLEAR i_rows.
*  IF NOT o_selections IS INITIAL.
*    i_rows = o_selections->get_selected_rows( ).
*
*    IF NOT campo_check IS INITIAL.
*      ASSIGN (nombre_tabla) TO <tabla>.
*      IF sy-subrc = 0.
*        LOOP AT <tabla> ASSIGNING <check>.
*          CONCATENATE '<CHECK>-' campo_check INTO l_campo.
*          ASSIGN (l_campo) TO <campo>.
*          READ TABLE i_rows FROM sy-tabix TRANSPORTING NO FIELDS.
*          IF sy-subrc = 0.
*            <campo> = 'X'.
*          ELSE.
*            CLEAR <campo>.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*
*    ENDIF.
*  ENDIF.

  endmethod.
method handle_double_click.

*  BREAK-POINT.
*    FIELD-SYMBOLS <scarr> TYPE scarr.
*    READ TABLE scarr_tab INDEX row ASSIGNING <scarr>.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*    IF column = 'CARRNAME'.
*      demo=>detail( <scarr>-carrid ).
*    ELSEIF column = 'URL'.
*      demo=>browser( <scarr>-url ).
*    ENDIF.

  endmethod.
method handle_end_of_page.
  endmethod.
method handle_top_of_page.

  endmethod.
method handle_user_command.

*  BREAK-POINT.

    get_seleccion( ).


*    FIELD-SYMBOLS <scarr> TYPE scarr.
*    READ TABLE scarr_tab INDEX row ASSIGNING <scarr>.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*    IF column = 'CARRNAME'.
*      demo=>detail( <scarr>-carrid ).
*    ELSEIF column = 'URL'.
*      demo=>browser( <scarr>-url ).
*    ENDIF.

  endmethod.
method inicializa_objetos.
    data: l_error  type string,
          l_string type string.

    check not o_alv is initial.
    o_columns = o_alv->get_columns( ).
    o_columns->set_optimize( optimize ).

*... § 2.1 build the hierarchy header
    data: settings type ref to cl_salv_tree_settings.

    settings = o_alv->get_tree_settings( ).
    settings->set_hierarchy_header( 'Jerarquía'(jer) ).
    settings->set_hierarchy_tooltip( 'Jerarquía'(jer) ).
    settings->set_hierarchy_size( 30 ).

    data: title type salv_de_tree_text.
    title = sy-title.
    settings->set_header( title ).


    o_layout = o_alv->get_layout( ).


    key_layout-report = cprog.
    if not handle is initial.
      key_layout-handle = handle.
    else.
      key_layout-handle = tabla.
    endif.

    o_layout->set_key( key_layout ).
    o_layout->set_default( 'X' ).
    o_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    me->status = status.

    o_selections = o_alv->get_selections( ).

* activate ALV generic Functions
    if botones_standard = 'X'.
      o_functions = o_alv->get_functions( ).
      o_functions->set_all( 'X' ).


*    o_alv->SET_SCREEN_STATUS( PFSTATUS = 'DISPLAY'
*                                 REPORT = SY-REPID
*                                 SET_FUNCTIONS = o_alv->C_FUNCTIONS_ALL ).
    endif.

    if not status is initial.
*        set_status( status ).
      call method o_alv->set_screen_status(
        exporting
          report        = sy-cprog
          pfstatus      = status
          set_functions = o_alv->c_functions_all ).
    endif.

    o_events = o_alv->get_event( ).
    set handler handle_double_click for o_events.
    set handler handle_user_command for o_events.


  endmethod.
method refresh.

  endmethod.
method set_agregacion.
    data l_columname type lvc_fname.

    l_columname = campo.
    if o_aggregation is initial.
      o_aggregation ?= o_alv->get_aggregations( ).
    endif.
    try.
        o_aggregation->add_aggregation(
          columnname  = l_columname
          aggregation = aggregation
        ).
*      catch cx_salv_data_error.
*      catch cx_salv_not_found.
*      catch cx_salv_existing.
      catch cx_root.
    endtry.

  endmethod.
method set_color.
    data: ls_color  type lvc_s_colo,
          lr_column type ref to cl_salv_column_table,
          l_column  type lvc_fname.

    l_column = campo.
    try.
        lr_column ?= o_columns->get_column( l_column ).
      catch cx_root.
        return.
    endtry.

    ls_color-col = color.

    ls_color-int = 0.

    ls_color-inv = 0.

    lr_column->set_color( ls_color ).

* El campo de color debe de ser: t_color TYPE lvc_t_scol
  endmethod.
method set_end_of_page.


  endmethod.
method set_field.
    data: l_column   type lvc_fname,
          i_column   type table of lvc_fname,
          l_error    type string,
          l_texto    type scrtext_s,
          l_texto2   type scrtext_l,
          l_texto3   type scrtext_m,
          l_decimals type lvc_decmls.

    split campo at ',' into table i_column.
    loop at i_column into l_column.
      translate l_column to upper case.
      try.
          call method o_columns->get_column
            exporting
              columnname = l_column
            receiving
              value      = o_column.
          case op.
            when 'EDIT' or 'INPUT'.  "o_column->set_edit( 'X' ).
            when 'NOOUT' or 'NO_OUT'. o_column->set_visible( ' ' ).
            when 'OUT'. o_column->set_visible( 'X' ).
            when 'DECIMALS'.
              l_decimals = valor.
              o_column->set_decimals( l_decimals ).
              if not valor2 is initial.
                try.
                    o_column->set_decimals_column( valor2 ).
                  catch cx_root.
                endtry.
              endif.
            when 'SUM' or 'DOSUM'.
              set_agregacion( campo ).
            when 'MAX'.
              set_agregacion(
                campo = campo
                aggregation = if_salv_c_aggregation=>maximum
              ).
            when 'MERGE'. "<fcat>-no_merging = ''.
            when 'KEY'. "<fcat>-key = 'X'.
            when 'LONG'. "<fcat>-outputlen = valor.
            when 'NO_CERO'. "<fcat>-no_zero = 'X'.
            when 'CHECKBOX'.
*           o_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
*            DATA lr_column  TYPE REF TO cl_salv_column_table.
*            lr_column ?= o_columns->get_column( l_column ).
*            lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).

            when 'DROPDOWN'.
              "<fcat>-drdn_hndl = valor.
            when 'TEXTO'.
              l_texto = valor.

              if valor2 is initial.
                l_texto2 = valor.
              else.
                l_texto2 = valor2.
              endif.
              o_column->set_short_text( l_texto ).
              l_texto3 = l_texto2.
              o_column->set_medium_text( l_texto3 ).
              o_column->set_long_text( l_texto2 ).
            when others.
              message e398(00) with 'Error en SET_FIELD OP='(esf) op
                                    'no definida'(nod) ''.
          endcase.
          .
        catch cx_salv_not_found .
          concatenate 'Campo'(cam) campo 'no existe'(noe) into l_error
            separated by space.
          message l_error type 'I'
                  display like 'E'.
      endtry.
    endloop.


  endmethod.
method set_layout.

    o_layout->set_initial_layout( layout ).

  endmethod.
method set_orden.
    data: l_columname type lvc_fname,
          l_sequence  type salv_de_sort_sequence.

    if down = 'X'.
      l_sequence = 2.
    else.
      l_sequence = 1.
    endif.

    l_columname = campo.
    try.
        o_sorts->add_sort( columnname = l_columname
                           subtotal = subtot
                           sequence = l_sequence
                           group    = group ).
      catch cx_root.
    endtry.

  endmethod.
method set_status.
    data: repid type sy-repid.

    repid = sy-repid.
    call method o_alv->set_screen_status(
      exporting
        report        = repid
        pfstatus      = status
        set_functions = o_alv->c_functions_all ).

  endmethod.
method set_top_of_page.

    set handler handle_top_of_page for o_events.
    top_of_page( ).
    o_alv->set_top_of_list( o_content ).

  endmethod.
method show.
    o_alv->display( ).
  endmethod.
method show_popup.

    o_alv->set_screen_popup(
      start_column = start_column
      end_column   = end_column
      start_line   = start_line
      end_line     = end_line ).

    o_alv->display( ).
  endmethod.
method top_of_page.

    clear o_content.
*  DATA: lr_grid   TYPE REF TO cl_salv_form_layout_grid,
*        lr_grid_1 TYPE REF TO cl_salv_form_layout_grid,
*        lr_label  TYPE REF TO cl_salv_form_label,
*        lr_text   TYPE REF TO cl_salv_form_text,
*        l_text    TYPE string.
*  CREATE OBJECT lr_grid.
*  l_text = 'TOP OF PAGE for the report' .
*  lr_grid->create_header_information(
*    row    = 1
*    column = 1
*    text    = l_text
*    tooltip = l_text ).
*  lr_grid->add_row( ).
*  lr_grid_1 = lr_grid->create_grid(
*                row    = 3
*                column = 1 ).
*  lr_label = lr_grid_1->create_label(
*    row     = 1
*    column  = 1
*    text    = 'Number of Data Records'
*    tooltip = 'Number of Data Records' ).
*  lr_text = lr_grid_1->create_text(
*    row     = 1
*    column  = 2
*    text    = '10'
*    tooltip = '10' ).
*  lr_label->set_label_for( lr_text ).
*  lr_label = lr_grid_1->create_label(
*    row    = 2
*    column = 1
*    text    = 'date'
*    tooltip = 'date' ).
*  l_text = 'today'.
*  lr_text = lr_grid_1->create_text(
*    row    = 2
*    column = 2
*    text    = 'today'
*    tooltip = 'today' ).
*  lr_label->set_label_for( lr_text ).
*  o_content = lr_grid.

  endmethod.
