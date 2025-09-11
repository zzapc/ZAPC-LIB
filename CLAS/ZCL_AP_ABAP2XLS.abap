CLASS zcl_ap_abap2xls DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_error,
        hoja    TYPE string,
        row     TYPE et_row,
        col     TYPE et_col,
        message TYPE bapi_msg,
      END OF t_error.
    TYPES tt_error TYPE STANDARD TABLE OF t_error.

    DATA o_excel                        TYPE REF TO zcl_excel.
    DATA o_border_x                     TYPE REF TO zcl_excel_style_border.
    DATA o_border_m                     TYPE REF TO zcl_excel_style_border.
    DATA o_worksheet                    TYPE REF TO zcl_excel_worksheet.
    DATA o_excel_writer                 TYPE REF TO zif_excel_writer.
    DATA o_column                       TYPE REF TO zcl_excel_column.
    DATA o_style_guid_recuadro          TYPE zexcel_cell_style.
    DATA o_style_guid_recuadro_centrado TYPE zexcel_cell_style.
    DATA nombre_hoja                    TYPE zexcel_sheet_title.
    DATA highest_column                 TYPE i.
    DATA highest_row                    TYPE i.
    DATA zcx_excel                      TYPE REF TO zcx_excel.
    DATA i_error                        TYPE tt_error.

    METHODS constructor
      IMPORTING o_excel TYPE REF TO zcl_excel OPTIONAL.

    METHODS get_style_guid
      IMPORTING borders       TYPE any                    DEFAULT ''
                bold          TYPE abap_bool              DEFAULT ''
                centrado      TYPE abap_bool              DEFAULT ''
                font_size     TYPE zexcel_style_font_size DEFAULT 0
                derecha       TYPE abap_bool              DEFAULT ''
                color_fondo   TYPE any                    DEFAULT ''
                number_format TYPE any                    OPTIONAL
      RETURNING VALUE(guid)   TYPE zexcel_cell_style.

    METHODS mostrar_en_pantalla.

    METHODS set_cell
      IMPORTING col       TYPE simple
                !row      TYPE zexcel_cell_row
                !value    TYPE simple                     OPTIONAL
                formula   TYPE zexcel_cell_formula        OPTIONAL
                !style    TYPE zexcel_cell_style          OPTIONAL
                hyperlink TYPE REF TO zcl_excel_hyperlink OPTIONAL
                data_type TYPE zexcel_cell_data_type      OPTIONAL
                abap_type TYPE abap_typekind              OPTIONAL.

    METHODS set_column
      IMPORTING col      TYPE simple    OPTIONAL
                !width   TYPE int4      DEFAULT 0
                !visible TYPE abap_bool DEFAULT ''
                autosize TYPE abap_bool DEFAULT ''
                col_int  TYPE int4      OPTIONAL
      PREFERRED PARAMETER col.

    METHODS set_border_outline_range
      IMPORTING col_start           TYPE string                   DEFAULT 'A'
                VALUE(row_start)    TYPE zexcel_cell_row          DEFAULT 1
                col_end             TYPE string                   OPTIONAL
                VALUE(row_end)      TYPE zexcel_cell_row          OPTIONAL
                VALUE(border_style) TYPE zexcel_border            DEFAULT zcl_excel_style_border=>c_border_thin
                VALUE(border_color) TYPE zexcel_s_style_color-rgb DEFAULT zcl_excel_style_color=>c_black.

    METHODS set_border_range
      IMPORTING col_start    TYPE string                   DEFAULT 'A'
                row_start    TYPE zexcel_cell_row          DEFAULT 1
                col_end      TYPE string
                row_end      TYPE zexcel_cell_row
                border_style TYPE zexcel_border            DEFAULT zcl_excel_style_border=>c_border_thin
                border_color TYPE zexcel_s_style_color-rgb DEFAULT zcl_excel_style_color=>c_black.

    METHODS set_bgcolor_range
      IMPORTING col_start TYPE string                   DEFAULT 'A'
                row_start TYPE zexcel_cell_row          DEFAULT 1
                col_end   TYPE string
                row_end   TYPE zexcel_cell_row
                bg_color  TYPE zexcel_s_style_color-rgb DEFAULT zcl_excel_style_color=>c_white.

    METHODS set_fontcolor_range
      IMPORTING col_start  TYPE string                   DEFAULT 'A'
                row_start  TYPE zexcel_cell_row          DEFAULT 1
                col_end    TYPE string
                row_end    TYPE zexcel_cell_row
                text_color TYPE zexcel_s_style_color-rgb DEFAULT zcl_excel_style_color=>c_black.

    METHODS set_fontsize_range
      IMPORTING col_start TYPE string                 DEFAULT 'A'
                row_start TYPE zexcel_cell_row        DEFAULT 1
                col_end   TYPE string
                row_end   TYPE zexcel_cell_row
                text_size TYPE zexcel_style_font_size DEFAULT 11.

    METHODS set_fontstyle_bold_range
      IMPORTING col_start TYPE string          DEFAULT 'A'
                row_start TYPE zexcel_cell_row DEFAULT 1
                col_end   TYPE string
                row_end   TYPE zexcel_cell_row.

    METHODS lee_fichero
      IMPORTING fichero              TYPE any            OPTIONAL
                hoja                 TYPE any            DEFAULT ''
                mostrar_error        TYPE abap_bool      DEFAULT 'X'
                servidor             TYPE abap_bool      DEFAULT ''
                get_datos            TYPE abap_bool      DEFAULT ''
                lineas_cabecera      TYPE int4           DEFAULT 1
                url                  TYPE abap_bool      DEFAULT ''
                huge                 TYPE abap_bool      DEFAULT ''
                xstring              TYPE xstring        OPTIONAL
                max_col              TYPE i              DEFAULT 0
                validar_maestros     TYPE abap_bool      DEFAULT ''
                popup_select_file    TYPE abap_bool      DEFAULT ''
                rfcdest              TYPE rfcdes-rfcdest DEFAULT 'SAPHTTP'
                existe_valor         TYPE any            DEFAULT ''
                col_existe_valor     TYPE int4           DEFAULT 0
                opciones             TYPE string         DEFAULT ''
      EXPORTING fila_existe_valor    TYPE int4
                columna_existe_valor TYPE int4
                !message             TYPE bapi_msg
                datos                TYPE STANDARD TABLE
                errores              TYPE table_of_strings
                si_existe_valor      TYPE abap_bool.

    METHODS get_tabla
      IMPORTING iv_skipped_rows  TYPE int4      DEFAULT 0
                iv_skipped_cols  TYPE int4      DEFAULT 0
                lineas_cabecera  TYPE int4      DEFAULT 1
                conv_ctd         TYPE abap_bool DEFAULT 'X'
                max_col          TYPE i         DEFAULT 0
                validar_maestros TYPE abap_bool DEFAULT ''
                opciones         TYPE string    DEFAULT ''
      EXPORTING et_table         TYPE STANDARD TABLE
                et_errores       TYPE table_of_strings.

    METHODS set_tabla
      IMPORTING tabla            TYPE table
                autosize         TYPE abap_bool             DEFAULT 'X'
                titulo           TYPE any                   DEFAULT ''
                nombre_columna   TYPE any                   DEFAULT ''
                it_field_catalog TYPE zexcel_t_fieldcatalog OPTIONAL.

    METHODS get_xstring
      IMPORTING huge     TYPE abap_bool DEFAULT ''
      EXPORTING xstring  TYPE xstring
                longitud TYPE int4.

    METHODS set_alv
      IMPORTING tabla                TYPE table
                alv                  TYPE REF TO object
                refresh_metadata     TYPE abap_bool DEFAULT ''
                convert_txt_2_number TYPE abap_bool DEFAULT ''.

    METHODS add_adj_mail
      IMPORTING o_mail  TYPE REF TO zcl_ap_envio_mail
                fichero TYPE any
                add_ext TYPE abap_bool DEFAULT ''.

    METHODS lee_hoja
      IMPORTING hoja                 TYPE any       DEFAULT ''
                lineas_cabecera      TYPE int4      DEFAULT 1
                get_datos            TYPE abap_bool DEFAULT ''
                mostrar_error        TYPE abap_bool DEFAULT 'X'
                max_col              TYPE i         DEFAULT 0
                validar_maestros     TYPE abap_bool DEFAULT ''
                existe_valor         TYPE any       DEFAULT ''
                col_existe_valor     TYPE int4      DEFAULT 0
                opciones             TYPE string    DEFAULT ''
      EXPORTING si_existe_valor      TYPE abap_bool
                !message             TYPE bapi_msg
                datos                TYPE STANDARD TABLE
                errores              TYPE table_of_strings
                fila_existe_valor    TYPE int4
                columna_existe_valor TYPE int4.

    METHODS graba_fichero
      IMPORTING fichero  TYPE any
                huge     TYPE abap_bool DEFAULT ''
                servidor TYPE abap_bool DEFAULT ''
                !local   TYPE abap_bool DEFAULT ''
                dialogo  TYPE abap_bool DEFAULT ''
                abrir    TYPE abap_bool DEFAULT ''
      EXPORTING !message TYPE bapi_msg.

    METHODS reemplazar_valor
      IMPORTING !var         TYPE any
                valor        TYPE any
                car_marca    TYPE any DEFAULT '&'
                parcial      TYPE any DEFAULT ''
      RETURNING VALUE(error) TYPE abap_bool.

    METHODS lee_plantilla
      IMPORTING tcode     TYPE any OPTIONAL
                plantilla TYPE any OPTIONAL
      EXPORTING !message  TYPE bapi_msg.

    CLASS-METHODS alv_2_xls
      IMPORTING tabla                     TYPE table
                alv                       TYPE REF TO object         OPTIONAL
                huge                      TYPE abap_bool             DEFAULT ''
                grabar                    TYPE abap_bool             DEFAULT ''
                abrir                     TYPE abap_bool             DEFAULT ''
                ruta                      TYPE any                   DEFAULT ''
                nombre_fichero            TYPE any                   DEFAULT ''
                nf1                       TYPE any                   DEFAULT ''
                nf2                       TYPE any                   DEFAULT ''
                convert_txt_2_number      TYPE abap_bool             DEFAULT ''
                servidor                  TYPE abap_bool             DEFAULT ''
                !local                    TYPE abap_bool             DEFAULT ''
                eliminar_columnas_ocultas TYPE abap_bool             DEFAULT 'X'
                mail_destino              TYPE any                   DEFAULT ''
                mail_texto                TYPE any                   DEFAULT ''
                mail_asunto               TYPE any                   DEFAULT ''
                refresh_metadata          TYPE abap_bool             DEFAULT ''
                titulo                    TYPE any                   DEFAULT 'Hoja1'
                nombre_columna            TYPE any                   DEFAULT ''
                it_field_catalog          TYPE zexcel_t_fieldcatalog OPTIONAL
                quitar_campos             TYPE any                   DEFAULT ''
      EXPORTING !error                    TYPE bapi_msg
      RETURNING VALUE(xstring)            TYPE xstring.

    METHODS mostrar_errores.

    METHODS get_msg
      IMPORTING !row           TYPE int4
                col            TYPE int4 OPTIONAL
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS get_alv_simple
      IMPORTING show         TYPE abap_bool DEFAULT 'X'
      CHANGING  tabla        TYPE STANDARD TABLE
      RETURNING VALUE(o_alv) TYPE REF TO zcl_ap_alv.

    CLASS-METHODS mail
      IMPORTING direccion      TYPE any
                subject        TYPE any
                tabla1         TYPE table                  OPTIONAL
                tabla2         TYPE table                  OPTIONAL
                tabla3         TYPE table                  OPTIONAL
                o_alv1         TYPE REF TO zcl_ap_alv      OPTIONAL
                tabla_alv1     TYPE table                  OPTIONAL
                nombre_tabla1  TYPE any                    OPTIONAL
                nombre_tabla2  TYPE any                    OPTIONAL
                nombre_tabla3  TYPE any                    OPTIONAL
                nombre_alv1    TYPE any                    OPTIONAL
                !commit        TYPE abap_bool              DEFAULT 'X'
                texto          TYPE any                    DEFAULT ''
                i_textos       TYPE rspc_t_text            OPTIONAL
                o_grid1        TYPE REF TO zcl_ap_alv_grid OPTIONAL
                tabla_grid1    TYPE table                  OPTIONAL
                nombre_grid1   TYPE any                    OPTIONAL
                o_alv2         TYPE REF TO zcl_ap_alv      OPTIONAL
                tabla_alv2     TYPE table                  OPTIONAL
                nombre_alv2    TYPE any                    OPTIONAL
                o_grid2        TYPE REF TO zcl_ap_alv_grid OPTIONAL
                tabla_grid2    TYPE table                  OPTIONAL
                nombre_grid2   TYPE any                    OPTIONAL
      RETURNING VALUE(message) TYPE bapi_msg.

    METHODS get_fichero_desde_zip
      IMPORTING mostrar_error TYPE abap_bool DEFAULT 'X'
                huge          TYPE abap_bool DEFAULT ''
                fichero       TYPE string    OPTIONAL
                binario       TYPE xstring
      EXPORTING xstring       TYPE xstring
                !message      TYPE bapi_msg.

    METHODS existe_valor
      IMPORTING valor            TYPE any
                col_valor        TYPE int4
      EXPORTING !message         TYPE bapi_msg
                fila             TYPE int4
                columna          TYPE int4
      RETURNING VALUE(si_existe) TYPE abap_bool.

    METHODS copiar_fila
      IMPORTING fila           TYPE int4
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS get_cell_coord
      IMPORTING fila         TYPE int4
                col          TYPE int4
      RETURNING VALUE(coord) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_ABAP2XLS definition
class ZCL_AP_ABAP2XLS implementation.
  METHOD add_adj_mail.
    DATA: l_xstring  TYPE xstring,
          l_longitud TYPE int4,
          l_fichero  TYPE string.

    IF o_mail IS INITIAL.
      RETURN.
    ENDIF.

    get_xstring( IMPORTING xstring  = l_xstring
                           longitud = l_longitud ).
    IF l_xstring IS INITIAL.
      RETURN.
    ENDIF.

    IF add_ext IS INITIAL.
      l_fichero = fichero.
    ELSE.
      l_fichero = zcl_ap_ficheros=>get_extension( fichero ).
      l_fichero = to_upper( l_fichero ).
      IF l_fichero = 'XLSX'.
        l_fichero = fichero.
      ELSE.
        CONCATENATE fichero '.xlsx' INTO l_fichero.
      ENDIF.
    ENDIF.

    o_mail->add_adjunto( tipo     = 'XLS'
                         titulo   = fichero
                         xstring  = l_xstring
                         longitud = l_longitud ).
  ENDMETHOD.
  METHOD alv_2_xls.
    DATA: o_xls     TYPE REF TO zcl_ap_abap2xls,
          l_aux1    TYPE text255,
          l_fichero TYPE string,
          l_message TYPE bapi_msg.
    DATA: o_alv_local TYPE REF TO cl_salv_table,
          o_colt      TYPE REF TO cl_salv_columns_table,
          i_cols      TYPE salv_t_column_ref.
    DATA: o_grid_local TYPE REF TO cl_gui_alv_grid,
          t_fcat       TYPE lvc_t_fcat.

    FIELD-SYMBOLS <col> TYPE salv_s_column_ref.

    CLEAR: error,
           xstring.

    o_xls = NEW #( ).
    IF alv IS INITIAL.
      o_xls->set_tabla( tabla            = tabla
                        nombre_columna   = nombre_columna
                        it_field_catalog = it_field_catalog ).
    ELSE.

      IF eliminar_columnas_ocultas = 'X'.
        l_aux1 = cl_abap_classdescr=>get_class_name( alv ).
        IF l_aux1 CS 'CL_SALV_TABLE'.
          o_alv_local ?= alv.

          o_colt = o_alv_local->get_columns( ).
          i_cols = o_colt->get( ).

          LOOP AT i_cols ASSIGNING <col>.
            IF <col>-r_column->is_visible( ) = ''.
              <col>-r_column->set_technical( if_salv_c_bool_sap=>true ).
            ELSEIF NOT quitar_campos IS INITIAL.
              IF zcl_ap_lista=>es_elemento( lista    = quitar_campos
                                            elemento = <col>-columnname ).
                <col>-r_column->set_technical( if_salv_c_bool_sap=>true ).
              ENDIF.
            ENDIF.
          ENDLOOP.

          o_xls->set_alv( alv                  = o_alv_local
                          tabla                = tabla
                          refresh_metadata     = refresh_metadata
                          convert_txt_2_number = convert_txt_2_number  ).
        ELSEIF l_aux1 CS 'CL_GUI_ALV_GRID'.
          o_grid_local ?= alv.
          o_grid_local->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = t_fcat ).
          DELETE t_fcat WHERE no_out = 'X'.              "#EC CI_STDSEQ
          IF NOT quitar_campos IS INITIAL.
            LOOP AT t_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
              IF zcl_ap_lista=>es_elemento( lista    = quitar_campos
                                            elemento = <fcat>-fieldname ).
                DELETE t_fcat.
              ENDIF.
            ENDLOOP.
          ENDIF.
          o_grid_local->set_frontend_fieldcatalog( it_fieldcatalog = t_fcat ).
          o_xls->set_alv( alv   = o_grid_local
                          tabla = tabla ).
        ELSE.
          o_xls->set_alv( alv                  = alv
                          tabla                = tabla
                          refresh_metadata     = refresh_metadata
                          convert_txt_2_number = convert_txt_2_number ).
        ENDIF.
      ELSE.
        o_xls->set_alv( alv                  = alv
                        tabla                = tabla
                        refresh_metadata     = refresh_metadata
                        convert_txt_2_number = convert_txt_2_number ).
      ENDIF.
    ENDIF.

    TRY.
        o_xls->o_worksheet->set_title( titulo ).
      CATCH zcx_excel.
        error = 'Error modificando título'.
        RETURN.
    ENDTRY.

    o_xls->get_xstring( EXPORTING huge    = huge
                        IMPORTING xstring = xstring ).

    IF nombre_fichero IS INITIAL.
      __concat3 l_aux1 'fichero' nf1 nf2.
    ELSE.
      __concat3 l_aux1 nombre_fichero nf1 nf2.
    ENDIF.
    DATA(l_extension) = zcl_ap_ficheros=>get_extension( l_aux1 ).
    l_extension = to_upper( l_extension ).
    IF l_extension = 'XLX' OR l_extension = 'XLSX'.
      l_fichero = l_aux1.
    ELSE.
      CONCATENATE l_aux1 '.xlsx' INTO l_fichero.
    ENDIF.

    IF grabar = 'X' OR abrir = 'X'.
      IF ruta IS INITIAL.
        l_fichero = zcl_ap_ficheros=>concat_ruta( directorio_temporal = 'X'
                                                  fichero             = l_fichero ).
      ELSEIF nombre_fichero IS INITIAL AND nf1 IS INITIAL AND nf2 IS INITIAL AND ruta CS '.'.
        l_fichero = ruta.
      ELSEIF ruta = '?'.
        zcl_ap_ficheros=>dialogo_grabar_fichero( EXPORTING fichero_inicial = l_fichero
                                                           mostrar_error   = 'X'
                                                 IMPORTING ruta            = l_fichero ).
      ELSE.
        l_fichero = zcl_ap_ficheros=>concat_ruta( directorio = ruta
                                                  fichero    = l_fichero ).
      ENDIF.
      o_xls->graba_fichero( EXPORTING fichero  = l_fichero
                                      huge     = huge
                                      servidor = servidor
                                      local    = local
                            IMPORTING message  = l_message ).
      IF grabar = 'X'.
        IF l_message IS INITIAL.
          MESSAGE s106(lx) WITH l_fichero.
        ELSE.
          error = |Error grabando fichero { l_fichero }: { l_message }|.
          MESSAGE l_message TYPE 'I'.
        ENDIF.
      ENDIF.
      IF abrir = 'X'.
        zcl_ap_gos=>visualizar_fichero_st( fichero = l_fichero ).
      ENDIF.
    ENDIF.

    IF NOT mail_destino IS INITIAL.
      zcl_ap_envio_mail=>mail( EXPORTING subject              = mail_asunto
                                         direccion            = mail_destino
                                         nombre_fichero_tabla = l_fichero
                                         texto                = mail_texto
                                         xstring              = xstring
                               IMPORTING message              = l_message ).
      IF l_message IS INITIAL.
        CONCATENATE 'Se ha enviado mail a' mail_destino INTO l_message SEPARATED BY space.
        MESSAGE l_message TYPE 'S'.
      ELSE.
        MESSAGE l_message TYPE 'I'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    IF o_excel IS INITIAL.
      me->o_excel = NEW #( ).
    ELSE.
      me->o_excel = o_excel.
    ENDIF.

    o_worksheet = me->o_excel->get_active_worksheet( ).

    o_border_x = NEW #( ).
    o_border_x->border_color-rgb = zcl_excel_style_color=>c_black.
    o_border_x->border_style = zcl_excel_style_border=>c_border_thin.

    o_border_m = NEW #( ).
    o_border_m->border_color-rgb = zcl_excel_style_color=>c_black.
    o_border_m->border_style = zcl_excel_style_border=>c_border_medium.

    o_style_guid_recuadro          = get_style_guid( borders = 'DOWN=X,TOP=X,LEFT=X,RIGHT=X'  ).
    o_style_guid_recuadro_centrado = get_style_guid( centrado = 'X'
                                                     borders  = 'DOWN=X,TOP=X,LEFT=X,RIGHT=X' ).
  ENDMETHOD.
  METHOD copiar_fila.
    DATA: i_nueva_fila TYPE zexcel_t_cell_data,
          l_fila       TYPE int4,
          i_cambios    TYPE zexcel_t_cell_data.

    CLEAR message.

    LOOP AT o_worksheet->sheet_content ASSIGNING FIELD-SYMBOL(<cell_data>) WHERE cell_row = fila.
      APPEND <cell_data> TO i_nueva_fila.
    ENDLOOP.
    IF sy-subrc <> 0.
      message = 'No existe la fila a copiar'.
      RETURN.
    ENDIF.

    LOOP AT o_worksheet->sheet_content INTO DATA(l_cambio) WHERE cell_row > fila. "#EC CI_SORTSEQ
      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA(l_fila_orig) = l_cambio-cell_row.
      l_fila = l_cambio-cell_row + 1.

      l_cambio-cell_row    = l_fila.
      l_cambio-cell_coords = get_cell_coord( fila = l_fila
                                             col  = l_cambio-cell_column ).
      APPEND l_cambio TO i_cambios.
      DELETE o_worksheet->sheet_content.
    ENDLOOP.

    LOOP AT i_nueva_fila INTO l_cambio.
      l_cambio-cell_row    = fila + 1.
      l_cambio-cell_coords = get_cell_coord( fila = l_cambio-cell_row
                                             col  = l_cambio-cell_column ).
      APPEND l_cambio TO o_worksheet->sheet_content.
    ENDLOOP.

    LOOP AT i_cambios ASSIGNING <cell_data>.
      APPEND <cell_data> TO o_worksheet->sheet_content.
    ENDLOOP.
  ENDMETHOD.
  METHOD existe_valor.
*--------------------------------------------------------------------*
* Comment D. Rauchenstein
* With this method, we get a fully functional Excel Upload, which solves
* a few issues of the other excel upload tools
* ZBCABA_ALSM_EXCEL_UPLOAD_EXT: Reads only up to 50 signs per Cell, Limit
* in row-Numbers. Other have Limitations of Lines, or you are not able
* to ignore filters or choosing the right tab.
*
* To get a fully functional XLSX Upload, you can use it e.g. with method
* CL_EXCEL_READER_2007->ZIF_EXCEL_READER~LOAD_FILE()
*--------------------------------------------------------------------*

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA: l_nombre_hoja TYPE string,
          o_error_excel TYPE REF TO zcx_excel.
    DATA lv_max_col    TYPE zexcel_cell_column.
    DATA lv_max_row    TYPE int4.
    DATA lv_actual_row TYPE int4.
    DATA lv_actual_col TYPE int4.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_delta_col  TYPE int4.
    DATA lv_value      TYPE zexcel_cell_value.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_rc         TYPE sysubrc.

    CLEAR: message,
           si_existe.

    l_nombre_hoja = o_worksheet->get_title( ).

    TRY.
        lv_max_col = o_worksheet->get_highest_column( ).

        IF lv_max_col > 16000. " Ha veces hay un error!
          lv_max_col = 255.
        ENDIF.
      CATCH zcx_excel INTO o_error_excel.
        message = o_error_excel->get_longtext( ).
        RETURN.
    ENDTRY.

    TRY.
        lv_max_row = o_worksheet->get_highest_row( ).
      CATCH zcx_excel INTO o_error_excel.
        message = o_error_excel->get_longtext( ).
        RETURN.
    ENDTRY.

    IF lv_max_col <= 0.
      RETURN.
    ENDIF.
    WHILE lv_actual_row <= lv_max_row.
      lv_actual_col = 1.

      WHILE lv_actual_col <= lv_max_col.
        IF lv_actual_col = col_valor OR col_valor = 0.
          lv_delta_col = lv_actual_col.

          TRY.
              o_worksheet->get_cell( EXPORTING ip_column = lv_actual_col    " Cell Column
                                               ip_row    = lv_actual_row    " Cell Row
                                     IMPORTING ep_value  = lv_value    " Cell Value
                                               ep_rc     = lv_rc ). " Return Value of ABAP Statements
            CATCH zcx_excel INTO o_error_excel.
              message = o_error_excel->get_longtext( ).
          ENDTRY.

          IF lv_value = valor.
            si_existe = 'X'.
            fila = lv_actual_row.
            columna = lv_actual_col.
            RETURN.
          ENDIF.
        ENDIF.
        lv_actual_col = lv_actual_col + 1.
      ENDWHILE.
      lv_actual_row = lv_actual_row + 1.
    ENDWHILE.
  ENDMETHOD.
  METHOD get_alv_simple.
    o_alv = NEW #( tabla = '' ).

    o_alv->constructor_tabla( CHANGING t_tabla = tabla ).

    IF show = 'X'.
      cl_salv_bs_runtime_info=>set( display  = abap_false
                                    metadata = abap_false
                                    data     = abap_true ).
      o_alv->show( ).
      cl_salv_bs_runtime_info=>clear_all( ).
    ENDIF.
  ENDMETHOD.
  METHOD get_cell_coord.
    DATA: lv_row_alpha TYPE string,
          lv_col_alpha TYPE zexcel_cell_column_alpha.

    lv_row_alpha = fila.
    CONDENSE lv_row_alpha NO-GAPS.
    TRY.
        lv_col_alpha = zcl_excel_common=>convert_column2alpha( col ).
      CATCH zcx_excel INTO DATA(zcx_excel).
        DATA(message) = zcx_excel->get_longtext( ).
        MESSAGE message TYPE 'E'.
    ENDTRY.
    CONCATENATE lv_col_alpha lv_row_alpha INTO coord.
  ENDMETHOD.
  METHOD get_fichero_desde_zip.
    DATA o_reader TYPE REF TO zif_excel_reader.

    IF huge IS INITIAL.
      o_reader = NEW zcl_excel_reader_2007( ).
    ELSE.
      o_reader = NEW zcl_excel_reader_huge_file( ).
    ENDIF.

    TRY.
        o_excel = o_reader->load( i_excel2007 = binario ).
      CATCH zcx_excel INTO zcx_excel.
        message = zcx_excel->get_longtext( ).
    ENDTRY.

*    xstring = o_reader->get_fichero_desde_zip( fichero ).
    TRY.
*    xstring = o_reader->get_fichero_desde_zip( fichero ).
        CALL METHOD o_reader->('get_fichero_desde_zip')
          EXPORTING
            I_FILENAME = fichero
          RECEIVING
            R_CONTENT = xstring.
      CATCH cx_root INTO DATA(o_root).
        message = o_root->get_longtext( ).
    ENDTRY.


    IF mostrar_error = 'X' AND NOT message IS INITIAL.
      MESSAGE message TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD get_msg.
    FIELD-SYMBOLS <error> TYPE t_error.

    CLEAR message.

    LOOP AT i_error ASSIGNING <error> WHERE row = row.   "#EC CI_STDSEQ
      IF <error>-col = col OR col IS INITIAL.
        __add_lista message <error>-message.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_style_guid.
    DATA: o_style  TYPE REF TO zcl_excel_style,
          tabla    TYPE TABLE OF string,
          lado     TYPE string,
          borde    TYPE string,
          o_border TYPE REF TO zcl_excel_style_border.

    FIELD-SYMBOLS <tabla> TYPE string.

    o_style = o_excel->add_new_style( ).

    SPLIT borders AT ',' INTO TABLE tabla.
    LOOP AT tabla ASSIGNING <tabla>.
      SPLIT <tabla> AT '=' INTO lado borde.
      IF borde = 'X' OR borde = ''.
        o_border = o_border_x.
      ELSEIF borde = 'M'.
        o_border = o_border_m.
      ENDIF.
      CASE lado.
        WHEN 'DOWN'. o_style->borders->down = o_border.
        WHEN 'TOP'. o_style->borders->top = o_border.
        WHEN 'LEFT'. o_style->borders->left = o_border.
        WHEN 'RIGHT'. o_style->borders->right = o_border.
        WHEN 'ALLBORDERS'. o_style->borders->allborders = o_border.
      ENDCASE.
    ENDLOOP.

    o_style->font->bold = bold.

    IF NOT font_size IS INITIAL.
      o_style->font->size = font_size.
    ENDIF.

    IF centrado = 'X'.
      o_style->alignment->horizontal = 'center'.
    ELSEIF derecha = 'X'.
      o_style->alignment->horizontal = 'right'.
    ENDIF.

    IF NOT color_fondo IS INITIAL.
      o_style->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
      o_style->fill->fgcolor-rgb = color_fondo.
    ENDIF.

    IF NOT number_format IS INITIAL.
      o_style->number_format->format_code = number_format.
    ENDIF.

    guid = o_style->get_guid( ).
  ENDMETHOD.
  METHOD get_tabla.
*--------------------------------------------------------------------*
* Comment D. Rauchenstein
* With this method, we get a fully functional Excel Upload, which solves
* a few issues of the other excel upload tools
* ZBCABA_ALSM_EXCEL_UPLOAD_EXT: Reads only up to 50 signs per Cell, Limit
* in row-Numbers. Other have Limitations of Lines, or you are not able
* to ignore filters or choosing the right tab.
*
* To get a fully functional XLSX Upload, you can use it e.g. with method
* CL_EXCEL_READER_2007->ZIF_EXCEL_READER~LOAD_FILE()
*--------------------------------------------------------------------*

    DATA: l_nombre_hoja TYPE string,
          o_error_excel TYPE REF TO zcx_excel,
          l_msg         TYPE bapi_msg,
          l_tipo        TYPE c LENGTH 1,
          l_mask        TYPE string,
          l_fecha       TYPE d,
          l_string      TYPE string,
          l_aux         TYPE string,
          l_horat       TYPE string,
          l_long        TYPE i,
          l_segundos    TYPE int4,
          l_hora        TYPE uzeit,
          l_potencia    TYPE string,
          cx_root       TYPE REF TO cx_root,
          l_long2       TYPE i,
          l_error       TYPE t_error.
    DATA lv_max_col       TYPE zexcel_cell_column.
    DATA lv_max_row       TYPE int4.
    DATA lv_actual_row    TYPE int4.
    DATA lv_actual_col    TYPE int4.
    DATA lv_errormessage  TYPE string.
    DATA lv_delta_col     TYPE int4.
    DATA lv_col_ficticias TYPE int4.
    DATA lv_value         TYPE zexcel_cell_value.
    DATA lv_rc            TYPE sysubrc.
    DATA l_campo_hora     TYPE fieldname.

    FIELD-SYMBOLS <ls_line>  TYPE data.
    FIELD-SYMBOLS <lv_value> TYPE data.

    FIELD-SYMBOLS <fs>       TYPE any.

    l_nombre_hoja = o_worksheet->get_title( ).
    DEFINE add_error.
      CLEAR l_error.
      l_error-hoja    = l_nombre_hoja.
      l_error-row     = lv_actual_row.
      l_error-col     = lv_actual_col.
      l_error-message = &1.
      APPEND l_error TO i_error.
    END-OF-DEFINITION.

    CLEAR et_errores.

    TRY.
        lv_max_col = o_worksheet->get_highest_column( ).

        IF lv_max_col > 16000. " Ha veces hay un error!
          lv_max_col = 255.
        ENDIF.
      CATCH zcx_excel INTO o_error_excel.
        l_msg = o_error_excel->get_longtext( ).
    ENDTRY.

    TRY.
        lv_max_row = o_worksheet->get_highest_row( ).
      CATCH zcx_excel INTO o_error_excel.
        l_msg = o_error_excel->get_longtext( ).
    ENDTRY.

    IF max_col <> 0.
      DATA(l_max_col) = max_col.
      IF max_col = -1. " Con esto indicamos que queremos las filas de la tabla interna
        l_max_col = lines( zcl_ap_dev=>get_fieldcatalog_tabla_alv( et_table ) ).
      ENDIF.
      IF l_max_col < lv_max_col.
        lv_max_col = l_max_col.
      ENDIF.
    ENDIF.

*--------------------------------------------------------------------*
* The row counter begins with 1 and should be corrected with the skips
*--------------------------------------------------------------------*
    lv_actual_row = iv_skipped_rows + 1.
    lv_actual_col = iv_skipped_cols + 1.

    TRY.
        APPEND INITIAL LINE TO et_table ASSIGNING <ls_line>.
        IF sy-subrc <> 0 OR <ls_line> IS NOT ASSIGNED.
          lv_errormessage = 'Error insertando nueva entrada en tabla interna'(eit).
          APPEND lv_errormessage TO et_errores.
          add_error lv_errormessage.
        ELSE.
          DATA(it_fieldcat) = zcl_ap_dev=>get_fieldcatalog_tabla_alv( et_table ).
          DO 20 TIMES.
            lv_delta_col = lv_max_col - iv_skipped_cols.
            ASSIGN COMPONENT lv_delta_col OF STRUCTURE <ls_line> TO <lv_value>.
            IF sy-subrc <> 0 OR <lv_value> IS NOT ASSIGNED.
              IF NOT opciones CS 'IGNORAR_FICHERO_CON_MAS_COLUMNAS'.
                lv_errormessage = 'Tabla interna tiene menos valores que columnas en Excel'(tme).
                COLLECT lv_errormessage INTO et_errores.
                add_error lv_errormessage.
              ENDIF.
              lv_max_col = lv_max_col - 1.
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.
*           ELSE. "APC20180702
          IF lv_max_col > 0. " APC20180702
*--------------------------------------------------------------------*
*now we are ready for handle the table data
*--------------------------------------------------------------------*
            REFRESH et_table.
*--------------------------------------------------------------------*
* Handle each Row until end on right side
*--------------------------------------------------------------------*
            WHILE lv_actual_row <= lv_max_row.
              IF lv_actual_row <= lineas_cabecera.
                lv_actual_row = lv_actual_row + 1.
                CONTINUE.
              ENDIF.

              CLEAR lv_col_ficticias.
*--------------------------------------------------------------------*
* Handle each Column until end on bottom
* First step is to step back on first column
*--------------------------------------------------------------------*
              lv_actual_col = iv_skipped_cols + 1.

              UNASSIGN <ls_line>.
              APPEND INITIAL LINE TO et_table ASSIGNING <ls_line>.
              IF sy-subrc <> 0 OR <ls_line> IS NOT ASSIGNED.
                lv_errormessage = 'Error insertando nueva entrada en tabla interna'(eit).
*APC
                APPEND lv_errormessage TO et_errores.
*           RAISE EXCEPTION TYPE zcx_excel
*             EXPORTING
*               error = lv_errormessage.

              ENDIF.
              WHILE lv_actual_col <= lv_max_col.
                TRY.
                    lv_delta_col = lv_actual_col - iv_skipped_cols + lv_col_ficticias.

                    DO 3 TIMES.
                      ASSIGN COMPONENT lv_delta_col OF STRUCTURE <ls_line> TO <lv_value>.
                      IF sy-subrc <> 0.
                        __concat4 lv_errormessage 'Error asignando campo(Col:'(eac) lv_actual_col ' Fila:'(005) lv_actual_row.
                        APPEND lv_errormessage TO et_errores.
                        __concat2 l_msg 'Error asignando campo'(eax) lv_delta_col.
                        add_error l_msg.
                        EXIT.
                      ELSE.
                        READ TABLE it_fieldcat INTO DATA(fcat) INDEX lv_delta_col.
                        IF sy-subrc <> 0.
                          CLEAR fcat.
                          EXIT.
                        ELSE.
                          IF fcat-fieldname CS '_HORA_NO_F'.
                            lv_delta_col = lv_delta_col + 1.
                            lv_col_ficticias = lv_col_ficticias + 1.
                          ELSE.
                            EXIT.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDDO.

                    TRY.
                        o_worksheet->get_cell( EXPORTING ip_column = lv_actual_col    " Cell Column
                                                         ip_row    = lv_actual_row    " Cell Row
                                               IMPORTING ep_value  = lv_value    " Cell Value
                                                         ep_rc     = lv_rc ). " Return Value of ABAP Statements
                      CATCH zcx_excel INTO o_error_excel.
                        l_msg = o_error_excel->get_longtext( ).
                        add_error l_msg.
                    ENDTRY.

                    IF     lv_rc <> 0
                       AND lv_rc <> 4.                                                   " No found error means, zero/no value in cell
                      IF lv_actual_row <> lv_max_row.
* En la última fila no están rellenas todas las columnas si están en blanco
                        __concat4 lv_errormessage 'Error leyendo campo'(elc) lv_actual_col ' Fila:'(005) lv_actual_row.
                        APPEND lv_errormessage TO et_errores.
                        add_error 'Error leyendo campo'(elc).
                      ENDIF.
                    ENDIF.

                    DESCRIBE FIELD <lv_value> TYPE l_tipo EDIT MASK l_mask.
                    CASE l_tipo.
                      WHEN 'D'.
                        CLEAR l_fecha.
                        IF lv_value CO '0123456789./' AND strlen( lv_value ) = 10.
                          l_fecha = zcl_ap_fechas=>string2fecha( cadena           = lv_value
                                                                 corregir_errores = 'X' ).
                        ENDIF.
                        IF l_fecha IS INITIAL.
                          l_string = lv_value.

                          CLEAR: l_fecha,
                                 l_aux,
                                 l_horat.
                          IF lv_value CS '.'.
                            IF lv_value CO '0123456789.'.
                              SPLIT lv_value AT '.' INTO lv_value l_horat.
                            ENDIF.
                          ENDIF.
                          IF lv_value CO '0123456789 '.
                            l_long = strlen( lv_value ).
                            IF l_long = 8.
                              IF lv_value(8) CO '0123456789'.
                                l_fecha = zcl_ap_fechas=>string2fecha( cadena           = lv_value
                                                                       corregir_errores = 'X' ).
                              ENDIF.
                            ENDIF.
                            IF l_fecha IS INITIAL.
                              TRY.
                                  IF lv_value = 0.
                                    l_fecha = '00000000'.
                                  ELSEIF lv_value <= 2958465.
                                    l_fecha = '19000101'.
                                    l_fecha = l_fecha + lv_value - 2.
*                      elseif lv_value = '2958465'.
*                        l_fecha = '99991231'.
                                  ENDIF.
                                CATCH cx_root. "#EC *
                                  MESSAGE 'Error convirtiendo fecha' TYPE 'S'.
                              ENDTRY.
                            ENDIF.
                          ENDIF.

                          IF l_fecha IS INITIAL.
                            l_fecha = zcl_ap_fechas=>string2fecha( cadena           = lv_value
                                                                   corregir_errores = 'X' ).
                          ENDIF.
                        ENDIF.
                        IF zcl_ap_fechas=>es_valida( l_fecha ) = 'X' OR lv_value = ''.
                          <lv_value> = l_fecha.
                        ELSE.
                          <lv_value> = '88888888'.
                          lv_errormessage = zcl_ap_utils=>concat( p1 = lv_value
                                                                  p2 = 'no es fecha válida. (Col:'(nfv)
                                                                  p3 = lv_actual_col
                                                                  p4 = ' Fila:'(005)
                                                                  p5 = lv_actual_row ).
                          CONCATENATE 'Fecha'(fec) lv_value 'no válida'(nva) INTO l_msg SEPARATED BY space.
                          add_error l_msg.
                        ENDIF.

                        IF NOT l_horat IS INITIAL.
                          ASSIGN it_fieldcat[ lv_delta_col ] TO FIELD-SYMBOL(<fcat>).
                          IF sy-subrc = 0.
                            l_campo_hora = |{ <fcat>-fieldname }_HORA_NO_F|.
                            IF line_exists( it_fieldcat[ fieldname = l_campo_hora ] ). "#EC CI_STDSEQ
                              CONCATENATE '<LS_LINE>-' l_campo_hora INTO l_campo_hora.
                              ASSIGN (l_campo_hora) TO <fs>.
                              IF sy-subrc = 0.
                                TRY.
                                    CONCATENATE '0.' l_horat INTO l_aux.
                                    l_segundos = ( 24 * 60 * 60 ) * l_aux.
                                    CLEAR l_hora.
                                    l_hora = l_hora + l_segundos.
                                    <fs> = l_hora.
                                  CATCH cx_root. "#EC *
                                    l_aux = 'X'.
                                ENDTRY.
                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      WHEN 'T'.
*                  if lv_actual_col = 21. BREAK-POINT. endif.
                        CLEAR: l_hora,
                               l_aux.

                        IF lv_value CO '0123456789: '.
                          l_hora = zcl_ap_fechas=>string2hora( lv_value ).
                          IF l_hora CO '0123456789'.
                            <lv_value> = l_hora.
                          ELSE.
                            l_aux = 'X'.
                          ENDIF.
                        ELSEIF lv_value CO '0123456789.E+- ' AND lv_value CS '.'.
                          TRY.
                              IF lv_value CS 'E'.
                                SPLIT lv_value AT 'E-' INTO l_string l_potencia.
                                IF l_potencia = '00'.
                                  l_segundos = l_string * 86400.
                                ELSE.
                                  DATA(l_pot) = 10 ** l_potencia.
                                  IF l_pot <> 0.
                                    l_segundos = ( l_string * 86400 ) / l_pot.
                                  ENDIF.
                                ENDIF.
                              ELSE.
                                l_segundos = lv_value * 86400.
                              ENDIF.
                              l_hora = l_hora + l_segundos.
                            CATCH cx_root. "#EC *
                              l_aux = 'X'.
                          ENDTRY.
                        ELSE.
                          l_aux = 'X'.
                        ENDIF.

                        IF l_aux = 'X'.
                          <lv_value> = '888888'.
                          lv_errormessage = zcl_ap_utils=>concat( p1 = lv_value
                                                                  p2 = 'no es hora válida. (Col:'(nfv)
                                                                  p3 = lv_actual_col
                                                                  p4 = ' Fila:'(005)
                                                                  p5 = lv_actual_row ).
                          CONCATENATE 'Hora'(hor) lv_value 'no válida'(nva) INTO l_msg SEPARATED BY space.
                          add_error l_msg.
                        ELSE.
                          <lv_value> = l_hora.
                        ENDIF.

                      WHEN 'P'
                        OR 'b'. " INT1
                        TRY.
                            IF conv_ctd = 'X'.
                              zcl_ap_string=>string2ctd( EXPORTING ctd_texto = lv_value
                                                         IMPORTING cantidad  = <lv_value>
                                                                   mensaje   = l_msg ).
                              IF NOT l_msg IS INITIAL.
                                CONCATENATE lv_value 'no es una cantidad válida'(ncv) INTO l_msg SEPARATED BY space.
                                add_error l_msg.
                              ENDIF.
                            ELSE.
                              <lv_value> = lv_value.
                            ENDIF.
                          CATCH cx_root INTO cx_root. "#EC *
                            l_msg = cx_root->get_longtext( ).
                            add_error l_msg.
                        ENDTRY.
                      WHEN OTHERS.
                        CASE l_mask.
                          WHEN '==CUNIT'.
                            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
                              EXPORTING
                                input          = lv_value
                                language       = sy-langu
                              IMPORTING
                                output         = <lv_value>
                              EXCEPTIONS
                                unit_not_found = 1
                                OTHERS         = 2.
                            IF sy-subrc <> 0.
                              __concat3 lv_errormessage 'Unidad' lv_value 'incorrecta.'.
                              COLLECT lv_errormessage INTO et_errores.
                              add_error lv_errormessage.
                            ENDIF.
                          WHEN '==ALPHA'.
                            DESCRIBE FIELD <lv_value> LENGTH l_long IN CHARACTER MODE.
                            l_long2 = strlen( lv_value ).
                            IF l_long2 > l_long.
                              <lv_value> = lv_value.
                            ELSE.
                              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                EXPORTING
                                  input  = lv_value
                                IMPORTING
                                  output = <lv_value>.
                            ENDIF.
                            IF validar_maestros = 'X'.
                              IF NOT <lv_value> IS INITIAL.
                                CASE fcat-rollname.
                                  WHEN 'KSTAR'.
                                    SELECT SINGLE kstar FROM cska
                                      INTO @DATA(l_kstar)
                                      WHERE ktopl = @zcl_c=>plan_cuentas AND kstar = @<lv_value>.
                                    IF sy-subrc <> 0.
                                      lv_errormessage = |Clase de coste { lv_value } no existe|.
                                      COLLECT lv_errormessage INTO et_errores.
                                      add_error lv_errormessage.
                                    ENDIF.
                                  WHEN 'KOSTL'.
                                    SELECT kostl FROM csks
                                      INTO @DATA(l_kostl)
                                      UP TO 1 ROWS
                                      WHERE kokrs = @zcl_c=>sociedad_co
                                        AND kostl = @<lv_value>
                                      ORDER BY PRIMARY KEY.
                                    ENDSELECT.
                                    IF sy-subrc <> 0.
                                      lv_errormessage = |Centro de coste { lv_value } no existe|.
                                      COLLECT lv_errormessage INTO et_errores.
                                      add_error lv_errormessage.
                                    ENDIF.
                                  WHEN 'PERNR'.
                                    SELECT pernr FROM pa0000 INTO @DATA(l_pernr)
                                     UP TO 1 ROWS
                                     WHERE pernr = @<lv_value>
                                     ORDER BY PRIMARY KEY.
                                    ENDSELECT.
                                    IF sy-subrc <> 0.
                                      lv_errormessage = |Empleado { lv_value } no existe|.
                                      COLLECT lv_errormessage INTO et_errores.
                                      add_error lv_errormessage.
                                    ENDIF.
                                  WHEN 'EBELN' OR 'BSTNR'.
                                    SELECT SINGLE ebeln FROM ekko INTO @DATA(l_ebeln) WHERE ebeln = @<lv_value>.
                                    IF sy-subrc <> 0.
                                      lv_errormessage = |Pedido { lv_value } no existe|.
                                      COLLECT lv_errormessage INTO et_errores.
                                      add_error lv_errormessage.
                                    ENDIF.
                                  WHEN 'AUFNR'.
                                    SELECT SINGLE aufnr FROM aufk INTO @DATA(l_aufnr) WHERE aufnr = @<lv_value>.
                                    IF sy-subrc <> 0.
                                      lv_errormessage = |Orden { lv_value } no existe|.
                                      COLLECT lv_errormessage INTO et_errores.
                                      add_error lv_errormessage.
                                    ENDIF.
                                  when 'WERKS'.
                                    SELECT SINGLE WERKS FROM T001W INTO @DATA(L_WERKS) WHERE WERKS = @<lv_value>.
                                    IF sy-subrc <> 0.
                                      lv_errormessage = |Centro { lv_value } no existe|.
                                      COLLECT lv_errormessage INTO et_errores.
                                      add_error lv_errormessage.
                                    ENDIF.
                                  when 'LGORT'.
                                    SELECT single lgort FROM T001l INTO @DATA(l_lgort) WHERE lgort = @<lv_value>.
                                    IF sy-subrc <> 0.
                                      lv_errormessage = |Almacén { lv_value } no existe|.
                                      COLLECT lv_errormessage INTO et_errores.
                                      add_error lv_errormessage.
                                    ENDIF.
                                ENDCASE.
                              ENDIF.
                            ENDIF.
                          WHEN '==MATN1'.
                            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                              EXPORTING
                                input        = lv_value
                              IMPORTING
                                output       = <lv_value>
                              EXCEPTIONS
                                length_error = 1
                                OTHERS       = 2.
                            IF sy-subrc <> 0.
                              __concat2 lv_errormessage 'Error convirtiendo material' lv_value.
                              COLLECT lv_errormessage INTO et_errores.
                              add_error lv_errormessage.
                            ELSE.
                              IF validar_maestros = 'X'.
                                IF NOT <lv_value> IS INITIAL.
                                  SELECT SINGLE matnr FROM mara
                                    INTO @DATA(l_matnr)
                                    WHERE matnr = @<lv_value>.
                                  IF sy-subrc <> 0.
                                    lv_errormessage = |Material { lv_value } no existe|.
                                    COLLECT lv_errormessage INTO et_errores.
                                    add_error lv_errormessage.
                                  ENDIF.
                                ENDIF.
                              ENDIF.
                            ENDIF.
                          WHEN OTHERS.
                            <lv_value> = lv_value.
                        ENDCASE.
                    ENDCASE.
                    lv_actual_col = lv_actual_col + 1.

                  CATCH cx_sy_assign_cast_illegal_cast.
                    __concat4 lv_errormessage 'Error asignando campo(Col:'(eac) lv_actual_col ' Fila:'(005) lv_actual_row.
                    APPEND lv_errormessage TO et_errores.
                    add_error 'Error asignando campo'.
                    lv_actual_col = lv_actual_col + 1.
                  CATCH cx_sy_assign_cast_unknown_type.
                    __concat4 lv_errormessage 'Error asignando campo(Col:'(eac) lv_actual_col ' Fila:'(005) lv_actual_row.
                    APPEND lv_errormessage TO et_errores.
                    add_error 'Error asignando campo'.
                    lv_actual_col = lv_actual_col + 1.
                  CATCH cx_sy_assign_out_of_range.
                    __concat4 lv_errormessage 'Tabla interna tiene menos valores que columnas en Excel'(tme) lv_actual_col ' Fila:'(005) lv_actual_row.
                    APPEND lv_errormessage TO et_errores.
                    lv_actual_col = lv_actual_col + 1.
                    add_error 'Tabla interna tiene menos valores que columnas en Excel'(tme).
                  CATCH cx_sy_conversion_error.
                    __concat4 lv_errormessage 'Error convirtiendo valor'(ecv) lv_actual_col ' Fila:'(005) lv_actual_row.
                    APPEND lv_errormessage TO et_errores.
                    add_error 'Error convirtiendo valor'(ecv).
                    lv_actual_col = lv_actual_col + 1.
                ENDTRY.

              ENDWHILE.
              lv_actual_row = lv_actual_row + 1.
            ENDWHILE.
          ENDIF.
        ENDIF.

      CATCH cx_sy_assign_cast_illegal_cast.
        __concat4 lv_errormessage 'Error asignando campo(Col:'(eac) lv_actual_col ' Fila:'(005) lv_actual_row.
        APPEND lv_errormessage TO et_errores.
        add_error 'Error asignando campo'.
      CATCH cx_sy_assign_cast_unknown_type.
        __concat4 lv_errormessage 'Error asignando campo(Col:'(eac) lv_actual_col ' Fila:'(005) lv_actual_row.
        APPEND lv_errormessage TO et_errores.
        add_error 'Error asignando campo'.
      CATCH cx_sy_assign_out_of_range.
        __concat4 lv_errormessage 'Tabla interna tiene menos valores que columnas en Excel'(tme) lv_actual_col ' Fila:'(005) lv_actual_row.
        APPEND lv_errormessage TO et_errores.
        add_error 'Tabla interna tiene menos valores que columnas en Excel'(tme).
      CATCH cx_sy_conversion_error.
        __concat4 lv_errormessage 'Error convirtiendo valor'(ecv) lv_actual_col ' Fila:'(005) lv_actual_row.
        APPEND lv_errormessage TO et_errores.
        add_error 'Error convirtiendo valor'(ecv).
    ENDTRY.
  ENDMETHOD.
  METHOD get_xstring.
    DATA cl_writer TYPE REF TO zif_excel_writer.

    IF huge IS INITIAL.
      cl_writer = NEW zcl_excel_writer_2007( ).
    ELSE.
      cl_writer = NEW zcl_excel_writer_huge_file( ).
    ENDIF.
    xstring = cl_writer->write_file( o_excel ).
    longitud = xstrlen( xstring ).
  ENDMETHOD.
  METHOD graba_fichero.
    DATA: l_xstring       TYPE xstring,
          l_fichero_final TYPE string.

    get_xstring( EXPORTING huge    = huge
                 IMPORTING xstring = l_xstring ).

    zcl_ap_ficheros=>grabar_xstring( EXPORTING xstring       = l_xstring
                                               fichero       = fichero
                                               servidor      = servidor
                                               local         = local
                                               dialogo       = dialogo
                                               mostrar_error = ''
                                     IMPORTING mensaje       = message
                                               fichero_final = l_fichero_final ).

    IF abrir = 'X' AND NOT l_fichero_final IS INITIAL AND message IS INITIAL.
      zcl_ap_ficheros=>visualizar( fichero = l_fichero_final ).
    ENDIF.
  ENDMETHOD.
  METHOD lee_fichero.
    DATA: o_reader  TYPE REF TO zif_excel_reader,
          l_xstring TYPE xstring,
          l_fichero TYPE string.

    IF huge IS INITIAL.
      o_reader = NEW zcl_excel_reader_2007( ).
    ELSE.
      o_reader = NEW zcl_excel_reader_huge_file( ).
    ENDIF.

    IF url = 'X'.
      zcl_ap_ws=>get_file( EXPORTING url     = fichero
                                     rfcdest = rfcdest
                           IMPORTING xstring = l_xstring
                                     message = message ).
      IF message IS INITIAL.
        IF l_xstring IS INITIAL.
          message = 'No se ha podido recuperar el fichero'(nrf).
        ELSE.
          TRY.
              o_excel = o_reader->load( i_excel2007 = l_xstring ).
            CATCH zcx_excel INTO zcx_excel.
              message = zcx_excel->get_longtext( ).
          ENDTRY.
        ENDIF.
      ENDIF.
    ELSEIF NOT fichero IS INITIAL OR popup_select_file = 'X'.
      IF NOT fichero IS INITIAL.
        l_fichero = fichero.
      ELSEIF popup_select_file = 'X'.
        l_fichero = zcl_ap_ficheros=>popup_select_fichero( file_filter = zcl_ap_ficheros=>c_filtro_xlsx ).
      ENDIF.
      IF zcl_ap_ficheros=>existe( fichero  = l_fichero
                                  servidor = servidor ) = ''.
        __concat2 message 'No existe el fichero'(nef) fichero.
      ELSE.
        TRY.
            o_excel = o_reader->load_file( i_filename        = l_fichero
                                            i_from_applserver = servidor ).
          CATCH zcx_excel INTO zcx_excel.
            message = zcx_excel->get_longtext( ).
        ENDTRY.
      ENDIF.
    ELSEIF NOT xstring IS INITIAL.
      TRY.
          o_excel = o_reader->load( i_excel2007 = xstring ).
        CATCH zcx_excel INTO zcx_excel.
          message = zcx_excel->get_longtext( ).
      ENDTRY.
    ENDIF.
    IF message IS INITIAL.
      IF o_excel IS INITIAL.
        message = 'Error leyendo fichero'(elf).
      ELSE.
        o_worksheet = o_excel->get_active_worksheet( ).
        IF o_worksheet IS INITIAL.
          message = 'Error leyendo fichero'(elf).
        ELSE.
          lee_hoja( EXPORTING hoja                 = hoja
                              lineas_cabecera      = lineas_cabecera
                              get_datos            = get_datos
                              mostrar_error        = mostrar_error
                              max_col              = max_col
                              validar_maestros     = validar_maestros
                              existe_valor         = existe_valor
                              col_existe_valor     = col_existe_valor
                              opciones             = opciones
                    IMPORTING message              = message
                              datos                = datos
                              errores              = errores
                              si_existe_valor      = si_existe_valor
                              fila_existe_valor    = fila_existe_valor
                              columna_existe_valor = columna_existe_valor ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF mostrar_error = 'X' AND NOT message IS INITIAL.
      MESSAGE message TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD lee_hoja.
    DATA: l_sheet_name  TYPE zexcel_sheet_title,
          o_error_excel TYPE REF TO zcx_excel,
          l_msg         TYPE c LENGTH 1.

    nombre_hoja = o_worksheet->get_title( ).

    IF hoja <> '' AND hoja <> nombre_hoja.
      l_sheet_name = hoja.
      o_worksheet = o_excel->get_worksheet_by_name( l_sheet_name ).
      IF o_worksheet IS INITIAL.
        __concat2 message 'No existe la hoja'(neh) l_sheet_name.
      ENDIF.
    ENDIF.

    IF NOT o_worksheet IS INITIAL.
      TRY.
          highest_column = o_worksheet->get_highest_column( ).
          highest_row    = o_worksheet->get_highest_row( ).
        CATCH zcx_excel INTO o_error_excel.
          l_msg = o_error_excel->get_longtext( ).
      ENDTRY.

      IF existe_valor <> ''.
        si_existe_valor = existe_valor( EXPORTING valor     = existe_valor
                                                  col_valor = col_existe_valor
                                        IMPORTING fila      = fila_existe_valor
                                                  columna   = columna_existe_valor ).
      ELSEIF get_datos = 'X'.
        get_tabla( EXPORTING lineas_cabecera  = lineas_cabecera
                             max_col          = max_col
                             validar_maestros = validar_maestros
                             opciones         = opciones
                   IMPORTING et_table         = datos
                             et_errores       = errores ).
      ENDIF.
    ENDIF.

    IF mostrar_error = 'X' AND NOT message IS INITIAL.
      MESSAGE message TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD lee_plantilla.
    DATA l_xstring TYPE xstring.

    SELECT SINGLE xstring FROM zdocumentos
      INTO l_xstring
      WHERE tcode  = tcode
        AND nombre = plantilla.
    IF sy-subrc <> 0.
      message = 'No existe plantilla'(nep).
    ELSE.
      lee_fichero( EXPORTING xstring = l_xstring
                   IMPORTING message = message ).
    ENDIF.
  ENDMETHOD.
  METHOD mail.
    DATA: o_mail    TYPE REF TO zcl_ap_envio_mail,
          l_texto   TYPE solisti1,
          o_xls     TYPE REF TO zcl_ap_abap2xls,
          l_fichero TYPE string.

    o_mail = NEW #( usar_clases = 'X' ).

    IF NOT texto IS INITIAL.
      o_mail->set_text( texto ).
    ENDIF.

    LOOP AT i_textos INTO l_texto.
      o_mail->set_text( l_texto ).
    ENDLOOP.

    DEFINE set_nombre.
      IF nombre_tabla&1 IS INITIAL.
        l_fichero = '&2&1.xlsx'.
      ELSE.
        IF nombre_tabla&1 CS '.'.
          l_fichero = nombre_tabla&1.
        ELSE.
          CONCATENATE nombre_tabla&1 '.xlsx' INTO l_fichero.
        ENDIF.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE set_nombre_alv.
      IF nombre_&2&1 IS INITIAL.
        l_fichero = '&2&1.xlsx'.
      ELSE.
        IF nombre_&2&1 CS '.'.
          l_fichero = nombre_&2&1.
        ELSE.
          CONCATENATE nombre_&2&1 '.xlsx' INTO l_fichero.
        ENDIF.
      ENDIF.
    END-OF-DEFINITION.

    IF NOT tabla1 IS INITIAL.
      o_xls = NEW #( ).
      o_xls->set_tabla( tabla = tabla1 ).
      set_nombre 1 tabla.
      o_xls->add_adj_mail( o_mail  = o_mail
                           fichero = l_fichero ).
      CLEAR o_xls.
    ENDIF.

    IF NOT tabla2 IS INITIAL.
      o_xls = NEW #( ).
      o_xls->set_tabla( tabla = tabla2 ).
      set_nombre 2 tabla.
      o_xls->add_adj_mail( o_mail  = o_mail
                           fichero = l_fichero ).
      CLEAR o_xls.
    ENDIF.

    IF NOT tabla3 IS INITIAL.
      o_xls = NEW #( ).
      o_xls->set_tabla( tabla = tabla3 ).
      set_nombre 3 tabla.
      o_xls->add_adj_mail( o_mail  = o_mail
                           fichero = l_fichero ).
      CLEAR o_xls.
    ENDIF.

    IF NOT o_alv1 IS INITIAL.
      o_xls = NEW #( ).
      o_xls->set_alv( alv              = o_alv1->o_alv
                      tabla            = tabla_alv1
                      refresh_metadata = 'X' ).
      set_nombre_alv 1 alv.
      o_xls->add_adj_mail( o_mail  = o_mail
                           fichero = l_fichero ).
      CLEAR o_xls.
    ENDIF.

    IF NOT o_alv2 IS INITIAL.
      o_xls = NEW #( ).
      o_xls->set_alv( alv              = o_alv2->o_alv
                      tabla            = tabla_alv2
                      refresh_metadata = 'X' ).
      set_nombre_alv 2 alv.
      o_xls->add_adj_mail( o_mail  = o_mail
                           fichero = l_fichero ).
      CLEAR o_xls.
    ENDIF.

    IF NOT o_grid1 IS INITIAL.
      o_xls = NEW #( ).
      o_xls->set_alv( alv              = o_grid1->o_grid
                      tabla            = tabla_grid1
                      refresh_metadata = 'X' ).
      set_nombre_alv 1 grid.
      o_xls->add_adj_mail( o_mail  = o_mail
                           fichero = l_fichero ).
      CLEAR o_xls.
    ENDIF.

    IF NOT o_grid2 IS INITIAL.
      o_xls = NEW #( ).
      o_xls->set_alv( alv              = o_grid2->o_grid
                      tabla            = tabla_grid2
                      refresh_metadata = 'X' ).
      set_nombre_alv 2 grid.
      o_xls->add_adj_mail( o_mail  = o_mail
                           fichero = l_fichero ).
      CLEAR o_xls.
    ENDIF.

    IF o_mail->envio_mail( subject        = subject
                           direccion      = direccion
                           commit         = commit
                           parar_en_error = '' ) = 'X'.
      message = o_mail->message.
    ENDIF.
  ENDMETHOD.
  METHOD mostrar_en_pantalla.
    DATA: xdata     TYPE xstring,   " Will be used for sending as email
          bytecount TYPE i,         " Will be used for downloading or open directly
          t_rawdata TYPE solix_tab. " Will be used for downloading or open directly
    DATA: cl_control  TYPE REF TO i_oi_container_control, " OIContainerCtrl "#EC DEFAULT_KEY
          error       TYPE REF TO i_oi_error,
          cl_document TYPE REF TO i_oi_document_proxy.    " Office Dokument

    get_xstring( IMPORTING xstring  = xdata
                           longitud = bytecount ).
    t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring = xdata ).

    c_oi_container_control_creator=>get_container_control( IMPORTING control = cl_control
                                                                     error   = error ).


    cl_control->init_control( EXPORTING  inplace_enabled     = 'X'
                                         no_flush            = 'X'
                                         r3_application_name = 'Contenedor Excel'(cxl)
                                         parent              = cl_gui_container=>screen0
                              IMPORTING  error               = error
                              EXCEPTIONS OTHERS              = 2 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error abriendo Excel' TYPE 'E'.
    ENDIF.


    cl_control->get_document_proxy( EXPORTING document_type  = 'Excel.Sheet'                " EXCEL
                                              no_flush       = ' '
                                    IMPORTING document_proxy = cl_document
                                              error          = error ).

* Errorhandling should be inserted here

    cl_document->open_document_from_table( document_size  = bytecount
                                           document_table = t_rawdata
                                           open_inplace   = 'X' ).
    WRITE '.'.  " To create an output.  That way screen0 will exist
  ENDMETHOD.
  METHOD mostrar_errores.
    IF i_error IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'Z_POPUP_ALV_AP'
      EXPORTING
        titulo  = 'Errores de conversión'(eco)
        texto   = 'Se han producido errores en la carga del fichero Excel'(spe)
*       TEXTO2  =
*       CHECK   = ''
*       ANCHO   = 120
*       ALTO    = 0
*       CAMPOS_NOOUT = ''
        botones = 'OK'
*       CAMPOS_TEXTO = ''
*       COMANDO_ENTER = ''
*       CAMPOS_INPUT = ''
*       GRID    = ''
*       OPCIONES = ''
*     IMPORTING
*       COMANDO =
*       UCOMM   =
*       OK      =
*       FILA    =
      TABLES
        t_datos = i_error.
  ENDMETHOD.
  METHOD reemplazar_valor.
    DATA: l_var   TYPE string,
          l_marca TYPE string,
          l_valor TYPE text255.

    FIELD-SYMBOLS <cell_data> TYPE zexcel_s_cell_data.

    IF car_marca IS INITIAL.
      l_var = var.
    ELSE.
*      IF car_marca = '&'.
*        l_marca = '&amp;'.
*      ELSE.
      l_marca = car_marca.
*      ENDIF.

      l_var = var.
      CONCATENATE l_marca l_var l_marca INTO l_var.
    ENDIF.

    WRITE valor TO l_valor LEFT-JUSTIFIED.

    ASSIGN o_worksheet->sheet_content[ cell_value = l_var ] TO <cell_data>. "#EC CI_SORTSEQ
    IF sy-subrc = 0.
      <cell_data>-cell_value = l_valor.
    ELSE.
      IF parcial = 'X'.
        LOOP AT o_worksheet->sheet_content ASSIGNING <cell_data> WHERE cell_value CS l_var. "#EC CI_SORTSEQ
          REPLACE l_var IN <cell_data>-cell_value WITH l_valor.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          error = 'X'.
        ENDIF.
      ELSE.
        error = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD set_alv.
    DATA lo_converter TYPE REF TO zcl_excel_converter.
    DATA l_tipo_alv   TYPE string.
    DATA o_excell     TYPE REF TO zcl_excel.

    FIELD-SYMBOLS <cell_data> TYPE zexcel_s_cell_data.

    lo_converter = NEW #( ).

    IF refresh_metadata = 'X'.
      TRY.
          cl_salv_bs_runtime_info=>clear_all( ).
          cl_salv_bs_runtime_info=>set( display  = abap_false
                                        metadata = abap_true
                                        data     = abap_true ).
          l_tipo_alv = cl_abap_classdescr=>get_class_name( alv ).
          IF l_tipo_alv CS 'CL_SALV_TABLE'.
            CALL METHOD alv->('DISPLAY').
          ELSE.
            NEW zcl_excel_converter( )->set_option( VALUE #(
                filter           = abap_true
                subtot           = abap_true
                hidenc           = abap_true
                hidehd           = abap_false ) ).
            o_worksheet->bind_alv(
                io_alv   = alv
                it_table = tabla  ).
          ENDIF.
        CATCH cx_root INTO DATA(o_root).  "#EC *
          MESSAGE |Error refrescando metadatos del ALV {  o_root->get_text(  ) }|  TYPE 'I'.
      ENDTRY.
    ENDIF.

    TRY.
        o_excell ?= o_excel.
        lo_converter->convert( EXPORTING io_alv       = alv
                                         it_table     = tabla
*                                         i_row_int    = i_top
*                                         i_column_int = i_left
*                                         i_table      = i_table
*                                         i_style_table = table_style
                                         io_worksheet = o_worksheet
                               CHANGING  co_excel     = o_excell ).

        o_worksheet->calculate_column_widths( ).
      CATCH zcx_excel INTO DATA(o_cx_excel). "#EC * " TODO: variable is assigned but never used (ABAP cleaner)
    ENDTRY.

    IF convert_txt_2_number = 'X'.
      LOOP AT o_worksheet->sheet_content ASSIGNING <cell_data> WHERE cell_value CS ',' AND data_type = 's'. "#EC CI_SORTSEQ
        IF <cell_data>-cell_value CO '0123456789, '.
          REPLACE ',' WITH '.' INTO <cell_data>-cell_value.
          CONDENSE <cell_data>-cell_value NO-GAPS.
          CLEAR <cell_data>-data_type.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF refresh_metadata = 'X'.
      cl_salv_bs_runtime_info=>clear_all( ).
    ENDIF.
  ENDMETHOD.
  METHOD set_bgcolor_range.
    "-Variables---------------------------------------------------------

    DATA lv_col_start TYPE i.
    DATA lv_row_start TYPE zexcel_cell_row.
    DATA lv_col_end   TYPE i.
    DATA lv_row_end   TYPE zexcel_cell_row.
    DATA lv_fill      TYPE zexcel_s_cstyle_fill.
    DATA lv_row       TYPE i.
    DATA lv_col       TYPE i.
    DATA lv_col_alpha TYPE string.

    "-Main--------------------------------------------------------------
    TRY.

        lv_col_start =
          zcl_excel_common=>convert_column2int( col_start ).

        lv_row_start = row_start.

        IF col_end IS INITIAL.
          lv_col_end = o_worksheet->get_highest_column( ).
        ELSE.
          lv_col_end =
            zcl_excel_common=>convert_column2int( col_end ).
        ENDIF.

        IF row_end IS INITIAL.
          lv_row_end = o_worksheet->get_highest_row( ).
        ELSE.
          lv_row_end = row_end.
        ENDIF.

        lv_fill-filltype = zcl_excel_style_fill=>c_fill_solid.
        lv_fill-fgcolor-rgb = bg_color.

        lv_row = lv_row_start.
        WHILE lv_row <= lv_row_end.
          lv_col = lv_col_start.
          WHILE lv_col <= lv_col_end.
            lv_col_alpha =
              zcl_excel_common=>convert_column2alpha( ip_column = lv_col ).
            o_worksheet->change_cell_style( ip_column = lv_col_alpha
                                            ip_row    = lv_row
                                            ip_fill   = lv_fill ).
            lv_col = lv_col + 1.
          ENDWHILE.
          lv_row = lv_row + 1.
        ENDWHILE.

      CATCH cx_root. "#EC *
        MESSAGE 'Error cambiando color' TYPE 'S'.

    ENDTRY.
  ENDMETHOD.
  METHOD set_border_outline_range.
    "-Variables---------------------------------------------------------

    DATA lv_col_start TYPE i.
    DATA lv_row_start TYPE zexcel_cell_row.
    DATA lv_col_end   TYPE i.
    DATA lv_row_end   TYPE zexcel_cell_row.
    DATA lv_col_txt   TYPE c LENGTH 20.
    DATA lv_row       TYPE i.
    DATA lv_col       TYPE i.

    "-Main--------------------------------------------------------------
    TRY.

        lv_col_start =
          zcl_excel_common=>convert_column2int( col_start ).

        lv_row_start = row_start.

        IF col_end IS INITIAL.
          lv_col_end = o_worksheet->get_highest_column( ).
        ELSE.
          lv_col_end =
            zcl_excel_common=>convert_column2int( col_end ).
        ENDIF.

        IF row_end IS INITIAL.
          lv_row_end = o_worksheet->get_highest_row( ).
        ELSE.
          lv_row_end = row_end.
        ENDIF.

        "-Left----------------------------------------------------------
        lv_col_txt = zcl_excel_common=>convert_column2alpha( ip_column = lv_col_start ).
        lv_row = lv_row_start.
        WHILE lv_row <= lv_row_end.
          o_worksheet->change_cell_style( ip_column                 = lv_col_txt
                                          ip_row                    = lv_row
                                          ip_borders_left_style     = border_style
                                          ip_borders_left_color_rgb = border_color ).
          lv_row = lv_row + 1.
        ENDWHILE.

        "-Right---------------------------------------------------------
        lv_col_txt = zcl_excel_common=>convert_column2alpha( ip_column = lv_col_end ).
        lv_row = lv_row_start.
        WHILE lv_row <= lv_row_end.

          o_worksheet->change_cell_style( ip_column                  = lv_col_txt
                                          ip_row                     = lv_row
                                          ip_borders_right_style     = border_style
                                          ip_borders_right_color_rgb = border_color ).
          lv_row = lv_row + 1.
        ENDWHILE.

        "-Top-----------------------------------------------------------
        lv_col = lv_col_start.
        WHILE lv_col <= lv_col_end.
          o_worksheet->change_cell_style( ip_column                = lv_col
                                          ip_row                   = lv_row_start
                                          ip_borders_top_style     = border_style
                                          ip_borders_top_color_rgb = border_color ).
          lv_col = lv_col + 1.
        ENDWHILE.

        "-Bottom--------------------------------------------------------
        lv_col = lv_col_start.
        WHILE lv_col <= lv_col_end.
          o_worksheet->change_cell_style( ip_column                 = lv_col
                                          ip_row                    = lv_row_end
                                          ip_borders_down_style     = border_style
                                          ip_borders_down_color_rgb = border_color ).
          lv_col = lv_col + 1.
        ENDWHILE.

      CATCH cx_root. "#EC *
        MESSAGE 'Error cambiando borde' TYPE 'S'.

    ENDTRY.
  ENDMETHOD.
  METHOD set_border_range.
    "-Variables---------------------------------------------------------

    DATA lv_col_start TYPE i.
    DATA lv_row_start TYPE zexcel_cell_row.
    DATA lv_col_end   TYPE i.
    DATA lv_row_end   TYPE zexcel_cell_row.
    DATA lv_borders   TYPE zexcel_s_cstyle_borders.
    DATA lv_row       TYPE i.
    DATA lv_col       TYPE i.
    DATA lv_col_alpha TYPE string.

    "-Main--------------------------------------------------------------
    TRY.

        lv_col_start =
          zcl_excel_common=>convert_column2int( col_start ).

        lv_row_start = row_start.

        IF col_end IS INITIAL.
          lv_col_end = o_worksheet->get_highest_column( ).
        ELSE.
          lv_col_end =
            zcl_excel_common=>convert_column2int( col_end ).
        ENDIF.

        IF row_end IS INITIAL.
          lv_row_end = o_worksheet->get_highest_row( ).
        ELSE.
          lv_row_end = row_end.
        ENDIF.

        lv_borders-left-border_style = border_style.
        lv_borders-left-border_color-rgb = border_color.
        lv_borders-right-border_style = border_style.
        lv_borders-right-border_color-rgb = border_color.
        lv_borders-top-border_style = border_style.
        lv_borders-top-border_color-rgb = border_color.
        lv_borders-down-border_style = border_style.
        lv_borders-down-border_color-rgb = border_color.

        lv_row = lv_row_start.
        WHILE lv_row <= lv_row_end.
          lv_col = lv_col_start.
          WHILE lv_col <= lv_col_end.
            lv_col_alpha =
              zcl_excel_common=>convert_column2alpha( ip_column = lv_col ).
            o_worksheet->change_cell_style( ip_column  = lv_col_alpha
                                            ip_row     = lv_row
                                            ip_borders = lv_borders ).
            lv_col = lv_col + 1.
          ENDWHILE.
          lv_row = lv_row + 1.
        ENDWHILE.

      CATCH cx_root. "#EC *
        MESSAGE 'Error cambiando borde' TYPE 'S'.

    ENDTRY.
  ENDMETHOD.
  METHOD set_cell.
    " TODO: parameter HYPERLINK is never used (ABAP cleaner)
    " TODO: parameter DATA_TYPE is never used (ABAP cleaner)
    " TODO: parameter ABAP_TYPE is never used (ABAP cleaner)

    DATA l_error TYPE t_error.

    TRY.
        IF value IS SUPPLIED.
          o_worksheet->set_cell( ip_column  = col
                                 ip_row     = row
                                 ip_value   = value
                                 ip_style   = style
                                 ip_formula = formula ).
        ELSE.
          o_worksheet->set_cell( ip_column  = col
                                 ip_row     = row
                                 ip_style   = style
                                 ip_formula = formula ).
        ENDIF.
      CATCH zcx_excel INTO zcx_excel.
        CLEAR l_error.
        l_error-row     = row.
        l_error-col     = col.
        l_error-message = zcx_excel->get_longtext( ).
        APPEND l_error TO i_error.
    ENDTRY.
  ENDMETHOD.
  METHOD set_column.
    DATA: l_col_txt TYPE char20,
          o_column  TYPE REF TO zcl_excel_column.
    DATA l_error TYPE t_error.

    IF NOT col IS INITIAL.
      l_col_txt = col.
    ELSE.
      TRY.
          l_col_txt = zcl_excel_common=>convert_column2alpha( ip_column = col_int ).
        CATCH zcx_excel INTO zcx_excel.
          CLEAR l_error.
          l_error-col     = col_int.
          l_error-message = zcx_excel->get_longtext( ).
          APPEND l_error TO i_error.
      ENDTRY.
    ENDIF.

    o_column = o_worksheet->get_column( l_col_txt ).
    IF o_column IS INITIAL.
      RETURN.
    ENDIF.

    IF NOT width IS INITIAL.
      TRY.
          o_column->set_width( width ).
        CATCH zcx_excel INTO zcx_excel.
          CLEAR l_error.
          l_error-col     = col_int.
          l_error-message = zcx_excel->get_longtext( ).
          APPEND l_error TO i_error.
      ENDTRY.
    ENDIF.

    IF NOT visible IS INITIAL.
      IF visible = 'N'.
        o_column->set_visible( '' ).
      ELSE.
        o_column->set_visible( visible ).
      ENDIF.
    ENDIF.

    IF NOT autosize IS INITIAL.
      IF autosize = 'N'.
        o_column->set_auto_size( '' ).
      ELSE.
        o_column->set_auto_size( autosize ).
      ENDIF.

    ENDIF.
  ENDMETHOD.
  METHOD set_fontcolor_range.
    "-Variables---------------------------------------------------------

    DATA lv_col_start TYPE i.
    DATA lv_row_start TYPE zexcel_cell_row.
    DATA lv_col_end   TYPE i.
    DATA lv_row_end   TYPE zexcel_cell_row.
    DATA lv_fonts     TYPE zexcel_s_cstyle_font.
    DATA lv_row       TYPE i.
    DATA lv_col       TYPE i.
    DATA lv_col_alpha TYPE string.

    "-Main--------------------------------------------------------------
    TRY.

        lv_col_start =
          zcl_excel_common=>convert_column2int( col_start ).

        lv_row_start = row_start.

        IF col_end IS INITIAL.
          lv_col_end = o_worksheet->get_highest_column( ).
        ELSE.
          lv_col_end =
            zcl_excel_common=>convert_column2int( col_end ).
        ENDIF.

        IF row_end IS INITIAL.
          lv_row_end = o_worksheet->get_highest_row( ).
        ELSE.
          lv_row_end = row_end.
        ENDIF.

        lv_fonts-color-rgb = text_color.

        lv_row = lv_row_start.
        WHILE lv_row <= lv_row_end.
          lv_col = lv_col_start.
          WHILE lv_col <= lv_col_end.
            lv_col_alpha =
              zcl_excel_common=>convert_column2alpha( ip_column = lv_col ).
            o_worksheet->change_cell_style( ip_column = lv_col_alpha
                                            ip_row    = lv_row
                                            ip_font   = lv_fonts ).
            lv_col = lv_col + 1.
          ENDWHILE.
          lv_row = lv_row + 1.
        ENDWHILE.

      CATCH cx_root. "#EC *
        MESSAGE 'Error cambiando fuente' TYPE 'S'.

    ENDTRY.
  ENDMETHOD.
  METHOD set_fontsize_range.
    "-Variables---------------------------------------------------------

    DATA lv_col_start TYPE i.
    DATA lv_row_start TYPE zexcel_cell_row.
    DATA lv_col_end   TYPE i.
    DATA lv_row_end   TYPE zexcel_cell_row.
    DATA lv_fonts     TYPE zexcel_s_cstyle_font.
    DATA lv_row       TYPE i.
    DATA lv_col       TYPE i.
    DATA lv_col_alpha TYPE string.

    "-Main--------------------------------------------------------------
    TRY.

        lv_col_start =
          zcl_excel_common=>convert_column2int( col_start ).

        lv_row_start = row_start.

        IF col_end IS INITIAL.
          lv_col_end = o_worksheet->get_highest_column( ).
        ELSE.
          lv_col_end =
            zcl_excel_common=>convert_column2int( col_end ).
        ENDIF.

        IF row_end IS INITIAL.
          lv_row_end = o_worksheet->get_highest_row( ).
        ELSE.
          lv_row_end = row_end.
        ENDIF.

        lv_fonts-size = text_size.

        lv_row = lv_row_start.
        WHILE lv_row <= lv_row_end.
          lv_col = lv_col_start.
          WHILE lv_col <= lv_col_end.
            lv_col_alpha =
              zcl_excel_common=>convert_column2alpha( ip_column = lv_col ).
            o_worksheet->change_cell_style( ip_column = lv_col_alpha
                                            ip_row    = lv_row
                                            ip_font   = lv_fonts ).
            lv_col = lv_col + 1.
          ENDWHILE.
          lv_row = lv_row + 1.
        ENDWHILE.

      CATCH cx_root. "#EC *
        MESSAGE 'Error cambiando fuente' TYPE 'S'.

    ENDTRY.
  ENDMETHOD.
  METHOD set_fontstyle_bold_range.
    "-Variables---------------------------------------------------------

    DATA lv_col_start TYPE i.
    DATA lv_row_start TYPE zexcel_cell_row.
    DATA lv_col_end   TYPE i.
    DATA lv_row_end   TYPE zexcel_cell_row.
    DATA lv_fonts     TYPE zexcel_s_cstyle_font.
    DATA lv_row       TYPE i.
    DATA lv_col       TYPE i.
    DATA lv_col_alpha TYPE string.

    "-Main--------------------------------------------------------------
    TRY.

        lv_col_start =
          zcl_excel_common=>convert_column2int( col_start ).

        lv_row_start = row_start.

        IF col_end IS INITIAL.
          lv_col_end = o_worksheet->get_highest_column( ).
        ELSE.
          lv_col_end =
            zcl_excel_common=>convert_column2int( col_end ).
        ENDIF.

        IF row_end IS INITIAL.
          lv_row_end = o_worksheet->get_highest_row( ).
        ELSE.
          lv_row_end = row_end.
        ENDIF.

        lv_fonts-bold = abap_true.

        lv_row = lv_row_start.
        WHILE lv_row <= lv_row_end.
          lv_col = lv_col_start.
          WHILE lv_col <= lv_col_end.
            lv_col_alpha =
              zcl_excel_common=>convert_column2alpha( ip_column = lv_col ).
            o_worksheet->change_cell_style( ip_column = lv_col_alpha
                                            ip_row    = lv_row
                                            ip_font   = lv_fonts ).
            lv_col = lv_col + 1.
          ENDWHILE.
          lv_row = lv_row + 1.
        ENDWHILE.

      CATCH cx_root. "#EC *
        MESSAGE 'Error cambiando a negrita' TYPE 'S'.

    ENDTRY.
  ENDMETHOD.
  METHOD set_tabla.
    DATA: table_settings   TYPE zexcel_s_table_settings,
          lt_field_catalog TYPE zexcel_t_fieldcatalog.
    DATA l_error    TYPE t_error.
    "-Variables---------------------------------------------------------
    DATA lv_col_end TYPE i.
    DATA lv_col     TYPE i.

    FIELD-SYMBOLS <field_catalog> TYPE zexcel_s_fieldcatalog.

    IF NOT titulo IS INITIAL.
      table_settings-table_name = titulo.
    ENDIF.

    table_settings-table_style      = zcl_excel_table=>builtinstyle_medium2.
    table_settings-show_row_stripes = abap_true.
*  table_settings-show_column_stripes = abap_true.
    table_settings-nofilters        = abap_true.

    IF it_field_catalog IS INITIAL.
      lt_field_catalog = zcl_excel_common=>get_fieldcatalog( ip_table = tabla ).
      DELETE lt_field_catalog WHERE abap_type = 'h'.     "#EC CI_STDSEQ

      IF nombre_columna = 'X' OR nombre_columna = '+'.
        LOOP AT lt_field_catalog ASSIGNING <field_catalog>.
          IF nombre_columna = 'X'.
            <field_catalog>-scrtext_l = <field_catalog>-fieldname.
            <field_catalog>-scrtext_m = <field_catalog>-scrtext_l.
            <field_catalog>-scrtext_s = <field_catalog>-scrtext_m.
          ELSE.
            <field_catalog>-scrtext_s = |{ <field_catalog>-fieldname } - { <field_catalog>-scrtext_s }|.
            <field_catalog>-scrtext_m = |{ <field_catalog>-fieldname } - { <field_catalog>-scrtext_m }|.
            <field_catalog>-scrtext_l = |{ <field_catalog>-fieldname } - { <field_catalog>-scrtext_l }|.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      lt_field_catalog = it_field_catalog.
    ENDIF.

    TRY.
        o_worksheet->bind_table( ip_table          = tabla
                                 is_table_settings = table_settings
                                 it_field_catalog  = lt_field_catalog ).
      CATCH zcx_excel INTO zcx_excel.
        CLEAR l_error.
        l_error-message = zcx_excel->get_longtext( ).
        APPEND l_error TO i_error.
    ENDTRY.

    TRY.
        lv_col_end = o_worksheet->get_highest_column( ).
      CATCH zcx_excel INTO zcx_excel.
        MESSAGE 'Error obteniendo nº columnas' TYPE 'S'.
    ENDTRY.

    lv_col = 1.
    WHILE lv_col <= lv_col_end.
      set_column( col_int  = lv_col
                  autosize = 'X' ).
      lv_col = lv_col + 1.
    ENDWHILE.

    "-Right---------------------------------

    TRY.
        o_worksheet->calculate_column_widths( ).
      CATCH zcx_excel INTO zcx_excel.
        CLEAR l_error.
        l_error-message = zcx_excel->get_longtext( ).
        APPEND l_error TO i_error.
    ENDTRY.
  ENDMETHOD.
