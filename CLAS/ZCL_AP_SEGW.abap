
DEFINE get_rango_may_ceros.
  DATA: &1 TYPE /iwbep/t_cod_select_options.
  &1 = get_filter( log = v_log
                           name = &2
                           it_filter_select_options = it_filter_select_options
                           it_key_tab = it_key_tab
                           mayusculas = &3
                           poner_ceros = &4  ).
  __add_lista v_no_campos &2.
END-OF-DEFINITION.

DEFINE get_rango.
  get_rango_may_ceros &1 &2 '' ''.
END-OF-DEFINITION.

DEFINE get_rango_ceros.
  get_rango_may_ceros &1 &2 '' &3.
END-OF-DEFINITION.

DEFINE get_rango_mayusculas.
  get_rango_may_ceros &1 &2 'X' ''.
END-OF-DEFINITION.

DEFINE get_key_may_ceros.
  get_key( EXPORTING log         = v_log
                             name        = &2
                             it_key_tab  = it_key_tab
                             mayusculas  = &3
                             poner_ceros = &4
                   IMPORTING key         = &1  ).
  __add_lista v_no_campos &2.
END-OF-DEFINITION.

DEFINE get_key.
  get_key_may_ceros &1 &2 '' ''.
END-OF-DEFINITION.

DEFINE get_key_ceros.
  get_key_may_ceros &1 &2 '' &3.
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
    get_sort_itab( EXPORTING it_order = it_order CHANGING et_entityset = et_entityset ).
  ENDIF.

  filter_itab( EXPORTING it_filter_select_options = it_filter_select_options
                                 is_paging                = is_paging
                                 it_key_tab               = it_key_tab
                                 no_campos                = v_no_campos
                       CHANGING  et_entityset             = et_entityset ).

  v_log = v_log_old.
END-OF-DEFINITION.

DEFINE ini_metodo.
  v_log_old = v_log.
  ini_metodo( entidad = &1 metodo = &2 ).
END-OF-DEFINITION.

DEFINE ini_metodo_set.
  DATA: l_entity LIKE LINE OF et_entityset,
        l_metodo TYPE string.
  CONCATENATE iv_entity_set_name '_GET_ENTITYSET' INTO l_metodo.
  TRANSLATE l_metodo TO UPPER CASE.
  ini_metodo( entidad = iv_entity_name metodo = l_metodo ).
  get_is_count.
END-OF-DEFINITION.

DEFINE ini_metodo_entity.
  DATA: l_metodo TYPE string.
  CONCATENATE iv_entity_set_name '_GET_ENTITY' INTO l_metodo.
  TRANSLATE l_metodo TO UPPER CASE.
  ini_metodo( entidad = iv_entity_name metodo = l_metodo ).
END-OF-DEFINITION.

DEFINE key_2_rango.
  DATA: &1 TYPE /iwbep/t_cod_select_options.
  &1 = key_2_rango( &2 ).
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


DEFINE filter_order_std.
** $inlinecount query option for all count entries.
  IF io_tech_request_context->has_inlinecount( ) = abap_true.
    DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
  ELSE.
    CLEAR es_response_context-inlinecount.
  ENDIF.

***The function module for Filter conditons
  CALL METHOD /iwbep/cl_mgw_data_util=>filtering
    EXPORTING it_select_options = it_filter_select_options
    CHANGING ct_data = et_entityset.



*** The function module for $top and $skip Query Options
  CALL METHOD /iwbep/cl_mgw_data_util=>paging
    EXPORTING is_paging = is_paging
    CHANGING ct_data = et_entityset.

****The function module for Orderby condition
  CALL METHOD /iwbep/cl_mgw_data_util=>orderby
    EXPORTING it_order = it_order
    CHANGING ct_data = et_entityset.
END-OF-DEFINITION.
CLASS zcl_ap_segw DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_entidad,
        name      TYPE /iwbep/i_sbo_et-name,
        node_uuid TYPE /iwbep/i_sbo_et-node_uuid,
      END OF t_entidad.
    TYPES tt_entidad TYPE TABLE OF t_entidad.

    DATA o_clase             TYPE REF TO /iwbep/cl_mgw_push_abs_data.
    DATA grabar_todo         TYPE abap_bool.
    DATA nombre_odata        TYPE string.
    DATA grabar_solo_errores TYPE abap_bool.
    DATA v_log               TYPE string.
    DATA grabar_filtros_no   TYPE abap_bool.
    DATA i_entidad           TYPE tt_entidad.
    DATA v_entidad           TYPE string.
    DATA i_sort              TYPE abap_sortorder_tab.
    DATA v_log_old           TYPE string.
    DATA v_no_campos         TYPE string.
    DATA v_id                TYPE string.
    DATA v_string            TYPE string.
    DATA v_so                TYPE /iwbep/s_cod_select_option.
    DATA v_filter            TYPE /iwbep/s_mgw_select_option.
    DATA rango_01            TYPE /iwbep/t_cod_select_options.
    DATA rango_02            TYPE /iwbep/t_cod_select_options.
    DATA rango_03            TYPE /iwbep/t_cod_select_options.
    DATA rango_04            TYPE /iwbep/t_cod_select_options.
    DATA rango_05            TYPE /iwbep/t_cod_select_options.
    DATA rango_06            TYPE /iwbep/t_cod_select_options.
    DATA rango_07            TYPE /iwbep/t_cod_select_options.
    DATA rango_08            TYPE /iwbep/t_cod_select_options.
    DATA rango_09            TYPE /iwbep/t_cod_select_options.
    DATA rango_10            TYPE /iwbep/t_cod_select_options.
    DATA v_filas             TYPE int4.
    DATA v_where             TYPE string.
    DATA v_clave_parametro   TYPE string.
    DATA headers             TYPE tihttpnvp.
    DATA header_read         TYPE abap_bool.

    METHODS constructor
      IMPORTING nombre_odata TYPE string
                o_clase      TYPE REF TO /iwbep/cl_mgw_push_abs_data OPTIONAL.

    METHODS get_key
      IMPORTING !name       TYPE any
                poner_ceros TYPE any       DEFAULT ''
                it_key_tab  TYPE /iwbep/t_mgw_name_value_pair
                !log        TYPE any       DEFAULT ''
                mayusculas  TYPE abap_bool DEFAULT ''
      EXPORTING VALUE(key)  TYPE any.

    METHODS set_log
      IMPORTING !log           TYPE any                                DEFAULT ''
                !type          TYPE msgty                              DEFAULT 'E'
                p1             TYPE any                                DEFAULT ''
                p2             TYPE any                                DEFAULT ''
                p3             TYPE any                                DEFAULT ''
                p4             TYPE any                                DEFAULT ''
                p5             TYPE any                                DEFAULT ''
                p6             TYPE any                                DEFAULT ''
                p7             TYPE any                                DEFAULT ''
                p8             TYPE any                                DEFAULT ''
                p9             TYPE any                                DEFAULT ''
                error_rfc      TYPE REF TO /iwbep/cl_mgw_push_abs_data OPTIONAL
                msgv1          TYPE sy-msgv1                           DEFAULT ''
                msgv2          TYPE sy-msgv2                           DEFAULT ''
                msgv3          TYPE sy-msgv3                           DEFAULT ''
                msgv4          TYPE sy-msgv4                           DEFAULT ''
      RETURNING VALUE(message) TYPE string.

    METHODS get_filter
      IMPORTING !name                    TYPE any
                poner_ceros              TYPE any                          OPTIONAL
                it_filter_select_options TYPE /iwbep/t_mgw_select_option
                !log                     TYPE any                          DEFAULT ''
                mayusculas               TYPE abap_bool                    DEFAULT ''
                it_key_tab               TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
      RETURNING VALUE(rango)             TYPE /iwbep/t_cod_select_options.

    METHODS get_key_rango
      IMPORTING !name        TYPE any
                poner_ceros  TYPE any       OPTIONAL
                it_key_tab   TYPE /iwbep/t_mgw_name_value_pair
                !log         TYPE any       DEFAULT ''
                mayusculas   TYPE abap_bool DEFAULT ''
      RETURNING VALUE(rango) TYPE /iwbep/t_cod_select_options.

    CLASS-METHODS get_valor_rango
      IMPORTING rango        TYPE /iwbep/t_cod_select_options
      RETURNING VALUE(valor) TYPE string.

    METHODS get_abap_field
      IMPORTING !id         TYPE any
                entidad     TYPE any DEFAULT ''
      RETURNING VALUE(abap) TYPE fieldname.

    METHODS get_sort_itab
      IMPORTING entidad      TYPE any   DEFAULT ''
                it_order     TYPE /iwbep/t_mgw_sorting_order
      CHANGING  et_entityset TYPE table OPTIONAL.

    METHODS filter_itab
      IMPORTING entidad                  TYPE any                          DEFAULT ''
                it_filter_select_options TYPE /iwbep/t_mgw_select_option   OPTIONAL
                is_paging                TYPE /iwbep/s_mgw_paging          OPTIONAL
                it_key_tab               TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
                no_campos                TYPE any                          DEFAULT ''
      CHANGING  et_entityset             TYPE table                        OPTIONAL.

    METHODS ini_metodo
      IMPORTING entidad TYPE any DEFAULT ''
                metodo  TYPE any DEFAULT ''.

    METHODS key_2_rango
      IMPORTING !key         TYPE any
                !option      TYPE any DEFAULT 'EQ'
                !sign        TYPE any DEFAULT 'I'
      RETURNING VALUE(rango) TYPE /iwbep/t_cod_select_options.

    METHODS get_field_from_abap
      IMPORTING entidad     TYPE any DEFAULT ''
                VALUE(abap) TYPE any
      RETURNING VALUE(id)   TYPE /iwbep/i_sbo_pr-name.

    METHODS get_entity_dominio
      IMPORTING it_key_tab TYPE /iwbep/t_mgw_name_value_pair
                dominio    TYPE dd07t-domname
                spras      TYPE sy-langu  DEFAULT sy-langu
                mayusculas TYPE abap_bool DEFAULT ''
      EXPORTING er_entity  TYPE dd07t.

    METHODS get_entityset_dominio
      IMPORTING it_filter_select_options TYPE /iwbep/t_mgw_select_option   OPTIONAL
                is_paging                TYPE /iwbep/s_mgw_paging          OPTIONAL
                it_key_tab               TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
                it_order                 TYPE /iwbep/t_mgw_sorting_order   OPTIONAL
                dominio                  TYPE dd07t-domname
                spras                    TYPE sy-langu                     DEFAULT sy-langu
                mayusculas               TYPE abap_bool                    DEFAULT ''
                nombre_id                TYPE any
                nombre_descripcion       TYPE any
      EXPORTING et_entityset             TYPE re_t_rexcjp_dd07t.

    METHODS get_where
      IMPORTING entidad                  TYPE any                          DEFAULT ''
                it_filter_select_options TYPE /iwbep/t_mgw_select_option   OPTIONAL
                it_key_tab               TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
                no_campos                TYPE any                          DEFAULT ''
                campos_mayusculas        TYPE string                       OPTIONAL
                campos_ceros             TYPE string                       OPTIONAL
      RETURNING VALUE(where)             TYPE string.

    METHODS get_abap_field_data
      IMPORTING !id        TYPE any
                entidad    TYPE any DEFAULT ''
      EXPORTING datos      TYPE /iwbep/i_sbo_pr
                mayusculas TYPE abap_bool
                ceros      TYPE int2.

    METHODS get_valor_filter
      IMPORTING it_filter_select_options TYPE /iwbep/t_mgw_select_option
                !property                TYPE any
      RETURNING VALUE(valor)             TYPE string.

    METHODS filtro_contiene_valor
      IMPORTING it_filter_select_options TYPE /iwbep/t_mgw_select_option
                !property                TYPE any
                VALUE(valor)             TYPE any
      RETURNING VALUE(si)                TYPE abap.

    CLASS-METHODS log_process.

    CLASS-METHODS get_filtro_format
      IMPORTING campo         TYPE any
                op            TYPE any DEFAULT '='
                valor         TYPE any
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS set_filter_str
      IMPORTING iv_filter_string         TYPE string
      CHANGING  it_filter_select_options TYPE /iwbep/t_mgw_select_option.

    CLASS-METHODS log_peticion
      IMPORTING !request           TYPE any                                                      OPTIONAL
      CHANGING  ev_entity_data     TYPE xstring                                                  OPTIONAL
                ev_model_id        TYPE /iwbep/med_model_identifier                              OPTIONAL
                ev_language        TYPE sylangu                                                  OPTIONAL
                et_text_keys       TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_texts            OPTIONAL
                ev_vocab_id        TYPE /iwbep/med_vocab_id                                      OPTIONAL
                ev_version         TYPE /iwbep/med_vocab_version                                 OPTIONAL
                es_request_context TYPE /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context OPTIONAL.

    METHODS raise_message
      IMPORTING iv_msg_number TYPE symsgno                      OPTIONAL
                iv_msg_type   TYPE symsgty                      DEFAULT 'E'
                iv_msg_v1     TYPE symsgv                       OPTIONAL
                iv_msg_v2     TYPE symsgv                       OPTIONAL
                iv_msg_v3     TYPE symsgv                       OPTIONAL
                iv_msg_v4     TYPE symsgv                       OPTIONAL
                iv_msg_id     TYPE symsgid                      OPTIONAL
                io_context    TYPE REF TO /iwbep/if_mgw_context OPTIONAL
                !message      TYPE any                          OPTIONAL
                textid        TYPE scx_t100key                  OPTIONAL
                cx_root       TYPE REF TO cx_root               OPTIONAL
                bapireturn    TYPE bapirettab                   OPTIONAL
      RAISING   /iwbep/cx_mgw_busi_exception
                /iwbep/cx_mgw_tech_exception.

    METHODS no_cache
      IMPORTING odata TYPE REF TO /iwbep/cl_mgw_push_abs_data.

    METHODS get_entityset_sample.

    CLASS-METHODS json2datos
      IMPORTING json            TYPE string
                remove_metadata TYPE abap_bool DEFAULT ''
                date_format     TYPE char10    DEFAULT ''
      EXPORTING datos           TYPE any
                !message        TYPE bapi_msg.

    CLASS-METHODS json2tabla
      IMPORTING json            TYPE string
                remove_metadata TYPE abap_bool DEFAULT ''
      EXPORTING tabla           TYPE table
                !message        TYPE bapi_msg.

    CLASS-METHODS tabla2json
      IMPORTING tabla    TYPE table
      EXPORTING json     TYPE string
                !message TYPE bapi_msg.

    METHODS get_entityset_parametros
      IMPORTING it_filter_select_options TYPE /iwbep/t_mgw_select_option   OPTIONAL
                is_paging                TYPE /iwbep/s_mgw_paging          OPTIONAL
                it_key_tab               TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
                it_order                 TYPE /iwbep/t_mgw_sorting_order   OPTIONAL
      EXPORTING et_entityset             TYPE zt_parametros
                es_response_context      TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context
                !message                 TYPE string
      RAISING   /iwbep/cx_mgw_busi_exception
                /iwbep/cx_mgw_tech_exception.

    METHODS get_entity_parametro
      IMPORTING it_key_tab TYPE /iwbep/t_mgw_name_value_pair OPTIONAL
      EXPORTING er_entity  TYPE zv_parametros
                !message   TYPE string.

    METHODS get_datos_tabla
      IMPORTING tabla            TYPE any
                campos           TYPE any
                !where           TYPE any
      RETURNING VALUE(respuesta) TYPE zap_sapui5_accion.

    CLASS-METHODS descomprimir_json
      CHANGING json TYPE string.

    METHODS get_headers.

    METHODS get_valor_header
      IMPORTING parametro    TYPE any
      RETURNING VALUE(value) TYPE string.

    CLASS-METHODS datos2json
      IMPORTING datos                     TYPE any
                pares                     TYPE string                      DEFAULT ''
                suppress_itab             TYPE boolean                     DEFAULT 'X'
                ignore_boolean            TYPE boolean                     OPTIONAL
                dont_escape_ltgt          TYPE boolean                     OPTIONAL
                numc_as_numeric           TYPE boolean                     DEFAULT ''
                date_format               TYPE char10                      OPTIONAL
                replace_underscore        TYPE boolean                     OPTIONAL
                replace_double_underscore TYPE boolean                     OPTIONAL
                name_mappings             TYPE /ui2/cl_json=>name_mappings OPTIONAL
                proyecto                  TYPE any                         DEFAULT ''
                entidad                   TYPE any                         DEFAULT ''
      EXPORTING json                      TYPE string
                !message                  TYPE bapi_msg.

    CLASS-METHODS json_format_tags
      IMPORTING camel_case TYPE abap_bool DEFAULT ''
                pares      TYPE string    DEFAULT ''
                conv_apc   TYPE abap_bool DEFAULT ''
      CHANGING  json       TYPE string.

    CLASS-METHODS popup_json
      IMPORTING json TYPE string.

    CLASS-METHODS get_name_mappings
      IMPORTING proyecto             TYPE any
                entidad              TYPE any
      RETURNING VALUE(name_mappings) TYPE /ui2/cl_json=>name_mappings.

    CLASS-METHODS get_json
      IMPORTING datos            TYPE any
                std              TYPE abap_bool                      DEFAULT ''
                compress         TYPE abap_bool                      DEFAULT 'X'
                pretty_name      TYPE /ui2/cl_json=>pretty_name_mode DEFAULT '*'
                conversion_exits TYPE abap_bool                      DEFAULT ''
                name_mappings    TYPE /ui2/cl_json=>name_mappings    OPTIONAL
                proyecto         TYPE any                            DEFAULT ''
                entidad          TYPE any                            DEFAULT ''
                numc_as_string   TYPE abap_bool                      DEFAULT ''
      EXPORTING json             TYPE string
                !message         TYPE bapi_msg.

    CLASS-METHODS set_json
      IMPORTING std              TYPE abap_bool                      DEFAULT ''
                compress         TYPE abap_bool                      DEFAULT 'X'
                pretty_name      TYPE /ui2/cl_json=>pretty_name_mode DEFAULT /ui2/cl_json=>pretty_mode-camel_case
                conversion_exits TYPE abap_bool                      DEFAULT ''
                name_mappings    TYPE /ui2/cl_json=>name_mappings    OPTIONAL
                proyecto         TYPE any                            DEFAULT ''
                entidad          TYPE any                            DEFAULT ''
                numc_as_string   TYPE abap_bool                      DEFAULT ''
                json             TYPE string
      EXPORTING datos            TYPE any
                !message         TYPE bapi_msg.

    CLASS-METHODS get_mimetype
      IMPORTING fichero         TYPE any
      RETURNING VALUE(mimetype) TYPE skwf_mime.

    CLASS-METHODS get_entityset_modelo.
    CLASS-METHODS get_entity_modelo.
    CLASS-METHODS entityset_create_entity_modelo.

  PROTECTED SECTION.
  PRIVATE SECTION.
endclass. "ZCL_AP_SEGW definition
class ZCL_AP_SEGW implementation.
  METHOD constructor.
    me->nombre_odata = nombre_odata.
    me->o_clase      = o_clase.

    grabar_solo_errores = 'X'.
    grabar_todo = zcl_ap_parametros=>get_atributo1( clave = 'ODATA' campo = nombre_odata ).
    grabar_filtros_no = zcl_ap_parametros=>get_atributo2( clave = 'ODATA' campo = nombre_odata ).
    IF me->grabar_todo = 'N'.
      CLEAR: me->grabar_solo_errores, me->grabar_todo.
    ENDIF.

    SELECT * FROM /iwbep/i_sbo_et
      APPENDING CORRESPONDING FIELDS OF TABLE i_entidad
     WHERE project = nombre_odata.

    SET PARAMETER ID 'Z_NO_VAR_DEF' FIELD 'X' ##EXISTS.
  ENDMETHOD.
  METHOD datos2json.
    " TODO: parameter NAME_MAPPINGS is never used (ABAP cleaner)
    " TODO: parameter PROYECTO is never used (ABAP cleaner)
    " TODO: parameter ENTIDAD is never used (ABAP cleaner)

    DATA: json_doc    TYPE REF TO zcl_json_document,
          l_excepcion TYPE REF TO cx_root.

    CLEAR: json, message.

    TRY.
        json_doc = zcl_json_document=>create_with_data( data = datos
                                                        suppress_itab               = suppress_itab
                                                        ignore_boolean              = ignore_boolean
                                                        dont_escape_ltgt            = dont_escape_ltgt
                                                        numc_as_numeric             = numc_as_numeric
                                                        date_format                 = date_format
                                                        replace_underscore          = replace_underscore
                                                        replace_double_underscore   = replace_double_underscore ).

        json = json_doc->get_json( ).

        IF NOT pares IS INITIAL.
          json_format_tags( EXPORTING pares = pares CHANGING json = json ).
        ENDIF.
      CATCH cx_root INTO l_excepcion.
        message = l_excepcion->get_longtext( ).
        IF message IS INITIAL.
          message = l_excepcion->get_text( ).
        ENDIF.
    ENDTRY.

*Usage zJSON
* https://github.com/se38/zJSON/wiki/Usage-zJSON
*Creating a JSON document with ABAP data (you can pass any structure/table/complex data, except objects)
*
*SELECT * FROM scarr
*   INTO TABLE scarr_t
*   UP TO 3 ROWS.
*
*json_doc = zcl_json_document=>create_with_data( scarr_t ).
*json = json_doc->get_json( ).
*String "json" now contains:
*
*[
*{
*"mandt" :"001",
*"carrid" :"AA",
*"carrname" :"American Airlines",
*"currcode" :"USD",
*"url" :"http://www.aa.com"
*},
*{
*"mandt" :"001",
*"carrid" :"AB",
*"carrname" :"Air Berlin",
*"currcode" :"EUR",
*"url" :"http://www.airberlin.de"
*},
*{
*"mandt" :"001",
*"carrid" :"AC",
*"carrname" :"Air Canada",
*"currcode" :"CAD",
*"url" :"http://www.aircanada.ca"
*}
*]
*New in version 0.2.1
*Append multiple data objects:
*
*SELECT SINGLE * FROM mara
*   INTO mara_data
*   WHERE matnr = '100-100'.
*
*SELECT * FROM marc
*   INTO TABLE marc_data_t
*   WHERE matnr = '100-100'.
*
*json_doc = zcl_json_document=>create( ).
*json_doc->append_data( data = mara_data iv_name = 'MARA' ).
*json_doc->append_data( data = marc_data_t iv_name = 'MARC' ).
*json = json_doc->get_json( ).
*Result
*
*{
*"MARA":
*{
*"mandt" :"800",
*"matnr" :"100-100",
*"ersda" :"19941107",
*"ernam" :"BALLER",
*..
*"fiber_part5" :"000",
*"fashgrd" :""
*},
*"MARC":
*[
*{
*"mandt" :"800",
*"matnr" :"100-100",
*"werks" :"1000",
*"pstat" :"VEDPALSQGF",
*..
*"ref_schema" :"",
*"min_troc" :"000",
*"max_troc" :"000",
*"target_stock" :0.000
*}
*]
*}
*New in version 0.2.3
*Get ABAP data object from JSON string:
*
*Assume our JSON contains the following data (a structure object and a table)
*
*{
*"scarr" :
*{"mandt" :"001",
*"carrid" :"LH",
*"carrname" :"Lufthansa",
*"currcode" :"EUR",
*"url" :"http://www.lufthansa.com"
*},
*"sflight" :
*[{"mandt" :"001",
*"carrid" :"LH",
*"connid" :"0400",
*"fldate" :"20100821",
*..
*"seatsocc_f" :10},
*{"mandt" :"001",
*"carrid" :"LH",
*"connid" :"0400",
*"fldate" :"20100918",
*..
*"seatsocc_f" :10}
*]
*}
*Create the JSON document class as always
*
*json_doc = zcl_json_document=>create_with_json( json ).
*Now get the ABAP data object
*
*TYPES: BEGIN OF ts_data,
*       scarr TYPE scarr,
*       sflight TYPE flighttab,
*     END OF ts_data.
*
*DATA: ls_data TYPE ts_data.
*
*json_doc->get_data( IMPORTING data = ls_data ).
*New in version 0.2.6
*Formatted output of a JSON string (for test purposes)
*
*json_doc = zcl_json_document=>create_with_data( scarr_t ).
*json_doc->dumps( IMPORTING result = result ).
*LOOP AT result
*  ASSIGNING <line>.
*  WRITE:/ <line>.
*ENDLOOP.
*Output
*
*{
* "itab" :
* [
*     {
*         "mandt" : 001,
*         "carrid" : "AA",
*         "carrname" : "American Airlines",
*         "currcode" : "USD",
*         "url" : "http://www.aa.com"
*     },
*     {
*         "mandt" : 001,
*         "carrid" : "AB",
*         "carrname" : "Air Berlin",
*         "currcode" : "EUR",
*         "url" : "http://www.airberlin.de"
*     },
*     {
*         "mandt" : 001,
*         "carrid" : "AC",
*         "carrname" : "Air Canada",
*         "currcode" : "CAD",
*         "url" : "http://www.aircanada.ca"
*     }
* ]
*}
*New in version 0.2.9
*Same example as above, but creation of JSON without "itab".
*
*json_doc = zcl_json_document=>create_with_data(
*  data          = scarr_t
*  suppress_itab = abap_true ).
*The result is now:
*
*[
* {
*     "mandt" : "001",
*     "carrid" : "AA",
*     "carrname" : "American Airlines",
*     "currcode" : "USD",
*     "url" : "http://www.aa.com"
* },
* {
*     "mandt" : "001",
*     "carrid" : "AB",
*     "carrname" : "Air Berlin",
*     "currcode" : "EUR",
*     "url" : "http://www.airberlin.de"
* },
* {
*     "mandt" : "001",
*     "carrid" : "AC",
*     "carrname" : "Air Canada",
*     "currcode" : "CAD",
*     "url" : "http://www.aircanada.ca"
* }
*]
*Working with nested arrays/tables (new in version 0.2.10)
*Please note: the JSON document class is only able to keep one array and one JSON data string in memory. If you have to parse a nested array, you need one JSON class instance per nested array.
*
*lv_json = '[[123,"abc"],[456,"def","another one"]]'.
*lo_json_doc = zcl_json_document=>create_with_json( json = lv_json ).
*
*WHILE lo_json_doc->get_next( ) IS NOT INITIAL.
*     lo_json_doc2 = zcl_json_document=>create_with_json( json = lo_json_doc->get_json( ) ).
*
*     WHILE lo_json_doc2->get_next( ) IS NOT INITIAL.
*       lv_json = lo_json_doc2->get_json( ).
*       WRITE:/ lv_json.
*     ENDWHILE.
*
*ENDWHILE.
*New in release 0.2.13
*if you are expecting a large JSON string, please use the following code to prevent unnecessary memory consumption ("call by ref" instead of "call by value")
*
* json_doc->get_json_large(
*   IMPORTING
*     json = json
* ).
*Creating a JSON document with JSON data and read content (array in this case)
*
*json_doc = zcl_json_document=>create_with_json( json ).
*
*WHILE json_doc->get_next( ) IS NOT INITIAL.
*
*   carrid = json_doc->get_value( 'carrid' ).
*   carrname = json_doc->get_value( 'carrname' ).
*
*   WRITE:/ carrid, carrname.
*
*ENDWHILE.
*Working with conversion exits (new in Release 0.2.14)
* TYPES: BEGIN OF ts_x,
*           field TYPE spras,
*         END OF ts_x.
*
* ls_x-field = 'S'. "Spanish
*
* json_doc->set_use_conversion_exit( abap_false ).   "default
* json_doc->set_data( ls_x ).
* json = json_doc->get_json( ).
*Result {"field" :"S"}
*
* json_doc->set_use_conversion_exit( abap_true ).
* json_doc->set_data( ls_x ).
* json = json_doc->get_json( ).
*Result {"field" :"ES"}
*
* json = '{"field" :"S"}'.
* json_doc->set_use_conversion_exit( abap_false ).   "default
* json_doc->set_json( json ).
* json_doc->get_data(
*   IMPORTING
*     data = ls_x
* ).
*Result ls-x = 'S'
*
* json = '{"field" :"ES"}'.
* json_doc->set_use_conversion_exit( abap_true ).
* json_doc->set_json( json ).
* json_doc->get_data(
*   IMPORTING
*     data = ls_x
* ).
*Result ls-x = 'S'
*
*Simple transformation (new in Release 0.2.17)
*Starting with Basis Release 7.02 SP11 (other Releases: see SAP note 1648418) and Kernel Release 720 patch 116 it is possible to parse JSON with SAP standard transformation. If you don't need special features of the JSON document class (ie. date format,
* conversion exits) you can use a NEW class METHOD to CONVERT json to ABAP DATA and vice versa (better performance and less memory consumption).
*
* TYPES: BEGIN OF ts_test,
*           s TYPE string,
*           d type d,
*         END OF ts_test.
*
*        DATA json  TYPE string.
* DATA test TYPE ts_test.
*
* "*--- ABAP -> JSON ---*
* test-s = 'aaa " bbb \ ccc < ddd > eee'.
* test-d = sy-datlo.
*
* zcl_json_document=>transform_simple(
*   EXPORTING
*     data_in  = test
*   IMPORTING
*     json_out = json
* ).
*
* WRITE:/ json.
*"-> {"RESULT":{"S":"aaa " bbb \ ccc < ddd > eee","D":"2013-02-01"}}
*
* "*--- JSON -> ABAP ---*
* CLEAR test.
*
* zcl_json_document=>transform_simple(
*   EXPORTING
*     json_in  = json
*   IMPORTING
*     data_out = test
* ).
*
* WRITE:/ test-s, test-d.
*"-> aaa " bbb \ ccc < ddd > eee 01022013
*
*Handling of boolean values (new in Release 0.2.24)
*If data element "boolean" is used, the class now returns real boolean values:
*
*DATA: BEGIN OF ls_test,
*        yyy TYPE boolean,
*        xxx TYPE boolean,
*      END OF ls_test.
*
*ls_test-xxx = abap_true.
*
*DATA(json_doc) = zcl_json_document=>create_with_data( ls_test ).
*cl_demo_output=>display( json_doc->get_json( ) ).
*-> {"yyy" :false,"xxx" :true}
*
*But the old handling is still possible:
*
*DATA(json_doc) = zcl_json_document=>create_with_data(
*                     data             = ls_test
*                     ignore_boolean   = abap_true
*                 ).
*-> {"yyy" :"","xxx" :"X"}
*
*Handling of boolean values (new in Release 0.2.25)
*We now can pass JSON boolean values to the JSON document class:
*
*DATA: BEGIN OF ls_test,
*        yyy TYPE boolean,
*        xxx TYPE boolean,
*      END OF ls_test.
*
*DATA(json_doc) = zcl_json_document=>create_with_json( '{"yyy" : false, "xxx" : true}' ).
*json_doc->get_data( IMPORTING data = ls_test ).
*cl_demo_output=>display( ls_test ).
*-> test-f1 = "" test-f2 = "X"
*
*Handling of data reference objects (new in Release 0.2.25)
*TYPES: BEGIN OF ts_test,
*         f1 TYPE boolean,
*         f2 TYPE string,
*       END OF ts_test.
*
*DATA test TYPE ts_test.
*DATA test_ref TYPE REF TO data.
*
*test-f1 = abap_true.
*test-f2 = 'And another Test'.
*
*GET REFERENCE OF test INTO test_ref.
*DATA(json_doc) = zcl_json_document=>create_with_data( test_ref ).
*cl_demo_output=>display( json_doc->get_json( ) ).
*-> {"f1" :true,"f2" :"And another Test"}
*
*Handling of NUMC fields (new in Release 0.2.26)
*NUMC fields are treated like strings (surrounded by quotes) -> attention: this is an incompatible change!
*
*If (like in previous versions) no quotes are required, the parameter "numc_as_numeric" must be set to TRUE:
*
*DATA tnumc TYPE n LENGTH 4 VALUE '10'.
*json_doc = zcl_json_document=>create_with_data( tnumc ).
*-> "0010"
*
*json_doc = zcl_json_document=>create_with_data(
*           data             = tnumc
*           numc_as_numeric  = abap_true
*       ).
*-> 10
*
*Handling of data references (new in Release 0.2.27)
*TYPES: BEGIN OF local_test,
*         field1 TYPE string,
*         field2 TYPE string,
*       END OF local_test.
*
*DATA input  TYPE REF TO data.
*DATA output TYPE REF TO data.
*
*FIELD-SYMBOLS <inp> TYPE local_test.
*
*CREATE DATA input TYPE local_test.
*ASSIGN input->* TO <inp>.
*<inp>-field1 = 'Dref string 1'.
*<inp>-field2 = 'Dref string 2'.
*
*DATA(json_doc) = zcl_json_document=>create_with_data( input ).
*
*DATA(json) = json_doc->get_json( ).
*
*json_doc->set_json( json ).
*
*"*--- Test 1: without creating data field -> returns a string ---*
*FIELD-SYMBOLS <ref1> TYPE string.
*
*json_doc->get_data(
*  IMPORTING
*    data = output
*).
*
*ASSIGN output->* TO <ref1>.
*
*cl_demo_output=>display( <ref1> ).
*-> {"field1" :"Dref string 1","field2" :"Dref string 2"}
*
*"*--- Test 2: with creating data field -> returns the structured values ---*
*FIELD-SYMBOLS <ref2> TYPE local_test.
*
*CLEAR output.
*CREATE DATA output TYPE local_test.
*
*json_doc->get_data(
*  IMPORTING
*    data = output
*).
*
*ASSIGN output->* TO <ref2>.
*
*cl_demo_output=>display( <ref2> ).
*-> FIELD1 = Dref string 1 -> FIELD2 = Dref string 2
*
*New option: replace underscores in fieldnames with hyphen (new in Release 2.32)
*DATA: BEGIN OF local_test,
*        field_number1 TYPE string,
*        field_number2 TYPE string,
*      END OF local_test.
*
*DATA(json) = zcl_json_document=>create_with_data(
*                 data               = local_test
*                 replace_underscore = abap_true
*             )->get_json( ).
*Result
*
*{
*  "field-number1" :"",
*  "field-number2" :""
*}
*New option: replace double underscores in fieldnames with CamelCase (new in Release 2.33)
*    DATA: BEGIN OF test,
*            field_one      TYPE string,
*            field__two     TYPE string,
*            __field__three TYPE string,
*          END OF test.
*
*    DATA(json) = zcl_json_document=>create_with_data(
*        data                      = test
*        replace_underscore        = abap_true
*        replace_double_underscore = abap_true
*    )->get_json( ).
*Result
*
*{
* "field-one":"",
* "fieldTwo":"",
* "FieldThree":""
*}
  ENDMETHOD.
  METHOD descomprimir_json.
    DATA: l_c3 TYPE char3,
          l_s2 TYPE string,
          l_s1 TYPE string,
          i_s  TYPE TABLE OF string.

    l_c3 = json.
    IF l_c3 = '!c#'. " String comprimido.
      SPLIT json AT l_c3 INTO l_s2 l_s1 json.
      SPLIT l_s1 AT '||' INTO TABLE i_s.
      LOOP AT i_s ASSIGNING FIELD-SYMBOL(<s>).
        SPLIT <s> AT '=' INTO l_s1 l_s2.
        REPLACE ALL OCCURRENCES OF l_s2 IN json WITH l_s1.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD entityset_create_entity_modelo.
*
*   DATA: l_ebeln TYPE ebeln.
*
*    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).
*
*    CASE er_entity-valor.
*      WHEN 'ConfirmarPedido'.
*        l_ebeln = er_entity-otros.
*        __poner_ceros l_ebeln.
*        DATA(l_respuesta) = confirmar_pedido( ebeln = l_ebeln datos = er_entity-respuesta ).
*        MOVE-CORRESPONDING l_respuesta TO er_entity.
*      WHEN OTHERS.
*        er_entity-type = 'E'.
*        er_entity-message = |Acción { er_entity-valor } no reconocida|.
*    ENDCASE.
*
  ENDMETHOD.
  METHOD filter_itab.
    DATA: l_entidad   TYPE string,
          r_no_campos TYPE rstt_t_range_string,
          r_rango     TYPE /iwbep/t_cod_select_options,
          l_campo     TYPE fieldname,
          o_campos    TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <filter> TYPE /iwbep/s_mgw_select_option,
                   <entity> TYPE any,
                   <valor>  TYPE any,
                   <key>    TYPE /iwbep/s_mgw_name_value_pair.

    IF entidad IS INITIAL.
      l_entidad = v_entidad.
    ELSE.
      l_entidad = entidad.
    ENDIF.

    IF NOT no_campos IS INITIAL.
      r_no_campos = zcl_ap_lista=>lista2rango( lista = no_campos option = 'EQ' sign = 'E' ).
    ENDIF.

    LOOP AT it_filter_select_options ASSIGNING <filter> WHERE property IN r_no_campos.
      r_rango = <filter>-select_options.
      IF r_rango IS INITIAL.
        CONTINUE.
      ENDIF.

      l_campo = get_abap_field( entidad = l_entidad id = <filter>-property ).
      IF l_campo IS INITIAL.
        CONTINUE.
      ENDIF.

      LOOP AT et_entityset ASSIGNING <entity>.
        IF o_campos IS NOT BOUND.
          o_campos ?= cl_abap_typedescr=>describe_by_data( <entity> ).
        ENDIF.
        IF o_campos IS BOUND.
          READ TABLE   o_campos->components TRANSPORTING NO FIELDS WITH KEY name = l_campo.
          IF sy-subrc = 0.
            ASSIGN COMPONENT sy-tabix OF STRUCTURE <entity> TO <valor>.
            IF sy-subrc = 0.
              IF NOT <valor> IN r_rango.
                DELETE et_entityset.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT it_key_tab ASSIGNING <key> WHERE name IN r_no_campos.
      l_campo = get_abap_field( entidad = l_entidad id = <key>-name ).
      IF l_campo IS INITIAL.
        CONTINUE.
      ENDIF.

      LOOP AT et_entityset ASSIGNING <entity>.
        IF o_campos IS NOT BOUND.
          o_campos ?= cl_abap_typedescr=>describe_by_data( <entity> ).
        ENDIF.
        IF o_campos IS BOUND.
          READ TABLE o_campos->components TRANSPORTING NO FIELDS WITH KEY name = l_campo.
          IF sy-subrc = 0.
            ASSIGN COMPONENT sy-tabix OF STRUCTURE <entity> TO <valor>.
            IF sy-subrc = 0.
              IF <valor> <> <key>-value.
                DELETE et_entityset.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF NOT is_paging-skip IS INITIAL.
      DELETE et_entityset FROM 1 TO is_paging-skip.
    ENDIF.
    IF NOT is_paging-top IS INITIAL.
      DELETE et_entityset FROM is_paging-top.
    ENDIF.
  ENDMETHOD.
  METHOD filtro_contiene_valor.
    FIELD-SYMBOLS <filtro> TYPE /iwbep/s_mgw_select_option.

    ASSIGN it_filter_select_options[ property = property ] TO <filtro>.
    IF sy-subrc = 0.
      IF valor IN <filtro>-select_options.
        si = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_abap_field.
    DATA l_entidad TYPE string.

    FIELD-SYMBOLS <entidad> TYPE t_entidad.

    IF entidad IS INITIAL.
      l_entidad = v_entidad.
    ELSE.
      l_entidad = entidad.
    ENDIF.

    ASSIGN i_entidad[ name = l_entidad ] TO <entidad>.
    IF sy-subrc = 0.
      SELECT abap_field FROM /iwbep/i_sbo_pr
        INTO abap
        UP TO 1 ROWS
        WHERE project     = nombre_odata
          AND parent_uuid = <entidad>-node_uuid
          AND name        = id
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc <> 0.
        set_log( p1 = 'No existe equivalencia para'(NEQ) p2 = l_entidad p3 = 'id' p4 = id ).
      ENDIF.
    ELSE.
      set_log( p1 = 'No existe entidad'(NEE) p2 = l_entidad ).
    ENDIF.
  ENDMETHOD.
  METHOD get_abap_field_data.
    DATA: l_entidad TYPE string,
          l_entity  TYPE /iwbep/i_sbo_et,
          l_dominio TYPE domname,
          l_dd01l   TYPE dd01l.

    FIELD-SYMBOLS <entidad> TYPE t_entidad.

    CLEAR: mayusculas, ceros, datos.
    IF entidad IS INITIAL.
      l_entidad = v_entidad.
    ELSE.
      l_entidad = entidad.
    ENDIF.

    ASSIGN i_entidad[ name = l_entidad ] TO <entidad>.
    IF sy-subrc = 0.
      SELECT * FROM /iwbep/i_sbo_pr
        INTO datos
        UP TO 1 ROWS
        WHERE project     = nombre_odata
          AND parent_uuid = <entidad>-node_uuid
          AND ( name = id OR abap_field = id )
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc <> 0.
        set_log( p1 = 'No existe equivalencia para'(neq) p2 = l_entidad p3 = 'id' p4 = id ).
      ELSE.
        SELECT SINGLE abap_struct FROM /iwbep/i_sbo_et
          INTO l_entity-abap_struct
          WHERE project = nombre_odata
            AND name    = l_entidad.
        IF sy-subrc = 0 AND NOT l_entity-abap_struct IS INITIAL.
          SELECT domname FROM dd03l
             INTO l_dominio
            UP TO 1 ROWS
            WHERE tabname   = l_entity-abap_struct
              AND fieldname = datos-abap_field
            ORDER BY PRIMARY KEY.
          ENDSELECT.
          IF sy-subrc = 0 AND NOT l_dominio IS INITIAL.
            SELECT convexit lowercase outputlen FROM dd01l
              INTO CORRESPONDING FIELDS OF l_dd01l
              UP TO 1 ROWS
             WHERE domname = l_dominio
             ORDER BY PRIMARY KEY.
            ENDSELECT.
            IF sy-subrc = 0.
              IF l_dd01l-convexit = 'ALPHA'.
                ceros = l_dd01l-outputlen.
              ENDIF.
              IF l_dd01l-convexit = 'MATN1'.
                ceros = l_dd01l-outputlen.
              ENDIF.
              IF l_dd01l-lowercase = ''.
                mayusculas = 'X'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      set_log( p1 = 'No existe entidad'(nee) p2 = l_entidad ).
    ENDIF.
  ENDMETHOD.
  METHOD get_datos_tabla.
    DATA l_message TYPE string.

    zcl_ap_fs=>get_tabla( EXPORTING tabla = tabla campos = campos where = where solo_campos_indicados = 'X' get_json = 'X'
                          IMPORTING message = l_message  json = respuesta-respuesta ).

    IF NOT l_message IS INITIAL.
      respuesta-message = l_message.
      respuesta-type    = 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD get_entity_dominio.
    DATA l_valor TYPE dd07t-domvalue_l.

    FIELD-SYMBOLS <key> TYPE /iwbep/s_mgw_name_value_pair.

    ini_metodo dominio 'GET_ENTITY_DOMINIO'.

    ASSIGN it_key_tab[ 1 ] TO <key>.
    IF sy-subrc = 0.
      l_valor = <key>-value.
    ENDIF.

    IF mayusculas = 'X'.
      l_valor = to_upper( l_valor ).
    ENDIF.

    er_entity-domname    = dominio.
    er_entity-ddlanguage = spras.
    er_entity-domvalue_l = l_valor.
    SELECT ddtext FROM dd07t
      INTO er_entity-ddtext
      UP TO 1 ROWS
     WHERE domname    = dominio
       AND ddlanguage = spras
       AND domvalue_l = l_valor
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      set_log( p1 = 'No existe valor'(NEV) p2 = l_valor ).
    ENDIF.
  ENDMETHOD.
  METHOD get_entity_modelo.
*    DATA: l_aufnr  TYPE aufnr.
*
*    ini_metodo_entity.
*    get_key: l_aufnr  'IdOrden'.
*
*    TRY.
*        ordenset_get_entityset(  EXPORTING
*            iv_entity_name           = iv_entity_name
*            iv_entity_set_name       = iv_entity_set_name
*            iv_source_name           = iv_source_name
*             IO_TECH_REQUEST_CONTEXT  = cast #( IO_TECH_REQUEST_CONTEXT )
*            it_filter_select_options = VALUE #( )
*            is_paging                = VALUE #( )
*            it_navigation_path       = VALUE #( )
*            it_order                 = VALUE #( )
*            iv_filter_string         = VALUE #( )
*            iv_search_string         = VALUE #( )
*            it_key_tab               = VALUE #( ( name = 'IdAviso' value = l_qmnum ) )
*          IMPORTING
*            et_entityset             = DATA(et_entityset) ).
*        IF et_entityset IS INITIAL.
*          o_segw->raise_message( io_context = me->mo_context message = |No existe la orden { l_aufnr }| ).
*        ELSE.
*          READ TABLE et_entityset INTO er_entity INDEX 1.
*        ENDIF.
*      CATCH /iwbep/cx_mgw_busi_exception .
*      CATCH /iwbep/cx_mgw_tech_exception .
*    ENDTRY.
  ENDMETHOD.
  METHOD get_entity_parametro.
    DATA: l_clave      TYPE zv_parametros-clave,
          l_campo      TYPE zv_parametros-campo,
          l_valor      TYPE zv_parametros-valor,
          l_valor2     TYPE zv_parametros-valor2,
          et_entityset TYPE zt_parametros.

    ini_metodo 'Parametro' 'GET_ENTITY_PARAMETRO'.
    get_key: l_clave  'Clave',
             l_campo  'Campo',
             l_valor  'Valor',
             l_valor2 'Valor2'.
    IF l_clave IS INITIAL.
      l_clave = v_clave_parametro.
    ENDIF.

    IF NOT l_clave IS INITIAL.
      TRY.
          get_entityset_parametros(
            EXPORTING
              it_key_tab   = it_key_tab
            IMPORTING
              et_entityset = et_entityset
              message      = message ).
          IF message IS INITIAL.
            READ TABLE et_entityset INTO er_entity INDEX 1.
            IF sy-subrc <> 0.
              message = 'No se ha encontrado valor'.
            ENDIF.
          ENDIF.
        CATCH /iwbep/cx_mgw_busi_exception.
        CATCH /iwbep/cx_mgw_tech_exception.
      ENDTRY.

    ELSE.
      message = 'No ha informado Clave'.
    ENDIF.

    IF NOT message IS INITIAL.
      set_log( p1 = message ).
    ENDIF.

    v_log = v_log_old.
  ENDMETHOD.
  METHOD get_entityset_dominio.
    DATA l_filas TYPE i.

    ini_metodo dominio 'GET_ENTITYSET_DOMINIO'.

    IF mayusculas = 'X'.
      get_rango_mayusculas r_valor_m nombre_id.
    ELSE.
      get_rango r_valor nombre_id.
    ENDIF.
    get_rango r_ddtext nombre_descripcion.

    l_filas = is_paging-top + is_paging-skip.
    IF l_filas = 0.
      l_filas = 100000.
    ENDIF.

    SELECT * FROM dd07t
      INTO TABLE et_entityset
     WHERE domname     = dominio
       AND ddlanguage  = spras
       AND domvalue_l IN r_valor
       AND domvalue_l IN r_valor_m
       AND ddtext     IN r_ddtext
      ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      set_log( p1 = 'No existen valores'(NEV) ).
    ENDIF.

    IF NOT is_paging-skip IS INITIAL.
      DELETE et_entityset FROM 1 TO is_paging-skip.
    ENDIF.

    filter_order.
  ENDMETHOD.
  METHOD get_entityset_modelo.
*    ini_metodo_set.
*
*    DATA(l_so) = io_tech_request_context->get_filter( )->get_filter_select_options( ).
*    get_rango_so: aufnr, qmnum, gstrp, gstri, gltri, equnr.
*
*    IF r_qmnum IS INITIAL AND r_equnr IS INITIAL AND NOT l_sql CS 'EQUNR'.
*      o_segw->raise_message( io_context = me->mo_context message = 'Debe filtrar por nº de equipo' ).
*    ENDIF.
*
*    TRY.
*        SELECT * FROM caufv JOIN afih ON caufv~aufnr = afih~aufnr
*          INTO CORRESPONDING FIELDS OF TABLE et_entityset
*         WHERE afih~aufnr IN r_aufnr
*           AND qmnum IN r_qmnum
*           AND gstrp IN r_gstrp
*           AND gstri IN r_gstri
*           AND gltri IN r_gltri
*           AND equnr IN r_equnr
*           AND (l_sql)
*         ORDER BY qmnum.
*      CATCH cx_root INTO DATA(o_root).
*        o_segw->raise_message( io_context = me->mo_context message = o_root->get_text( ) ).
*    ENDTRY.

* Otros a tener en cuenta

**    TRY.
**        CALL METHOD cl_clb2_tools=>odata_filter2select_option
**          EXPORTING
**            iv_filter_string        = iv_filter_string
**          RECEIVING
**            rt_filter_select_option = DATA(i_filter).
**        .
**      CATCH cx_clb2_parse .
**    ENDTRY.
  ENDMETHOD.
  METHOD get_entityset_parametros.
    DATA: l_so        TYPE /iwbep/s_cod_select_option,
          l_char2     TYPE char2,
          l_tabla     TYPE tabname,
          l_dominio   TYPE domname,
          l_campo     TYPE string,
          l_campos    TYPE string,
          l_atributo1 TYPE string,
          l_where     TYPE string,
          l_entity    TYPE zv_parametros.
    DATA datos TYPE REF TO data.

    FIELD-SYMBOLS: <tabla> TYPE STANDARD TABLE,
                   <linea> TYPE any,
                   <fs>    TYPE any.

    ini_metodo 'Parametro' 'GET_ENTITYSET_PARAMETROS'.

    get_rango: r_clave      'Clave',
               r_campo      'Campo',
               r_valor      'Valor',
               r_valor2     'Valor2',
               r_atributo1  'Atributo1',
               r_atributo2  'Atributo2',
               r_atributo3  'Atributo3',
               r_atributo4  'Atributo4',
               r_comentario 'Comentario'.

    IF r_clave IS INITIAL.
      IF NOT v_clave_parametro IS INITIAL.
        l_so-sign   = 'I'.
        l_so-option = 'EQ'.
        l_so-low    = v_clave_parametro.
        APPEND l_so TO r_clave.
      ENDIF.
    ELSE.
* Vemos si queremos valores de otra tabla diferente
      READ TABLE r_clave INTO l_so INDEX 1.
      IF sy-subrc = 0.
        IF strlen( l_so-low ) > 5.
          l_char2 = l_so-low(2).
          IF l_char2 = 'T '.
            l_tabla = l_so-low+2.
          ELSEIF l_char2 = 'D '.
            l_dominio = l_so-low+2.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF r_clave IS INITIAL.
      IF sy-sysid <> zcl_c=>entorno_desarrollo.
        message = 'No ha definido clave acceso parámetros'.
        set_log( p1 = message ).
        RETURN.
      ENDIF.
    ENDIF.

    IF NOT l_tabla IS INITIAL.
* /sap/opu/odata/SAP/ZGSDUI_FIRMA_ENTREGAS_SRV/Parametros?$filter=Clave eq 'T T001' and Campo eq 'BUKRS' and Atributo1 eq 'BUTXT'  and Comentario eq 'BUKRS LIKE |A!!|'
      READ TABLE r_campo INTO l_so INDEX 1.
      IF sy-subrc = 0.
        l_campo = l_so-low.
        __add_lista l_campos l_campo.
      ELSE.
        message = 'Debe especificar la clave a recuperar en CAMPO'.
        set_log( p1 = message ).
        RETURN.
      ENDIF.
      READ TABLE r_atributo1 INTO l_so INDEX 1.
      IF sy-subrc = 0.
        l_atributo1 = l_so-low.
        __add_lista l_campos l_atributo1.
      ELSE.
        message = 'Debe especificar el valor a recuperar en ATRIBUTO1'.
        set_log( p1 = message ).
        RETURN.
      ENDIF.

      READ TABLE r_comentario INTO l_so INDEX 1.
      IF sy-subrc = 0.
        l_where = l_so-low.
        REPLACE ALL OCCURRENCES OF '|' IN l_where WITH ''''.
        REPLACE ALL OCCURRENCES OF '!!' IN l_where WITH '%'.
      ENDIF.

      zcl_ap_fs=>get_tabla( EXPORTING tabla = l_tabla campos = l_campos where = l_where solo_campos_indicados = 'X'
                            IMPORTING message = message datos = datos ).

      ASSIGN datos->* TO <tabla>.
      LOOP AT <tabla> ASSIGNING <linea>.
        CLEAR l_entity.
        l_entity-clave = l_tabla.
        ASSIGN COMPONENT l_campo OF STRUCTURE <linea> TO <fs>.
        IF sy-subrc = 0.
          l_entity-campo = <fs>.
        ENDIF.
        ASSIGN COMPONENT l_atributo1 OF STRUCTURE <linea> TO <fs>.
        IF sy-subrc = 0.
          l_entity-atributo1 = <fs>.
        ENDIF.
        APPEND l_entity TO et_entityset.
      ENDLOOP.
    ELSEIF NOT l_dominio IS INITIAL.
* /sap/opu/odata/SAP/ZGSDUI_FIRMA_ENTREGAS_SRV/Parametros?$filter=Clave eq 'D SHKZG'
      l_entity-clave = l_dominio.
      SELECT domvalue_l ddtext FROM  dd07t                 "#EC CI_BYPASS
        INTO (l_entity-campo, l_entity-atributo1)
       WHERE domname    = l_dominio
         AND ddlanguage = sy-langu
       ORDER BY domvalue_l.
        APPEND l_entity TO et_entityset.
      ENDSELECT.
    ELSE.
*items="{ path: '/Parametros', templateShareable: true, sorter: { path: 'Valor' }, filters: [{path: 'Clave', operator: 'EQ', value1: 'FIRMAS'}, {path:'Campo', operator: 'EQ', value1: 'INCIDENCIA'}]}">
      SELECT * FROM (zcl_c=>tabla_zparametros)
        INTO CORRESPONDING FIELDS OF TABLE et_entityset
       WHERE clave     IN r_clave
         AND campo     IN r_campo
         AND valor     IN r_valor
         AND valor2    IN r_valor2
         AND atributo1 IN r_atributo1
         AND atributo2 IN r_atributo2
         AND atributo3 IN r_atributo3
         AND atributo4 IN r_atributo4
       ORDER BY PRIMARY KEY.
    ENDIF.

    filter_order.

    IF et_entityset IS NOT INITIAL.
** -- Inline Count
      es_response_context-inlinecount = lines( et_entityset ).
** -- Paging
      /iwbep/cl_mgw_data_util=>paging(
        EXPORTING
          is_paging = is_paging
        CHANGING
          ct_data   = et_entityset ).
    ENDIF.
  ENDMETHOD.
  METHOD get_entityset_sample.
**** --- Get the Selected string
*    DATA(lv_str) = io_tech_request_context->get_search_string( ).
***** --- Replace
*    REPLACE ALL OCCURRENCES OF '*' IN lv_str WITH '%'.
**** --- Get Airline Details
*    SELECT carrid carrname currcode url FROM
*          scarr INTO CORRESPONDING FIELDS OF TABLE et_entityset
*          WHERE carrid LIKE lv_str.
*    IF et_entityset IS NOT INITIAL.
*** -- Inline Count
*      es_response_context-inlinecount = lines( et_entityset ).
*** -- Paging
*      CALL METHOD /iwbep/cl_mgw_data_util=>paging
*        EXPORTING
*          is_paging = is_paging
*        CHANGING
*          ct_data   = et_entityset.
*    ENDIF.
  ENDMETHOD.
  METHOD get_field_from_abap.
    DATA l_entidad TYPE string.

    FIELD-SYMBOLS <entidad> TYPE t_entidad.

    IF entidad IS INITIAL.
      l_entidad = v_entidad.
    ELSE.
      l_entidad = entidad.
    ENDIF.

    ASSIGN i_entidad[ name = l_entidad ] TO <entidad>.
    IF sy-subrc = 0.
      SELECT name FROM /iwbep/i_sbo_pr
        INTO id
        UP TO 1 ROWS
        WHERE project     = nombre_odata
          AND parent_uuid = <entidad>-node_uuid
          AND abap_field  = abap
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc <> 0.
        set_log( p1 = 'No existe equivalencia para'(NEQ) p2 = l_entidad p3 = 'campo ABAP'(CAB) p4 = abap ).
      ENDIF.
    ELSE.
      set_log( p1 = 'No existe entidad'(NEE) p2 = l_entidad ).
    ENDIF.
  ENDMETHOD.
  METHOD get_filter.
    DATA: l_ceros TYPE int2,
          l_numc  TYPE c LENGTH 100.

    FIELD-SYMBOLS: <filter> TYPE /iwbep/s_mgw_select_option,
                   <so>     TYPE /iwbep/s_cod_select_option.

    DATA(l_mayusculas) = mayusculas.

    DESCRIBE FIELD poner_ceros TYPE DATA(l_type).
    IF l_type = 'C'.
      IF poner_ceros CO '0123456789 '.
        l_ceros = poner_ceros.
      ENDIF.
    ELSEIF l_type = 'I'.
      l_ceros = poner_ceros.
    ENDIF.

    ASSIGN it_filter_select_options[ property = name ] TO <filter>.
    IF sy-subrc = 0.
      rango = <filter>-select_options.

      IF l_mayusculas = 'A'.
        get_abap_field_data( EXPORTING entidad = v_entidad id = <filter>-property
                             IMPORTING datos = DATA(l_datos_abap) mayusculas = l_mayusculas ceros = l_ceros ).
      ENDIF.

      IF l_ceros > 0.
        LOOP AT rango ASSIGNING <so>.
          l_numc = <so>-low.
          __poner_ceros l_numc(l_ceros).
          <so>-low = l_numc.

          IF NOT <so>-high IS INITIAL.
            l_numc = <so>-high.
            __poner_ceros l_numc(l_ceros).
            <so>-high = l_numc.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF l_mayusculas = 'X'.
        LOOP AT rango ASSIGNING <so>.
          <so>-low  = to_upper( <so>-low ).
          <so>-high = to_upper( <so>-high ).
        ENDLOOP.
      ENDIF.

    ELSE.
      IF NOT it_key_tab IS INITIAL.
        rango = get_key_rango( name = name poner_ceros = l_ceros it_key_tab = it_key_tab log = log mayusculas = l_mayusculas ).
      ENDIF.
    ENDIF.

    IF rango IS INITIAL.
      set_log( log = log p1 = 'No existe filtro por'(nef) p2 = name type = 'W' ).
    ELSE.
      IF me->grabar_filtros_no = 'D'.
        LOOP AT rango ASSIGNING <so>.
          set_log( log = log p1 = 'Rango '(ran) p2 = name  p3 = '=' p4 = <so>-low p5 = <so>-high type = 'I' ).
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_filtro_format.
    CONCATENATE '''' valor '''' INTO string.
    IF op = '='.
      CONCATENATE campo op string INTO string.
    ELSE.
      CONCATENATE campo op string INTO string SEPARATED BY space.
    ENDIF.
  ENDMETHOD.
  METHOD get_headers.
* 1. recuperamos el registro de la pa0105 correspondiente al usuario
    DATA l_facade TYPE REF TO /iwbep/if_mgw_dp_int_facade.

    header_read = 'X'.

    IF o_clase IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        l_facade ?= o_clase->/iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        headers = l_facade->get_request_header( ).
      CATCH cx_root INTO DATA(o_root). " TODO: variable is assigned but never used (ABAP cleaner)
    ENDTRY.
  ENDMETHOD.
  METHOD get_json.
    " TODO: parameter STD is never used (ABAP cleaner)
    " TODO: parameter CONVERSION_EXITS is never used (ABAP cleaner)

    DATA l_key TYPE text132.

    CLEAR: json, message.

    TRY.
        IF NOT proyecto IS INITIAL AND NOT entidad IS INITIAL.
          DATA(l_name_mappings) = get_name_mappings( proyecto = proyecto entidad = entidad ).
        ENDIF.
        LOOP AT name_mappings ASSIGNING FIELD-SYMBOL(<nm>).
          INSERT <nm> INTO TABLE l_name_mappings.
        ENDLOOP.

        DATA(l_pretty_name) = COND char1( WHEN pretty_name = '*' THEN 'X' ELSE pretty_name ).
        json = /ui2/cl_json=>serialize( data = datos
                                        compress = compress
                                        pretty_name = l_pretty_name
*                                      conversion_exits = conversion_exits
                                        numc_as_string = numc_as_string
                                        name_mappings = l_name_mappings ).

        IF pretty_name = '*'.
          DATA(keys) = zcl_ap_regexp=>buscar_patron( string = json patron = '\"([^"]*)\":' ).
          SORT keys.
          DELETE ADJACENT DUPLICATES FROM keys.
          LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).
            DATA(l_long) = strlen( <key> ).
            IF l_long > 3.
              l_long = l_long - 3.
              IF NOT line_exists( l_name_mappings[ json = <key>+1(l_long) ] ).
                l_key = <key>.
                l_key+1(1) = to_upper( l_key+1(1) ).
                IF <key> <> l_key.
                  REPLACE ALL OCCURRENCES OF <key> IN json WITH l_key.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

      CATCH cx_root INTO DATA(l_excepcion).
        message = l_excepcion->get_longtext( ).
        IF message IS INITIAL.
          message = l_excepcion->get_text( ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.
  METHOD get_key.
    DATA: l_tipo        TYPE c LENGTH 1,
          l_poner_ceros TYPE c LENGTH 1,
          l_num_car     TYPE i,
          l_char        TYPE c LENGTH 255.

    FIELD-SYMBOLS <key> TYPE /iwbep/s_mgw_name_value_pair.

    IF NOT poner_ceros IS INITIAL.
      DESCRIBE FIELD poner_ceros TYPE l_tipo.
      IF l_tipo = 'C'.
        l_poner_ceros = 'X'.
      ELSE.
        l_num_car = poner_ceros.
      ENDIF.
    ENDIF.

    ASSIGN it_key_tab[ name = name ] TO <key>.
    IF sy-subrc <> 0.
      IF NOT v_entidad IS INITIAL.
        DATA(l_name) = get_field_from_abap( entidad = v_entidad abap = name ).
        IF NOT l_name IS INITIAL.
          ASSIGN it_key_tab[ name = l_name ] TO <key>.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <key> IS ASSIGNED.
      key = <key>-value.
      IF l_poner_ceros = 'X'.
        zcl_ap_string=>poner_ceros_c( CHANGING cadena = key ).
      ELSEIF l_num_car > 0.
        DESCRIBE FIELD key TYPE l_tipo.
        IF l_tipo = 'g'. " String
          l_char = key.
          zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_char(l_num_car) ).
          key = l_char.
        ELSE.
          zcl_ap_string=>poner_ceros_c( CHANGING cadena = key(l_num_car) ).
        ENDIF.
      ENDIF.
      IF mayusculas = 'X'.
        key = to_upper( key ).
      ENDIF.
      IF me->grabar_filtros_no = 'D' OR me->grabar_todo = 'X'.
        set_log( log = log p1 = 'Key '(key) p2 = name  p3 = '=' p4 = key type = 'I' ).
      ENDIF.
    ELSE.
      set_log( log = log p1 = 'No existe clave'(nec) p2 = name  type = 'W' ).
    ENDIF.
  ENDMETHOD.
  METHOD get_key_rango.
    DATA: l_key    TYPE text255,
          l_select TYPE /iwbep/s_cod_select_option.

    get_key( EXPORTING name = name poner_ceros = poner_ceros it_key_tab = it_key_tab log = log mayusculas = mayusculas
             IMPORTING key = l_key ).
    IF NOT l_key IS INITIAL.
      CLEAR l_select.
      l_select-option = 'EQ'.
      l_select-sign   = 'I'.
      l_select-low    = l_key.
      APPEND l_select TO rango.
    ENDIF.
  ENDMETHOD.
  METHOD get_mimetype.
    DATA filename TYPE skwf_filnm.

    CLEAR mimetype.
    filename = fichero.
    CALL FUNCTION 'SKWF_MIMETYPE_OF_FILE_GET'
      EXPORTING
        filename = filename
      IMPORTING
        mimetype = mimetype.
  ENDMETHOD.
  METHOD get_name_mappings.
    SELECT pr~abap_field AS abap, pr~name AS json FROM /iwbep/i_sbo_et AS et JOIN /iwbep/i_sbo_pr  AS pr
        ON  et~project   = pr~project
        AND et~node_uuid = pr~parent_uuid
      INTO TABLE @name_mappings
     WHERE et~project = @proyecto
       AND et~name    = @entidad
     ORDER BY pr~abap_field.

*SELECT et~abap_struct as estructura, et~name as entityd, pr~name as atributo, pr~abap_field as campo FROM /iwbep/i_sbo_et AS et JOIN /iwbep/i_sbo_pr  AS pr
*    ON et~project = pr~project
*    AND et~node_uuid  = pr~parent_uuid
*  INTO table @DATA(i_campos)
* WHERE et~project = 'Z5PDEAL'.
  ENDMETHOD.
  METHOD get_sort_itab.
    DATA: l_entidad TYPE string,
          l_campo   TYPE string,
          l_sort    TYPE abap_sortorder.

    FIELD-SYMBOLS <order> TYPE /iwbep/s_mgw_sorting_order.

    IF entidad IS INITIAL.
      l_entidad = v_entidad.
    ELSE.
      l_entidad = entidad.
    ENDIF.

    CLEAR i_sort.

    LOOP AT it_order ASSIGNING <order>.
      CLEAR l_campo.
      l_campo = get_abap_field( entidad = l_entidad id = <order>-property ).
      IF NOT l_campo IS INITIAL.
        CLEAR l_sort.
        l_sort-name = l_campo.
        IF <order>-order = 'desc'.
          l_sort-descending = 'X'.
        ENDIF.
        APPEND l_sort TO i_sort.
      ENDIF.
    ENDLOOP.

    IF NOT i_sort IS INITIAL.
      SORT et_entityset BY (i_sort).
    ENDIF.
  ENDMETHOD.
  METHOD get_valor_filter.
    FIELD-SYMBOLS <filtro> TYPE /iwbep/s_mgw_select_option.

    ASSIGN it_filter_select_options[ property = property ] TO <filtro>.
    IF sy-subrc = 0.
      valor = get_valor_rango( <filtro>-select_options ).
    ENDIF.
  ENDMETHOD.
  METHOD get_valor_header.
    CLEAR value.

    IF header_read IS INITIAL.
      get_headers( ).
    ENDIF.

    ASSIGN headers[ name = parametro ] TO FIELD-SYMBOL(<header>).
    IF sy-subrc = 0.
      IF <header>-value = 'null' OR <header>-value = 'undefined'.

      ELSE.
        value = <header>-value.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_valor_rango.
    DATA l_so TYPE /iwbep/s_cod_select_option.

    READ TABLE rango INTO l_so INDEX 1.
    IF sy-subrc = 0.
      valor = l_so-low.
    ENDIF.
  ENDMETHOD.
  METHOD get_where.
    DATA: l_entidad           TYPE string,
          r_no_campos         TYPE rstt_t_range_string,
          r_campos_mayusculas TYPE rstt_t_range_string,
          r_campos_ceros      TYPE rstt_t_range_string,
          l_datos_abap        TYPE /iwbep/i_sbo_pr,
          l_mayusculas        TYPE c LENGTH 1,
          l_ceros             TYPE int2,
          r_rango             TYPE /iwbep/t_cod_select_options,
          l_campo             TYPE fieldname,
          l_cont_rangos       TYPE n LENGTH 2,
          l_nombre_rango      TYPE string,
          l_clausula          TYPE string,
          l_valor             TYPE string,
          o_campos            TYPE REF TO cl_abap_structdescr,
          l_dd01l             TYPE dd01l.

    FIELD-SYMBOLS: <filter> TYPE /iwbep/s_mgw_select_option,
                   <rango>  TYPE any,
                   <key>    TYPE /iwbep/s_mgw_name_value_pair,
                   <so>     TYPE /iwbep/s_cod_select_option,
                   <entity> TYPE any,
                   <valor>  TYPE any.

    CLEAR: rango_01, rango_02, rango_03, rango_04, rango_05, rango_06, rango_07, rango_08, rango_09, rango_10.

    CLEAR where.
    IF entidad IS INITIAL.
      l_entidad = v_entidad.
    ELSE.
      l_entidad = entidad.
    ENDIF.

    IF NOT no_campos IS INITIAL.
      r_no_campos = zcl_ap_lista=>lista2rango( lista = no_campos option = 'NE' ).
    ENDIF.
    r_campos_mayusculas = zcl_ap_lista=>lista2rango( lista = campos_mayusculas ).
    r_campos_ceros = zcl_ap_lista=>lista2rango( lista = campos_ceros ).

    LOOP AT it_filter_select_options ASSIGNING <filter>.

      IF <filter>-property NOT IN r_no_campos.
        CONTINUE.
      ENDIF.

      IF <filter>-select_options IS INITIAL.
        CONTINUE.
      ENDIF.

      get_abap_field_data( EXPORTING entidad = l_entidad id = <filter>-property
                           IMPORTING datos = l_datos_abap mayusculas = l_mayusculas ceros = l_ceros ).
      IF l_datos_abap IS INITIAL.
        CONTINUE.
      ENDIF.

      IF <filter>-property IN r_campos_mayusculas.
        l_mayusculas = 'X'.
      ENDIF.
      IF l_datos_abap-max_length <> 0 AND <filter>-property IN r_campos_mayusculas.
        l_ceros = l_datos_abap-max_length.
      ENDIF.
      r_rango = get_filter(      name = <filter>-property
                                 it_filter_select_options = it_filter_select_options
                                 mayusculas = l_mayusculas
                                 poner_ceros = l_ceros  ).

      l_campo = l_datos_abap-abap_field.
      l_cont_rangos = l_cont_rangos + 1.
      CONCATENATE 'RANGO_' l_cont_rangos INTO l_nombre_rango.
      ASSIGN (l_nombre_rango) TO <rango>.
      IF sy-subrc = 0.
        where = l_clausula.
      ELSE.
        __concat3 where where 'AND' l_clausula.
      ENDIF.
      <rango> = r_rango.
      CONCATENATE 'O_SEGW->' l_nombre_rango INTO l_nombre_rango.
      CONCATENATE l_campo 'IN' l_nombre_rango INTO l_clausula SEPARATED BY space.

      IF where IS INITIAL.
        where = l_clausula.
      ELSE.
        __concat3 where where 'AND' l_clausula.
      ENDIF.
    ENDLOOP.

    LOOP AT it_key_tab ASSIGNING <key> WHERE name IN r_no_campos.
      get_abap_field_data( EXPORTING entidad = l_entidad id = <key>-name
                           IMPORTING datos = l_datos_abap mayusculas = l_mayusculas ceros = l_ceros ).
      IF l_datos_abap IS INITIAL.
        CONTINUE.
      ENDIF.

      IF <key>-name IN r_campos_mayusculas.
        l_mayusculas = 'X'.
      ENDIF.
      IF l_datos_abap-max_length <> 0 AND <key>-name IN r_campos_mayusculas.
        l_ceros = l_datos_abap-max_length.
      ENDIF.
      l_valor = <key>-value.
      get_key( EXPORTING name = <key>-name poner_ceros = l_ceros it_key_tab = it_key_tab log = v_log mayusculas = l_mayusculas
               IMPORTING key = l_valor ).

      CONCATENATE '''' l_valor '''' INTO l_clausula.
      CONCATENATE l_datos_abap-abap_field '=' l_clausula INTO l_clausula SEPARATED BY space.

      IF where IS INITIAL.
        where = l_clausula.
      ELSE.
        __concat3 where where 'AND' l_clausula.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD ini_metodo.
    CLEAR v_no_campos.
    v_entidad = entidad.
    v_log = metodo.
  ENDMETHOD.
  METHOD json2datos.
    DATA: l_json      TYPE string,
          json_doc    TYPE REF TO zcl_json_document,
          l_excepcion TYPE REF TO zcx_json_document.

    CLEAR: datos, message.

    IF json IS INITIAL.
      message = 'JSON vacío'.
      RETURN.
    ENDIF.

    l_json = json.
    descomprimir_json( CHANGING json = l_json ).

    IF remove_metadata = 'X'.
      REPLACE ALL OCCURRENCES OF '{"__metadata":' IN l_json WITH '{'.
    ENDIF.

    json_doc = zcl_json_document=>create_with_json( json = l_json date_format = date_format ).
    TRY.
        json_doc->get_data( IMPORTING  data = datos ).
      CATCH zcx_json_document INTO l_excepcion.
        message = l_excepcion->get_longtext( ).
    ENDTRY.
  ENDMETHOD.
  METHOD json2tabla.
    DATA: l_json      TYPE string,
          json_doc    TYPE REF TO zcl_json_document,
          l_excepcion TYPE REF TO zcx_json_document.

    CLEAR: tabla, message.

    IF json IS INITIAL.
      message = 'JSON vacío'.
      RETURN.
    ENDIF.

    l_json = json.
    descomprimir_json( CHANGING json = l_json ).

    IF remove_metadata = 'X'.
      REPLACE ALL OCCURRENCES OF '{"__metadata":' IN l_json WITH '{'.
    ENDIF.

    json_doc = zcl_json_document=>create_with_json( json = l_json ).

    TRY.
        json_doc->get_data( IMPORTING  data = tabla ).
      CATCH zcx_json_document INTO l_excepcion.
        message = l_excepcion->get_longtext( ).
    ENDTRY.
  ENDMETHOD.
  METHOD json_format_tags.
    " TODO: parameter CAMEL_CASE is never used (ABAP cleaner)
    " TODO: parameter CONV_APC is never used (ABAP cleaner)

    DATA: orig TYPE string,
          dest TYPE string.

    SPLIT pares AT ',' INTO TABLE DATA(i_pares).
    LOOP AT i_pares ASSIGNING FIELD-SYMBOL(<pares>).
      IF <pares> CS '|'.
        SPLIT <pares> AT '|' INTO orig dest.
        CONCATENATE '"' orig '"' INTO orig.
        CONCATENATE '"' dest '"' INTO dest.
        REPLACE ALL OCCURRENCES OF orig IN json WITH dest.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD key_2_rango.
    DATA l_rango TYPE /iwbep/s_cod_select_option.

    CLEAR l_rango.
    l_rango-option = option.
    l_rango-sign   = sign.
    l_rango-low    = key.
    APPEND l_rango TO rango.
  ENDMETHOD.
  METHOD log_peticion.
    " TODO: parameter EV_ENTITY_DATA is never used or assigned (ABAP cleaner)
    " TODO: parameter EV_MODEL_ID is never used or assigned (ABAP cleaner)
    " TODO: parameter EV_LANGUAGE is never used or assigned (ABAP cleaner)
    " TODO: parameter ET_TEXT_KEYS is never used or assigned (ABAP cleaner)
    " TODO: parameter EV_VOCAB_ID is never used or assigned (ABAP cleaner)
    " TODO: parameter EV_VERSION is never used or assigned (ABAP cleaner)
    " TODO: parameter ES_REQUEST_CONTEXT is never used or assigned (ABAP cleaner)

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_string TYPE string.

    l_string = zcl_ap_string=>xstring2string( request ).
  ENDMETHOD.
  METHOD log_process.
*  /IWCOR/CL_DS_PROC_DISPATCHER->/IWCOR/IF_DS_PROCESSOR~PROCESS
  ENDMETHOD.
  METHOD no_cache.
    DATA ls_header TYPE ihttpnvp.

    ls_header-name  = 'Cache-Control' ##NO_TEXT.
    ls_header-value = 'no-cache, no-store' ##NO_TEXT.
    odata->set_header( ls_header ).
    ls_header-name  = 'Pragma' ##NO_TEXT.
    ls_header-value = 'no-cache' ##NO_TEXT.
    odata->set_header( ls_header ).
  ENDMETHOD.
  METHOD popup_json.
    CALL TRANSFORMATION sjson2html SOURCE XML json
         RESULT XML DATA(lvc_html).
    cl_abap_browser=>show_html( title = 'JSON' html_string = cl_abap_codepage=>convert_from( lvc_html ) ).
  ENDMETHOD.
  METHOD raise_message.
    IF NOT io_context IS INITIAL.
      DATA(lo_message_container) = io_context->get_message_container( ).

      IF NOT bapireturn IS INITIAL.
        lo_message_container->add_messages_from_bapi( it_bapi_messages = bapireturn
                                                         iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first ).

      ELSEIF NOT iv_msg_number IS INITIAL.
        lo_message_container->add_message(
           iv_msg_id      = iv_msg_id
           iv_msg_number  = iv_msg_number
           iv_msg_type    = iv_msg_type
           iv_msg_v1      = iv_msg_v1
           iv_msg_v2      = iv_msg_v2
           iv_msg_v3      = iv_msg_v3
           iv_msg_v4      = iv_msg_v4
           iv_add_to_response_header = abap_true ).

      ELSEIF NOT message IS INITIAL.
        lo_message_container->add_message_text_only( iv_msg_type = iv_msg_type iv_msg_text = |{ message }| ).
      ELSEIF NOT cx_root IS INITIAL.
        DATA(l_string) = cx_root->get_text( ).
        lo_message_container->add_message_text_only( iv_msg_type = iv_msg_type iv_msg_text = |{ l_string }| ).
      ENDIF.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ELSE.
      IF NOT textid IS INITIAL.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = textid
            message = |{ message }|.
      ELSEIF NOT iv_msg_number IS INITIAL.
        DATA(lv_message) = VALUE scx_t100key( msgid = iv_msg_id
                                              msgno = iv_msg_number
                                              attr1 = iv_msg_v1
                                              attr2 = iv_msg_v2
                                              attr3 = iv_msg_v3
                                              attr4 = iv_msg_v4 ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid = lv_message.
      ELSEIF NOT cx_root IS INITIAL.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = |{ cx_root->get_text( ) }|.
      ELSEIF NOT message IS INITIAL.
*      lv_message = VALUE scx_t100key( msgid = '00'
*                                      msgno = '398'
*                                      attr1 = iv_msg_v1
*                                                  attr2 = iv_msg_v2
*                                                  attr3 = iv_msg_v3
*                                                  attr4 = iv_msg_v4 ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = |{ message }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD set_filter_str.
    DATA:
      lt_filter_select_options TYPE TABLE OF /iwbep/s_mgw_select_option,
      lt_key_value             TYPE /iwbep/t_mgw_name_value_pair,
      lv_input                 TYPE string,
      lt_filter_string         TYPE TABLE OF string,
      ls_filter_string         TYPE string,
      ls_select_options        TYPE /iwbep/s_cod_select_option,
      ls_filter_select_options TYPE /iwbep/s_mgw_select_option,
      lt_select_options        TYPE /iwbep/t_cod_select_options,
      lv_name                  TYPE string,
      lv_sign                  TYPE string,
      lv_value                 TYPE string.
    DATA: count      TYPE i VALUE 1,
          ls_temp    LIKE ls_filter_select_options,
          countplus  TYPE i,
          ls_sel_op  LIKE ls_select_options,
          lt_temp_op LIKE lt_select_options,
          ls_temp_op LIKE ls_select_options,
          lt_sel_op  LIKE lt_select_options,
          ls_results LIKE ls_filter_select_options,
          lt_results LIKE lt_filter_select_options.

    FIELD-SYMBOLS:
      " TODO: variable is assigned but never used (ABAP cleaner)
      <fs_key_value>     LIKE LINE OF lt_key_value,
      <fs_range_tab>     LIKE LINE OF lt_filter_select_options,
      <fs_select_option> TYPE /iwbep/s_cod_select_option.

*******************************************************************************
    IF iv_filter_string IS NOT INITIAL.
      lv_input = iv_filter_string.

* *— get rid of )( & ' and make AND's uppercase
      REPLACE ALL OCCURRENCES OF ')' IN lv_input WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN lv_input WITH ''.
      REPLACE ALL OCCURRENCES OF `'` IN lv_input WITH ''.
      REPLACE ALL OCCURRENCES OF ' OR ' IN lv_input WITH ' AND '. "?
      REPLACE ALL OCCURRENCES OF ' or ' IN lv_input WITH ' AND '. "?
      REPLACE ALL OCCURRENCES OF ' and ' IN lv_input WITH ' AND '.
      REPLACE ALL OCCURRENCES OF ' eq ' IN lv_input WITH ' EQ '.
      SPLIT lv_input AT ` AND ` INTO TABLE lt_filter_string.

* *— build a table of key value pairs based on filter string
      LOOP AT lt_filter_string INTO ls_filter_string.
        CLEAR: ls_select_options, ls_filter_select_options, lt_select_options.
        APPEND INITIAL LINE TO lt_key_value ASSIGNING <fs_key_value>.

        CONDENSE ls_filter_string.
*       Split at space, then split into 3 parts
        SPLIT ls_filter_string AT ' ' INTO lv_name lv_sign lv_value.
        lv_sign = to_upper( lv_sign ).
        ls_select_options-sign   = 'I'.
        ls_select_options-option = lv_sign.
        ls_select_options-low    = lv_value.
        APPEND ls_select_options TO lt_select_options.
        ls_filter_select_options-property       = lv_name.
        ls_filter_select_options-select_options = lt_select_options.

        APPEND ls_filter_select_options TO lt_filter_select_options.
        CLEAR ls_filter_select_options.
      ENDLOOP.
      CLEAR: ls_select_options, ls_filter_select_options, lt_select_options.
    ENDIF.

*  Loop through the select options, look for two with the same property name, if it exists, copy into one!
    SORT lt_filter_select_options ASCENDING BY property.

    READ TABLE lt_filter_select_options INTO ls_filter_select_options INDEX count.
    WHILE sy-tabix <> 0.
      ls_temp = ls_filter_select_options.
      countplus = count + 1.
      READ TABLE lt_filter_select_options INTO ls_filter_select_options INDEX countplus.
      IF sy-tabix <> 0.
        IF ls_temp-property = ls_filter_select_options-property.
          ls_sel_op-sign   = 'I'.
          ls_sel_op-option = 'EQ'.
          lt_temp_op = ls_temp-select_options.
          lt_select_options = ls_filter_select_options-select_options.
          READ TABLE lt_temp_op INTO ls_temp_op INDEX 1.
          IF ls_temp_op-option = 'GE' OR ls_temp_op-option = 'GE'.
            ls_sel_op-low = ls_temp_op-low.
            READ TABLE lt_select_options INTO ls_select_options INDEX 1.
            ls_sel_op-high = ls_select_options-low.
          ELSE.
            ls_sel_op-high = ls_temp_op-low.
            READ TABLE lt_select_options INTO ls_select_options INDEX 1.
            ls_sel_op-low = ls_select_options-low.
          ENDIF.
          APPEND ls_sel_op TO lt_sel_op.

          ls_results-property       = ls_temp-property.
          ls_results-select_options = lt_sel_op.
          APPEND ls_results TO lt_results.
          count = count + 2.
        ELSE.
          APPEND ls_temp TO lt_results.
          count = count + 1.
        ENDIF.
      ELSE.
        APPEND ls_temp TO lt_results.
        EXIT.
      ENDIF.

      CLEAR: ls_temp, lt_temp_op[], ls_temp_op, lt_sel_op[], ls_sel_op, ls_results, ls_filter_select_options.
      READ TABLE lt_filter_select_options INTO ls_filter_select_options INDEX count.
    ENDWHILE.

*?
    LOOP AT lt_results ASSIGNING <fs_range_tab>.
      LOOP AT <fs_range_tab>-select_options ASSIGNING <fs_select_option> WHERE option = 'EQ' AND high <> ''.
        IF <fs_select_option>-high > <fs_select_option>-low.
          <fs_select_option>-option = 'BT'.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    it_filter_select_options = lt_results.
  ENDMETHOD.
  METHOD set_json.
    " TODO: parameter STD is never used (ABAP cleaner)
    " TODO: parameter COMPRESS is never used (ABAP cleaner)
    " TODO: parameter CONVERSION_EXITS is never used (ABAP cleaner)
    " TODO: parameter NUMC_AS_STRING is never used (ABAP cleaner)

    CLEAR: datos, message.

    TRY.
        IF NOT proyecto IS INITIAL AND NOT entidad IS INITIAL.
          DATA(l_name_mappings) = get_name_mappings( proyecto = proyecto entidad = entidad ).
        ENDIF.
        LOOP AT name_mappings ASSIGNING FIELD-SYMBOL(<nm>).
          INSERT <nm> INTO TABLE l_name_mappings.
        ENDLOOP.

        /ui2/cl_json=>deserialize( EXPORTING json = json
                                             pretty_name = pretty_name
*                                           conversion_exits = conversion_exits
                                             name_mappings = l_name_mappings
                                   CHANGING  data = datos ).

      CATCH cx_root INTO DATA(l_excepcion).
        message = l_excepcion->get_longtext( ).
        IF message IS INITIAL.
          message = l_excepcion->get_text( ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.
  METHOD set_log.
    " TODO: parameter ERROR_RFC is only used in commented-out code (ABAP cleaner)

    DATA: l_log    TYPE string,
          l_string TYPE string.

    IF log IS INITIAL.
      l_log = v_log.
    ELSE.
      l_log = log.
    ENDIF.

    IF ( type = 'E' AND grabar_solo_errores = 'X' ) OR grabar_todo = 'X'.
      l_string = p1.
      IF grabar_filtros_no IS INITIAL AND ( l_string = 'No existe filtro por' OR l_string = 'No existe clave' ) AND type = 'W'.
        EXIT.
      ENDIF.
      message = zcl_ap_utils=>concat( p1 = p1 p2 = p2 p3 = p3 p4 = p4 p5 = p5 p6 = p6 p7 = p7 p8 = p8 p9 = p9  ).
      zcl_ap_log=>set_log( proceso = 'ODATA' progname = nombre_odata clave = l_log msgty = type message = message
                           msgv1 = msgv1 msgv2 = msgv2 msgv3 = msgv3 msgv4 = msgv4 ).
    ENDIF.

*  IF NOT error_rfc IS INITIAL.
*    error_rfc->/iwbep/if_sb_dpc_comm_services~rfc_exception_handling(
*      EXPORTING
*        iv_subrc            = '4'
*        iv_exp_message_text = l_string ).
*  ENDIF.
  ENDMETHOD.
  METHOD tabla2json.
    DATA: json_doc    TYPE REF TO zcl_json_document,
          l_excepcion TYPE REF TO cx_root.

    CLEAR: json, message.

    json_doc = zcl_json_document=>create_with_data( data = tabla suppress_itab = 'X' ).

    TRY.
        json = json_doc->get_json( ).
      CATCH cx_root INTO l_excepcion.
        message = l_excepcion->get_longtext( ).
    ENDTRY.
  ENDMETHOD.