CLASS zcl_ap_clasificacion DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA i_car_num         TYPE tt_bapi1003_alloc_values_num.
    DATA i_car_char        TYPE tt_bapi1003_alloc_values_char.
    DATA i_car_curr        TYPE tt_bapi1003_alloc_values_curr.
    DATA i_return          TYPE bapiret2_t.
    DATA car_char          TYPE bapi1003_alloc_values_char.
    DATA car_num           TYPE bapi1003_alloc_values_num.
    DATA car_curr          TYPE bapi1003_alloc_values_curr.
    DATA return            TYPE bapiret2.
    DATA objecttable       TYPE bapi1003_key-objecttable.
    DATA classnum          TYPE bapi1003_key-classnum.
    DATA classtype         TYPE bapi1003_key-classtype.
    DATA i_lista_char      TYPE tt_bapi1003_alloc_values_char.
    DATA objectname        TYPE bapi1003_key-object.
    DATA i_caracteristicas TYPE wrf_apc_char_tty.
    DATA caracteristica    TYPE wrf_apc_char_sty.
    DATA i_car_unif        TYPE zt_sel_car.
    DATA i_caract_mem      TYPE tt_api_vali.
    DATA caract_mem        TYPE api_vali.
    DATA hay_caract_mem    TYPE abap_bool.
    DATA car_unif          TYPE zsel_car.
    DATA objectkey         TYPE bapi1003_key-object.

    CONSTANTS c_objectclas TYPE cdhdr-objectclas VALUE 'CLASSIFY' ##NO_TEXT.

    METHODS constructor
      IMPORTING objecttable TYPE bapi1003_key-objecttable OPTIONAL
                classnum    TYPE any
                classtype   TYPE bapi1003_key-classtype.

    METHODS get_datos
      IMPORTING objectkey        TYPE any
                unif             TYPE abap_bool DEFAULT ''
                unvaluated_chars TYPE abap_bool DEFAULT ''
                !language        TYPE spras     DEFAULT sy-langu.

    METHODS get_char
      IMPORTING charact       TYPE any
                tabla         TYPE any   OPTIONAL
                nombre_campo  TYPE any   OPTIONAL
                nombre_campo2 TYPE any   OPTIONAL
                idioma        TYPE spras DEFAULT sy-langu
                campo_clave   TYPE any   OPTIONAL
                get_clave     TYPE any   DEFAULT ''
      RETURNING VALUE(valor)  TYPE string.

    METHODS get_num
      IMPORTING charact      TYPE any
      RETURNING VALUE(valor) TYPE atflv.

    METHODS get_fecha
      IMPORTING charact      TYPE any
      RETURNING VALUE(valor) TYPE datum.

    METHODS get_char_tabla
      IMPORTING charact      TYPE any
      RETURNING VALUE(tabla) TYPE tt_bapi1003_alloc_values_char.

    METHODS get_char_clave
      IMPORTING charact      TYPE any
      RETURNING VALUE(valor) TYPE string.

    CLASS-METHODS get_caract_interno
      IMPORTING caract       TYPE any
      RETURNING VALUE(atinn) TYPE atinn.

    CLASS-METHODS get_valores_caract
      IMPORTING caract        TYPE any
      RETURNING VALUE(i_cawn) TYPE tt_cawn.

    CLASS-METHODS get_valores_caract_text
      IMPORTING caract         TYPE any
      RETURNING VALUE(i_cawnt) TYPE tt_cawnt.

    METHODS free.

    CLASS-METHODS nombre2num
      IMPORTING nombre        TYPE any
      RETURNING VALUE(numero) TYPE atinn.

    CLASS-METHODS get_caract_externo
      IMPORTING VALUE(atinn)  TYPE atinn
      RETURNING VALUE(caract) TYPE string.

    METHODS get_lista_caract.

    METHODS set_datos
      IMPORTING objectkey   TYPE any       OPTIONAL
                !commit     TYPE abap_bool DEFAULT 'X'
                dequeue_all TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER objectkey.

    METHODS set_by_key
      IMPORTING objectkey TYPE bapi1003_key-object_guid.

    CLASS-METHODS get_valor_caract_text
      IMPORTING caract               TYPE any       OPTIONAL
                caract_int           TYPE any       OPTIONAL
                valor                TYPE any
                spras                TYPE sy-langu  DEFAULT sy-langu
                solo_idioma_indicado TYPE abap_bool DEFAULT ''
      RETURNING VALUE(descripcion)   TYPE atwtb.

    METHODS get_curr_clave
      IMPORTING charact      TYPE any
      RETURNING VALUE(valor) TYPE string.

    METHODS set_char
      IMPORTING charact       TYPE any
                value_char    TYPE any
                value_neutral TYPE any       OPTIONAL
                charact_descr TYPE any       OPTIONAL
                add_new       TYPE abap_bool DEFAULT ''
                !delete       TYPE abap_bool DEFAULT ''.

    METHODS set_num
      IMPORTING charact       TYPE any
                value_from    TYPE any
                charact_descr TYPE any OPTIONAL
                value_to      TYPE any OPTIONAL.

    METHODS set_curr
      IMPORTING charact       TYPE any
                value_from    TYPE any
                charact_descr TYPE any OPTIONAL.

    CLASS-METHODS es_valor_caract_valido
      IMPORTING caract           TYPE any
                valor            TYPE any
      RETURNING VALUE(es_valido) TYPE abap_bool.

    METHODS get_curr
      IMPORTING charact      TYPE any
      RETURNING VALUE(valor) TYPE tslvt.

    CLASS-METHODS get_valor_caract_text2key
      IMPORTING caract             TYPE any OPTIONAL
                caract_int         TYPE any OPTIONAL
                valor              TYPE any
      RETURNING VALUE(descripcion) TYPE atwrt.

    CLASS-METHODS get_caract_simple
      IMPORTING i_car_char      TYPE tt_bapi1003_alloc_values_char OPTIONAL
                i_car_num       TYPE tt_bapi1003_alloc_values_num  OPTIONAL
                i_car_curr      TYPE tt_bapi1003_alloc_values_curr OPTIONAL
      RETURNING VALUE(i_caract) TYPE zt_sel_car.

    CLASS-METHODS get_desc_caract
      IMPORTING caract             TYPE any      OPTIONAL
                atinn              TYPE atinn    OPTIONAL
                spras              TYPE sy-langu DEFAULT sy-langu
      RETURNING VALUE(descripcion) TYPE atbez.

    METHODS crear_clase
      IMPORTING objectkey   TYPE any
                !commit     TYPE abap_bool DEFAULT 'X'
                dequeue_all TYPE abap_bool DEFAULT ''.

    CLASS-METHODS existe_clase
      IMPORTING classtype     TYPE any
                !object       TYPE any
                objecttable   TYPE any
      RETURNING VALUE(existe) TYPE abap_bool.

    CLASS-METHODS matchcode_caract
      IMPORTING caract       TYPE any
      RETURNING VALUE(valor) TYPE atwrt.

    METHODS get_caract_mem
      IMPORTING default_values TYPE abap_bool DEFAULT ''.

    CLASS-METHODS get_caract_mem_st
      IMPORTING default_values      TYPE abap_bool DEFAULT ''
      RETURNING VALUE(i_caract_mem) TYPE tt_api_vali.

    CLASS-METHODS del_caract_mem_st
      IMPORTING i_caract_mem TYPE tt_api_vali
      RETURNING VALUE(subrc) TYPE subrc.

    CLASS-METHODS set_caract_mem_st
      IMPORTING i_caract_mem  TYPE tt_api_vali
                forzar_cambio TYPE any DEFAULT ''
      RETURNING VALUE(subrc)  TYPE subrc.

    METHODS set_caract_mem
      IMPORTING forzar_cambio TYPE any DEFAULT ''
      RETURNING VALUE(subrc)  TYPE sy-subrc.

    METHODS set_valor_caract_mem
      IMPORTING caract    TYPE any OPTIONAL
                atinn     TYPE any OPTIONAL
                valor     TYPE any
                !multiple TYPE any DEFAULT ''.

    METHODS get_valor_caract_mem
      IMPORTING caract       TYPE any OPTIONAL
                atinn        TYPE any OPTIONAL
      RETURNING VALUE(valor) TYPE atwrt.

    METHODS set_valor_caract_mem_date
      IMPORTING caract        TYPE any OPTIONAL
                atinn         TYPE any OPTIONAL
                valor         TYPE dats
                forzar_cambio TYPE any DEFAULT ''.

    CLASS-METHODS get_object_number
      IMPORTING class_type           TYPE tcla-klart
                object_id            TYPE any
                !table               TYPE any
      RETURNING VALUE(object_number) TYPE inob-cuobj.

    CLASS-METHODS get_cambios
      IMPORTING class_type     TYPE tcla-klart
                object_id      TYPE any
                !table         TYPE any
                caract         TYPE any OPTIONAL
      RETURNING VALUE(i_cdpos) TYPE iscdpos_tab.

    METHODS del_datos
      IMPORTING objectkey TYPE bapi1003_key-object.

    METHODS set_valor_caract_mem_num
      IMPORTING caract TYPE any OPTIONAL
                atinn  TYPE any OPTIONAL
                valor  TYPE any.

    METHODS get_char_lista
      IMPORTING charact       TYPE any
                funcion       TYPE any       DEFAULT ''
                tabla         TYPE any       OPTIONAL
                nombre_campo  TYPE any       OPTIONAL
                nombre_campo2 TYPE any       OPTIONAL
                idioma        TYPE spras     DEFAULT sy-langu
                separador     TYPE any       DEFAULT ','
                clave         TYPE abap_bool DEFAULT ''
      RETURNING VALUE(lista)  TYPE string.

    METHODS get_valor_caract_mem_p
      IMPORTING caract       TYPE any OPTIONAL
                atinn        TYPE any OPTIONAL
      RETURNING VALUE(valor) TYPE atflv.

    METHODS get_char_tabla_string
      IMPORTING charact         TYPE any       OPTIONAL
                separador_enter TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER charact
      RETURNING VALUE(valores)  TYPE string.

    METHODS get_tabla_valores_caract_mem
      IMPORTING caract         TYPE any OPTIONAL
                atinn          TYPE any OPTIONAL
      RETURNING VALUE(valores) TYPE wisp_t_atwrt.

    METHODS del_caract_mem
      IMPORTING caract TYPE any OPTIONAL
                atinn  TYPE any OPTIONAL.

    METHODS get_lista_valores_caract_mem
      IMPORTING caract       TYPE any OPTIONAL
                atinn        TYPE any OPTIONAL
      RETURNING VALUE(lista) TYPE string.

    METHODS copy_caract_bd_to_mem.

    METHODS set_fecha
      IMPORTING charact       TYPE any
                fecha         TYPE any
                charact_descr TYPE any OPTIONAL.

    CLASS-METHODS asignar_clase
      IMPORTING classnum       TYPE any
                classtype      TYPE any
                tabla          TYPE any       DEFAULT ''
                objectkey      TYPE any
                !commit        TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(mensaje) TYPE bapi_msg.

    CLASS-METHODS tiene_clasificacion
      IMPORTING classnum  TYPE any
                classtype TYPE any
                tabla     TYPE any DEFAULT ''
                objectkey TYPE any
      RETURNING VALUE(si) TYPE abap_bool.

    CLASS-METHODS set_valor_clase_simple
      IMPORTING classnum       TYPE any
                classtype      TYPE any
                objectkey      TYPE any
                valor          TYPE any
                tipo           TYPE any       DEFAULT 'C'
                caracteristica TYPE any
                tabla          TYPE any       DEFAULT ''
                valor_hasta    TYPE any       OPTIONAL
                !commit        TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(mensaje) TYPE bapi_msg.

    CLASS-METHODS get_valor_clase_multiple
      IMPORTING classnum       TYPE any
                classtype      TYPE any
                objectkey      TYPE any
                tipo           TYPE any DEFAULT 'C'
                caracteristica TYPE any
      RETURNING VALUE(tabla)   TYPE tt_bapi1003_alloc_values_char.

    CLASS-METHODS get_valor_clase_simple
      IMPORTING classnum       TYPE any
                classtype      TYPE any
                objectkey      TYPE any
                tipo           TYPE any DEFAULT 'C'
                caracteristica TYPE any
                get_clave      TYPE any DEFAULT ''
      EXPORTING VALUE(valor)   TYPE any.

    CLASS-METHODS get_fecha_cambio_valor
      IMPORTING campo        TYPE any DEFAULT 'ATWRT'
                caract       TYPE any
                valor        TYPE any OPTIONAL
                objectid     TYPE any
      RETURNING VALUE(fecha) TYPE dats.

    CLASS-METHODS get_cambios_caract
      IMPORTING caract         TYPE any OPTIONAL
                r_udate        TYPE rstt_t_range_date
      RETURNING VALUE(i_cdpos) TYPE iscdpos_tab.

  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_CLASIFICACION definition
class ZCL_AP_CLASIFICACION implementation.
  METHOD asignar_clase.
    DATA: l_objectkey TYPE bapi1003_key-object,
          l_tabla     TYPE bapi1003_key-objecttable,
          l_classtype TYPE bapi1003_key-classtype,
          l_classnum  TYPE bapi1003_key-classnum,
          i_return    TYPE TABLE OF bapiret2,
          l_return    TYPE bapiret2.

    l_objectkey = objectkey.
    l_tabla     = tabla.
    l_classtype = classtype.
    l_classnum  = classnum.
    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        objectkeynew   = l_objectkey
        objecttablenew = l_tabla
        classnumnew    = l_classnum
        classtypenew   = l_classtype
        keydate        = sy-datum
      TABLES
        return         = i_return.

    LOOP AT i_return INTO l_return WHERE type = 'E'.
      IF mensaje IS INITIAL.
        mensaje = l_return-message.
      ELSE.
        CONCATENATE mensaje l_return-message INTO mensaje SEPARATED BY space.
      ENDIF.
    ENDLOOP.
    IF sy-subrc <> 0.
      IF commit = 'X'.
        zcl_ap_dev=>commit( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    me->classnum  = classnum.
    me->classtype = classtype.

    IF objecttable IS INITIAL.
      SELECT SINGLE obtab FROM tcla
        INTO me->objecttable
       WHERE klart = classtype.
    ELSE.
      me->objecttable = objecttable.
    ENDIF.
  ENDMETHOD.
  METHOD copy_caract_bd_to_mem.
    DATA l_caract_mem TYPE api_vali.

    FIELD-SYMBOLS: <car_char> TYPE bapi1003_alloc_values_char,
                   <car_num>  TYPE bapi1003_alloc_values_num,
                   <car_curr> TYPE bapi1003_alloc_values_curr.

    CLEAR i_caract_mem.
    LOOP AT i_car_char ASSIGNING <car_char>.
      CLEAR l_caract_mem.
      l_caract_mem-atnam = <car_char>-charact.
      l_caract_mem-atinn = get_caract_interno( l_caract_mem-atnam ).
      l_caract_mem-atwrt = <car_char>-value_char.
      l_caract_mem-atbez = <car_char>-charact_descr.
      APPEND l_caract_mem TO i_caract_mem.
    ENDLOOP.

    LOOP AT i_car_num ASSIGNING <car_num>.
      CLEAR l_caract_mem.
      l_caract_mem-atnam = <car_num>-charact.
      l_caract_mem-atinn = get_caract_interno( l_caract_mem-atnam ).
      l_caract_mem-atflv = <car_num>-value_from.
      l_caract_mem-atflb = <car_num>-value_to.
      l_caract_mem-atbez = <car_num>-charact_descr.
      APPEND l_caract_mem TO i_caract_mem.
    ENDLOOP.

    LOOP AT i_car_curr ASSIGNING <car_curr>.
      CLEAR l_caract_mem.
      l_caract_mem-atnam = <car_curr>-charact.
      l_caract_mem-atinn = get_caract_interno( l_caract_mem-atnam ).
      l_caract_mem-atflv = <car_curr>-value_from.
      l_caract_mem-atflb = <car_curr>-value_to.
      l_caract_mem-atbez = <car_curr>-charact_descr.
      APPEND l_caract_mem TO i_caract_mem.
    ENDLOOP.
  ENDMETHOD.
  METHOD crear_clase.
    DATA l_objectkey TYPE bapi1003_key-object.

    l_objectkey = objectkey.
    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        objectkeynew           = l_objectkey
        objecttablenew         = objecttable
        classnumnew            = classnum
        classtypenew           = classtype
*   KEYDATE                = SY-DATUM
*   UNVALUATED_CHARS       = ' '
*   LANGUAGE               = SY-LANGU
* IMPORTING
*   STATUS                 =
*   STANDARDCLASS          =
      TABLES
        allocvaluesnum         = i_car_num
        allocvalueschar        = i_car_char
        allocvaluescurr        = i_car_curr
        return                 = i_return.

    IF commit = 'X'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      IF dequeue_all = 'X'.
        CALL FUNCTION 'DEQUEUE_ALL'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD del_caract_mem.
    DATA l_atinn TYPE atinn.

    IF atinn IS INITIAL.
      l_atinn = get_caract_interno( caract ).
    ELSE.
      l_atinn = atinn.
    ENDIF.

    DELETE i_caract_mem WHERE atinn = l_atinn.
  ENDMETHOD.
  METHOD del_caract_mem_st.
    DATA: l_caract_mem TYPE api_vali,
          l_value      TYPE api_val_i,
          i_values     TYPE TABLE OF api_val_i.

    LOOP AT i_caract_mem INTO l_caract_mem.
      MOVE-CORRESPONDING l_caract_mem TO l_value.
      l_value-atwtb = l_caract_mem-atwrt.
      APPEND l_value TO i_values.
    ENDLOOP.

    CALL FUNCTION 'CTMS_DDB_DEL_VALUE'
* EXPORTING
*   INSTANCE                            =
*   UDEF_INSTANCE                       =
*   MESSAGE                             = ' '
* IMPORTING
*   EXP_NOT_SELECTED                    =
      TABLES
        imp_values                          = i_values
*   EXP_VALUES_ERROR                    =
     EXCEPTIONS
       currency_check                      = 1
       date_check                          = 2
       format_check                        = 3
       illegal_internal_baseunit           = 4
       interval_check                      = 5
       pattern_check                       = 6
       time_check                          = 7
       unit_check                          = 8
       value_not_found                     = 9
       display_mode                        = 10
       characteristic_not_found            = 11
       characteristic_enqueue              = 12
       objectcharacteristic                = 13
       characteristic_not_selectable       = 14
       no_valid_dimension                  = 15
       interval_not_allowed                = 16
       input_to_long                       = 17
       value_contradiction                 = 18
       OTHERS                              = 19.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDMETHOD.
  METHOD del_datos.
    CLEAR i_return.

    CALL FUNCTION 'BAPI_OBJCL_DELETE'
      EXPORTING
        objectkey              = objectkey
        objecttable            = objecttable
        classnum               = classnum
        classtype              = classtype
*     CHANGENUMBER       =
*     KEYDATE            = SY-DATUM
      TABLES
        return             = i_return.
  ENDMETHOD.
  METHOD es_valor_caract_valido.
    DATA: l_atinn TYPE atinn,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_ATWRT TYPE atwrt.

    l_atinn = get_caract_interno( caract ).

    CLEAR es_valido.
    SELECT SINGLE atwrt FROM cawn
      INTO l_atwrt
     WHERE atinn = l_atinn
       AND atwrt = valor.
    IF sy-subrc = 0.
      es_valido = 'X'.
    ELSE.
      SELECT SINGLE atwrt FROM cawn
        INTO l_atwrt
       WHERE atinn = l_atinn.
      IF sy-subrc <> 0.
        es_valido = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD existe_clase.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_inob TYPE inob.

    CLEAR existe.
    IF classtype = '023'.
      SELECT SINGLE klart FROM inob
        INTO l_inob-klart
       WHERE klart = classtype
         AND obtab = objecttable
         AND objek = object.
      IF sy-subrc = 0.
        existe = 'X'.
      ENDIF.
    ELSE.
      SELECT SINGLE klart FROM kssk
        INTO l_inob-klart
       WHERE objek = object
         AND klart = classtype.
      IF sy-subrc = 0.
        existe = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD free.
    CLEAR: i_car_num, i_car_char, i_car_curr, i_return.
  ENDMETHOD.
  METHOD get_cambios.
    DATA: l_object   TYPE inob-cuobj,
          l_objectid TYPE cdhdr-objectid,
          l_atinn    TYPE atinn,
          l_tabkey   TYPE cdpos-tabkey.

    FIELD-SYMBOLS <cdpos> TYPE iscdpos.

    l_object = get_object_number( class_type = class_type
                                  object_id  = object_id
                                  table      = table ).
    CONCATENATE l_object 'O' INTO l_objectid.

    i_cdpos = zcl_ap_control_cambios=>get_cdpos( objectclas = 'CLASSIFY'
                                                 objectid   = l_objectid ).

    IF NOT caract IS INITIAL.
      l_atinn = get_caract_interno( caract ).
      CONCATENATE l_object l_atinn INTO l_tabkey.
      LOOP AT i_cdpos ASSIGNING <cdpos>.
        IF NOT ( <cdpos>-tabname = 'ABAUSP' AND <cdpos>-tabkey(28) = l_tabkey ).
          DELETE i_cdpos.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_cambios_caract.
    DATA: i_cdhdr  TYPE TABLE OF iscdpos,
          r_tabkey TYPE RANGE OF cdpos-tabkey.
    DATA l_atflv    TYPE ausp-atflv.
    DATA l_ctd_char TYPE cha_class_view-sollwert.

    SELECT * FROM cdhdr
      INTO CORRESPONDING FIELDS OF TABLE i_cdhdr
     WHERE objectclas  = c_objectclas
       AND udate      IN r_udate.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF NOT caract IS INITIAL.
      DATA(l_atinn) = get_caract_interno( caract ).
      r_tabkey = VALUE #( ( option = 'CP' sign = 'I' low = |*{ l_atinn }*| ) ).
    ENDIF.

    SELECT * FROM cdpos "#EC CI_NO_TRANSFORM
      INTO CORRESPONDING FIELDS OF TABLE i_cdpos
      FOR ALL ENTRIES IN i_cdhdr
     WHERE objectclas  = c_objectclas
       AND objectid    = i_cdhdr-objectid
       AND tabname     = 'ABAUSP'
       AND tabkey     IN r_tabkey
      ORDER BY PRIMARY KEY.
    LOOP AT i_cdpos ASSIGNING FIELD-SYMBOL(<cdpos>).
      ASSIGN i_cdhdr[ objectid = <cdpos>-objectid ] TO FIELD-SYMBOL(<cdhdr>).
      IF sy-subrc = 0.
        <cdpos>-username = <cdhdr>-username.
        <cdpos>-udate    = <cdhdr>-udate.
        <cdpos>-utime    = <cdhdr>-utime.
        <cdpos>-tcode    = <cdhdr>-tcode.
      ENDIF.

* En cantidades, hace cosas raras, con los decimales, si detectamos que tiene muchos, formateamos
      IF <cdpos>-fname <> 'ATFLV'.
        CONTINUE.
      ENDIF.

      SPLIT <cdpos>-value_new AT '.' INTO DATA(l_entero) DATA(l_decimal).
      IF strlen( l_decimal ) > 2.
        l_atflv = <cdpos>-value_new.
        CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
          EXPORTING
            i_number_of_digits = 6
            i_fltp_value       = l_atflv
          IMPORTING
            e_char_field       = l_ctd_char.
        <cdpos>-value_new = l_ctd_char.
        REPLACE ALL OCCURRENCES OF ',' IN <cdpos>-value_new WITH '.'.
      ENDIF.

      SPLIT <cdpos>-value_old AT '.' INTO l_entero l_decimal.
      IF strlen( l_decimal ) > 2.
        l_atflv = <cdpos>-value_old.
        CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
          EXPORTING
            i_number_of_digits = 6
            i_fltp_value       = l_atflv
          IMPORTING
            e_char_field       = l_ctd_char.
        <cdpos>-value_old = l_ctd_char.
        REPLACE ALL OCCURRENCES OF ',' IN <cdpos>-value_old WITH '.'.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_caract_externo.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
      EXPORTING
        input  = atinn
      IMPORTING
        output = caract.
  ENDMETHOD.
  METHOD get_caract_interno.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input         = caract
     IMPORTING
        output        = atinn.
  ENDMETHOD.
  METHOD get_caract_mem.
    FIELD-SYMBOLS <fs> TYPE table.

    CLEAR hay_caract_mem.
    i_caract_mem = get_caract_mem_st( default_values = default_values ).

    IF NOT me->i_caract_mem IS INITIAL.
      hay_caract_mem = 'X'.
    ELSE.
      ASSIGN ('(SAPLCTMS)MI[]') TO <fs>. "#EC *
      IF sy-subrc = 0.
        IF NOT <fs> IS INITIAL.
          hay_caract_mem = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_caract_mem_st.
    DATA i_imp TYPE TABLE OF api_char.

    CLEAR i_caract_mem.
    CALL FUNCTION 'CTMS_DDB_HAS_VALUES_INTERNAL'
      EXPORTING
        assigned_values            = 'X'
        allowed_values             = ' '
        valid_values               = ' '
*   INCONSISTENT_VALUES        = ' '
*   FIRST_ASSIGNED_VALUE       = ' '
        default_values             = default_values
*   LANGUAGE                   = SY-LANGU
*   DOCUMENT_INFO              = ' '
*   EXCL_KNOWLEDGE             = ' '
*   I_CALLED_FROM_DDB          = ' '
      TABLES
        imp_characteristics        = i_imp
        exp_values                 = i_caract_mem
     EXCEPTIONS
       not_found                  = 1
       OTHERS                     = 2.
    IF sy-subrc <> 0.
*      message 'No hay datos en memoria' type 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD get_caract_simple.
    DATA: l_car_char TYPE bapi1003_alloc_values_char,
          l_orden    TYPE zsel_car-orden,
          l_caract   TYPE zsel_car,
          l_car_num  TYPE bapi1003_alloc_values_num,
          l_car_curr TYPE bapi1003_alloc_values_curr.
    DATA: l_num   TYPE i,
          l_fecha TYPE d.

    CLEAR i_caract.

    LOOP AT i_car_char INTO l_car_char.
      l_orden = l_orden + 1.
      CLEAR l_caract.
      l_caract-charact       = l_car_char-charact.
      l_caract-charact_descr = l_car_char-charact_descr.
      l_caract-atinn         = get_caract_interno( l_car_char-charact ).
      l_caract-tipo_car      = 'C'.
      l_caract-orden         = l_orden.
      IF NOT l_car_char-value_neutral IS INITIAL.
        l_caract-value = l_car_char-value_neutral.
      ELSE.
        l_caract-value = l_car_char-value_char.
      ENDIF.
      IF NOT l_car_char-value_char IS INITIAL.
        l_caract-value_desc = l_car_char-value_char.
      ELSE.
        l_caract-value_desc = l_car_char-value_neutral.
      ENDIF.

      APPEND l_caract TO i_caract.
    ENDLOOP.

    LOOP AT i_car_num INTO l_car_num.
      l_orden = l_orden + 1.
      CLEAR l_caract.
      l_caract-charact       = l_car_num-charact.
      l_caract-charact_descr = l_car_num-charact_descr.
      l_caract-atinn         = get_caract_interno( l_car_num-charact ).
      l_caract-tipo_car      = 'N'.
      l_caract-orden         = l_orden.

      l_num = l_car_num-value_from.
      IF l_num >= '20010101' AND l_num <= '99991231'.
        WRITE l_num TO l_fecha.
        IF zcl_ap_fechas=>es_valida( l_fecha ) = 'X'.
          WRITE l_fecha TO l_caract-value.
        ENDIF.
      ENDIF.
      IF l_caract-value IS INITIAL.
        l_caract-value = zcl_ap_string=>string_pot_2importe( l_car_num-value_from ).
      ENDIF.
      IF NOT l_car_num-value_to IS INITIAL.
        l_caract-value_to = zcl_ap_string=>string_pot_2importe( l_car_num-value_to ).
      ENDIF.

      CONDENSE l_caract-value NO-GAPS.
      CONDENSE l_caract-value_to NO-GAPS.
      l_caract-value_num    = l_car_num-value_from.
      l_caract-value_num_to = l_car_num-value_to.
      l_caract-value_desc   = l_car_num-value_from.
      APPEND l_caract TO i_caract.
    ENDLOOP.

    LOOP AT i_car_curr INTO l_car_curr.
      l_orden = l_orden + 1.
      CLEAR l_caract.
      l_caract-charact       = l_car_curr-charact.
      l_caract-charact_descr = l_car_curr-charact_descr.
      l_caract-atinn         = get_caract_interno( l_car_curr-charact ).
      l_caract-tipo_car      = 'C'.
      l_caract-orden         = l_orden.
      l_caract-value         = zcl_ap_string=>string_pot_2importe( l_car_num-value_from ).
      l_caract-value_to      = zcl_ap_string=>string_pot_2importe( l_car_num-value_to ).
      CONDENSE l_caract-value NO-GAPS.
      CONDENSE l_caract-value_to NO-GAPS.

      l_caract-value_num    = l_car_num-value_from.
      l_caract-value_num_to = l_car_num-value_to.
      l_caract-value_desc   = l_car_curr-value_from.
      APPEND l_caract TO i_caract.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_char.
    CLEAR valor.
    READ TABLE i_car_char INTO car_char WITH KEY charact = charact.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF get_clave = 'X'.
      valor = car_char-value_neutral.
    ELSE.
      valor = car_char-value_char.

      IF NOT tabla IS INITIAL.
        valor = zcl_ap_dev=>get_descripcion( tabla = tabla
                                             valor = valor
                                             campo_clave = campo_clave
                                             nombre_campo = nombre_campo
                                             nombre_campo2 = nombre_campo2
                                             idioma = idioma ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_char_clave.
    CLEAR valor.
    READ TABLE i_car_char INTO car_char WITH KEY charact = charact.
    IF sy-subrc = 0.
      valor = car_char-value_neutral.
    ENDIF.
  ENDMETHOD.
  METHOD get_char_lista.
    DATA: l_valor     TYPE cawn-atwrt,
          l_atinn     TYPE atinn,
          l_valor_new TYPE cawn-atwrt.

    CLEAR lista.
    LOOP AT i_car_char INTO car_char WHERE charact = charact.
      IF clave IS INITIAL.
        l_valor = car_char-value_char.
      ELSE.
        l_valor = car_char-value_neutral.
      ENDIF.

      IF NOT funcion IS INITIAL.
        l_atinn = get_caract_interno( charact ).
        CALL FUNCTION funcion
          EXPORTING
            charact_no = l_atinn
            charact    = charact
            value      = l_valor_new
          EXCEPTIONS
            not_found  = 1.
        IF sy-subrc = 0.
          l_valor = l_valor_new.
        ENDIF.
      ENDIF.

      IF NOT tabla IS INITIAL.
        l_valor = zcl_ap_dev=>get_descripcion( tabla = tabla
                                               valor = l_valor
                                               nombre_campo = nombre_campo
                                               nombre_campo2 = nombre_campo2
                                               idioma = idioma ).
      ENDIF.

      IF lista IS INITIAL.
        lista = l_valor.
      ELSE.
        CONCATENATE lista separador INTO lista.
        CONCATENATE lista l_valor INTO lista SEPARATED BY space.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
  METHOD get_char_tabla.
    CLEAR tabla.
    LOOP AT i_car_char INTO car_char WHERE charact = charact.
      APPEND car_char TO tabla.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_char_tabla_string.
    DATA tabla TYPE tt_bapi1003_alloc_values_char.

    FIELD-SYMBOLS <tabla> TYPE bapi1003_alloc_values_char.

    CLEAR valores.
    tabla = get_char_tabla( charact ).

    LOOP AT tabla ASSIGNING <tabla>.
      IF sy-tabix = 1.
        valores = <tabla>-value_char.
      ELSEIF sy-tabix > 1.
        IF separador_enter IS INITIAL.
          CONCATENATE valores ',' INTO valores.
          CONCATENATE valores <tabla>-value_char INTO valores SEPARATED BY space.
        ELSE.
          CONCATENATE valores cl_abap_char_utilities=>newline INTO valores.
          CONCATENATE valores <tabla>-value_char INTO valores.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_curr.
    DATA l_valor TYPE glt0-tslvt.

    CLEAR valor.
    READ TABLE i_car_curr INTO car_curr WITH KEY charact = charact.
    IF sy-subrc = 0.
      l_valor = car_curr-value_from.
      valor = l_valor.
    ENDIF.
  ENDMETHOD.
  METHOD get_curr_clave.
    DATA l_p TYPE p LENGTH 16 DECIMALS 8.

    CLEAR valor.
    READ TABLE i_car_curr INTO car_curr WITH KEY charact = charact.
    IF sy-subrc = 0.
      l_p   = car_curr-value_from.
      valor = l_p.
    ENDIF.
  ENDMETHOD.
  METHOD get_datos.
    CLEAR: i_car_num, i_car_char, i_car_curr, i_return.

    me->objectkey = objectkey.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey              = me->objectkey
        objecttable            = objecttable
        classnum               = classnum
        classtype              = classtype
*   KEYDATE                = SY-DATUM
        unvaluated_chars       = unvaluated_chars
*   LANGUAGE               = SY-LANGU
        language               = language
* IMPORTING
*   STATUS                 =
*   STANDARDCLASS          =
      TABLES
        allocvaluesnum         = i_car_num
        allocvalueschar        = i_car_char
        allocvaluescurr        = i_car_curr
        return                 = i_return.

    IF unif = 'X'.
      i_car_unif = get_caract_simple( i_car_char = i_car_char
                                      i_car_num  = i_car_num
                                      i_car_curr = i_car_curr ).
    ENDIF.
  ENDMETHOD.
  METHOD get_desc_caract.
    DATA l_atinn TYPE atinn.

    IF atinn IS INITIAL.
      l_atinn = get_caract_interno( caract ).
    ELSE.
      l_atinn = atinn.
    ENDIF.

    CLEAR descripcion.
    SELECT atbez
      INTO descripcion
      UP TO 1 ROWS
      FROM cabn JOIN cabnt ON  cabn~atinn = cabnt~atinn
                           AND cabn~adzhl = cabnt~adzhl
     WHERE cabn~atinn  = l_atinn
       AND cabnt~spras = spras
       ORDER BY cabn~atinn cabn~adzhl cabnt~spras.
    ENDSELECT.
    IF sy-subrc <> 0.
      SELECT atbez
        INTO descripcion
        UP TO 1 ROWS
        FROM cabn JOIN cabnt ON  cabn~atinn = cabnt~atinn
                             AND cabn~adzhl = cabnt~adzhl
       WHERE cabn~atinn  = l_atinn
         AND cabnt~spras = 'E'
        ORDER BY cabn~atinn cabn~adzhl cabnt~spras.
      ENDSELECT.
      IF sy-subrc <> 0.
        SELECT atbez
          INTO descripcion
          UP TO 1 ROWS
          FROM cabn JOIN cabnt ON  cabn~atinn = cabnt~atinn
                               AND cabn~adzhl = cabnt~adzhl
         WHERE cabn~atinn = l_atinn
          ORDER BY cabn~atinn cabn~adzhl cabnt~spras.
        ENDSELECT.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_fecha.
    DATA: l_num TYPE i,
          l_str TYPE string.

    CLEAR valor.
    l_num = get_num( charact ).
    IF l_num <> 0.
      l_str = l_num.
      valor = l_str.
    ENDIF.
  ENDMETHOD.
  METHOD get_fecha_cambio_valor.
    " TODO: parameter CAMPO is never used (ABAP cleaner)

    DATA: l_atinn    TYPE atinn,
          l_tabkey   TYPE cdpos-tabkey,
          l_objectid TYPE cdpos-objectid,
          l_cdpos    TYPE cdpos.

    l_atinn = get_caract_interno( caract ).
    CONCATENATE objectid l_atinn '%' INTO l_tabkey.
    CONCATENATE objectid 'O' INTO l_objectid.

    SELECT * FROM cdpos
      INTO l_cdpos
      UP TO 1 ROWS
     WHERE objectclas    = c_objectclas
       AND objectid      = l_objectid
       AND tabname       = 'ABAUSP'
       AND tabkey     LIKE l_tabkey
       AND value_new     = valor
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF NOT l_cdpos IS INITIAL.
      SELECT SINGLE udate FROM cdhdr
        INTO fecha
       WHERE objectclas = c_objectclas
         AND objectid   = l_objectid
         AND changenr   = l_cdpos-changenr.
    ENDIF.
  ENDMETHOD.
  METHOD get_lista_caract.
    CLEAR i_caracteristicas.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE i_caracteristicas
      FROM klah INNER JOIN ksml ON klah~clint = ksml~clint
                INNER JOIN cabn ON  ksml~imerk = cabn~atinn
                                AND ksml~adzhl = cabn~adzhl
     WHERE klah~class = classnum.
  ENDMETHOD.
  METHOD get_lista_valores_caract_mem.
    DATA valores TYPE wisp_t_atwrt.

    FIELD-SYMBOLS <valor> TYPE atwrt.

    valores = get_tabla_valores_caract_mem( caract = caract atinn = atinn ).

    LOOP AT valores ASSIGNING <valor>.
      IF lista IS INITIAL.
        lista = <valor>.
      ELSE.
        CONCATENATE lista <valor> INTO lista SEPARATED BY ','.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_num.
    CLEAR valor.
    READ TABLE i_car_num INTO car_num WITH KEY charact = charact.
    IF sy-subrc = 0.
      valor = car_num-value_from.
    ENDIF.
  ENDMETHOD.
  METHOD get_object_number.
    DATA l_object_id TYPE kssk-objek.

    l_object_id = object_id.

    CALL FUNCTION 'CUOB_GET_NUMBER'
      EXPORTING
        class_type             = class_type
        object_id              = l_object_id
        table                  = table
*     I_READ_DB_FIRST        = ' '
     IMPORTING
       object_number          = object_number
     EXCEPTIONS
       lock_problem           = 1
       object_not_found       = 2.
    IF sy-subrc <> 0.
      MESSAGE 'No se ha encontrado el objeto' TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD get_tabla_valores_caract_mem.
    DATA l_atinn TYPE atinn.

    FIELD-SYMBOLS <caract> TYPE api_vali.

    IF atinn IS INITIAL.
      l_atinn = get_caract_interno( caract ).
    ELSE.
      l_atinn = atinn.
    ENDIF.

    CLEAR valores.
    LOOP AT i_caract_mem ASSIGNING <caract> WHERE atinn = l_atinn.
      APPEND <caract>-atwrt TO valores.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_valor_caract_mem.
    DATA l_atinn TYPE atinn.

    FIELD-SYMBOLS <caract> TYPE api_vali.

    IF atinn IS INITIAL.
      l_atinn = get_caract_interno( caract ).
    ELSE.
      l_atinn = atinn.
    ENDIF.

    CLEAR valor.
    ASSIGN i_caract_mem[ atinn = l_atinn ] TO <caract>.
    IF sy-subrc = 0.
      valor = <caract>-atwrt.
    ENDIF.
  ENDMETHOD.
  METHOD get_valor_caract_mem_p.
    DATA l_atinn TYPE atinn.

    FIELD-SYMBOLS <caract> TYPE api_vali.

    IF atinn IS INITIAL.
      l_atinn = get_caract_interno( caract ).
    ELSE.
      l_atinn = atinn.
    ENDIF.

    CLEAR valor.
    ASSIGN i_caract_mem[ atinn = l_atinn ] TO <caract>.
    IF sy-subrc = 0.
      valor = <caract>-atflv.
    ENDIF.
  ENDMETHOD.
  METHOD get_valor_caract_text.
    DATA l_atinn TYPE atinn.

    IF caract_int IS INITIAL.
      l_atinn = get_caract_interno( caract ).
    ELSE.
      l_atinn = caract_int.
    ENDIF.

    CLEAR descripcion.
    SELECT atwtb INTO descripcion
      UP TO 1 ROWS
      FROM cawnt JOIN cawn ON  cawn~atinn = cawnt~atinn
                           AND cawn~atzhl = cawnt~atzhl
                           AND cawn~adzhl = cawnt~adzhl
     WHERE cawn~atinn  = l_atinn
       AND cawn~atwrt  = valor
       AND cawnt~spras = spras
        ORDER BY cawnt~atinn cawnt~atzhl cawnt~adzhl cawnt~spras.
    ENDSELECT.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    IF solo_idioma_indicado IS INITIAL.
      SELECT atwtb INTO descripcion
        UP TO 1 ROWS
            FROM cawnt JOIN cawn ON  cawn~atinn = cawnt~atinn
                                 AND cawn~atzhl = cawnt~atzhl
                                 AND cawn~adzhl = cawnt~adzhl
           WHERE cawn~atinn  = l_atinn
             AND cawn~atwrt  = valor
             AND cawnt~spras = 'E'
          ORDER BY cawnt~atinn cawnt~atzhl cawnt~adzhl cawnt~spras.
      ENDSELECT.
      IF sy-subrc <> 0.
        SELECT atwtb INTO descripcion
          UP TO 1 ROWS
                FROM cawnt JOIN cawn ON  cawn~atinn = cawnt~atinn
                                     AND cawn~atzhl = cawnt~atzhl
                                     AND cawn~adzhl = cawnt~adzhl
               WHERE cawn~atinn = l_atinn
                 AND cawn~atwrt = valor
            ORDER BY cawnt~atinn cawnt~atzhl cawnt~adzhl cawnt~spras.
        ENDSELECT.
        IF sy-subrc <> 0.
          descripcion = valor.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_valor_caract_text2key.
    DATA l_atinn TYPE atinn.

    IF caract_int IS INITIAL.
      l_atinn = get_caract_interno( caract ).
    ELSE.
      l_atinn = caract_int.
    ENDIF.

    CLEAR descripcion.
    SELECT atwrt
      INTO descripcion
      UP TO 1 ROWS
      FROM cawnt JOIN cawn ON  cawn~atinn = cawnt~atinn
                           AND cawn~atzhl = cawnt~atzhl
                           AND cawn~adzhl = cawnt~adzhl
     WHERE cawn~atinn  = l_atinn
       AND cawnt~atwtb = valor
       AND cawnt~spras = sy-langu
              ORDER BY cawnt~atinn cawnt~atzhl cawnt~adzhl cawnt~spras.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_valor_clase_multiple.
    DATA o_cl TYPE REF TO zcl_ap_clasificacion.

    o_cl = NEW #(
        classnum  = classnum
        classtype = classtype ).

    o_cl->get_datos( objectkey = objectkey ).

    CASE tipo.
      WHEN 'C'.
        tabla = o_cl->get_char_tabla( caracteristica ).
    ENDCASE.
  ENDMETHOD.
  METHOD get_valor_clase_simple.
    DATA o_cl TYPE REF TO zcl_ap_clasificacion.

    o_cl = NEW #(
        classnum  = classnum
        classtype = classtype ).

    o_cl->get_datos( objectkey = objectkey ).

    CASE tipo.
      WHEN 'C'.
        valor = o_cl->get_char( charact = caracteristica get_clave = get_clave ).
      WHEN 'P'.
        valor = o_cl->get_num( charact = caracteristica ).
      WHEN 'F'.
        valor = o_cl->get_fecha( caracteristica ).
    ENDCASE.
  ENDMETHOD.
  METHOD get_valores_caract.
    DATA l_atinn TYPE atinn.

    l_atinn = get_caract_interno( caract ).

    CLEAR i_cawn.
    SELECT * FROM cawn
      INTO TABLE i_cawn
     WHERE atinn = l_atinn.
  ENDMETHOD.
  METHOD get_valores_caract_text.
    DATA l_atinn TYPE atinn.

    l_atinn = get_caract_interno( caract ).

    CLEAR i_cawnt.
    SELECT * FROM cawnt
      INTO TABLE i_cawnt
     WHERE spras = sy-langu
       AND atinn = l_atinn.
  ENDMETHOD.
  METHOD matchcode_caract.
    DATA: i_cawn  TYPE TABLE OF cawn,
          o_mc    TYPE REF TO zcl_ap_matchcode_z,
          l_cawn  TYPE cawn,
          l_cawnt TYPE cawnt.

    i_cawn = get_valores_caract( caract ).
    IF i_cawn IS INITIAL.
      RETURN.
    ENDIF.

    o_mc = NEW #(
        tabname = 'CAWN' ).

    o_mc->add_field( field = 'ATWRT' selectflag = 'X' ).
    o_mc->add_field( tabname = 'CAWNT' field = 'ATWTB' ).

    LOOP AT i_cawn INTO l_cawn.
      o_mc->add_valor( l_cawn-atwrt ).

      CLEAR l_cawnt.
      SELECT SINGLE atwtb FROM  cawnt
        INTO l_cawnt-atwtb
       WHERE atinn = l_cawn-atinn
         AND atzhl = l_cawn-atzhl
         AND spras = sy-langu
         AND adzhl = l_cawn-adzhl.
      IF sy-subrc <> 0.
        SELECT atwtb FROM  cawnt
          INTO l_cawnt-atwtb
          UP TO 1 ROWS
         WHERE atinn = l_cawn-atinn
           AND atzhl = l_cawn-atzhl
           AND adzhl = l_cawn-adzhl
         ORDER BY PRIMARY KEY.
        ENDSELECT.
      ENDIF.
      o_mc->add_valor( l_cawnt-atwtb ).
    ENDLOOP.

    o_mc->matchcode( CHANGING valor = valor ).
  ENDMETHOD.
  METHOD nombre2num.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input         = nombre
      IMPORTING
        output        = numero.
  ENDMETHOD.
  METHOD set_by_key.
    CALL FUNCTION 'BAPI_OBJCL_GET_OBJECT_OF_KEY'
      EXPORTING
        clobjectkeyin = objectkey
      IMPORTING
        objectname    = objectname
        objecttable   = objecttable
        classtype     = classtype
      TABLES
        return        = i_return.
  ENDMETHOD.
  METHOD set_caract_mem.
    IF hay_caract_mem = 'X'.
      subrc = set_caract_mem_st( i_caract_mem  = i_caract_mem forzar_cambio = forzar_cambio ).
    ENDIF.
  ENDMETHOD.
  METHOD set_caract_mem_st.
    DATA t_sel TYPE STANDARD TABLE OF comw.
    DATA: l_caract_mem TYPE api_vali,
          l_cabn       TYPE cabn,
          l_value      TYPE api_val_i,
          l_num        TYPE i,
          i_values     TYPE TABLE OF api_val_i.

    FIELD-SYMBOLS <fs> TYPE any.

    CALL FUNCTION 'CTMS_DDB_CLOSE'
      TABLES
        exp_selection  = t_sel
      EXCEPTIONS
        inconsistency  = 1
        incomplete     = 2
        verification   = 3
        not_assigned   = 4
        another_object = 5
        other_objects  = 6
        display_mode   = 7
        OTHERS         = 8.
    IF sy-subrc <> 0.
*      message 'Error seleccioanndo datos de memoria' type 'S'.
    ENDIF.

    LOOP AT i_caract_mem INTO l_caract_mem.
      ASSIGN t_sel[ atinn = l_caract_mem-atinn ] TO FIELD-SYMBOL(<sel>).
      IF sy-subrc = 0.
        IF NOT <sel>-ataut IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.

      SELECT atfor FROM cabn
        INTO l_cabn-atfor
        UP TO 1 ROWS
       WHERE atinn = l_caract_mem-atinn
      ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING l_caract_mem TO l_value.
        IF l_cabn-atfor = 'NUM'.
          l_num = l_caract_mem-atflv.
          l_value-atwtb = l_num.
          CONDENSE l_value-atwtb NO-GAPS.
        ELSE.
          l_value-atwtb = l_caract_mem-atwrt.
        ENDIF.
        APPEND l_value TO i_values.
      ENDIF.
    ENDLOOP.

    IF forzar_cambio = 'X'.
      ASSIGN ('(SAPLCTMS)F_MODE') TO <fs>.
      IF sy-subrc = 0.
        IF <fs> IS INITIAL.
          <fs> = 'S'.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'CTMS_DDB_SET_VALUE'
* EXPORTING
*   INSTANCE                            =
*   MESSAGE                             = ' '
*   UDEF_INSTANCE                       =
      TABLES
        imp_values                    = i_values
*     EXP_VALUES_ERROR              =
      EXCEPTIONS
        currency_check                = 1
        date_check                    = 2
        format_check                  = 3
        illegal_internal_baseunit     = 4
        interval_check                = 5
        pattern_check                 = 6
        time_check                    = 7
        unit_check                    = 8
        value_not_found               = 9
        no_valid_dimension            = 10
        interval_not_allowed          = 11
        display_mode                  = 12
        characteristic_not_found      = 13
        value_not_possible            = 14
        characteristic_enqueue        = 15
        objectcharacteristic          = 16
        only_one_value_allowed        = 17
        characteristic_not_selectable = 18
        input_to_long                 = 19
        value_contradiction           = 20
        OTHERS                        = 21.
    subrc = sy-subrc.

*  CALL FUNCTION 'CTMS_DDB_SET_VALUE_INTERNAL'
*    TABLES
*      imp_values                          = i_caract_mem
**     EXP_VALUES_ERROR                    =
*   EXCEPTIONS
*     currency_check                      = 1
*     date_check                          = 2
*     format_check                        = 3
*     illegal_internal_baseunit           = 4
*     interval_check                      = 5
*     pattern_check                       = 6
*     time_check                          = 7
*     unit_check                          = 8
*     value_not_found                     = 9
*     no_valid_dimension                  = 10
*     interval_not_allowed                = 11
*     display_mode                        = 12
*     characteristic_not_found            = 13
*     value_not_possible                  = 14
*     characteristic_enqueue              = 15
*     objectcharacteristic                = 16
*     only_one_value_allowed              = 17
*     characteristic_not_selectable       = 18
*     input_to_long                       = 19
*     value_contradiction                 = 20
*     OTHERS                              = 21.
*
*  subrc = sy-subrc.
  ENDMETHOD.
  METHOD set_char.
    " TODO: parameter CHARACT_DESCR is only used in commented-out code (ABAP cleaner)

    IF delete = 'X'.
      IF NOT value_char IS INITIAL.
        DELETE i_car_char WHERE     charact    = charact
                                AND value_char = value_char.
      ELSE.
        DELETE i_car_char WHERE     charact       = charact
                                AND value_neutral = value_char.
      ENDIF.
    ELSE.
      READ TABLE i_car_char INTO car_char WITH KEY charact = charact.
      IF sy-subrc = 0 AND add_new = ''.
        car_char-value_char = value_char.
        IF value_neutral IS SUPPLIED.
          car_char-value_neutral = value_neutral.
        ELSE.
          car_char-value_neutral = value_char.
        ENDIF.
        MODIFY i_car_char FROM car_char INDEX sy-tabix.
      ELSE.
        CLEAR car_char.
        car_char-charact    = charact.
*    IF charact_descr IS INITIAL.
*      car_char-charact_descr  = charact.
*    ELSE.
*      car_char-charact_descr  = charact_descr.
*    ENDIF.
        car_char-value_char = value_char.
        IF value_neutral IS SUPPLIED.
          car_char-value_neutral = value_neutral.
        ELSE.
          car_char-value_neutral = value_char.
        ENDIF.
        APPEND car_char TO i_car_char.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD set_curr.
    READ TABLE i_car_curr INTO car_curr WITH KEY charact = charact.
    IF sy-subrc = 0.
      car_curr-value_from = value_from.
      MODIFY i_car_curr FROM car_curr INDEX sy-tabix.
    ELSE.
      CLEAR car_curr.
      car_curr-charact = charact.
      IF charact_descr IS INITIAL.
        car_curr-charact_descr = charact.
      ELSE.
        car_curr-charact_descr = charact_descr.
      ENDIF.
      car_curr-value_from = value_from.
      APPEND car_curr TO i_car_curr.
    ENDIF.
  ENDMETHOD.
  METHOD set_datos.
    DATA l_objectkey TYPE bapi1003_key-object.

    IF objectkey IS INITIAL.
      l_objectkey = me->objectkey.
    ELSE.
      l_objectkey = objectkey.
    ENDIF.

    SET UPDATE TASK LOCAL.
    CALL FUNCTION 'BAPI_OBJCL_CHANGE'
      EXPORTING
        objectkey              = l_objectkey
        objecttable            = objecttable
        classnum               = classnum
        classtype              = classtype
*   KEYDATE                = SY-DATUM
*   UNVALUATED_CHARS       = ' '
*   LANGUAGE               = SY-LANGU
* IMPORTING
*   STATUS                 =
*   STANDARDCLASS          =
      TABLES
        allocvaluesnumnew         = i_car_num
        allocvaluescharnew        = i_car_char
        allocvaluescurrnew        = i_car_curr
        return                 = i_return.

    IF commit = 'X'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      IF dequeue_all = 'X'.
        CALL FUNCTION 'DEQUEUE_ALL'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD set_fecha.
    DATA: l_fechac TYPE c LENGTH 8,
          l_atflv  TYPE atflv.

    l_fechac = fecha.
    l_atflv = l_fechac.
    set_num( charact = charact value_from = l_atflv charact_descr = charact_descr ).
  ENDMETHOD.
  METHOD set_num.
    READ TABLE i_car_num INTO car_num WITH KEY charact = charact.
    IF sy-subrc = 0.
      car_num-value_from = value_from.
      car_num-value_to   = value_to.
      MODIFY i_car_num FROM car_num INDEX sy-tabix.
    ELSE.
      CLEAR car_num.
      car_num-charact = charact.
      IF charact_descr IS INITIAL.
        car_num-charact_descr = charact.
      ELSE.
        car_num-charact_descr = charact_descr.
      ENDIF.
      car_num-value_from = value_from.
      car_num-value_to   = value_to.
      APPEND car_num TO i_car_num.
    ENDIF.
  ENDMETHOD.
  METHOD set_valor_caract_mem.
    DATA l_atinn TYPE atinn.

    FIELD-SYMBOLS <caract> TYPE api_vali.

    IF atinn IS INITIAL.
      l_atinn = get_caract_interno( caract ).
    ELSE.
      l_atinn = atinn.
    ENDIF.

    IF multiple IS INITIAL.
      ASSIGN i_caract_mem[ atinn = l_atinn ] TO <caract>.
      IF sy-subrc = 0.
        <caract>-atwrt = valor.
      ELSE.
        CLEAR caract_mem.
        caract_mem-atinn = l_atinn.
        caract_mem-atwrt = valor.
        APPEND caract_mem TO i_caract_mem.
      ENDIF.
    ELSE.
      ASSIGN i_caract_mem[ atinn = l_atinn
                           atwrt = valor ] TO <caract>.
      IF sy-subrc = 0.
        <caract>-atwrt = valor.
      ELSE.
        CLEAR caract_mem.
        caract_mem-atinn = l_atinn.
        caract_mem-atwrt = valor.
        APPEND caract_mem TO i_caract_mem.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD set_valor_caract_mem_date.
    DATA: l_atinn TYPE atinn,
          l_atnam TYPE atnam,
          l_fecha TYPE d,
          l_atwrt TYPE atwrt.

    FIELD-SYMBOLS <caract> TYPE api_vali.
    FIELD-SYMBOLS: <fs>    TYPE table,
                   <line>  TYPE any,
                   <atinn> TYPE any,
                   <atglo> TYPE any.

    IF atinn IS INITIAL.
      l_atinn = get_caract_interno( caract ).
      l_atnam = caract.
    ELSE.
      l_atinn = atinn.
      l_atnam = get_caract_externo( l_atinn ).
    ENDIF.

    WRITE valor TO l_fecha.
    l_atwrt = valor.
    ASSIGN i_caract_mem[ atinn = l_atinn ] TO <caract>.
    IF sy-subrc = 0.
      <caract>-atwrt = l_fecha.
      <caract>-atwtb = l_fecha.
      CALL FUNCTION 'CTCV_CONVERT_DATE_TO_FLOAT'
        EXPORTING
          date  = l_atwrt
        IMPORTING
          float = <caract>-atflv.
    ELSE.
      CLEAR caract_mem.
      caract_mem-atinn = l_atinn.
      caract_mem-atnam = l_atnam.
      caract_mem-atwrt = l_fecha.
      caract_mem-atwtb = l_fecha.
      CALL FUNCTION 'CTCV_CONVERT_DATE_TO_FLOAT'
        EXPORTING
          date  = l_atwrt
        IMPORTING
          float = caract_mem-atflv.
      APPEND caract_mem TO i_caract_mem.
    ENDIF.

    IF forzar_cambio = 'X'.
      DATA l_data TYPE REF TO data.

      ASSIGN ('(SAPLCTMS)MI[]') TO <fs>. "#EC *
      IF sy-subrc = 0.
        IF NOT <fs> IS INITIAL.
          LOOP AT <fs> ASSIGNING <line>.
            ASSIGN COMPONENT 'ATINN' OF STRUCTURE <line> TO <atinn>.
            IF sy-subrc = 0.
              IF <atinn> = l_atinn.
                ASSIGN COMPONENT 'ATGLO' OF STRUCTURE <line> TO <atglo>.
                IF sy-subrc = 0.
                  <atglo> = 'X'.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD set_valor_caract_mem_num.
    DATA l_atinn TYPE atinn.

    FIELD-SYMBOLS <caract> TYPE api_vali.

    IF atinn IS INITIAL.
      l_atinn = get_caract_interno( caract ).
    ELSE.
      l_atinn = atinn.
    ENDIF.

    ASSIGN i_caract_mem[ atinn = l_atinn ] TO <caract>.
    IF sy-subrc = 0.
      <caract>-atflv = valor.
    ELSE.
      CLEAR caract_mem.
      caract_mem-atinn = l_atinn.
      caract_mem-atflv = valor.
      APPEND caract_mem TO i_caract_mem.
    ENDIF.
  ENDMETHOD.
  METHOD set_valor_clase_simple.
    DATA: o_cl          TYPE REF TO zcl_ap_clasificacion,
          l_valor_char  TYPE atwrt,
          l_cambio      TYPE c LENGTH 1,
          l_valor_fecha TYPE atwrt.

    o_cl = NEW #(
        classnum    = classnum
        classtype   = classtype
        objecttable = tabla ).

    o_cl->get_datos( objectkey = objectkey ).

    CASE tipo.
      WHEN 'C'.
        l_valor_char = o_cl->get_char( caracteristica ).
        IF l_valor_char <> valor.
          l_cambio = 'X'.
          o_cl->set_char( charact = caracteristica value_char = valor ).
        ENDIF.

      WHEN 'N'.
        l_valor_char = o_cl->get_char( caracteristica ).
        IF l_valor_char <> valor.
          l_cambio = 'X'.
          o_cl->set_num( charact = caracteristica value_from = valor value_to = valor_hasta ).
        ENDIF.

      WHEN 'F'.
        l_valor_fecha = o_cl->get_fecha( caracteristica ).
        IF l_valor_fecha <> valor.
          l_cambio = 'X'.
          o_cl->set_fecha( charact = caracteristica fecha = valor ).
        ENDIF.
    ENDCASE.
    IF l_cambio = 'X'.
      o_cl->set_datos( objectkey = objectkey commit = commit ).

      READ TABLE o_cl->i_return INTO o_cl->return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        mensaje = o_cl->return-message.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD tiene_clasificacion.
    DATA: l_klah  TYPE klah,
          l_objek TYPE objnum,
          l_inob  TYPE inob,
          l_kssk  TYPE kssk.

    SELECT clint FROM klah
      INTO l_klah-clint
      UP TO 1 ROWS
     WHERE klart = classtype
       AND class = classnum
     ORDER BY PRIMARY KEY.
    ENDSELECT.

    IF tabla IS INITIAL.
      l_objek = objectkey.
    ELSE.
      SELECT cuobj FROM inob
        INTO l_inob-cuobj
        UP TO 1 ROWS
       WHERE klart = classtype
         AND obtab = tabla
         AND objek = objectkey
       ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc <> 0.
        l_objek = objectkey.
      ELSE.
        l_objek = l_inob-cuobj.
      ENDIF.
    ENDIF.

    IF classnum IS INITIAL.
      SELECT SINGLE objek FROM  kssk
        INTO l_kssk-objek
       WHERE objek = objectkey
         AND mafid = 'O'
         AND klart = classtype.
    ELSE.
      SELECT objek FROM  kssk
        INTO l_kssk-objek
        UP TO 1 ROWS
       WHERE objek = l_objek
         AND mafid = 'O'
         AND klart = classtype
         AND clint = l_klah-clint
       ORDER BY PRIMARY KEY.
      ENDSELECT.
    ENDIF.

    IF NOT l_kssk IS INITIAL.
      si = 'X'.
    ENDIF.
  ENDMETHOD.
