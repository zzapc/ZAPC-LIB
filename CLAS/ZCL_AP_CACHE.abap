CLASS zcl_ap_cache DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_cache_mem,
        tabla   TYPE tabname,
        clave   TYPE string,
        clave2  TYPE string,
        clave3  TYPE string,
        clave4  TYPE string,
        clave5  TYPE string,
        valor   TYPE string,
        valor2  TYPE string,
        valor3  TYPE string,
        xstring TYPE xstring,
      END OF t_cache_mem.
    TYPES tt_cache_mem TYPE SORTED TABLE OF t_cache_mem WITH NON-UNIQUE KEY tabla clave clave2 clave3 clave4 clave5.

    DATA i_cache_mem TYPE tt_cache_mem.
    DATA enc         TYPE abap_bool.
    DATA inhabilitar TYPE abap_bool.

    CLASS-METHODS set_cache
      IMPORTING clave           TYPE any
                subclave        TYPE any                           OPTIONAL
                fecha           TYPE dats                          DEFAULT sy-datum
                seleccion_rango TYPE table                         OPTIONAL
                valores         TYPE table                         OPTIONAL
                !report         TYPE any                           DEFAULT sy-cprog
                !uname          TYPE any                           DEFAULT sy-uname
                hora            TYPE any                           DEFAULT sy-uzeit
                ok              TYPE any                           DEFAULT ''
                variables       TYPE any                           OPTIONAL
                !string         TYPE any                           OPTIONAL
                valores2        TYPE table                         OPTIONAL
                !commit         TYPE any                           DEFAULT ''
                objeto          TYPE REF TO if_serializable_object OPTIONAL
                aux1            TYPE any                           DEFAULT ''
                aux2            TYPE any                           DEFAULT ''
                aux3            TYPE any                           DEFAULT ''
                variables2      TYPE any                           OPTIONAL
                max_duracion    TYPE i                             DEFAULT 0
                no_dup          TYPE abap_bool                     DEFAULT ''
                json            TYPE abap_bool                     DEFAULT ''
                valores3        TYPE table                         OPTIONAL
      EXPORTING !message        TYPE bapi_msg.

    CLASS-METHODS get_cache
      IMPORTING clave             TYPE any
                subclave          TYPE any DEFAULT ''
                fecha             TYPE dats DEFAULT sy-datum
                seleccion_rango   TYPE table OPTIONAL
                max_duracion      TYPE i DEFAULT 1
                !report           TYPE sy-cprog DEFAULT sy-cprog
                visualizar_xml    TYPE abap_bool DEFAULT ''
                campos_visualizar TYPE string DEFAULT ''
      EXPORTING aux1              TYPE any
                aux2              TYPE any
                aux3              TYPE any
                objeto            TYPE REF TO if_serializable_object
                valores           TYPE table
                existe            TYPE abap_bool
                variables         TYPE any
                !string           TYPE any
                valores2          TYPE table
                variables2        TYPE any
                valores3          TYPE table
                !message          TYPE bapi_msg.

    CLASS-METHODS f4_subclave
      IMPORTING !report         TYPE zcache-report DEFAULT sy-cprog
                clave           TYPE zcache-clave
      RETURNING VALUE(subclave) TYPE zcache-subclave.

    CLASS-METHODS borrar
      IMPORTING clave          TYPE any       OPTIONAL
                subclave       TYPE any       OPTIONAL
                fecha          TYPE any       OPTIONAL
                !report        TYPE any       DEFAULT sy-cprog
                !uname         TYPE any       OPTIONAL
                hora           TYPE any       OPTIONAL
                solo_caducados TYPE abap_bool DEFAULT ''.

    METHODS set_cache_mem
      IMPORTING tabla   TYPE any
                clave   TYPE any
                clave2  TYPE any DEFAULT ''
                clave3  TYPE any DEFAULT ''
                clave4  TYPE any DEFAULT ''
                clave5  TYPE any DEFAULT ''
                valor   TYPE any OPTIONAL
                valor2  TYPE any DEFAULT ''
                valor3  TYPE any DEFAULT ''
                xstring TYPE any OPTIONAL.

    METHODS get_cache_mem
      IMPORTING tabla         TYPE any
                clave         TYPE any
                clave2        TYPE any       DEFAULT ''
                clave3        TYPE any       DEFAULT ''
                clave4        TYPE any       DEFAULT ''
                clave5        TYPE any       DEFAULT ''
                buscar_auto   TYPE abap_bool DEFAULT ''
                nombre_campo  TYPE any       DEFAULT ''
                nombre_campo2 TYPE any       DEFAULT ''
                idioma        TYPE spras     DEFAULT sy-langu
                campo_clave   TYPE any       DEFAULT ''
                campo_idioma  TYPE any       DEFAULT ''
      EXPORTING xstring       TYPE any
                valor         TYPE any
                valor2        TYPE any
                valor3        TYPE any
                encontrado    TYPE abap_bool.

    METHODS get
      IMPORTING tabla         TYPE any
                clave         TYPE any
                clave2        TYPE any   DEFAULT ''
                clave3        TYPE any   DEFAULT ''
                clave4        TYPE any   DEFAULT ''
                clave5        TYPE any   DEFAULT ''
                nombre_campo  TYPE any   DEFAULT ''
                nombre_campo2 TYPE any   DEFAULT ''
                idioma        TYPE spras DEFAULT sy-langu
                campo_clave   TYPE any   DEFAULT ''
                campo_idioma  TYPE any   DEFAULT ''
      RETURNING VALUE(valor)  TYPE string.

    CLASS-METHODS get_shmm
      IMPORTING !update     TYPE abap_bool DEFAULT ''
      EXPORTING !message    TYPE bapi_msg
      RETURNING VALUE(area) TYPE REF TO zcl_ap_shma_area.

    METHODS get_shma
      IMPORTING cprog          TYPE any       DEFAULT sy-cprog
                users          TYPE any       DEFAULT ''
                tcode          TYPE any       DEFAULT ''
                solo_en_inicio TYPE abap_bool DEFAULT 'X'.

    METHODS set_shma
      IMPORTING cprog    TYPE any DEFAULT sy-cprog
                users    TYPE any DEFAULT ''
                tcode    TYPE any DEFAULT ''
      EXPORTING !message TYPE bapi_msg.

    METHODS get_mat
      IMPORTING matnr          TYPE any
                cantidad       TYPE any
                unidad_origen  TYPE any
                unidad_destino TYPE any OPTIONAL
                !conversion    TYPE any DEFAULT 'CUM'
      RETURNING VALUE(valor)   TYPE string.

    CLASS-METHODS visualizar
      IMPORTING !cache              TYPE zcache
                campo               TYPE fieldname
                tipo_visualizacion  TYPE char1 DEFAULT 'P'
      RETURNING VALUE(visualizable) TYPE abap_bool.

  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_CACHE definition
class ZCL_AP_CACHE implementation.
  METHOD borrar.
    DEFINE add_rango.
      DATA o_&1 TYPE REF TO zcl_ap_rango.
      o_&1 = NEW #( ).
      IF NOT &1 IS INITIAL.
        o_&1->set_eq( &1 ).
      ENDIF.
    END-OF-DEFINITION.

    DEFINE add_rango_vacio.
      DATA o_&1 TYPE REF TO zcl_ap_rango.
      o_&1 = NEW #( ).
      IF &1 = '_VACIO_'.
        o_&1->set_eq( '' ).
      ELSEIF NOT &1 IS INITIAL.
        o_&1->set_eq( &1 ).
      ENDIF.
    END-OF-DEFINITION.

    add_rango: clave, uname, fecha, hora.
    add_rango_vacio: subclave, report.

    IF solo_caducados IS INITIAL.
      DELETE FROM zcache
       WHERE clave    IN o_clave->rango
         AND subclave IN o_subclave->rango
         AND report   IN o_report->rango
         AND fecha    IN o_fecha->rango
         AND ernam    IN o_uname->rango
         AND hora     IN o_hora->rango.
    ELSE.
      DELETE FROM zcache
       WHERE clave        IN o_clave->rango
         AND subclave     IN o_subclave->rango
         AND report       IN o_report->rango
         AND fecha        IN o_fecha->rango
         AND ernam        IN o_uname->rango
         AND hora         IN o_hora->rango
         AND fecha_limite <> '00000000'
         AND fecha_limite  < sy-datum.
    ENDIF.
  ENDMETHOD.
  METHOD f4_subclave.
    DATA: o_popup TYPE REF TO zcl_ap_matchcode_z,
          zcache  TYPE zcache.

    o_popup = NEW #( tabname = 'ZCACHE' ).

    o_popup->add_field( field = 'SUBCLAVE' selectflag = 'X' ).
    o_popup->add_field( 'FECHA,HORA,ERNAM' ).

    SELECT subclave fecha hora ernam FROM zcache        "#EC CI_GENBUFF
      INTO CORRESPONDING FIELDS OF zcache
     WHERE report = report
       AND clave  = clave.
      o_popup->add_valor( zcache-subclave ).
      o_popup->add_valor( zcache-fecha ).
      o_popup->add_valor( zcache-hora ).
      o_popup->add_valor( zcache-ernam ).
    ENDSELECT.

    o_popup->matchcode( EXPORTING field = 'SUBCLAVE'
                        CHANGING  valor = subclave ).
  ENDMETHOD.
  METHOD get.
    get_cache_mem( EXPORTING tabla         = tabla
                             clave         = clave
                             clave2        = clave2
                             clave3        = clave3
                             clave4        = clave4
                             clave5        = clave5
                             buscar_auto   = 'X'
                             nombre_campo  = nombre_campo
                             nombre_campo2 = nombre_campo2
                             idioma        = idioma
                             campo_clave   = campo_clave
                             campo_idioma  = campo_idioma
                   IMPORTING valor         = valor ).
  ENDMETHOD.
  METHOD get_cache.
    DATA: l_fecha       TYPE d,
          l_subclave    TYPE zcache-subclave,
          l_string      TYPE string,
          i_cache       TYPE TABLE OF zcache,
          l_seleccion   TYPE xstring,
          cache         TYPE zcache,
          lx_valores    TYPE xstring,
          lx_valores2   TYPE xstring,
          lx_valores3   TYPE xstring,
          lx_variables  TYPE xstring,
          lx_variables2 TYPE xstring.

    l_fecha = fecha - max_duracion.

    IF zcl_ap_string=>es_numero( subclave ) = 'X'.
      l_subclave = subclave.
    ELSEIF subclave = '_VACIO_'.
      CLEAR l_subclave.
    ELSE.
      l_subclave = subclave.
    ENDIF.

    LOOP AT seleccion_rango INTO l_string.
      CONCATENATE l_subclave l_string INTO l_subclave.
      CONDENSE l_subclave NO-GAPS.
    ENDLOOP.

    IF l_subclave IS INITIAL AND subclave <> '_VACIO_'.
      IF max_duracion <> 99999.
        DELETE FROM zcache
         WHERE report  = report
           AND clave   = clave
           AND fecha  <= l_fecha.
      ENDIF.

      SELECT * FROM zcache                              "#EC CI_GENBUFF
        INTO TABLE i_cache
       WHERE report = report
         AND clave  = clave.
      IF sy-subrc = 0. "#EC *
        existe = 'X'.
      ENDIF.
    ELSE.
      IF max_duracion <> 99999.
        DELETE FROM zcache
         WHERE report    = report
           AND clave     = clave
           AND subclave  = l_subclave
           AND fecha    <= l_fecha.
      ENDIF.

      SELECT * FROM zcache
        INTO TABLE i_cache
       WHERE report   = report
         AND clave    = clave
         AND subclave = l_subclave.
      IF sy-subrc = 0.
        existe = 'X'.
      ENDIF.
    ENDIF.

    IF existe IS INITIAL.
      RETURN.
    ENDIF.

    IF i_cache IS INITIAL.
      RETURN.
    ENDIF.

    IF NOT seleccion_rango IS INITIAL.
      l_seleccion = zcl_ap_string=>transform_object_to_rawstring( tabla = seleccion_rango json = cache-json ).
    ENDIF.
    LOOP AT i_cache INTO cache.
      IF l_seleccion = cache-seleccion.
        IF NOT cache-valores IS INITIAL.
          IF visualizar_xml = 'X' AND NOT zcl_ap_lista=>es_elemento( lista = campos_visualizar elemento = 'VALORES' ).

          ELSEIF valores IS SUPPLIED.
            zcl_ap_string=>transform_rawstring_to_object( EXPORTING  xstring               = cache-valores
                                                                     get_tabla             = 'X'
                                                                     json                  = cache-json
                                                                     visualizar_xml        = visualizar_xml
                                                          IMPORTING  xstring_decompressed  = lx_valores
                                                          CHANGING   tabla                 = valores
                                                          EXCEPTIONS transformation_failed = 1
                                                                     OTHERS                = 2 ).
            IF sy-subrc <> 0.
              message = 'Error recuperando objeto VALORES'.
            ENDIF.
          ENDIF.
        ENDIF.

        IF NOT cache-valores2 IS INITIAL.
          IF visualizar_xml = 'X' AND NOT zcl_ap_lista=>es_elemento( lista = campos_visualizar elemento = 'VALORES2' ).

          ELSEIF valores2 IS SUPPLIED.
            zcl_ap_string=>transform_rawstring_to_object( EXPORTING  xstring               = cache-valores2
                                                                     get_tabla             = 'X'
                                                                     json                  = cache-json
                                                                     visualizar_xml        = visualizar_xml
                                                          IMPORTING  xstring_decompressed  = lx_valores2
                                                          CHANGING   tabla                 = valores2
                                                          EXCEPTIONS transformation_failed = 1
                                                                     OTHERS                = 2 ).
            IF sy-subrc <> 0.
              message = 'Error recuperando objeto VALORES2'.
            ENDIF.
          ENDIF.
        ENDIF.

        IF NOT cache-valores3 IS INITIAL.
          IF visualizar_xml = 'X' AND NOT zcl_ap_lista=>es_elemento( lista = campos_visualizar elemento = 'VALORES3' ).

          ELSEIF valores3 IS SUPPLIED.
            zcl_ap_string=>transform_rawstring_to_object( EXPORTING  xstring               = cache-valores3
                                                                     get_tabla             = 'X'
                                                                     json                  = cache-json
                                                                     visualizar_xml        = visualizar_xml
                                                          IMPORTING  xstring_decompressed  = lx_valores3
                                                          CHANGING   tabla                 = valores3
                                                          EXCEPTIONS transformation_failed = 1
                                                                     OTHERS                = 2 ).
            IF sy-subrc <> 0.
              message = 'Error recuperando objeto VALORES3'.
            ENDIF.
          ENDIF.
        ENDIF.

        IF NOT cache-variables IS INITIAL.
          IF visualizar_xml = 'X' AND NOT zcl_ap_lista=>es_elemento( lista = campos_visualizar elemento = 'VARIABLES' ).

          ELSEIF variables IS SUPPLIED.
            zcl_ap_string=>transform_rawstring_to_object( EXPORTING  xstring               = cache-variables
                                                                     get_variable          = 'X'
                                                                     json                  = cache-json
                                                                     visualizar_xml        = visualizar_xml
                                                          IMPORTING  xstring_decompressed  = lx_variables
                                                          CHANGING   variable              = variables
                                                          EXCEPTIONS transformation_failed = 1
                                                                     OTHERS                = 2 ).
            IF sy-subrc <> 0.
              message = 'Error recuperando objeto VARIABLES'.
            ENDIF.
          ENDIF.
        ENDIF.

        IF NOT cache-variables2 IS INITIAL.
          IF visualizar_xml = 'X' AND NOT zcl_ap_lista=>es_elemento( lista = campos_visualizar elemento = 'VARIABLES2' ).

          ELSEIF variables2 IS SUPPLIED.
            zcl_ap_string=>transform_rawstring_to_object( EXPORTING  xstring               = cache-variables2
                                                                     get_variable          = 'X'
                                                                     json                  = cache-json
                                                                     visualizar_xml        = visualizar_xml
                                                          IMPORTING  xstring_decompressed  = lx_variables2
                                                          CHANGING   variable              = variables2
                                                          EXCEPTIONS transformation_failed = 1
                                                                     OTHERS                = 2 ).
            IF sy-subrc <> 0.
              message = 'Error recuperando objeto VARIABLES2'.
            ENDIF.
          ENDIF.
        ENDIF.

        IF NOT cache-objeto IS INITIAL AND objeto IS SUPPLIED.
          zcl_ap_string=>transform_rawstring_to_object( EXPORTING  xstring               = cache-objeto
                                                                   get_variable          = 'X'
                                                                   json                  = cache-json
                                                                   visualizar_xml        = visualizar_xml
                                                        CHANGING   objeto                = objeto
                                                        EXCEPTIONS transformation_failed = 1
                                                                   OTHERS                = 2 ).
          IF sy-subrc <> 0.
            message = 'Error recuperando OBJETO'.
          ENDIF.
        ENDIF.

        string = cache-string.
        aux1 = cache-aux1.
        aux2 = cache-aux2.
        aux3 = cache-aux3.

        EXIT.                                           "#EC CI_NOORDER
      ENDIF.
    ENDLOOP.

    IF visualizar_xml = 'Y'.
      IF NOT valores IS INITIAL AND valores IS SUPPLIED.
        DATA(l_output) = 'X'.
        cl_demo_output=>begin_section( 'Valores' ).
        cl_demo_output=>write_data( valores ).
        cl_demo_output=>end_section( ).
      ELSEIF NOT lx_valores IS INITIAL.
        l_output = 'X'.
        cl_demo_output=>begin_section( 'Valores XML' ).
        cl_demo_output=>write_xml( lx_valores ).
        cl_demo_output=>end_section( ).
      ENDIF.
      IF NOT valores2 IS INITIAL AND valores2 IS SUPPLIED.
        l_output = 'X'.
        cl_demo_output=>begin_section( 'Valores2' ).
        cl_demo_output=>write_data( valores2 ).
        cl_demo_output=>end_section( ).
      ELSEIF NOT lx_valores2 IS INITIAL.
        l_output = 'X'.
        cl_demo_output=>begin_section( 'Valores 2 XML' ).
        cl_demo_output=>write_xml( lx_valores2 ).
        cl_demo_output=>end_section( ).
      ENDIF.
      IF NOT valores3 IS INITIAL AND valores3 IS SUPPLIED.
        l_output = 'X'.
        cl_demo_output=>begin_section( 'Valores3' ).
        cl_demo_output=>write_data( valores3 ).
        cl_demo_output=>end_section( ).
      ELSEIF NOT lx_valores3 IS INITIAL.
        l_output = 'X'.
        cl_demo_output=>begin_section( 'Valores 3 XML' ).
        cl_demo_output=>write_xml( lx_valores3 ).
        cl_demo_output=>end_section( ).
      ENDIF.
      IF NOT variables IS INITIAL.
        l_output = 'X'.
        cl_demo_output=>begin_section( 'Variables' ).
        cl_demo_output=>write_data( variables ).
        cl_demo_output=>end_section( ).
      ELSEIF NOT lx_variables IS INITIAL.
        l_output = 'X'.
        cl_demo_output=>begin_section( 'Variables XML' ).
        cl_demo_output=>write_xml( lx_variables ).
        cl_demo_output=>end_section( ).
      ENDIF.
      IF NOT variables2 IS INITIAL.
        l_output = 'X'.
        cl_demo_output=>begin_section( 'Variables2' ).
        cl_demo_output=>write_data( variables2 ).
        cl_demo_output=>end_section( ).
      ELSEIF NOT lx_variables2 IS INITIAL.
        l_output = 'X'.
        cl_demo_output=>begin_section( 'Variables2 XML' ).
        cl_demo_output=>write_xml( lx_variables2 ).
        cl_demo_output=>end_section( ).
      ENDIF.
      IF l_output = 'X'.
        cl_demo_output=>display( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_cache_mem.
    DATA: l_clave3 TYPE string,
          l_cache  TYPE t_cache_mem,
          l_tabla  TYPE c LENGTH 20,
          l_aux1   TYPE c LENGTH 40,
          l_aux2   TYPE c LENGTH 20,
          l_endda  TYPE endda,
          l_fecha  TYPE dats,
          l_matnr  TYPE matnr,
          l_m1     TYPE meins,
          l_m2     TYPE meins.
    DATA: i_campos TYPE ddfields,
          l_campo  TYPE dfies,
          l_where  TYPE string.

    IF NOT inhabilitar IS INITIAL.
      RETURN.
    ENDIF.

    l_clave3 = clave3.

    READ TABLE i_cache_mem INTO l_cache WITH KEY tabla = tabla clave = clave clave2 = clave2 clave3 = l_clave3 clave4 = clave4 clave5 = clave5.
    IF sy-subrc = 0.
      encontrado = 'X'.

      l_tabla = tabla.
      IF l_tabla(3) = 'CUM'.
        IF l_cache-valor = '?'.
          valor = campo_clave.
        ELSE.
          valor = l_cache-valor * campo_clave / 10000.
        ENDIF.
      ELSE.
        valor = l_cache-valor.
      ENDIF.

      valor2 = l_cache-valor2.
      valor3 = l_cache-valor3.

      IF NOT l_cache-xstring IS INITIAL.
        cl_enh_utilities_xstring=>get_data( EXPORTING pi_xstring = l_cache-xstring IMPORTING pe_output = xstring ).
      ENDIF.
    ELSE.
      CLEAR encontrado.

      IF buscar_auto = 'X'.
        CLEAR l_cache.
        l_cache-tabla  = tabla.
        l_cache-clave  = clave.
        l_cache-clave2 = clave2.
        l_cache-clave3 = clave3.
        l_cache-clave4 = clave4.
        l_cache-clave5 = clave5.
        CASE tabla.
          WHEN 'MATNR' OR 'MAKT'.
            IF NOT clave2 IS INITIAL.
              l_cache-valor = zcl_ap_material=>get_descripcion( matnr = clave spras = clave2 ).
            ELSE.
              l_cache-valor = zcl_ap_material=>get_descripcion( matnr = clave spras = idioma ).
            ENDIF.
          WHEN 'MEINS'.
            SELECT SINGLE meins FROM mara INTO l_cache-valor WHERE matnr = clave.
          WHEN 'MTART'.
            SELECT SINGLE mtart FROM mara INTO l_cache-valor WHERE matnr = clave.
          WHEN 'KNA1'.
            SELECT SINGLE name1 FROM kna1 INTO l_cache-valor WHERE kunnr = clave.
          WHEN 'LFA1'.
            SELECT SINGLE name1 FROM lfa1 INTO l_cache-valor WHERE lifnr = clave.
          WHEN 'T024'.
            SELECT SINGLE eknam FROM t024 INTO l_cache-valor WHERE ekgrp = clave.
          WHEN 'T024D'.
            SELECT SINGLE dsnam FROM t024d INTO l_cache-valor WHERE werks = clave AND dispo = clave2.
          WHEN 'TC24'.
            SELECT SINGLE ktext FROM tc24 INTO l_cache-valor WHERE werks = clave AND veran = clave2.
          WHEN 'T005T' OR 'LANDX' or 'PAIS'.
            SELECT SINGLE landx FROM t005t INTO l_cache-valor WHERE spras = idioma AND land1 = clave.
          WHEN 'T005U' OR 'PROVINCIA'.
            SELECT SINGLE bezei FROM t005u INTO l_cache-valor WHERE spras = idioma AND land1 = clave and bland = clave2.
          WHEN 'T512T' OR 'LGART'.
            IF clave2 IS INITIAL. l_aux1 = idioma. ELSE. l_aux1 = clave2. ENDIF.
            IF clave3 IS INITIAL. l_aux2 = '04'. ELSE. l_aux2 = clave3. ENDIF.
            SELECT SINGLE lgtxt FROM  t512t INTO l_cache-valor WHERE sprsl = l_aux1 AND molga = l_aux2 AND lgart = clave.
          WHEN 'ENAME'.
            IF clave2 IS INITIAL.
              SELECT SINGLE ename FROM pa0001 INTO l_cache-valor WHERE pernr = clave AND endda >= sy-datum AND begda <= sy-datum.
            ELSE.
              SELECT SINGLE ename FROM pa0001 INTO l_cache-valor WHERE pernr = clave AND endda >= clave2 AND begda <= clave2.
            ENDIF.
          WHEN 'T554T' OR 'AWART'.
            IF clave2 IS INITIAL. l_aux1 = idioma. ELSE. l_aux1 = clave2. ENDIF.
            IF clave3 IS INITIAL. l_aux2 = '04'. ELSE. l_aux2 = clave3. ENDIF.
            SELECT SINGLE atext FROM t554t INTO l_cache-valor WHERE sprsl = l_aux1 AND moabw = l_aux2 AND awart = clave.
          WHEN 'NIF'.
            IF clave2 IS INITIAL.
              SELECT perid FROM pa0002 INTO l_aux1 UP TO 1 ROWS WHERE pernr = clave AND endda >= sy-datum AND begda <= sy-datum  ORDER BY PRIMARY KEY. ENDSELECT.
            ELSE.
              SELECT perid FROM pa0002 INTO l_aux1 UP TO 1 ROWS WHERE pernr = clave AND endda >= clave2 AND begda <= clave2 ORDER BY PRIMARY KEY. ENDSELECT.
            ENDIF.
            IF NOT l_aux1 IS INITIAL.
              IF l_aux1 = 'ES'.
                l_cache-valor = l_aux1+2(13).
              ELSE.
                l_cache-valor = l_aux1(13).
              ENDIF.
            ENDIF.

          WHEN 'CSKT'.
            IF clave2 IS INITIAL.
              SELECT SINGLE ktext FROM cskt INTO l_cache-valor WHERE spras = idioma AND kokrs = zcl_c=>sociedad_co AND kostl = clave AND datbi = '99991231'.
            ELSE.
              SELECT SINGLE ktext FROM cskt INTO l_cache-valor WHERE spras = idioma AND kokrs = clave2 AND kostl = clave AND datbi = '99991231'.
            ENDIF.
          WHEN 'SKAT'.
            IF clave2 IS INITIAL.
              SELECT SINGLE txt20 FROM skat INTO l_cache-valor WHERE spras = idioma AND ktopl = zcl_c=>plan_cuentas AND saknr = clave.
            ELSE.
              SELECT SINGLE txt20 FROM skat INTO l_cache-valor WHERE spras = idioma AND ktopl = clave2 AND saknr = clave.
            ENDIF.
          WHEN 'SKAT5'.
            IF clave2 IS INITIAL.
              SELECT SINGLE txt50 FROM skat INTO l_cache-valor WHERE spras = idioma AND ktopl = zcl_c=>plan_cuentas AND saknr = clave.
            ELSE.
              SELECT SINGLE txt50 FROM skat INTO l_cache-valor WHERE spras = idioma AND ktopl = clave2 AND saknr = clave.
            ENDIF.
          WHEN 'CSLT'.
            IF clave2 IS INITIAL.
              SELECT SINGLE ktext FROM cslt INTO l_cache-valor WHERE spras = idioma AND kokrs = zcl_c=>sociedad_co AND lstar = clave.
            ELSE.
              SELECT SINGLE ktext FROM cslt INTO l_cache-valor WHERE spras = idioma AND kokrs = clave2 AND lstar = clave.
            ENDIF.
          WHEN 'CSKU'. " Descripciones clases de coste
            IF clave2 IS INITIAL.
              SELECT SINGLE ktext FROM csku INTO l_cache-valor WHERE spras = idioma AND ktopl = zcl_c=>plan_cuentas AND kstar = clave.
            ELSE.
              SELECT SINGLE ktext FROM csku INTO l_cache-valor WHERE spras = idioma AND ktopl = clave2 AND kstar = clave.
            ENDIF.
          WHEN 'T685T'.
            IF clave2 IS INITIAL.
              SELECT SINGLE vtext FROM t685t INTO l_cache-valor WHERE spras = idioma AND kvewe = 'A' AND kappl = 'V' AND kschl = clave.
            ELSE.
              SELECT SINGLE vtext FROM t685t INTO l_cache-valor WHERE spras = idioma AND kvewe = clave2 AND kappl = clave3 AND kschl = clave.
            ENDIF.
          WHEN 'QPCT'.
            IF clave4 IS INITIAL.
              l_aux1 = idioma.
            ELSE.
              l_aux1 = clave4.
            ENDIF.
            IF clave5 IS INITIAL.
              SELECT SINGLE kurztext FROM qpct INTO l_cache-valor WHERE katalogart = clave AND codegruppe = clave2 AND code = clave3 AND sprache = l_aux1.
            ELSE.
              SELECT SINGLE kurztext FROM qpct INTO l_cache-valor WHERE katalogart = clave AND codegruppe = clave2 AND code = clave3 AND sprache = l_aux1 AND version = clave5.
            ENDIF.
          WHEN 'HRP1000' OR 'HRP1001'.
            SELECT SINGLE stext FROM hrp1000 INTO l_cache-valor WHERE plvar = '01' AND otype = clave2 AND objid = clave AND begda <= clave3 AND endda >= clave3.
          WHEN 'PLANS_T'.
            l_endda = clave2.
            l_fecha = l_endda.
            IF clave3 IS INITIAL.
              l_endda = clave3.
            ENDIF.
            SELECT plstx FROM  t528t
              INTO l_cache-valor
              UP TO 1 ROWS
             WHERE sprsl  = idioma
               AND otype  = 'S'
               AND plans  = clave
               AND endda >= l_fecha
               AND begda <= l_endda
              ORDER BY PRIMARY KEY.
            ENDSELECT.
          WHEN 'ORGEH_T'.
            l_endda = clave2.
            l_fecha = l_endda.
            IF clave3 IS INITIAL.
              l_endda = clave3.
            ENDIF.
            SELECT orgtx FROM  t527x
              INTO l_cache-valor
              UP TO 1 ROWS
             WHERE sprsl  = sy-langu
               AND orgeh  = clave
               AND endda >= l_fecha
               AND begda <= l_endda
             ORDER BY PRIMARY KEY.
            ENDSELECT.
          WHEN 'UMB'.
            l_matnr = clave.
            l_cache-valor = zcl_ap_material=>get_unidad_base( matnr = l_matnr ).
          WHEN 'UMB_WM'.
            SELECT SINGLE lhme1 FROM  mlgn INTO l_cache-valor WHERE matnr = clave AND lgnum = clave2.
          WHEN 'CUM' OR 'CUM_WM' OR 'CUM_KG'. " Convertir unidad material
            " En clave = material, clave2 = un.origen clave3 = un.destino. En campo_clave = cantidad
            l_matnr = clave.
            l_m1 = clave2.
            IF tabla = 'CUM_WM'.
              l_m2 = get( tabla = 'UMB_WM' clave = clave clave2 = clave3 ).
            ELSEIF tabla = 'CUM_KG'.
              l_m2 = 'KG'.
            ELSE.
              IF clave3 IS INITIAL.
                l_m2 = get( tabla = 'UMB' clave = clave ).
              ELSE.
                l_m2 = clave3.
              ENDIF.
            ENDIF.

            IF l_m1 = l_m2.
              l_cache-valor  = 10000.
              l_cache-valor2 = l_m2.
            ELSE.
              IF tabla = 'CUM_KG'.
                l_cache-valor = zcl_ap_material=>get_kg( matnr = l_matnr cantidad = 10000 unidad = l_m1 ).
              ELSE.
                l_cache-valor = zcl_ap_material=>convertir_unidad( matnr = l_matnr cantidad = 10000 unidad_origen = l_m1 unidad_destino = l_m2 ).
              ENDIF.
              IF l_cache-valor CO '0. '.
                l_cache-valor  = '?'.
                l_cache-valor2 = l_m1.
              ELSE.
                l_cache-valor2 = l_m2.
              ENDIF.
            ENDIF.
          WHEN 'SEMANA'.
            l_fecha = clave.
            l_cache-valor = zcl_ap_fechas=>get_semana( l_fecha ).
          WHEN 'CLASS_MAT'.
            l_matnr = clave.
            l_cache-valor = zcl_ap_material=>get_valor_clas_c( matnr = l_matnr caract = clave2 descripcion = clave3 spras = idioma ).
          WHEN 'PAIS'.
            l_cache-valor = zcl_ap_direcciones=>get_nombre_pais( land1 = clave spras = idioma ).
          WHEN 'PROVINCIA'.
            l_cache-valor = zcl_ap_direcciones=>get_nombre_provincia( land1 = clave regio = clave2 spras = idioma ).
          WHEN 'NOMBRE_USUARIO'.
            l_cache-valor = zcl_ap_usuario=>get_nombre( uname = clave ).
          WHEN 'EMAIL_USUARIO'.
            l_cache-valor = zcl_ap_usuario=>get_email( uname = clave ).
          WHEN 'ATINN'.
            l_cache-valor = zcl_ap_clasificacion=>get_caract_interno( clave ).
          WHEN 'ATINN_EXT'.
            l_cache-valor = zcl_ap_clasificacion=>get_caract_externo( CONV #( clave ) ).
          WHEN OTHERS.
            IF tabla(1) = '*'.
              i_campos = zcl_ap_dev=>get_fieldcatalog_tabla( tabla+1 ).
              DELETE i_campos WHERE fieldname = 'MANDT'.
              READ TABLE i_campos INTO l_campo INDEX 1.
              IF sy-subrc = 0.
                l_where = clave.
                CONCATENATE l_campo-fieldname ` = ` '''' l_where '''' INTO l_where.

                SELECT SINGLE * FROM (tabla+1)
                  INTO xstring
                 WHERE (l_where).

                IF NOT xstring IS INITIAL.
                  TRY.
                      cl_enh_utilities_xstring=>store_data( EXPORTING pi_input = xstring CHANGING pe_xstring = l_cache-xstring ).
                    CATCH cx_enh_no_valid_input_type.
                      RETURN.
                  ENDTRY.
                ENDIF.
              ENDIF.
            ELSEIF tabla(2) = 'D '.
              l_cache-valor = zcl_ap_utils=>get_texto_dominio( dominio = tabla+2 valor = clave idioma = idioma ).
            ELSEIF tabla(3) = 'CL '.
              l_cache-valor = zcl_ap_clasificacion=>get_valor_caract_text( caract = tabla+3 valor = clave spras = idioma ).
            ELSE.
              l_cache-valor = zcl_ap_dev=>get_descripcion( tabla         = tabla
                                                           valor         = clave
                                                           valor2        = clave2
                                                           nombre_campo  = nombre_campo
                                                           nombre_campo2 = nombre_campo2
                                                           campo_clave   = campo_clave
                                                           campo_idioma  = campo_idioma
                                                           idioma        = idioma ).
            ENDIF.
        ENDCASE.
        INSERT l_cache INTO TABLE i_cache_mem.

        encontrado = 'X'.

        IF tabla(3) = 'CUM'.
          IF l_cache-valor = '?'.
            valor = campo_clave.
          ELSE.
            valor = l_cache-valor * campo_clave / 10000.
          ENDIF.
        ELSE.
          valor = l_cache-valor.
        ENDIF.
        valor2 = l_cache-valor2.
        valor3 = l_cache-valor3.
      ENDIF.
    ENDIF.

    enc = encontrado.
  ENDMETHOD.
  METHOD get_mat.
    DATA l_conversion TYPE tabname.

    l_conversion = conversion.
    valor = get( tabla       = l_conversion
                 clave       = matnr
                 clave2      = unidad_origen
                 clave3      = unidad_destino
                 campo_clave = cantidad ).
  ENDMETHOD.
  METHOD get_shma.
    DATA: l_area  TYPE REF TO zcl_ap_shma_area,
          i_cache TYPE tt_cache_mem.

    FIELD-SYMBOLS: <cache>  TYPE t_cache_mem,
                   <cachem> TYPE t_cache_mem.

    IF solo_en_inicio = 'X'.
      IF NOT i_cache_mem IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    IF NOT users IS INITIAL.
      IF NOT users CS sy-uname.
        RETURN.
      ENDIF.
    ENDIF.
    IF NOT tcode IS INITIAL.
      IF NOT tcode CS sy-tcode.
        RETURN.
      ENDIF.
    ENDIF.

    l_area = get_shmm( ).
    IF NOT l_area IS INITIAL.
      l_area->root->get_cache( EXPORTING cprog = cprog IMPORTING i_cache = i_cache ).

      LOOP AT i_cache ASSIGNING <cache>.
        ASSIGN i_cache_mem[ tabla  = <cache>-tabla
                            clave  = <cache>-clave
                            clave2 = <cache>-clave2
                            clave3 = <cache>-clave3
                            clave4 = <cache>-clave4
                            clave5 = <cache>-clave5 ] TO <cachem>.
        IF sy-subrc = 0.
          IF <cache> = <cachem>.
            CONTINUE.
          ELSE.
            DELETE i_cache_mem INDEX sy-tabix.
          ENDIF.
        ENDIF.

        INSERT <cache> INTO TABLE i_cache_mem.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_shmm.
    TRY.
        IF update IS INITIAL.
          area = zcl_ap_shma_area=>attach_for_read( ).
        ELSE.
          area = zcl_ap_shma_area=>attach_for_update( ).
        ENDIF.
      CATCH cx_shm_no_active_version.
        WAIT UP TO 1 SECONDS.
        TRY.
            zcl_ap_shma_area=>build( ).
            IF update IS INITIAL.
              area = zcl_ap_shma_area=>attach_for_read( ).
            ELSE.
              area = zcl_ap_shma_area=>attach_for_update( ).
            ENDIF.
          CATCH cx_root INTO DATA(o_root).
            message = o_root->get_text( ).
        ENDTRY.
      CATCH cx_root INTO o_root.
        message = o_root->get_text( ).
    ENDTRY.
  ENDMETHOD.
  METHOD set_cache.
    DATA: cache    TYPE zcache,
          l_string TYPE string.

    cache-report = report.
    cache-clave  = clave.
    IF NOT subclave IS INITIAL.
      cache-subclave = subclave.
    ENDIF.

    cache-fecha = fecha.
    IF NOT max_duracion IS INITIAL.
      cache-fecha_limite = sy-datum + max_duracion.
    ENDIF.

    cache-hora  = hora.
    cache-ernam = uname.
    cache-ok    = ok.

    IF NOT seleccion_rango IS INITIAL.
      cache-seleccion = zcl_ap_string=>transform_object_to_rawstring( tabla = seleccion_rango json = json ).
      LOOP AT seleccion_rango INTO l_string.
        CONCATENATE cache-subclave l_string INTO cache-subclave.
        CONDENSE cache-subclave NO-GAPS.
      ENDLOOP.
    ENDIF.

    IF NOT valores IS INITIAL.
      cache-valores = zcl_ap_string=>transform_object_to_rawstring( tabla = valores json = json ).
    ENDIF.

    IF NOT valores2 IS INITIAL.
      cache-valores2 = zcl_ap_string=>transform_object_to_rawstring( tabla = valores2 json = json ).
    ENDIF.
    IF NOT valores3 IS INITIAL.
      cache-valores3 = zcl_ap_string=>transform_object_to_rawstring( tabla = valores3 json = json ).
    ENDIF.

    IF NOT variables IS INITIAL.
      zcl_ap_string=>transform_object_to_rawstring( EXPORTING  variable              = variables
                                                               json                  = json
                                                    RECEIVING  xstring               = cache-variables
                                                    EXCEPTIONS transformation_failed = 1
                                                               OTHERS                = 2 ).
      IF sy-subrc <> 0.
        message = 'Error convirtiendo VARIABLES a XSTRING'.
      ENDIF.
    ENDIF.

    IF NOT variables2 IS INITIAL.
      zcl_ap_string=>transform_object_to_rawstring( EXPORTING  variable              = variables2
                                                               json                  = json
                                                    RECEIVING  xstring               = cache-variables2
                                                    EXCEPTIONS transformation_failed = 1
                                                               OTHERS                = 2 ).
      IF sy-subrc <> 0.
        message = 'Error convirtiendo VARIABLES2 a XSTRING'.
      ENDIF.
    ENDIF.

    IF NOT objeto IS INITIAL.
      cache-objeto = zcl_ap_string=>transform_object_to_rawstring( objeto = objeto json = json ).
    ENDIF.

    IF NOT string IS INITIAL.
      cache-string = string.
    ENDIF.

    cache-aux1 = aux1.
    cache-aux2 = aux2.
    cache-aux3 = aux3.

    IF no_dup = 'X'.
      SELECT * FROM zcache
        INTO TABLE @DATA(i_cache)
       WHERE clave  = @cache-clave
         AND report = @cache-report.
      LOOP AT i_cache TRANSPORTING NO FIELDS WHERE     valores    = cache-valores
                                                   AND valores2   = cache-valores2
                                                   AND variables  = cache-variables
                                                   AND variables2 = cache-variables2
                                                   AND objeto     = cache-objeto.
        RETURN. " Salidmos sin grabar
      ENDLOOP.
    ENDIF.

    cache-json = json.
    MODIFY zcache FROM cache.

    IF commit = 'X'.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.
  METHOD set_cache_mem.
    DATA l_cache TYPE t_cache_mem.

    CLEAR l_cache.
    l_cache-tabla  = tabla.
    l_cache-clave  = clave.
    l_cache-clave2 = clave2.
    l_cache-clave3 = clave3.
    l_cache-clave4 = clave4.
    l_cache-clave5 = clave5.
    l_cache-valor  = valor.
    l_cache-valor2 = valor2.
    l_cache-valor3 = valor3.
    IF NOT xstring IS INITIAL.
      TRY.
          cl_enh_utilities_xstring=>store_data( EXPORTING pi_input = xstring CHANGING pe_xstring = l_cache-xstring ).
        CATCH cx_enh_no_valid_input_type.
          RETURN.
      ENDTRY.
    ENDIF.
    INSERT l_cache INTO TABLE i_cache_mem.
  ENDMETHOD.
  METHOD set_shma.
    DATA: l_area  TYPE REF TO zcl_ap_shma_area,
          i_cache TYPE tt_cache_mem.

    CHECK NOT i_cache_mem IS INITIAL.
    IF NOT users IS INITIAL.
      IF NOT users CS sy-uname.
        RETURN.
      ENDIF.
    ENDIF.
    IF NOT tcode IS INITIAL.
      IF NOT tcode CS sy-tcode.
        RETURN.
      ENDIF.
    ENDIF.

    l_area = get_shmm( update = 'X' ).
    IF NOT l_area IS INITIAL.
      TRY.
          l_area->root->set_cache( cprog = cprog i_cache = i_cache_mem ).
          l_area->detach_commit( ).
        CATCH cx_root INTO DATA(o_root).
          message = o_root->get_text( ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar.
    DATA: l_estructura TYPE zatrib_param,
          l_tipo       TYPE zatrib_param,
          tabla        TYPE REF TO data,
          var          TYPE REF TO data.

    FIELD-SYMBOLS: <fs>    TYPE any,
                   <tabla> TYPE STANDARD TABLE.

    IF cache IS INITIAL.
      RETURN.
    ELSEIF NOT campo IS INITIAL.
      ASSIGN COMPONENT campo OF STRUCTURE cache TO <fs>.
      IF sy-subrc <> 0.
        RETURN.
      ELSEIF <fs> IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    SELECT SINGLE atributo1 atributo2 FROM (zcl_c=>tabla_zparametros)
      INTO (l_estructura, l_tipo)
     WHERE clave  = 'CACHE'
       AND campo  = cache-report
       AND valor  = cache-clave
       AND valor2 = campo.
    IF sy-subrc = 0.
      visualizable = 'X'.
      IF tipo_visualizacion = 'C'.
        RETURN.
      ENDIF.

      zcl_ap_fs=>create_it_from_struc( EXPORTING i_struc    = l_estructura
                                       IMPORTING e_table    = tabla
                                                 e_workarea = var ).

      ASSIGN tabla->* TO <tabla>.
      ASSIGN var->* TO <fs>.

      IF l_tipo = 'E'.
        CASE campo.
          WHEN 'VARIABLES'.
            zcl_ap_cache=>get_cache( EXPORTING report       = cache-report
                                               clave        = cache-clave
                                               subclave     = cache-subclave
                                               max_duracion = 99999
                                     IMPORTING variables    = <fs>
                                               " TODO: variable is assigned but never used (ABAP cleaner)
                                               existe       = DATA(l_existe) ).
            APPEND <fs> TO <tabla>.
        ENDCASE.
      ENDIF.

      IF tipo_visualizacion = 'P'.
        IF <tabla> IS ASSIGNED.
          DATA(o_alv) = NEW zcl_ap_alv( tabla = '' ).

          o_alv->constructor_tabla( CHANGING t_tabla = <tabla> ).

          o_alv->show_popup( titulo = l_estructura ).
        ENDIF.
      ENDIF.
    ELSE.
      IF tipo_visualizacion = 'C'.
        RETURN.
      ENDIF.

      IF campo(2) = 'VA'.
        zcl_ap_cache=>get_cache( report            = cache-report
                                 clave             = cache-clave
                                 subclave          = cache-subclave
                                 max_duracion      = 99999
                                 visualizar_xml    = 'X'
                                 campos_visualizar = CONV #( campo ) ).
      ELSE.
        zcl_ap_cache=>get_cache( report            = cache-report
                                 clave             = cache-clave
                                 subclave          = cache-subclave
                                 max_duracion      = 99999
                                 visualizar_xml    = 'Y'
                                 campos_visualizar = CONV #( campo ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
