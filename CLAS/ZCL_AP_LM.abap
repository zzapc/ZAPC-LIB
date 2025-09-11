CLASS zcl_ap_lm DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS c_werks TYPE werks_d VALUE '0002' ##NO_TEXT.
    CONSTANTS c_stlan TYPE stlan   VALUE '1' ##NO_TEXT.

    DATA i_stb  TYPE roij_stpox_t.
    DATA stb    TYPE stpox.
    DATA topmat TYPE cstmat.

    CONSTANTS c_stlal TYPE stlal VALUE '01' ##NO_TEXT.

    DATA wultb TYPE zt_stpov.

    METHODS get_lista
      IMPORTING  matnr         TYPE matnr
                 werks         TYPE werks_d      DEFAULT c_werks
                 fecha         TYPE datum        DEFAULT sy-datum
                 stlan         TYPE stlan        DEFAULT '1'
                 stlal         TYPE stlal        DEFAULT '01'
                 capid         TYPE capid        DEFAULT 'PP01'
                 menge         TYPE basmn        DEFAULT 1
                 mehrs         TYPE mehrs        DEFAULT 'X'
                 aumgb         TYPE csdata-xfeld DEFAULT ''
                 mktls         TYPE csdata-xfeld DEFAULT 'X'
                 svwvo         TYPE csdata-xfeld DEFAULT 'X'
                 vrsvo         TYPE csdata-xfeld DEFAULT 'X'
                 vbeln         TYPE vbeln_va     OPTIONAL
                 vbpos         TYPE posnr_va     DEFAULT '000010'
                 pspnr         TYPE proj-pspnr   OPTIONAL
                 posid         TYPE prps-posid   OPTIONAL
                 auskz         TYPE csdata-xfeld DEFAULT ''
                 no_raise      TYPE abap_bool    DEFAULT ''
                 unidad_origen TYPE meins        DEFAULT ''
      EXPORTING  !message      TYPE bapi_msg
      EXCEPTIONS no_existe_material
                 error_al_explosionar
                 no_existe_lista
                 error_autorizacion.

    CLASS-METHODS nivel2string
      IMPORTING stufe         TYPE histu
      RETURNING VALUE(string) TYPE string.

    METHODS get_utilizacion
      IMPORTING matnr TYPE matnr
                werks TYPE werks_d DEFAULT c_werks
                fecha TYPE datum   DEFAULT sy-datum
                stlan TYPE stlan   DEFAULT '1'.

    METHODS tiene_utilizacion
      IMPORTING matnr        TYPE matnr
                werks        TYPE werks_d DEFAULT c_werks
                fecha        TYPE datum   DEFAULT sy-datum
                stlan        TYPE stlan   DEFAULT '1'
      RETURNING VALUE(tiene) TYPE abap_bool.

    CLASS-METHODS visualizar
      IMPORTING matnr TYPE matnr
                werks TYPE werks_d    DEFAULT c_werks
                fecha TYPE datum      DEFAULT sy-datum
                stlan TYPE stlan      DEFAULT '1'
                stlal TYPE stlal      DEFAULT '01'
                revlv TYPE revlv      DEFAULT ''
                posnr TYPE stpo-posnr DEFAULT ''
                aennr TYPE any        DEFAULT ''.

    CLASS-METHODS existe_lista
      IMPORTING matnr         TYPE matnr
                werks         TYPE werks_d DEFAULT zcl_c=>werks
                stlan         TYPE stlan
      RETURNING VALUE(existe) TYPE abap_bool.

    METHODS get_cs11
      IMPORTING matnr TYPE matnr
                werks TYPE werks_d DEFAULT c_werks
                fecha TYPE datum   DEFAULT sy-datum
                menge TYPE basmn   DEFAULT 1
                mehrs TYPE mehrs   DEFAULT 'X'
                stlan TYPE stlan   OPTIONAL
                stlal TYPE stlal   OPTIONAL
                capid TYPE capid   DEFAULT 'PP01'.

    METHODS exportar_excel
      IMPORTING campos         TYPE string
                mostrar_popup  TYPE abap_bool DEFAULT ''
                exportar_excel TYPE abap_bool DEFAULT 'X'
                fichero_salida TYPE any       DEFAULT ''
                exportar_csv   TYPE abap_bool DEFAULT ''.

    CLASS-METHODS cambiar_status
      IMPORTING matnr          TYPE matnr
                werks          TYPE werks_d
                stlan          TYPE stlan
                stlal          TYPE stlal
                stlst          TYPE rc29k-stlst
                modobi         TYPE char1 DEFAULT 'N'
      RETURNING VALUE(message) TYPE bapi_msg.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA stpov   TYPE stpov.
    DATA equicat TYPE zt_cscequi.
    DATA kndcat  TYPE zt_cscknd.
    DATA matcat  TYPE zt_cscmat.
    DATA stdcat  TYPE zt_cscstd.
    DATA tplcat  TYPE zt_csctpl.
endclass. "ZCL_AP_LM definition
class ZCL_AP_LM implementation.
  METHOD cambiar_status.
    DATA: mast TYPE mast,
          stko TYPE stko,
          o_bi TYPE REF TO zcl_ap_batch_input.

    SELECT stlnr FROM mast
      INTO mast-stlnr
      UP TO 1 ROWS
     WHERE matnr = matnr
       AND werks = werks
       AND stlan = stlan
       AND stlal = stlal
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      message = 'No existe la lista de materiales'(nel).
      RETURN.
    ENDIF.

    SELECT stlst FROM stko
      INTO stko-stlst
      UP TO 1 ROWS
     WHERE stlty = 'M'
       AND stlnr = mast-stlnr
       AND stlal = stlal
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      message = 'No existen datos de lista de materiales'(ned).
      RETURN.
    ENDIF.

    IF stko-stlst = stlst.
      RETURN.
    ENDIF.

    o_bi = NEW #( ).

    o_bi->inicio( ).

* Pantalla acceso lista materiales para material
    o_bi->dynpro( program = 'SAPLCSDI' dynpro = '0100' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=KALL' ).
    o_bi->campos( campo = 'RC29N-MATNR' valor = matnr ). " Número de material
    o_bi->campos( campo = 'RC29N-WERKS' valor = werks ). " Centro
    o_bi->campos( campo = 'RC29N-STLAN' valor = stlan ). " Utilización de la lista de materiales
    o_bi->campos( campo = 'RC29N-STLAL' valor = stlal ). " Alternativa de la lista de materiales

    o_bi->dynpro( program = 'SAPLCSDI' dynpro = '2110' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=FCBU' ).
    o_bi->campos( campo = 'RC29K-STLST' valor = stlst ). " Status de lista de materiales

    message = o_bi->llamar_transaccion( tcode = 'CS02' modo = modobi ).

    IF NOT message IS INITIAL.
      SELECT SINGLE stlnr FROM stko
        INTO stko-stlnr
       WHERE stlty = 'M'
         AND stlnr = mast-stlnr
         AND stlal = stlal
         AND stlst = stlst.
      IF sy-subrc <> 0.
        CLEAR message.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD existe_lista.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_mast TYPE mast.

    CLEAR existe.
    SELECT SINGLE matnr FROM  mast
      INTO l_mast-matnr
     WHERE matnr = matnr
       AND werks = werks
       AND stlan = stlan.
    IF sy-subrc = 0.
      existe = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD exportar_excel.
    DATA: i_wa      TYPE REF TO data,
          l_wa      TYPE REF TO data,
          o_alv     TYPE REF TO zcl_ap_alv,
          l_dialogo TYPE c LENGTH 1,
          l_cerrar  TYPE c LENGTH 1.

    FIELD-SYMBOLS: <subtabla>     TYPE STANDARD TABLE,
                   <reg_subtabla> TYPE any.

    zcl_ap_fs=>create_it_from_struc( EXPORTING i_struc = 'STPOX' i_struc2 = 'CI_STPO'
                                     solo_campos = campos
                                     IMPORTING e_table = i_wa
                                               e_workarea = l_wa ).

    ASSIGN i_wa->* TO <subtabla>.
    LOOP AT i_stb INTO stb.
      ASSIGN l_wa->* TO <reg_subtabla>.
      MOVE-CORRESPONDING stb TO <reg_subtabla>.
      APPEND <reg_subtabla> TO <subtabla>.
    ENDLOOP.

    o_alv = NEW #(
        tabla = '' ).

    o_alv->constructor_tabla( CHANGING t_tabla = <subtabla> ).

    IF mostrar_popup = 'X'.
      o_alv->show_popup( ).
    ENDIF.

    IF exportar_excel = 'X'.
      IF fichero_salida IS INITIAL.
        l_dialogo = 'X'.
      ELSE.
        l_cerrar = 'X'.
      ENDIF.
      o_alv->exportar_excel( EXPORTING dialogo_fichero = l_dialogo
                                       cerrar          = l_cerrar
                                       fichero_salida  = fichero_salida
                             CHANGING  t_tabla = <subtabla> ).
    ENDIF.

    IF exportar_csv = 'X'.
      IF fichero_salida IS INITIAL.
        l_dialogo = 'X'.
      ENDIF.
      o_alv->exportar_csv( EXPORTING dialogo_fichero = l_dialogo
                                     fichero_salida  = fichero_salida
                           CHANGING  t_tabla = <subtabla> ).
    ENDIF.
  ENDMETHOD.
  METHOD get_cs11.
    " TODO: parameter MENGE is never used (ABAP cleaner)

    DATA csbomex TYPE csbomex.

    CLEAR i_stb.

    csbomex-submf = 'X'.
    csbomex-aclas = 'CA'.

    EXPORT csbomex FROM csbomex TO MEMORY ID 'CSNN_BOMEX'.

    SUBMIT rcs11001                                        "#EC CI_SUBMIT
*     VIA SELECTION-SCREEN
       AND RETURN
           WITH pm_mtnrv = matnr
           WITH pm_werks = werks
           WITH pm_stlal = stlal
           WITH pm_stlan = stlan
           WITH pm_capid = capid
           WITH pm_datuv = fecha
           WITH pm_mehrs = mehrs
           WITH pm_dsprf = 'SAPCSLBLMP01'
           WITH pm_prprf = 'SAPCSLBLMP02'.

    IMPORT stb TO i_stb FROM MEMORY ID 'CSNN_STB'.
  ENDMETHOD.
  METHOD get_lista.
    DATA r_stlan TYPE RANGE OF stlan.
    DATA r_stlaL TYPE RANGE OF stlaL.

    CLEAR: i_stb, topmat, message.

    IF sy-subrc <> 0.
      RAISE no_existe_material.
    ENDIF.

    DATA(l_stlan) = stlan.
    DATA(l_stlal) = stlal.
    DATA(l_menge) = menge.

    IF NOT unidad_origen IS INITIAL.
      IF NOT stlan IS INITIAL.
        r_stlan = VALUE #( ( option = 'EQ' sign = 'I' low = stlan ) ).
      ENDIF.
      IF NOT stlaL IS INITIAL.
        r_stlaL = VALUE #( ( option = 'EQ' sign = 'I' low = stlal ) ).
      ENDIF.
      SELECT bmeng, bmein, stko~stlal, stlan FROM stko JOIN mast ON  stko~stlty = 'M'
                                                                 AND stko~stlnr = mast~stlnr
                                                                 AND stko~stlal = mast~stlal
       INTO (@DATA(l_bmeng), @DATA(l_bmein), @l_stlal, @l_stlan)
        UP TO 1 ROWS
       WHERE matnr       = @matnr
         AND werks       = @werks
         AND stlan      IN @r_stlan
         AND stko~stlal IN @r_stlal
       ORDER BY stko~stlal, mast~stlan.
      ENDSELECT.
      IF sy-subrc = 0.
        l_menge = zcl_ap_material=>convertir_unidad( matnr          = matnr
                                       cantidad       = menge
                                       unidad_origen  = unidad_origen
                                       unidad_destino = L_bmein ).
      ENDIF.
    ENDIF.

    SELECT SINGLE stlan FROM mast
      INTO l_stlan
     WHERE matnr = matnr
       AND werks = werks
       AND stlan = l_stlan
       AND stlal = l_stlal.
    IF sy-subrc <> 0.
      message = 'No existe la lista de materiales'.
      IF no_raise IS INITIAL.
        RAISE no_existe_lista.
      ENDIF.
    ENDIF.

    IF vbeln IS INITIAL AND pspnr IS INITIAL AND posid IS INITIAL.
      CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
        EXPORTING
          capid                 = capid
          datuv                 = fecha
          emeng                 = l_menge
          mehrs                 = mehrs
          mtnrv                 = matnr
          werks                 = werks
          stlal                 = l_stlal
          stlan                 = l_stlan
          aumgb                 = aumgb " Calcular cantidad rechazada
          mktls                 = mktls
          svwvo                 = svwvo
          vrsvo                 = vrsvo
          auskz                 = auskz " Considerar rechazo
        IMPORTING
          topmat                = topmat
        TABLES
          stb                   = i_stb
        EXCEPTIONS
          alt_not_found         = 1
          call_invalid          = 2
          material_not_found    = 3
          missing_authorization = 4
          no_bom_found          = 5
          no_plant_data         = 6
          no_suitable_bom_found = 7
          conversion_error      = 8
          OTHERS                = 9.

    ELSE.
      CALL FUNCTION 'CS_BOM_EXPL_KND_V1'
        EXPORTING
          capid                 = capid
          datuv                 = fecha
          emeng                 = menge
          mehrs                 = mehrs
          mtnrv                 = matnr
          werks                 = werks
          stlal                 = stlal
          stlan                 = stlan
          vbeln                 = vbeln
          vbpos                 = vbpos
        IMPORTING
          topmat                = topmat
        TABLES
          stb                   = i_stb
        EXCEPTIONS
          alt_not_found         = 1
          call_invalid          = 2
          material_not_found    = 3
          missing_authorization = 4
          no_bom_found          = 5
          no_plant_data         = 6
          no_suitable_bom_found = 7
          conversion_error      = 8
          OTHERS                = 9.
    ENDIF.

    IF sy-subrc <> 0.
      IF sy-subrc = 3.
        message = 'No existe el material'.
        IF no_raise IS INITIAL.
          RAISE no_existe_material.
        ENDIF.
      ELSEIF sy-subrc = 4.
        message = 'Error autorización'.
        IF no_raise IS INITIAL.
          RAISE error_autorizacion.
        ENDIF.
      ELSEIF sy-subrc = 5.
        message = 'No existe lista'.
        IF no_raise IS INITIAL.
          RAISE no_existe_lista.
        ENDIF.
      ELSEIF sy-subrc = 6.
        message = 'No existen datos de centro'.
        IF no_raise IS INITIAL.
          RAISE no_existe_lista.
        ENDIF.
      ELSE.
        message = |Error { sy-subrc } al explosionar lista|.
        IF no_raise IS INITIAL.
          RAISE error_al_explosionar.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_utilizacion.
    CLEAR: wultb, equicat, kndcat, matcat, stdcat, tplcat.
    CALL FUNCTION 'CS_WHERE_USED_MAT'
      EXPORTING
        datub                      = fecha
        datuv                      = fecha
        matnr                      = matnr
        stlan                      = stlan
        werks                      = werks
        mclmt                      = '00000000'
        stltp                      = 'M'
      TABLES
        wultb                      = wultb
        equicat                    = equicat
        kndcat                     = kndcat
        matcat                     = matcat
        stdcat                     = stdcat
        tplcat                     = tplcat
      EXCEPTIONS
        call_invalid               = 1
        material_not_found         = 2
        no_where_used_rec_found    = 3
        no_where_used_rec_selected = 4
        no_where_used_rec_valid    = 5
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      MESSAGE |Error { sy-subrc } recuperando utilización| TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD nivel2string.
    DATA aux TYPE n LENGTH 2.

    CLEAR string.
    DO stufe TIMES.
      IF sy-index > 7.
        EXIT.
      ELSE.
        CONCATENATE '.' string INTO string.
      ENDIF.
    ENDDO.
    aux = stufe.
    CONCATENATE string aux INTO string.
    CONDENSE string.
  ENDMETHOD.
  METHOD tiene_utilizacion.
    CLEAR tiene.
    get_utilizacion(
        matnr = matnr
        werks = werks
        fecha = fecha
        stlan = stlan ).

    IF NOT wultb IS INITIAL.
      tiene = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar.
    DATA o_bi      TYPE REF TO zcl_ap_batch_input.

    o_bi = NEW #( ).

    o_bi->inicio( ).

* Pantalla acceso lista materiales para material
    o_bi->dynpro( program = 'SAPLCSDI' dynpro = '0100' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'RC29N-MATNR'
                  valor = matnr ). " Número de material
    o_bi->campos( campo = 'RC29N-WERKS'
                  valor = werks ). " Centro
    o_bi->campos( campo = 'RC29N-STLAN'
                  valor = stlan ). " Utilización de la lista de materiales
    o_bi->campos( campo = 'RC29N-STLAL'
                  valor = stlal ). " Alternativa de la lista de materiales
    o_bi->campos( campo = 'RC29N-DATUV'
                  valor = fecha ). " Fecha inicio validez
    o_bi->campos( campo = 'RC29N-DATUB'
                  valor = fecha ). " Fecha fin validez

    IF NOT revlv IS INITIAL.
      o_bi->campos( campo = 'RC29N-REVLV'
                    valor = revlv ). " Estado de revisión
    ENDIF.

    IF NOT aennr IS INITIAL.
      o_bi->campos( campo = 'RC29N-AENNR'
                    valor = aennr ). " Nº Modificación
    ENDIF.

    IF NOT posnr IS INITIAL.
* Resumen posición material
      o_bi->dynpro( program = 'SAPLCSDI' dynpro = '0150' ).
      o_bi->campos( campo = 'BDC_OKCODE'
                    valor = '=SETP' ).

* Posicionar posición ...
      o_bi->dynpro( program = 'SAPLCSDI' dynpro = '0708' ).
      o_bi->campos( campo = 'BDC_OKCODE'
                    valor = '=CLWI' ).
      o_bi->campos( campo = 'RC29P-SELPO'
                    valor = posnr ). " Número de posición de lista de mater
    ENDIF.

    o_bi->llamar_transaccion( tcode = 'CS03' modo = 'E' ).
  ENDMETHOD.
