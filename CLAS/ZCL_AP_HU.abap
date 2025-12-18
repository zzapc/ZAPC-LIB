class ZCL_AP_HU definition
  public
  create public .

public section.

  data VEKP type VEKP .
  data VEPO type VEPO .
  data I_VEPO type TAB_VEPO .
  data RETURN type BAPIRET2 .
  data I_RETURN type BAPIRET2_T .
  data HUHEADER type BAPIHUHEADER .
  data EMKPF type EMKPF .
  constants C_TRASPASO type HUWBEVENT value '0012' ##NO_TEXT.
  data LIGHTS type LIGHTS .
  data MENSAJE type BAPI_MSG .

  methods CONSTRUCTOR
    importing
      !EXIDV type EXIDV optional
      !VENUM type VENUM optional
    preferred parameter EXIDV .
  methods GET_VEKP
    importing
      !VENUM type VENUM optional
      !EXIDV type EXIDV optional
      !SOLO_GET_VENUM type ABAP_BOOL default '' .
  methods GET_VEPO
    importing
      !EXIDV type EXIDV optional
      !VENUM type VENUM optional
    preferred parameter EXIDV .
  methods CREAR
    importing
      !MATNR type MATNR
      !WERKS type WERKS_D optional
      !LGORT type LGORT_D optional
      !EXIDV type EXIDV optional
      !VALIDAR_EXISTENCIA type ABAP_BOOL default ''
      !EXIDV2 type EXIDV2 optional
      !VEGR1 type VEGR1 default ''
    returning
      value(HUKEY) type BAPIHUKEY-HU_EXID .
  methods PACK
    importing
      !HUKEY type EXIDV
      !MATNR type MATNR
      !CHARG type CHARG_D
      !WERKS type WERKS_D
      !LGORT type LGORT_D
      !ITEM_TYPE type VELIN default '1'
      !PACK_QTY type ANY
      !MEINS type MEINS
      !BESTQ type BESTQ default ''
    returning
      value(MENSAJE) type STRING .
  class-methods CONTIENE_STATUS
    importing
      !VENUM type VENUM
      !STATUS type ANY
      !SPRAS type SY-LANGU default SY-LANGU
    returning
      value(SI_CONTIENE) type ABAP_BOOL .
  class-methods GET_SIGUIENTE_N_EMB_PROP
    importing
      !MATNR type MATNR
    returning
      value(NUM) type NRIV-NRLEVEL .
  class-methods VISUALIZAR
    importing
      !VENUM type VENUM optional
      !EXIDV type ANY optional
    preferred parameter VENUM .
  class-methods ESTA_ENCAJADO
    importing
      !MATNR type MATNR
      !CHARG type CHARG_D
      !WERKS type WERKS_D
      !LGORT type LGORT_D
    returning
      value(EXIDV) type VEKP-EXIDV .
  methods BORRAR
    importing
      !EXIDV type EXIDV optional
      !VENUM type VENUM optional
      !COMMIT type ABAP_BOOL default ''
    preferred parameter EXIDV
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods GET_MATERIALS
    importing
      !EXIDV type EXIDV
    returning
      value(I_MATERIALES) type HUM_CUM_MATERERIAL_T .
  methods UNPACK
    importing
      !HUKEY type EXIDV optional
      !VENUM type VENUM optional
    returning
      value(MESSAGE) type BAPI_MSG .
  methods MOVER
    importing
      !HUWBEVENT type HUWBEVENT default '0010'
      !MATNR type MATNR optional
      !WERKS type WERKS_D optional
      !LGORT type LGORT_D optional
      !BWART type BWART optional
      !GRUND type MB_GRBEW optional
      !KOSTL type KOSTL optional
      !ESPERA_A_GRABADO type ABAP_BOOL default 'X'
      !DEQUEUE_ALL type ABAP_BOOL default 'X'
      !I_CAJAS_EXT type HUM_EXIDV_T optional
      !I_DATOS_MOV type HUM_DATA_MOVE_TO_T optional
      !I_CAJAS_INT type HUM_VENUM_T optional
      !TCODE type SY-TCODE default 'VLMOVE'
      !XBLNR type XBLNR optional
      !BUDAT type BUDAT default SY-DATUM
      !BKTXT type BKTXT default ''
      !CHARG type CHARG_D optional .
  class-methods GET_STATUS
    importing
      !VENUM type VENUM
    returning
      value(STATUS) type BSVX-STTXT .
  class-methods GET_EXIDV
    importing
      !VENUM type VENUM
    returning
      value(EXIDV) type EXIDV .
  class-methods CAMBIAR_ESTADO_HU
    importing
      !VENUM type VENUM
      !ESTADO_ORIGEN type ANY
      !ESTADO_DESTINO type ANY
      !TCODE type SY-TCODE default 'VLMOVE'
      !XBLNR type XBLNR optional
      !BUDAT type BUDAT default SY-DATUM
      !BKTXT type BKTXT default ''
      !GRUND type MB_GRBEW optional
    exporting
      !MENSAJE type ANY
      !EMKPF type EMKPF
      !TIPO_MOV type STRING .
  class-methods GET_VENUM
    importing
      value(EXIDV) type ANY
    returning
      value(VENUM) type VENUM .
  class-methods GET_VEKP_ST
    importing
      !VENUM type VENUM optional
      !EXIDV type EXIDV optional
      !SOLO_GET_VENUM type ABAP_BOOL default ''
    returning
      value(VEKP) type VEKP .
  class-methods GET_MAT_EMBALAJE
    importing
      !MATNR type ANY
      !WERKS type ANY
    returning
      value(VHILM) type VHILM .
  class-methods GENERAR_NUM_EXIDV
    importing
      !EXIDV type ANY
    returning
      value(EXIDV_COMPLETA) type EXIDV .
  class-methods LANZAR_HUMO
    importing
      !EXIDV type ANY optional
      !LISTA_HUS type ANY optional
      !MODO type BDC_MODE default 'E' .
  class-methods CAMBIAR_STATUS
    importing
      !VENUM type VENUM optional
      !EXIDV type EXIDV optional
      !STAT type J_STATUS
      !INACT type J_INACT default '' .
  class-methods GET_NUEVA_SSCC
    importing
      !OBJECT type INRI-OBJECT default 'LVS_LENUM'
      !RANGE type INRI-NRRANGENR default '01'
    returning
      value(EXIDV) type EXIDV .
  class-methods INSERTAR_LOG_HU
    importing
      !EXIDV type EXIDV optional
      !VENUM type VENUM optional
      !HANDLE type VEVW-HANDLE optional
      !OBJECT type VEVW-OBJECT
      !OBJKEY type ANY
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods ESTA_BLOQUEADA
    importing
      !EXIDV type EXIDV optional
      !VENUM type VENUM optional
    returning
      value(BLOQUEADA) type ABAP_BOOL .
  class-methods ESPERA_SI_BLOQUEADA
    importing
      !EXIDV type EXIDV optional
      !VENUM type VENUM optional
      !SEGUNDOS_ESPERA type INT2 default 10
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods CALC_DIGITO_CONTROL
    importing
      !HU type CHAR17
    returning
      value(DIGITO) type NUMC1 .
  class-methods CAMBIA_MATERIAL_EMBALAJE
    importing
      !VENUM type VENUM optional
      !EXIDV type EXIDV optional
      !VHILM type VHILM
    returning
      value(MESSAGE) type BAPI_MSG .
protected section.
private section.
endclass. "ZCL_AP_HU definition
class ZCL_AP_HU implementation.
METHOD borrar.
    DATA l_exidv TYPE exidv.

    IF NOT exidv IS INITIAL.
      l_exidv = exidv.
    ELSE.
      SELECT SINGLE exidv FROM vekp
        INTO l_exidv
       WHERE venum = venum.
    ENDIF.

    CLEAR: i_return, message.

    CALL FUNCTION 'BAPI_HU_DELETE'
      EXPORTING
        hukey  = l_exidv
*     WITHLOWERHUS       = ' '
*     IFPACKED           = ' '
      TABLES
        return = i_return.

    CLEAR return.
    READ TABLE i_return INTO return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      LOOP AT i_return INTO return WHERE type = 'E'.
        __add_lista message return-message.
      ENDLOOP.
    ELSE.
      IF commit = 'X'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD calc_digito_control.
    DATA: lv_len   TYPE i,
          lv_digit TYPE i,
          lv_pos   TYPE i,
          lv_sum   TYPE i,
          lv_mult  TYPE i.

    CLEAR digito.
    lv_len = strlen( hu ).
    lv_sum = 0.

    " Recorremos de derecha a izquierda
    DO lv_len TIMES.
      lv_pos = lv_len - sy-index.
      lv_digit = hu+lv_pos(1).
      " Alternar multiplicador: derecha primero con 3
      IF ( sy-index MOD 2 ) = 1.
        lv_mult = 3.
      ELSE.
        lv_mult = 1.
      ENDIF.
      lv_sum = lv_sum + ( lv_digit * lv_mult ).
    ENDDO.

    digito = ( 10 - ( lv_sum MOD 10 ) ) MOD 10.
  ENDMETHOD.
  METHOD cambia_material_embalaje.
    DATA: lt_fields TYPE vsep_t_changed,
          g_hus     TYPE hum_exidv_t,
          g_venum   TYPE hum_venum_t.


    DATA(l_vekp) = get_vekp_st( venum = venum exidv = exidv ).
    IF l_vekp IS INITIAL.
      message = 'No existe la HU'.
      RETURN.
    ENDIF.

    IF l_vekp-vhilm = vhilm.
* El material de embalaje ya es el correcto
      RETURN.
    ENDIF.

    APPEND l_vekp-exidv TO g_hus.
    APPEND l_vekp-venum TO g_venum.

    CALL FUNCTION 'HU_GET_HUS'
      EXPORTING
        if_lock_hus = 'X'
        if_no_loop  = 'X'
        it_hus      = g_hus
        it_venum    = g_venum
      EXCEPTIONS
        hus_locked  = 1
        no_hu_found = 2
        fatal_error = 3
        OTHERS      = 4.
    IF sy-subrc <> 0.
      message = 'No se pudo bloquear/leer la HU'.
      RETURN.
    ENDIF.

    lt_fields = VALUE #( ( changed_f = 'VHILM'
                           f_value = vhilm ) ).

    CALL FUNCTION 'V51F_HU_HEADER_UPDATE'
      EXPORTING
        if_with_update      = 'U'
        if_venum            = l_vekp-venum
        it_changed_fields   = lt_fields
      EXCEPTIONS
        not_found           = 1
        exidv_already_exist = 2
        not_possible        = 3
        overloading_w       = 4
        overloading_v       = 5
        fatal_error         = 6
        OTHERS              = 7.
    IF sy-subrc <> 0.
      message = 'Error al actualizar cabecera HU'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'HU_POST'
      EXPORTING
        if_commit = 'X'.

  ENDMETHOD.
METHOD cambiar_estado_hu.
    DATA: o_hu   TYPE REF TO zcl_ap_hu,
          l_mov  TYPE huwbevent,
          l_aux1 TYPE c LENGTH 1,
          l_aux2 TYPE c LENGTH 1.

    CLEAR: emkpf, mensaje, tipo_mov.

    o_hu = NEW #( ).
    o_hu->get_vekp( venum = venum ).
    IF o_hu->vekp IS INITIAL.
      mensaje = 'HU no existe'(hne).
    ENDIF.
    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    o_hu->get_vepo( venum = venum ).

    IF o_hu->i_vepo IS INITIAL.
      mensaje = 'HU sin posiciones'(hsp).
    ENDIF.
    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    l_mov = '9999'.
    CASE estado_origen.
      WHEN ''.
        CASE estado_destino.
          WHEN 'S'.
            l_mov = '0023'.
            tipo_mov = 'de libre a bloqueado'(l2b).
          WHEN 'Q'.
            l_mov = '0004'.
            tipo_mov = 'de libre a control de calidad'(l2q).
          WHEN 'D'. " Desguace!!!!
            l_mov = '0014'.
            tipo_mov = 'de libre a desguace'(l2d).
        ENDCASE.
      WHEN 'S'.
        CASE estado_destino.
          WHEN ''.
*          l_mov = '0022'.
            l_mov = '0003'.
            tipo_mov = 'de bloqueado a libre'(b2l).
          WHEN 'Q'.
            l_mov = '0021'.
            tipo_mov = 'de bloqueado a calidad'(b2q).
          WHEN 'D'. " Desguace!!!!
            l_mov = '0014'.
            tipo_mov = 'de bloqueado a desguace'(b2d).
        ENDCASE.
      WHEN 'Q'.
        CASE estado_destino.
          WHEN ''.
            l_mov = '0003'.
            tipo_mov = 'de calidad a libre'(q2l).
          WHEN 'D'. " Desguace!!!!
            l_mov = '0014'.
            tipo_mov = 'de calidad a desguace'(q2d).
          WHEN 'S'.
            l_mov = '0005'.
            tipo_mov = 'de calidad a bloqueado'(q2b).
        ENDCASE.
    ENDCASE.

    IF l_mov = '9999'.
      l_aux1 = estado_origen.
      l_aux2 = estado_destino.
      CONCATENATE 'No es posible determinar tipo mov. de'(npt) l_aux1 'a' l_aux2 INTO mensaje SEPARATED BY space.
    ENDIF.
    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

* El paso de bloqueado a calidad, en un paso, da problemas, por eso lo hacemos en dos pasos
    IF l_mov = '0021'.
      o_hu->mover( huwbevent = '0003' tcode = tcode xblnr = xblnr bktxt = bktxt budat = budat grund = grund ).    " De bloqueado a libre
      IF NOT o_hu->emkpf-mblnr IS INITIAL.
        o_hu->mover( huwbevent = '0004' tcode = tcode xblnr = xblnr bktxt = bktxt budat = budat grund = grund ).    " De libre a calidad
      ENDIF.
    ELSE. " Caso normal
      IF l_mov = '0014' AND estado_origen = 'S'. " Para desguazar, hay que liberar primero
        o_hu->mover( huwbevent = '0003' tcode = tcode xblnr = xblnr bktxt = bktxt budat = budat grund = grund ).    " De bloqueado a libre
      ENDIF.
      o_hu->mover( huwbevent = l_mov tcode = tcode xblnr = xblnr bktxt = bktxt budat = budat grund = grund ).
    ENDIF.

    mensaje = o_hu->mensaje.
    emkpf = o_hu->emkpf.
  ENDMETHOD.
METHOD cambiar_status.
    DATA: l_venum TYPE venum,
          l_objnr TYPE j_objnr,
          husstat TYPE husstat.

    IF venum IS INITIAL.
      SELECT venum FROM vekp
        INTO l_venum
        UP TO 1 ROWS
       WHERE exidv = exidv
         ORDER BY venum DESCENDING.
      ENDSELECT.
    ELSE.
      l_venum = venum.
    ENDIF.

    IF l_venum IS INITIAL.
      RETURN.
    ENDIF.
    CONCATENATE 'HU' l_venum INTO l_objnr.

    SELECT SINGLE * FROM  husstat
      INTO husstat
     WHERE objnr = l_objnr
       AND stat  = stat.
    IF sy-subrc = 0.
      IF husstat-inact <> inact.
        UPDATE husstat
          SET inact = inact
         WHERE objnr = l_objnr
           AND stat  = stat.
      ENDIF.
    ELSE.
      CLEAR husstat.
      husstat-objnr = l_objnr.
      husstat-stat  = stat.
      husstat-inact = inact.
      MODIFY husstat FROM husstat.
    ENDIF.
  ENDMETHOD.
METHOD constructor.
    IF NOT exidv IS INITIAL OR NOT venum IS INITIAL.
      get_vekp( venum = venum
                exidv = exidv ).
      get_vepo( ).
    ENDIF.
  ENDMETHOD.
METHOD contiene_status.
    DATA: r_status  TYPE RANGE OF j_status,
          i_status  TYPE TABLE OF j_txt04,
          l_txt04   TYPE j_txt04,
          l_istat   TYPE j_status,
          lr_status LIKE LINE OF r_status,
          l_objnr   TYPE jest-objnr.

    SPLIT status AT ',' INTO TABLE i_status.
    IF i_status IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT i_status INTO l_txt04.
      SELECT istat FROM tj02t                            "#EC CI_GENBUFF.
        INTO l_istat
        UP TO 1 ROWS
       WHERE spras = spras
         AND txt04 = l_txt04
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.
        CLEAR lr_status.
        lr_status-option = 'EQ'.
        lr_status-sign   = 'I'.
        lr_status-low    = l_istat.
        APPEND lr_status TO r_status.
      ENDIF.
    ENDLOOP.

    IF r_status IS INITIAL.
      EXIT.
    ENDIF.

    CONCATENATE 'HU' venum INTO l_objnr.
    SELECT SINGLE stat FROM  husstat
      INTO l_istat
     WHERE objnr  = l_objnr
       AND stat  IN r_status
       AND inact  = ''.
    IF sy-subrc = 0.
      si_contiene = 'X'.
    ENDIF.
  ENDMETHOD.
METHOD crear.
    DATA l_exidv TYPE exidv.
    DATA: l_header TYPE bapihuhdrproposal,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_venum  TYPE venum.

    IF validar_existencia = 'X'.
      zcl_ap_string=>poner_ceros( EXPORTING cadena = exidv
                                  CHANGING  salida = l_exidv ).
      SELECT SINGLE exidv FROM vekp
        INTO l_exidv
       WHERE exidv = l_exidv.
      IF sy-subrc = 0.
        CLEAR return.
        return-type       = 'E'.
        return-id         = 'L3'.
        return-number     = '415'.
        return-message_v1 = exidv.
        return-message    = zcl_ap_log=>get_text_message( return ).
        APPEND return TO i_return.
        EXIT.
      ENDIF.
    ENDIF.

    CLEAR l_header.
    l_header-pack_mat    = matnr.
    l_header-hu_exid     = exidv.
    l_header-plant       = werks.
    l_header-stge_loc    = lgort.
    l_header-ext_id_hu_2 = exidv2.
    l_header-hu_grp1     = vegr1.

    CLEAR: huheader, hukey, return, i_return.
    CALL FUNCTION 'BAPI_HU_CREATE'
      EXPORTING
        headerproposal       = l_header
     IMPORTING
       huheader             = huheader
       hukey                = hukey
      TABLES
*       itemsproposal        = items
*       ITEMSSERIALNO        =
        return               = i_return.

    IF NOT hukey IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      DO 10 TIMES.
        SELECT SINGLE venum FROM vekp
          INTO l_venum
         WHERE venum = huheader-hu_id.
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDMETHOD.
METHOD espera_si_bloqueada.
    DATA bloqueada TYPE c LENGTH 1.

    DO segundos_espera TIMES.
      bloqueada = esta_bloqueada( venum = venum exidv = exidv ).
      IF bloqueada = 'X'.
        message = |HU bloqueada por { sy-msgv1 }|.
        WAIT UP TO 1 SECONDS.
      ELSE.
        CLEAR message.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.
METHOD esta_bloqueada.
    DATA(vekp) = get_vekp_st( venum = venum exidv = exidv solo_get_venum = 'X' ).

    bloqueada = zcl_ap_utils=>comprobar_bloqueo( tabla = 'VEKP'
                                                 clave = sy-mandt
                                                 clave2 = vekp-venum ).
  ENDMETHOD.
METHOD esta_encajado.
    DATA l_venum TYPE venum.

    CLEAR exidv.

    SELECT venum FROM vepo "#EC CI_NOFIELD.
      INTO l_venum
      UP TO 1 ROWS
     WHERE matnr = matnr
       AND charg = charg
       AND werks = werks
       AND lgort = lgort
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc = 0.
      SELECT SINGLE exidv FROM vekp
        INTO exidv
       WHERE venum = l_venum.
    ENDIF.
  ENDMETHOD.
METHOD generar_num_exidv.
    DATA l_exidv TYPE exidv.

    zcl_ap_string=>poner_ceros( EXPORTING cadena = exidv
                                CHANGING salida = l_exidv ).

    CALL FUNCTION 'LE_CHECK_DIGIT_CALCULATION'
      EXPORTING
        if_number_wo_check_digit       = l_exidv+1(19)
*   IF_CALC_METHOD                 = 'A'
*   IF_USER_CALC_METHOD            =
*   IF_ONLY_CHECKING               = ' '
      IMPORTING
        ef_number_w_check_digit        = exidv_completa
*   EF_CHECK_DIGIT_OK              =
*   EF_CHECK_DIGIT                 =
      EXCEPTIONS
        invalid_parameter              = 1
        OTHERS                         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_exidv ).
    ENDIF.
  ENDMETHOD.
METHOD get_exidv.
    CLEAR exidv.
    SELECT SINGLE exidv FROM vekp
      INTO exidv
     WHERE venum = venum.
  ENDMETHOD.
METHOD get_mat_embalaje.
    DATA: l_komgp            TYPE komgp,
          Datos TYPE REF TO data.
FIELD-SYMBOLS: <tabla> TYPE STANDARD TABLE.

    l_komgp-werks = werks.
    l_komgp-matnr = matnr.

  IF zcl_c=>hana IS INITIAL.
    data(l_estructura) = 'ISUATO_PINSTRUCTION'.
  ELSE.
    L_ESTRUCTURA = 'VHU_PINSTRUCTION'.
  ENDIF.

      zcl_ap_fs=>create_it_from_struc( EXPORTING i_struc = L_ESTRUCTURA
                                       IMPORTING e_table = datos ).
                                       ASSIGN datos->* TO <tabla>.

    CALL FUNCTION 'VHUPIBAPI_BUILD_PACK_INST_INFO'
      EXPORTING
        i_komgp       = l_komgp
      TABLES
        tpinstruction = <tabla>
      EXCEPTIONS
        locked        = 1
        not_found     = 2
        fatal_error   = 3
        OTHERS        = 4.
    IF sy-subrc = 0.
      READ TABLE <tabla> ASSIGNING FIELD-SYMBOL(<instruction>) INDEX 1.
      IF sy-subrc = 0.
      ASSIGN COMPONENT 'LOADCARR' OF STRUCTURE <INSTRUCTION> TO FIELD-SYMBOL(<LOADCARR>).
        vhilm = <LOADCARR>.
      ENDIF.
    ENDIF.

  ENDMETHOD.
METHOD get_materials.
    CALL FUNCTION 'HU_GET_MATERIALS'
     EXPORTING
        if_identification         = exidv
*   IF_OBJECT                 =
*   IT_INTERNAL_NUMBERS       =
     IMPORTING
*   ES_HEADER                 =
*   ET_HEADERS                =
        et_materials              = i_materiales
     EXCEPTIONS
       error                     = 1
       OTHERS                    = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Error recuperando materiales' TYPE 'S'.
    ENDIF.
  ENDMETHOD.
METHOD get_nueva_sscc.
    CLEAR exidv.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = range
        object                  = object
        quantity                = 1
      IMPORTING
        number                  = exidv
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*   Calculamos el dígito de control
    __quitar_ceros exidv.
    CALL FUNCTION 'LE_CHECK_DIGIT_CALCULATION'
      EXPORTING
        if_number_wo_check_digit = exidv
      IMPORTING
        ef_number_w_check_digit  = exidv
      EXCEPTIONS
        invalid_parameter        = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    __poner_ceros exidv.
  ENDMETHOD.
METHOD get_siguiente_n_emb_prop.
    DATA: l_magrv     TYPE mara-magrv,
          l_vhart     TYPE mara-vhart,
          l_traty     TYPE tervh-traty,
          l_nrverg    TYPE tvty-nrverg,
          l_int_nkr   TYPE tvty-int_nkr,
          l_exidv_max TYPE exidv,
          l_exidv_min TYPE exidv,
          l_exidv     TYPE exidv.

    CLEAR num.
* Busco grupo de material de embalaje
    SELECT SINGLE magrv vhart FROM mara
      INTO (l_magrv, l_vhart)
     WHERE matnr = matnr.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* Obtengo el tipo de material de embalaje.
    IF l_vhart IS INITIAL.
      SELECT traty FROM tervh
        INTO l_traty
        UP TO 1 ROWS
       WHERE magrv = l_magrv
        ORDER BY PRIMARY KEY.
      ENDSELECT.
    ELSE.
      SELECT traty FROM tervh
        INTO l_traty
        UP TO 1 ROWS
       WHERE traty = l_vhart
       ORDER BY PRIMARY KEY.
      ENDSELECT.
    ENDIF.
    IF NOT l_traty IS INITIAL.
* Recuperamos el nº de
      SELECT SINGLE nrverg int_nkr FROM tvty
        INTO (l_nrverg, l_int_nkr )
       WHERE traty = l_traty.
      IF sy-subrc = 0.
        IF l_nrverg = 'B'.
          zcl_ap_rango_numero=>info_numero( EXPORTING rango = 'HU_VEKP'
                                                        rango_no = l_int_nkr
                                              IMPORTING numero = num ).
        ELSEIF l_nrverg = ''.
          zcl_ap_rango_numero=>info_numero( EXPORTING rango = 'RV_VEKP'
                                                        rango_no = '01'
                                              IMPORTING numero = num ).
        ENDIF.
        l_exidv_max = num.
        l_exidv_min = num - 50.
        zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_exidv_min ).
        SELECT MAX( exidv ) FROM vekp
          INTO l_exidv
         WHERE exidv >= l_exidv_min
           AND exidv <= l_exidv_max.
        IF sy-subrc = 0 AND NOT l_exidv IS INITIAL.
          l_exidv = l_exidv + 1.
          zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_exidv ).
          num = l_exidv.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
METHOD get_status.
    DATA: l_objnr TYPE jest-objnr,
          l_istat TYPE husstat-stat,
          l_txt04 TYPE tj02t-txt04.

    CONCATENATE 'HU' venum INTO l_objnr.
*  CALL FUNCTION 'STATUS_TEXT_EDIT'
*    EXPORTING
*      objnr            = l_objnr
*      FLG_USER_STAT     = 'X'
*      spras            = sy-langu
*      BYPASS_BUFFER    = 'X'
*    IMPORTING
*      E_STSMA          = l_stsma
*      line             = status
*      USER_LINE        = l_USER_LINE
*    EXCEPTIONS
*      object_not_found = 1
*      OTHERS           = 2.

    SELECT stat FROM  husstat
      INTO l_istat
     WHERE objnr = l_objnr
       AND inact = ''.
      SELECT SINGLE txt04 FROM tj02t
        INTO l_txt04
       WHERE spras = sy-langu
         AND istat = l_istat.
      IF sy-subrc = 0.
        IF status IS INITIAL.
          status = l_txt04.
        ELSE.
          CONCATENATE status l_txt04 INTO status SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDSELECT.
  ENDMETHOD.
METHOD get_vekp.
    CLEAR vekp.

    IF NOT venum IS INITIAL.
      IF solo_get_venum IS INITIAL.
        SELECT SINGLE * FROM vekp
          INTO vekp
         WHERE venum = venum.
      ELSE.
        SELECT SINGLE venum FROM vekp
          INTO vekp-venum
         WHERE venum = venum.
      ENDIF.
    ELSE.
      IF solo_get_venum IS INITIAL.
        SELECT * FROM vekp
          INTO vekp
          UP TO 1 ROWS
         WHERE exidv = exidv
         ORDER BY venum DESCENDING.
        ENDSELECT.
      ELSE.
        SELECT venum FROM vekp
          INTO vekp-venum
          UP TO 1 ROWS
         WHERE exidv = exidv
         ORDER BY venum DESCENDING.
        ENDSELECT.
      ENDIF.
    ENDIF.
  ENDMETHOD.
METHOD get_vekp_st.
    CLEAR vekp.

    IF NOT venum IS INITIAL.
      IF solo_get_venum IS INITIAL.
        SELECT SINGLE * FROM vekp
          INTO vekp
         WHERE venum = venum.
      ELSE.
        SELECT SINGLE venum FROM vekp
          INTO vekp-venum
         WHERE venum = venum.
      ENDIF.
    ELSE.
      IF solo_get_venum IS INITIAL.
        SELECT * FROM vekp
          INTO vekp
          UP TO 1 ROWS
         WHERE exidv = exidv
         ORDER BY venum DESCENDING.
        ENDSELECT.
      ELSE.
        SELECT venum FROM vekp
          INTO vekp-venum
          UP TO 1 ROWS
         WHERE exidv = exidv
         ORDER BY venum DESCENDING.
        ENDSELECT.
      ENDIF.
    ENDIF.
  ENDMETHOD.
METHOD get_venum.
    CLEAR venum.
    SELECT venum FROM vekp
      INTO venum
      UP TO 1 ROWS
     WHERE exidv = exidv
     ORDER BY venum DESCENDING.
    ENDSELECT.
  ENDMETHOD.
METHOD get_vepo.
    CLEAR: i_vepo, vepo.

    IF NOT venum IS INITIAL OR NOT exidv IS INITIAL.
      get_vekp( venum = venum
                exidv = exidv ).
    ENDIF.
    IF NOT vekp-venum IS INITIAL.
      SELECT * FROM vepo
        INTO TABLE i_vepo
       WHERE venum = vekp-venum
        ORDER BY PRIMARY KEY.

      READ TABLE i_vepo INTO vepo INDEX 1.
    ENDIF.
  ENDMETHOD.
METHOD insertar_log_hu.
    DATA l_handle TYPE vevw-handle.
    DATA vevw     TYPE vevw.

    CLEAR message.

    IF NOT handle IS INITIAL.
      l_handle = handle.
    ELSEIF NOT venum IS INITIAL.
      SELECT SINGLE handle FROM vekp
        INTO l_handle
       WHERE venum = venum.
    ELSE.
      SELECT handle FROM vekp
        INTO l_handle
        UP TO 1 ROWS
       WHERE exidv   = exidv
         AND status <> '0060'
       ORDER BY venum DESCENDING.
      ENDSELECT.
    ENDIF.

    IF l_handle IS INITIAL.
      message = 'No se ha encontrado la SSCC'.
    ELSE.
      SELECT SINGLE handle FROM vevw
        INTO vevw-handle
       WHERE handle = l_handle
         AND object = object
         AND objkey = objkey.
      IF sy-subrc <> 0.
        CLEAR vevw.
        vevw-handle = l_handle.
        vevw-object = object.
        vevw-objkey = objkey.
        CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP vevw-tstamp TIME ZONE sy-zonlo.
        INSERT vevw FROM vevw.
      ENDIF.
    ENDIF.
  ENDMETHOD.
METHOD lanzar_humo.
    DATA: l_exidv   TYPE exidv,
          i_exidv   TYPE TABLE OF exidv,
          o_bi      TYPE REF TO zcl_ap_batch_input,
          l_rc      TYPE i,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_mensaje TYPE bapireturn1-message.

    IF NOT exidv IS INITIAL.
      l_exidv = exidv.
    ELSEIF NOT lista_hus IS INITIAL.
      SPLIT lista_hus AT ',' INTO TABLE i_exidv.
      DESCRIBE TABLE i_exidv LINES sy-tfill.
      IF sy-tfill = 1.
        READ TABLE i_exidv INTO l_exidv INDEX 1.
      ENDIF.
    ENDIF.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    IF NOT l_exidv IS INITIAL.
      o_bi->dynpro( program = 'RHU_HELP' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=ONLI' ).
      o_bi->campos( campo = 'SELEXIDV-LOW' valor = l_exidv ).
      o_bi->campos( campo = 'LSTAND' valor = 'X' ).
    ELSEIF NOT i_exidv IS INITIAL.
      cl_gui_frontend_services=>clipboard_export(
  IMPORTING
    data                 = i_exidv
  CHANGING
    rc                   = l_rc
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
*    no_authority         = 4
    OTHERS               = 5 ).
      IF sy-subrc <> 0.
        MESSAGE 'Error dejando exidv en portapapeles' TYPE 'I'.
        RETURN.
      ENDIF.

      o_bi->dynpro( program = 'RHU_HELP' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=%00120100000198451' ).
      o_bi->campos( campo = 'SELEXIDV-LOW' valor = '' ).
      o_bi->campos( campo = 'LSTAND' valor = 'X' ).

      o_bi->dynpro( program = 'SAPLALDB' dynpro = '3000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=CLIP' ).

      o_bi->dynpro( program = 'SAPLALDB' dynpro = '3000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=ACPT' ).

      o_bi->dynpro( program = 'RHU_HELP' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=ONLI' ).
    ENDIF.

    l_mensaje = o_bi->llamar_transaccion( tcode = 'HUMO' modo = modo ).
  ENDMETHOD.
METHOD mover.
    DATA gt_venum TYPE hum_venum_t.
    DATA: gt_exidv TYPE hum_exidv_t,
          imkpf    TYPE imkpf.
    DATA lt_move_to TYPE hum_data_move_to_t.
    DATA ls_venum   TYPE hum_venum.
    DATA ls_move_to TYPE hum_data_move_to.
    DATA ef_posted  TYPE sysubrc.
    DATA: message    TYPE huitem_messages,
          i_messages TYPE huitem_messages_t.

*  DATA: ls_hu_items   TYPE hum_humseg.

    CLEAR: emkpf, i_return, return.

    REFRESH: gt_venum, gt_exidv, lt_move_to.

    IF i_datos_mov IS INITIAL.
      IF vekp IS INITIAL.
        MESSAGE 'No hay datos de caja'(ndc) TYPE 'E'.
      ENDIF.

      ls_venum-venum = vekp-venum.
      APPEND ls_venum TO gt_venum.

      ls_move_to-huwbevent = huwbevent.
      ls_move_to-matnr     = matnr.
      IF NOT charg IS INITIAL.
        ls_move_to-charg = charg.
      ENDIF.
      ls_move_to-werks = werks.
      ls_move_to-lgort = lgort.
      ls_move_to-bwart = bwart.
      ls_move_to-grund = grund.
      ls_move_to-kostl = kostl.

*  LOOP AT me->i_vepo INTO vepo.
*    ls_hu_items-venum = vekp-venum.
*    ls_hu_items-vepos = vepo-vepos.
*    APPEND ls_hu_items TO ls_move_to-hu_items.
*  ENDLOOP.

      APPEND ls_move_to TO lt_move_to.
    ELSE.
      lt_move_to = i_datos_mov.
      gt_venum   = i_cajas_int.
      gt_exidv   = i_cajas_ext.
    ENDIF.

    IF NOT xblnr IS INITIAL OR NOT bktxt IS INITIAL.
      imkpf-xblnr = xblnr.
      imkpf-budat = budat.
      imkpf-bktxt = bktxt.
    ENDIF.

    CALL FUNCTION 'HU_PACKING_REFRESH'.

    CALL FUNCTION 'HU_CREATE_GOODS_MOVEMENT'
      EXPORTING
        if_simulate    = ' '
        if_commit      = ' '
        if_tcode       = tcode
        it_move_to     = lt_move_to
        it_internal_id = gt_venum
        it_external_id = gt_exidv
        is_imkpf       = imkpf
      IMPORTING
        ef_posted      = ef_posted
        es_message     = message
        et_messages    = i_messages
        es_emkpf       = emkpf.

    IF NOT emkpf IS INITIAL.
      COMMIT WORK AND WAIT.
      lights = zcl_ap_alv=>c_sem_verde.

      IF espera_a_grabado = 'X'.
        DO 5 TIMES.
          SELECT SINGLE mblnr FROM mkpf
            INTO emkpf-mblnr
           WHERE mblnr = emkpf-mblnr
             AND mjahr = emkpf-mjahr.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDDO.
      ENDIF.

      IF NOT message-msgid IS INITIAL.
        MESSAGE ID message-msgid TYPE 'S' NUMBER message-msgno
                WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4
                INTO mensaje.
      ENDIF.
    ELSE.
      lights = zcl_ap_alv=>c_sem_rojo.

      IF NOT message-msgid IS INITIAL.
        CLEAR return.
        return-id = message-msgid.
        IF emkpf IS INITIAL.
          return-type = 'E'.
        ELSE.
          return-type = message-msgty.
        ENDIF.
        return-number     = message-msgno.
        return-message_v1 = message-msgv1.
        return-message_v2 = message-msgv2.
        return-message_v3 = message-msgv3.
        return-message_v4 = message-msgv4.

        MESSAGE ID message-msgid TYPE 'S' NUMBER message-msgno
                WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4
                INTO return-message.
        mensaje = return-message.
        APPEND return TO i_return.
      ENDIF.

      LOOP AT i_messages ASSIGNING FIELD-SYMBOL(<MESSAGE>) WHERE msgid <> ''.
        CLEAR return.
        return-id         = <MESSAGE>-msgid.
        return-type       = <MESSAGE>-msgty.
        return-number     = <MESSAGE>-msgno.
        return-message_v1 = <MESSAGE>-msgv1.
        return-message_v2 = <MESSAGE>-msgv2.
        return-message_v3 = <MESSAGE>-msgv3.
        return-message_v4 = <MESSAGE>-msgv4.

        MESSAGE ID <MESSAGE>-msgid TYPE <MESSAGE>-msgty NUMBER <MESSAGE>-msgno
                WITH <MESSAGE>-msgv1 <MESSAGE>-msgv2 <MESSAGE>-msgv3 <MESSAGE>-msgv4
                INTO return-message.
        APPEND return TO i_return.
        IF mensaje IS INITIAL.
          mensaje = return-message.
        ENDIF.
      ENDLOOP.

      IF mensaje IS INITIAL.
        mensaje = 'No ha sido posible hacer el movimiento'(npm).
      ENDIF.

    ENDIF.

    IF dequeue_all = 'X'.
      CALL FUNCTION 'DEQUEUE_ALL'.
    ENDIF.

    IF message = 'S::000'.
      CLEAR message.
    ENDIF.
    IF mensaje = 'S::000'.
      CLEAR mensaje.
    ENDIF.
  ENDMETHOD.
METHOD pack.
    DATA item TYPE bapihuitmproposal.

    CLEAR item.
    item-base_unit_qty = meins.
    item-hu_item_type  = item_type.
    item-material      = matnr.
    item-batch         = charg.
    item-plant         = werks.
    item-stge_loc      = lgort.
    item-pack_qty      = pack_qty.
    item-stock_cat     = bestq.

    CLEAR: return, i_return.
    CALL FUNCTION 'BAPI_HU_PACK'
      EXPORTING
        hukey               = hukey
        itemproposal        = item
*        IMPORTING
*         huitem              = huitem
*   HUHEADER            =
      TABLES
*   SERIALNUMBERS       =
        return              = i_return.

    IF NOT hukey IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

    READ TABLE i_return INTO return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      mensaje = return-message.
    ENDIF.
  ENDMETHOD.
METHOD unpack.
    DATA: l_venum TYPE vekp-venum,
          l_exidv TYPE vekp-exidv,
          i_vepo  TYPE TABLE OF vepo,
          l_item  TYPE bapihuitmunpack.

    IF hukey IS INITIAL.
      l_venum = venum.
      SELECT SINGLE exidv FROM vekp
        INTO l_exidv
      WHERE venum = venum.
    ELSE.
      l_exidv = hukey.
      IF venum IS INITIAL.
        SELECT venum FROM vekp
          INTO l_venum
          UP TO 1 ROWS
         WHERE exidv = hukey
          ORDER BY PRIMARY KEY.
        ENDSELECT.
      ELSE.
        l_venum = venum.
      ENDIF.
    ENDIF.

    SELECT velin vepos matnr charg vemng vemeh werks lgort FROM vepo
      INTO CORRESPONDING FIELDS OF TABLE i_vepo
     WHERE venum = l_venum
      ORDER BY PRIMARY KEY.

    LOOP AT i_vepo ASSIGNING FIELD-SYMBOL(<VEPO>).
      CLEAR l_item.
      l_item-hu_item_type   = <VEPO>-velin.
      l_item-hu_item_number = <VEPO>-vepos.
      l_item-material       = <VEPO>-matnr.
      l_item-batch          = <VEPO>-charg.
      l_item-pack_qty       = <VEPO>-vemng.
      l_item-base_unit_qty  = <VEPO>-vemeh.
      l_item-plant          = <VEPO>-werks.
      l_item-stge_loc       = <VEPO>-lgort.

      CLEAR: return, i_return.
      CALL FUNCTION 'BAPI_HU_UNPACK'
        EXPORTING
          hukey      = l_exidv
          itemunpack = l_item
        TABLES
          return     = i_return.

      READ TABLE i_return INTO return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        message = return-message.
        RETURN.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDLOOP.
    IF sy-subrc <> 0.
      message = 'No hay posiciones a desembalar'(npd).
    ENDIF.
  ENDMETHOD.
METHOD visualizar.
    DATA: l_venum TYPE venum,
          i_venum TYPE hum_venum_t.

    IF venum IS INITIAL AND NOT exidv IS INITIAL.
      SELECT venum FROM vekp
        INTO l_venum
        UP TO 1 ROWS
       WHERE exidv   = exidv
         AND status <> '0060'
        ORDER BY venum DESCENDING.
      ENDSELECT.
      IF sy-subrc <> 0.
        SELECT venum FROM vekp
          INTO l_venum
          UP TO 1 ROWS
         WHERE exidv = exidv
          ORDER BY venum DESCENDING.
        ENDSELECT.
      ENDIF.

    ELSE.
      l_venum = venum.
    ENDIF.

    APPEND l_venum TO i_venum.
    CALL FUNCTION 'HU_DISPLAY'
      EXPORTING
*     IF_DISPLAY   = 'X'
*     IF_COMMIT    = ' '
        it_venum     = i_venum
*     IF_AR_HUS    = ' '
*     IF_EXIDV_FOR_DELETED       = ' '
* IMPORTING
*     EF_FCODE     =
*     EF_DATA_CHANGED            =
*     ET_MESSAGES  =
*     ET_HEADER    =
*     ET_ITEMS     =
      EXCEPTIONS
        no_hus_found = 1
        not_allowed  = 2
        fatal_error  = 3
        OTHERS       = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
