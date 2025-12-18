
class ZCL_AP_CONDICIONES definition
  public
  final
  create public .

public section.

  methods GET_DATOS_COND
    importing
      !KNUMH type KONH-KNUMH
      !INCLUIR_BORRADOS type ABAP_BOOL default ''
    exporting
      !KONH type KONH
      !I_KONP type KONP_T
      !I_KONM type MMPUR_T_KONM
      !MESSAGE type BAPI_MSG .
  methods COPIAR_CONDICION
    importing
      !KEY_FIELDS type KOMG
      !OPERACION type CHAR1
      !FINICIO type DATAB
      !FFIN type DATBI
      !REGISTROS type TY_KOMV
      !O_LOG type ref to ZCL_AP_LOG optional
      !KAPPL type KAPPL default 'V'
      !KOTABNR type KOTABNR
      !KSCHL type KSCHL
      !ESCALAS type COND_SCALE_T optional
    exporting
      !KNUMH_NEW type KNUMH
      !MESSAGE type BAPI_MSG .
  methods ACTUALIZAR_PRECIO
    importing
      !KAPPL type KAPPL default 'V'
      !KOTABNR type KOTABNR
      !KSCHL type KSCHL
      !REGISTRO type ANY
      !KBETR type KONP-KBETR optional
      !KPEIN type KONP-KPEIN optional
      !KMEIN type KONP-KMEIN optional
      !KONWA type KONP-KONWA optional
      !DATAB type DATAB optional
      !DATBI type DATBI optional
      !O_LOG type ref to ZCL_AP_LOG optional
      !ESCALAS type COND_SCALE_T optional
      !STFKZ type KONP-STFKZ optional
      !KZBZG type KONP-KZBZG optional
      !KONMS type KONP-KONMS default ''
      !ACCION type CHAR1 default 'C'
    exporting
      !MESSAGE type BAPI_MSG
      !KNUMH_NEW type KNUMH .
  methods VISUALIZAR
    importing
      !KNUMH type KONH-KNUMH
      !PANTALLA type CHAR1 default 'C' .
protected section.
private section.
endclass. "ZCL_AP_CONDICIONES definition
class ZCL_AP_CONDICIONES implementation.
  METHOD actualizar_precio.
    DATA: key_fields TYPE komg,
          registros  TYPE ty_komv.

    CLEAR: message, knumh_new.

    IF accion = 'C'.
      ASSIGN COMPONENT 'KNUMH' OF STRUCTURE registro TO FIELD-SYMBOL(<knumh>).
      IF sy-subrc NE 0.
        message = 'Datos origen no tienen campo KNUMH'.
      ENDIF.

      IF message IS INITIAL.
        get_datos_cond( EXPORTING knumh   = <knumh>
                        IMPORTING konh    = DATA(konh)
                                  i_konp  = DATA(i_konp)
                                  i_konm  = DATA(i_konm)
                                  message = message ).
      ENDIF.


      IF NOT message IS INITIAL.
        IF NOT o_log IS INITIAL.
          o_log->log( p1 = message msgty = 'E' ).
        ENDIF.
        RETURN.
      ENDIF.
    ELSEIF accion = 'I'.
      APPEND VALUE #( kopos = '01' kbetr = kbetr kpein = kpein kmein = kmein konwa = konwa ) TO i_konp.
    ENDIF.


    MOVE-CORRESPONDING registro TO key_fields.

    APPEND INITIAL LINE TO registros ASSIGNING FIELD-SYMBOL(<copy>).
    LOOP AT i_konp ASSIGNING FIELD-SYMBOL(<konp>).
      MOVE-CORRESPONDING <konp> TO <copy>.
      <copy>-knumv = <konp>-knumh.
      <copy>-knprs = <konp>-stfkz. "Clase de escala
      IF NOT stfkz IS INITIAL.
        <copy>-stfkz = stfkz.
      ENDIF.

* Si convertimos un registro sin escalas en uno con escalas, tiene que tener unidad
      IF NOT konms IS INITIAL.
        <copy>-konms = konms.
      ENDIF.
      IF NOT kzbzg  IS INITIAL.
        <copy>-kzbzg = kzbzg .
      ENDIF.
      <copy>-waers = <konp>-konwa.
      IF kbetr NE 0.
        <copy>-kbetr = kbetr.
      ENDIF.
      IF NOT kpein IS INITIAL.
        <copy>-kpein = kpein.
      ENDIF.
    ENDLOOP.

    IF datab IS INITIAL AND accion NE 'I'.
      DATA(l_operacion) = 'B'. "Actualizamos sólo precio
    ELSE.
      IF ( datab = konh-datab AND datbi = konh-datbi ) OR ( datab IS INITIAL AND datbi IS INITIAL ).
        l_operacion = 'B'. "La fecha de inicio es la misma, actualizamos y no creamos
      ELSE.
        l_operacion = 'A'. "Creamos
        konh-datab = datab.
        IF NOT datbi IS INITIAL.
          konh-datbi = datbi.
        ELSEIF konh-datbi < datab.
          konh-datbi = konh-datab.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT escalas IS INITIAL.
      DATA(i_escalas) = escalas.
      SELECT MAX( klfn1 ) FROM konm
        INTO @DATA(l_max_linea)
       WHERE knumh = @<knumh>.

      IF l_operacion = 'B'.
        l_operacion = 'A'. "En escalas siempre quiero actualizar que si no falla
      ENDIF.

      LOOP AT i_escalas ASSIGNING FIELD-SYMBOL(<escala>) WHERE updkz IS INITIAL.
        IF <escala>-kopos IS INITIAL.
          <escala>-kopos  = <konp>-kopos.
        ENDIF.
        IF l_operacion = 'A'.
          <escala>-updkz = 'I'.
          ADD 1 TO l_max_linea.
          <escala>-klfn1 = l_max_linea.
        ELSE.
          READ TABLE i_konm  ASSIGNING FIELD-SYMBOL(<konm>) WITH KEY kopos = <escala>-kopos
                                                                     klfn1 = <escala>-klfn1.
          IF sy-subrc NE 0.
            <escala>-updkz = 'I'.
            ADD 1 TO l_max_linea.
            <escala>-klfn1 = l_max_linea.
          ELSE.
            READ TABLE i_konp ASSIGNING <konp> WITH KEY knumh = <escala>-knumh
                                                     kopos = <escala>-kopos.
            IF sy-subrc = 0.
              <escala>-rv13akonwa = <konp>-konws.
              <escala>-konpkmein = <konp>-kmein.
              <escala>-konpkonws = <konp>-konws.
              <escala>-konpkonms = <konp>-konms.
            ENDIF.

            DATA(l_tabix) = sy-tabix.
            IF <konm>-kstbm = <escala>-kstbm AND NOT <escala>-klfn1 IS INITIAL.
              IF <konm>-kbetr = <escala>-kbetr.
                CLEAR <escala>-updkz.
              ELSE.
                <escala>-updkz = 'U'.
*                ADD 1 TO l_max_linea.
*                <escala>-klfn1 = l_max_linea.

*                <escala>-updkz = 'D'.
*                DATA(l_escala) = <escala>.
*                l_escala-updkz = 'I'.
*                ADD 1 TO l_max_linea.
*                l_escala-klfn1 = l_max_linea.
*                APPEND l_escala TO i_escalas.
              ENDIF.
            ELSE.
*            <escala>-updkz = 'U'.
              <escala>-updkz = 'I'.
              ADD 1 TO l_max_linea.
              <escala>-klfn1 = l_max_linea.
            ENDIF.
            DELETE i_konm INDEX l_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF l_operacion NE 'A'.
        LOOP AT i_konm ASSIGNING <konm>.
          APPEND INITIAL LINE TO i_escalas ASSIGNING <escala>.
          MOVE-CORRESPONDING <konm> TO <escala>.
          <escala>-updkz = 'D'.
        ENDLOOP.
      ENDIF.
    ENDIF.

    copiar_condicion( EXPORTING key_fields = key_fields
                                operacion  = l_operacion
                                finicio    = konh-datab
                                ffin       = konh-datbi
                                registros  = registros
                                escalas    = i_escalas
                                o_log      = o_log
                                kappl      = 'V'
                                kotabnr    = kotabnr
                                kschl      = kschl
                      IMPORTING message    = message
                                knumh_new  = knumh_new ).

  ENDMETHOD.
  METHOD copiar_condicion.
    DATA: e_komk      TYPE komk,
          e_komp      TYPE komp,
          e_datab     TYPE vake-datab,
          e_datbi     TYPE vake-datbi,
          e_prdat     TYPE vake-datbi,
          i_komv_idoc TYPE TABLE OF komv_idoc.

    CLEAR: message, knumh_new.

    READ TABLE registros ASSIGNING FIELD-SYMBOL(<reg>) INDEX 1.
    IF sy-subrc NE 0.
      message = 'No se han pasado registros a actualizar'.
    ELSEIF <reg>-knumv IS INITIAL and operacion ne 'A'.
      message = 'No se ha indicado el nº de registro de condición'.
    ENDIF.

    IF NOT message IS INITIAL.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = message msgty = 'E' ).
      ENDIF.
      RETURN.
    ENDIF.

    IF NOT escalas IS INITIAL.
      DATA(l_used_by_idoc) = 'X'.
      i_komv_idoc = VALUE #( ( kznep = '' ) ).
      DATA(l_keep_old_records) = 'X'.
      DATA(i_escalas) = escalas.

      LOOP AT i_escalas ASSIGNING FIELD-SYMBOL(<escala>).
        READ TABLE registros ASSIGNING <reg> WITH KEY knumh = <escala>-knumh
                                             kopos = <escala>-kopos.
        IF sy-subrc = 0.
          <escala>-rv13akonwa = <reg>-konws.
          <escala>-konpkmein = <reg>-kmein.
          <escala>-konpkonws = <reg>-konws.
          <escala>-konpkonms = <reg>-konms.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'RV_CONDITION_COPY'
      EXPORTING
        application                 = 'V'
        condition_table             = kotabnr
        condition_type              = kschl
        date_from                   = finicio
        date_to                     = ffin
        enqueue                     = 'X'
*       I_KOMK                      = ' '
*       I_KOMP                      = ' '
        key_fields                  = key_fields
        maintain_mode               = operacion
        no_authority_check          = 'X'
*       NO_FIELD_CHECK              = ' '
*       SELECTION_DATE              = '00000000'
        keep_old_records            = l_keep_old_records
*       MATERIAL_M                  =
        used_by_idoc                = l_used_by_idoc
*       I_KONA                      =
        overlap_confirmed           = 'X'
*       NO_DB_UPDATE                = ' '
*       USED_BY_RETAIL              = ' '
*       IT_KONPT                    =
      IMPORTING
        e_komk                      = e_komk
        e_komp                      = e_komp
        e_datab                     = e_datab
        e_datbi                     = e_datbi
        e_prdat                     = e_prdat
      TABLES
        copy_records                = registros
        copy_staffel                = i_escalas
        copy_recs_idoc              = i_komv_idoc
      EXCEPTIONS
        enqueue_on_record           = 1
        invalid_application         = 2
        invalid_condition_number    = 3
        invalid_condition_type      = 4
        no_authority_ekorg          = 5
        no_authority_kschl          = 6
        no_authority_vkorg          = 7
        no_selection                = 8
        table_not_valid             = 9
        no_material_for_settlement  = 10
        no_unit_for_period_cond     = 11
        no_unit_reference_magnitude = 12
        invalid_condition_table     = 13
        OTHERS                      = 14.
    IF sy-subrc <> 0.
      IF sy-msgty = 'E'.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      ELSE.
        message = |Error { sy-subrc } actualizando registro de condición|.
      ENDIF.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = message msgty = 'E' ).
      ENDIF.
      RETURN.
    ELSE.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = 'Se ha actualizado el registro de condición' msgty = 'S' ).
      ENDIF.
    ENDIF.

    DATA knumh_map TYPE TABLE OF knumh_comp.
    CALL FUNCTION 'RV_CONDITION_SAVE'
      TABLES
        knumh_map = knumh_map.

    CALL FUNCTION 'RV_CONDITION_RESET'.
    COMMIT WORK AND WAIT.


* Ha veces no se copia bien las unidades, si es así corregimos algunos valroes
    LOOP AT knumh_map ASSIGNING FIELD-SYMBOL(<knumh>) WHERE knumh_new NE '' AND knumh_new(1) NE '$'.
      DO 2 TIMES.
        SELECT * FROM konp
          INTO TABLE @DATA(i_konp)
         WHERE knumh = @<knumh>-knumh_new.
        IF sy-subrc = 0.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.
      IF NOT i_konp IS INITIAL.
        knumh_new = <knumh>-knumh_new.


* En las escalas no limita KONH!
        IF NOT escalas IS INITIAL.
          ASSIGN registros[ 1 ] TO FIELD-SYMBOL(<regc>).
          IF sy-subrc = 0.
            IF <regc>-knumh NE knumh_new.
              SELECT SINGLE datbi FROM konh
                INTO @DATA(l_datbi)
               WHERE knumh = @<reg>-knumh
                 AND datbi >= @finicio
                 AND datab <= @ffin.
              IF sy-subrc = 0.
                l_datbi = finicio - 1.
                UPDATE konh
                   SET datbi = l_datbi
                 WHERE knumh = <regc>-knumh.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        IF lines( i_konp ) = 1 AND lines( registros ) = 1.
          LOOP AT i_konp ASSIGNING FIELD-SYMBOL(<konp>).
            ASSIGN registros[ kopos = <konp>-kopos ] TO FIELD-SYMBOL(<registro>).
            IF sy-subrc = 0.
              IF <konp>-konms NE <registro>-konms OR
                 <konp>-kmein NE <registro>-kmein.
                UPDATE konp
                   SET konms = <registro>-konms
                       kmein = <registro>-kmein
                 WHERE knumh = <konp>-knumh
                   AND kopos = <konp>-kopos.

                COMMIT WORK AND WAIT.
              ENDIF.

*              DATA l_konp TYPE konp.
*              SELECT SINGLE kznep FROM konp
*                INTO CORRESPONDING FIELDS OF l_konp
*                   WHERE knumh = <registro>-knumh
*                     AND kopos = <registro>-kopos.
*              IF sy-subrc = 0 AND l_konp-kznep NE <konp>-kznep.
*                UPDATE konp
*                   SET kznep = l_konp-kznep
*                 WHERE knumh = <konp>-knumh
*                   AND kopos = <konp>-kopos.
*
*                COMMIT WORK AND WAIT.
*              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_datos_cond.

    CLEAR: konh, i_konp, i_konm, message.

    SELECT SINGLE * FROM konh
      INTO konh
      WHERE knumh = knumh.
    IF sy-subrc NE 0.
      message = 'No existe la condición'.
      RETURN.
    ENDIF.

    IF incluir_borrados IS INITIAL.
      DATA r_loevm_ko TYPE RANGE OF konp-loevm_ko.
      r_loevm_ko = VALUE #( ( option = 'EQ' sign = 'I' low = '' ) ).
    ENDIF.

    SELECT * FROM konp
      INTO TABLE i_konp
      WHERE knumh = knumh
       AND loevm_ko IN r_loevm_ko
     ORDER BY PRIMARY KEY.
    IF sy-subrc NE 0.
      IF incluir_borrados IS INITIAL.
        message = 'La condición está borrada'.
      ENDIF.
    ENDIF.

    SELECT * FROM konm
      INTO TABLE i_konm
      WHERE knumh = knumh
      ORDER BY PRIMARY KEY.


  ENDMETHOD.
  METHOD visualizar.

    CALL FUNCTION 'RV_CONDITION_RESET'.

    CALL FUNCTION 'RV_CONDITION_RECORD'
      EXPORTING
        condition_number   = knumh
        condition_use      = 'A'
        first_screen       = pantalla
*       item_number        = '01'
        maintain_mode      = 'C' "B Editar / C Visualizar
*       POPUP_CONFIRM      = ' '
*       SELECTION_DATE     =
*       USE_SELECTION_DATE =
*       CHECK_MEMORY       = ' '
      EXCEPTIONS
        no_existing_record = 1
        OTHERS             = 2.

    CALL FUNCTION 'RV_CONDITION_RESET'.
  ENDMETHOD.