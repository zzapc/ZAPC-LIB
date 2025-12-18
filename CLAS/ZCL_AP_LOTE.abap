
CLASS zcl_ap_lote DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA i_return TYPE bapiret2_t.
    DATA return   TYPE bapiret2.
    DATA mensaje  TYPE bapi_msg.

    CLASS-METHODS ver
      IMPORTING matnr TYPE matnr
                charg TYPE charg_d
                werks TYPE werks_d  DEFAULT ''
                tcode TYPE sy-tcode DEFAULT 'MSC3N'.

    CLASS-METHODS get_fcaducidad
      IMPORTING matnr        TYPE matnr
                charg        TYPE charg_d
                werks        TYPE werks_d DEFAULT ''
      RETURNING VALUE(vfdat) TYPE vfdat.

    CLASS-METHODS get_lote_proveedor
      IMPORTING matnr        TYPE matnr
                charg        TYPE charg_d
      RETURNING VALUE(licha) TYPE mcha-licha.

    METHODS crear
      IMPORTING matnr            TYPE any
                charg            TYPE charg_d
                werks            TYPE werks_d           OPTIONAL
                vfdat            TYPE vfdat             OPTIONAL
                hsdat            TYPE hsdat             DEFAULT sy-datum
                lichn            TYPE any               DEFAULT ''
                fvdt1            TYPE mcha-fvdt1        OPTIONAL
                verab            TYPE verab             OPTIONAL
                o_log            TYPE REF TO zcl_ap_log OPTIONAL
                lifnr            TYPE lifnr             OPTIONAL
                !commit          TYPE abap_bool         DEFAULT 'X'
      RETURNING VALUE(atributos) TYPE bapibatchatt.

    METHODS modificar
      IMPORTING matnr            TYPE any
                charg            TYPE charg_d
                werks            TYPE werks_d           OPTIONAL
                vfdat            TYPE vfdat             OPTIONAL
                hsdat            TYPE hsdat             OPTIONAL
                forzar_si_error  TYPE abap_bool         DEFAULT ''
                lifnr            TYPE lifnr             OPTIONAL
                commit_work      TYPE abap_bool         DEFAULT 'X'
                qndat            TYPE qnpdat            OPTIONAL
                fvdt1            TYPE fvdat             OPTIONAL
                o_log            TYPE REF TO zcl_ap_log OPTIONAL
      RETURNING VALUE(atributos) TYPE bapibatchatt.

    METHODS detalle
      IMPORTING matnr            TYPE any
                charg            TYPE charg_d
                werks            TYPE werks_d OPTIONAL
      RETURNING VALUE(atributos) TYPE bapibatchatt.

    CLASS-METHODS get_ctd_total
      IMPORTING matnr           TYPE matnr
                charg           TYPE charg_d
                libre           TYPE abap_bool  DEFAULT 'X'
                bloqueado       TYPE abap_bool  DEFAULT 'X'
                calidad         TYPE abap_bool  DEFAULT 'X'
                bestq           TYPE vepo-bestq DEFAULT '*'
                otros           TYPE abap_bool  DEFAULT 'X'
                traslado        TYPE abap_bool  DEFAULT 'X'
                werks           TYPE werks_d    DEFAULT ''
                lgort           TYPE lgort_d    DEFAULT ''
                meins           TYPE meins      DEFAULT ''
      RETURNING VALUE(cantidad) TYPE mchb-clabs.

    CLASS-METHODS modificar_st
      IMPORTING matnr            TYPE matnr
                charg            TYPE charg_d
                werks            TYPE werks_d           OPTIONAL
                vfdat            TYPE vfdat             OPTIONAL
                hsdat            TYPE hsdat             OPTIONAL
                forzar_si_error  TYPE abap_bool         DEFAULT ''
                lifnr            TYPE lifnr             OPTIONAL
                commit_work      TYPE abap_bool         DEFAULT 'X'
                qndat            TYPE qnpdat            OPTIONAL
                fvdt1            TYPE fvdat             OPTIONAL
                o_log            TYPE REF TO zcl_ap_log OPTIONAL
      RETURNING VALUE(atributos) TYPE bapibatchatt.

    CLASS-METHODS get_fproduccion
      IMPORTING matnr        TYPE matnr
                charg        TYPE charg_d
                werks        TYPE werks_d DEFAULT ''
      RETURNING VALUE(hsdat) TYPE hsdat.

    CLASS-METHODS get_ctd_caducada
      IMPORTING matnr           TYPE matnr
                fecha           TYPE datum     DEFAULT sy-datum
                werks           TYPE werks_d   DEFAULT ''
                libre           TYPE abap_bool DEFAULT 'X'
                bloqueado       TYPE abap_bool DEFAULT 'X'
                calidad         TYPE abap_bool DEFAULT 'X'
                show_popup      TYPE abap_bool DEFAULT ''
      RETURNING VALUE(cantidad) TYPE labst.

    CLASS-METHODS info_lotes_fcad
      IMPORTING matnr               TYPE matnr
                werks               TYPE werks_d         OPTIONAL
                libre               TYPE abap_bool       DEFAULT 'X'
                bloqueado           TYPE abap_bool       DEFAULT 'X'
                calidad             TYPE abap_bool       DEFAULT 'X'
                r_lgort             TYPE range_t_lgort_d OPTIONAL
                solo_fcad_informado TYPE abap_bool       DEFAULT 'X'
                mcha                TYPE abap_bool       DEFAULT ''
      RETURNING VALUE(i_lotes)      TYPE zlotes_fcad_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
endclass. "ZCL_AP_LOTE definition
class ZCL_AP_LOTE implementation.
  METHOD crear.
    " TODO: parameter COMMIT is never used (ABAP cleaner)

    DATA l_atributos TYPE bapibatchatt.
    DATA l_batch     TYPE charg_d.
    DATA l_material  TYPE bapibatchkey-material.

    CLEAR: i_return,
           atributos.

    CLEAR l_atributos.
    l_atributos-expirydate = vfdat.
    l_atributos-prod_date  = hsdat.
    l_atributos-available  = verab.
    l_atributos-vendor_no  = lifnr.
    l_atributos-vendrbatch = lichn.
    l_atributos-free_date1 = fvdt1.

    l_material = matnr.
    CALL FUNCTION 'BAPI_BATCH_CREATE'
      EXPORTING material        = l_material
                batch           = charg
                plant           = werks
                batchattributes = l_atributos
*                BATCHCONTROLFIELDS =
*                BATCHSTORAGELOCATION =
*                INTERNALNUMBERCOM =
*                EXTENSION1      =
*                MATERIAL_EVG    =
      IMPORTING batch           = l_batch
                batchattributes = atributos
      TABLES    return          = i_return.

    IF l_batch IS NOT INITIAL.
      zcl_ap_dev=>commit( ).
    ENDIF.

    CLEAR: return,
           mensaje.
    READ TABLE i_return INTO return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      mensaje = return-message.
      IF o_log IS NOT INITIAL.
        o_log->log( p1 = 'Error creando lote'
                    p2 = matnr
                    p3 = charg
                    p4 = mensaje ).
      ENDIF.
    ELSE.
      IF o_log IS NOT INITIAL.
        o_log->log( p1    = 'Se ha creado lote'
                    p2    = matnr
                    p3    = charg
                    p4    = 'F.Cad='
                    p5    = vfdat
                    p6    = 'F.Prod='
                    p7    = hsdat
                    msgty = 'S' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD detalle.
    DATA l_material TYPE bapibatchkey-material.

    CLEAR: i_return,
           atributos.

    l_material = matnr.
    CALL FUNCTION 'BAPI_BATCH_GET_DETAIL'
      EXPORTING material        = l_material
                batch           = charg
                plant           = werks
*                MATERIAL_EVG    =
      IMPORTING batchattributes = atributos
*                BATCHSTATUS     =
      TABLES    return          = i_return.
  ENDMETHOD.
  METHOD get_ctd_caducada.
    DATA i_lotes TYPE zlotes_fcad_t.
    FIELD-SYMBOLS <lote> TYPE zlotes_fcad.

    i_lotes = info_lotes_fcad( matnr     = matnr
                               werks     = werks
                               libre     = libre
                               bloqueado = bloqueado
                               calidad   = calidad ).

    DELETE i_lotes WHERE vfdat >= fecha.

    CLEAR cantidad.
    LOOP AT i_lotes ASSIGNING <lote>.
      cantidad = cantidad + <lote>-clabs + <lote>-cinsm + <lote>-cspem.
    ENDLOOP.

    IF show_popup = 'X'.
      zcl_ap_alv=>show_popup_st( CHANGING t_tabla = i_lotes ).
    ENDIF.
  ENDMETHOD.
  METHOD get_ctd_total.
    DATA l_mchb      TYPE mchb.
    DATA l_libre     TYPE c LENGTH 1.
    DATA l_bloqueado TYPE c LENGTH 1.
    DATA l_calidad   TYPE c LENGTH 1.
    DATA l_traslado  TYPE c LENGTH 1.
    DATA l_otros     TYPE c LENGTH 1.
    DATA r_werks     TYPE RANGE OF werks_d.
    DATA r_lgort     TYPE RANGE OF lgort_d.
    DATA lr_werks    LIKE LINE OF r_werks.
    DATA lr_lgort    LIKE LINE OF r_lgort.

    IF bestq = '*'.
      l_libre = libre.
      l_bloqueado = bloqueado.
      l_calidad = calidad.
      l_traslado = traslado.
      l_otros = otros.
    ELSEIF bestq = ''.
      l_libre = 'X'.
    ELSEIF bestq = 'S'.
      l_bloqueado = 'X'.
    ELSEIF bestq = 'Q'.
      l_calidad = 'X'.
    ENDIF.

    IF werks IS NOT INITIAL.
      CLEAR lr_werks.
      lr_werks-option = 'EQ'.
      lr_werks-sign   = 'I'.
      lr_werks-low    = werks.
      APPEND lr_werks TO r_werks.
    ENDIF.

    IF lgort IS NOT INITIAL.
      CLEAR lr_lgort.
      lr_lgort-option = 'EQ'.
      lr_lgort-sign   = 'I'.
      lr_lgort-low    = lgort.
      APPEND lr_lgort TO r_lgort.
    ENDIF.

    SELECT SINGLE SUM( clabs )
                  SUM( cumlm )
                  SUM( cinsm )
                  SUM( ceinm )
                  SUM( cspem )
                  SUM( cretm )
      FROM mchb
      INTO ( l_mchb-clabs, l_mchb-cumlm, l_mchb-cinsm, l_mchb-ceinm, l_mchb-cspem, l_mchb-cretm )
      WHERE matnr  = matnr
        AND charg  = charg
        AND werks IN r_werks
        AND lgort IN r_lgort.

    IF l_libre = 'X'.
      cantidad = cantidad + l_mchb-clabs.
    ENDIF.
    IF l_calidad = 'X'.
      cantidad = cantidad + l_mchb-cinsm.
    ENDIF.
    IF l_bloqueado = 'X'.
      cantidad = cantidad + l_mchb-cspem.
    ENDIF.
    IF l_traslado = 'X'.
      cantidad = cantidad + l_mchb-cumlm.
    ENDIF.
    IF l_otros = 'X'.
      cantidad = cantidad + l_mchb-ceinm + l_mchb-cretm.
    ENDIF.

    IF meins IS NOT INITIAL AND cantidad <> 0.
      DATA(l_umb) = zcl_ap_material=>get_unidad_base( matnr ).
      IF l_umb <> meins.
        cantidad = zcl_ap_material=>convertir_unidad( matnr          = matnr
                                                      cantidad       = cantidad
                                                      unidad_origen  = l_umb
                                                      unidad_destino = meins ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_fcaducidad.
    IF werks IS INITIAL.
      SELECT SINGLE vfdat FROM mch1
        INTO vfdat
        WHERE matnr = matnr
          AND charg = charg.
      IF vfdat IS INITIAL.
        SELECT SINGLE vfdat FROM mcha
          INTO vfdat
          WHERE matnr = matnr
            AND werks = werks
            AND charg = charg.
      ENDIF.
    ELSE.
      SELECT SINGLE vfdat FROM mcha
        INTO vfdat
        WHERE matnr = matnr
          AND werks = werks
          AND charg = charg.
      IF vfdat IS INITIAL.
        SELECT SINGLE vfdat FROM mch1
          INTO vfdat
          WHERE matnr = matnr
            AND charg = charg.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_fproduccion.
    IF werks IS INITIAL.
      SELECT SINGLE hsdat FROM mch1
        INTO hsdat
        WHERE matnr = matnr
          AND charg = charg.
      IF hsdat IS INITIAL.
        SELECT SINGLE hsdat FROM mcha
          INTO hsdat
          WHERE matnr = matnr
            AND werks = werks
            AND charg = charg.
      ENDIF.
    ELSE.
      SELECT SINGLE hsdat FROM mcha
        INTO hsdat
        WHERE matnr = matnr
          AND werks = werks
          AND charg = charg.
      IF hsdat IS INITIAL.
        SELECT SINGLE hsdat FROM mch1
          INTO hsdat
          WHERE matnr = matnr
            AND charg = charg.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_lote_proveedor.
    SELECT licha FROM mcha
      INTO licha
      UP TO 1 ROWS
      WHERE matnr = matnr
        AND charg = charg
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF licha IS INITIAL.
      SELECT SINGLE licha FROM mch1
        INTO licha
        WHERE matnr  = matnr
          AND charg  = charg
          AND licha <> ''.
    ENDIF.
  ENDMETHOD.
  METHOD info_lotes_fcad.
    DATA r_werks  TYPE RANGE OF werks_d.
    DATA lr_werks LIKE LINE OF r_werks.
    DATA l_meins  TYPE meins.

    FIELD-SYMBOLS <lotes> TYPE zlotes_fcad.

    IF werks IS NOT INITIAL.
      CLEAR lr_werks.
      lr_werks-option = 'EQ'.
      lr_werks-sign   = 'I'.
      lr_werks-low    = werks.
      COLLECT lr_werks INTO r_werks.
    ENDIF.

    " Busco los lotes con cantidad del material
    SELECT * FROM mchb
      INTO CORRESPONDING FIELDS OF TABLE i_lotes
      WHERE matnr  = matnr
        AND werks IN r_werks
        AND lgort IN r_lgort
        AND ( clabs > 0 OR cinsm > 0 OR cspem > 0 ).

    LOOP AT i_lotes ASSIGNING <lotes>.
      IF libre IS INITIAL.
        <lotes>-clabs = 0.
      ENDIF.
      IF bloqueado IS INITIAL.
        <lotes>-cspem = 0.
      ENDIF.
      IF calidad IS INITIAL.
        <lotes>-cinsm = 0.
      ENDIF.
    ENDLOOP.

    DELETE i_lotes WHERE clabs = 0 AND cinsm = 0 AND cspem = 0.

    IF i_lotes IS INITIAL.
      RETURN.
    ENDIF.

    l_meins = zcl_ap_material=>get_unidad_base( matnr ).

    LOOP AT i_lotes ASSIGNING <lotes>.
      <lotes>-meins = l_meins.

      IF mcha IS INITIAL.
        SELECT SINGLE vfdat hsdat FROM mch1
          INTO ( <lotes>-vfdat, <lotes>-hsdat )
          WHERE matnr = <lotes>-matnr
            AND charg = <lotes>-charg.
      ELSE.
        SELECT SINGLE vfdat hsdat FROM mcha
          INTO ( <lotes>-vfdat, <lotes>-hsdat )
          WHERE matnr = <lotes>-matnr
            AND charg = <lotes>-charg
            AND werks = werks.
      ENDIF.
    ENDLOOP.

    IF solo_fcad_informado = 'X'.
      DELETE i_lotes WHERE vfdat IS INITIAL.
    ENDIF.

    SORT i_lotes BY vfdat.
  ENDMETHOD.
  METHOD modificar.
    DATA l_atributos     TYPE bapibatchatt.
    DATA l_atributosx    TYPE bapibatchattx.
    DATA l_atributos_old TYPE bapibatchatt.
    DATA l_cambios       TYPE c LENGTH 1.
    DATA l_material      TYPE bapibatchkey-material.

    CLEAR: i_return,
           return,
           atributos.

    l_atributos_old = detalle( matnr = matnr
                               charg = charg
                               werks = werks ).

    CLEAR l_atributos.

    IF vfdat IS NOT INITIAL.
      IF l_atributos_old-expirydate <> vfdat.
        l_atributos-expirydate = vfdat.
        l_atributosx-expirydate = 'X'.
        l_cambios = 'X'.
      ENDIF.
    ENDIF.

    IF hsdat IS NOT INITIAL.
      IF l_atributos_old-prod_date <> hsdat.
        l_atributos-prod_date = hsdat.
        l_atributosx-prod_date = 'X'.
        l_cambios = 'X'.
      ENDIF.
    ENDIF.

    IF lifnr IS NOT INITIAL.
      IF l_atributos_old-vendor_no <> lifnr.
        l_atributos-vendor_no = lifnr.
        l_atributosx-vendor_no = 'X'.
        l_cambios = 'X'.
      ENDIF.
    ENDIF.

    IF qndat IS NOT INITIAL.
      IF l_atributos_old-nextinspec <> qndat.
        l_atributos-nextinspec = qndat.
        l_atributosx-nextinspec = 'X'.
        l_cambios = 'X'.
      ENDIF.
    ENDIF.

    IF fvdt1 IS NOT INITIAL.
      IF l_atributos_old-free_date1 <> fvdt1.
        l_atributos-free_date1 = fvdt1.
        l_atributosx-free_date1 = 'X'.
        l_cambios = 'X'.
      ENDIF.
    ENDIF.

    IF l_cambios = 'X'.
      l_material = matnr.
      CALL FUNCTION 'BAPI_BATCH_CHANGE'
        EXPORTING material         = l_material
                  batch            = charg
                  plant            = werks
                  batchattributes  = l_atributos
                  batchattributesx = l_atributosx
        IMPORTING batchattributes  = atributos
        TABLES    return           = i_return.

      READ TABLE i_return INTO return WITH KEY type = 'E'.
      IF return IS INITIAL.
        IF o_log IS NOT INITIAL.
          o_log->log( p1    = 'Se ha modificado lote'
                      p2    = matnr
                      p3    = charg
                      p4    = 'F.Cad='
                      p5    = vfdat
                      p6    = 'F.Prod='
                      p7    = hsdat
                      msgty = 'S' ).
        ENDIF.

        IF commit_work = 'X'.
          zcl_ap_dev=>commit( ).
        ENDIF.
      ELSE.
        IF o_log IS NOT INITIAL.
          o_log->log( p1 = 'Error modificando lote lote'
                      p2 = matnr
                      p3 = charg
                      p4 = mensaje
                      p5 = 'F.Cad='
                      p6 = vfdat
                      p7 = 'F.Prod='
                      p8 = hsdat  ).
        ENDIF.

        IF forzar_si_error = 'X'.
          IF l_atributosx-expirydate = 'X'.
            IF o_log IS NOT INITIAL.
              o_log->log( p1    = 'Se fuerza nueva fecha de caducidad'
                          p2    = vfdat
                          msgty = 'W' ).
            ENDIF.
            UPDATE mch1
               SET vfdat = vfdat
             WHERE matnr = matnr
               AND charg = charg.
            IF sy-subrc = 0.
              UPDATE mcha
                 SET vfdat = vfdat
               WHERE matnr = matnr
                 AND charg = charg
                 AND werks = werks.
              atributos-expirydate = vfdat.
            ENDIF.
          ENDIF.

          IF l_atributosx-prod_date = 'X'.
            IF o_log IS NOT INITIAL.
              o_log->log( p1    = 'Se ha fuerza nueva fecha de produccion'
                          p2    = vfdat
                          msgty = 'W' ).
            ENDIF.
            UPDATE mch1
               SET hsdat = hsdat
             WHERE matnr = matnr
               AND charg = charg.
            IF sy-subrc = 0.
              UPDATE mcha
                 SET hsdat = hsdat
               WHERE matnr = matnr
                 AND charg = charg
                 AND werks = werks.
              atributos-prod_date = hsdat.
            ENDIF.
          ENDIF.

          IF l_atributosx-free_date1 = 'X'.
            UPDATE mch1
               SET fvdt1 = fvdt1
             WHERE matnr = matnr
               AND charg = charg.
            IF sy-subrc = 0.
              UPDATE mcha
                 SET fvdt1 = fvdt1
               WHERE matnr = matnr
                 AND charg = charg
                 AND werks = werks.
              atributos-free_date1 = hsdat.
            ENDIF.
          ENDIF.

          IF l_atributosx-nextinspec = 'X'.
            UPDATE mch1
               SET qndat = qndat
             WHERE matnr = matnr
               AND charg = charg.
            IF sy-subrc = 0.
              UPDATE mcha
                 SET qndat = qndat
               WHERE matnr = matnr
                 AND charg = charg
                 AND werks = werks.
              atributos-nextinspec = qndat.
            ENDIF.
          ENDIF.
          IF commit_work = 'X'.
            zcl_ap_dev=>commit( ).
          ENDIF.
        ELSE.
          READ TABLE i_return INTO return WITH KEY type = 'E'.
        ENDIF.
      ENDIF.
    ELSE.
      atributos = l_atributos_old.
    ENDIF.
  ENDMETHOD.
  METHOD modificar_st.
    DATA o_lote TYPE REF TO zcl_ap_lote.

    o_lote = NEW #( ).

    atributos = o_lote->modificar( matnr           = matnr
                                   werks           = werks
                                   charg           = charg
                                   vfdat           = vfdat
                                   hsdat           = hsdat
                                   qndat           = qndat
                                   lifnr           = lifnr
                                   fvdt1           = fvdt1
                                   commit_work     = commit_work
                                   forzar_si_error = forzar_si_error
                                   o_log           = o_log ).
  ENDMETHOD.
  METHOD ver.
    DATA o_bi      TYPE REF TO zcl_ap_batch_input.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_mensaje TYPE bapireturn1-message.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    SET PARAMETER ID 'MAT' FIELD matnr.
    SET PARAMETER ID 'CHA' FIELD charg.
    SET PARAMETER ID 'WRK' FIELD werks.

    o_bi->dynpro( program = 'SAPLCHRG'
                  dynpro  = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE'
                  valor = '/00' ).
    o_bi->campos( campo = 'DFBATCH-MATNR'
                  valor = matnr ). " Número de material
    o_bi->campos( campo = 'DFBATCH-CHARG'
                  valor = charg ). " Número de lote
    o_bi->campos( campo = 'DFBATCH-WERKS'
                  valor = werks ). " Centro
    o_bi->campos( campo = 'DFBATCH-LGORT'
                  valor = '' ). " Almacén

    l_mensaje = o_bi->llamar_transaccion( tcode = tcode
                                          modo  = 'E' ).
  ENDMETHOD.
