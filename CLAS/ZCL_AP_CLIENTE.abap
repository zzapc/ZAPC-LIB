class ZCL_AP_CLIENTE definition
  public
  create public .

public section.

  class-methods URLS_GOS_ST
    importing
      !KUNNR type KUNNR
    returning
      value(TABLA) type ZTAB_URL_GOS .
  class-methods INSERTAR_URL_GOS_ST
    importing
      !KUNNR type KUNNR
      !URL type STRING
      !TITULO type STRING .
  class-methods VISUALIZAR_CLIENTE_ST
    importing
      !KUNNR type KUNNR .
  class-methods GET_URL_POR_TITULO_ST
    importing
      !KUNNR type KUNNR
      !TITULO type STRING
    returning
      value(URL) type STRING .
  class-methods GET_EMAILS
    importing
      !KUNNR type KUNNR
      !REMARK type ANY default ''
    returning
      value(EMAILS) type ISU_ADSMTP_TAB .
  class-methods GET_EMAIL
    importing
      !KUNNR type KUNNR
      !LISTA type ABAP_BOOL default ''
      !REMARK type ANY default ''
      !FLGDEFAULT type ABAP_BOOL default ''
    returning
      value(EMAIL) type AD_SMTPADR .
  class-methods GET_NOMBRE
    importing
      !KUNNR type KUNNR
    returning
      value(NAME1) type NAME1 .
  class-methods GET_DIRECCION
    importing
      !KUNNR type KUNNR optional
      !ADRNR type ADRNR optional
    returning
      value(DIRECCION) type ADDR1_DATA .
  methods GET_KNA1
    importing
      !KUNNR type KUNNR
    returning
      value(KNA1) type KNA1 .
  methods GET_NOMBRE_KNA1
    importing
      !KUNNR type KUNNR
    returning
      value(NAME1) type KNA1-NAME1 .
  class-methods GET_EMAILS_LISTA
    importing
      !KUNNR type KUNNR
      !REMARK type ANY default ''
    returning
      value(EMAIL) type STRING .
  class-methods GET_TEXTO_STRING
    importing
      !KUNNR type KUNNR
      !ID type STXH-TDID
      !SPRAS type SPRAS default SY-LANGU
    returning
      value(STRING) type STRING .
  class-methods SET_TEXTO_STRING
    importing
      !KUNNR type KUNNR
      !ID type STXH-TDID
      !SPRAS type SPRAS default ''
      !STRING type STRING .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA i_kna1 TYPE cif_kna1_tab.
endclass. "ZCL_AP_CLIENTE definition
class ZCL_AP_CLIENTE implementation.
  METHOD get_direccion.
    DATA l_adrnr TYPE adrnr.

    IF adrnr IS INITIAL.
      SELECT SINGLE adrnr FROM kna1
        INTO l_adrnr
       WHERE kunnr = kunnr.
    ELSE.
      l_adrnr = adrnr.
    ENDIF.

    direccion = zcl_ap_direcciones=>get_direccion2( l_adrnr ).
  ENDMETHOD.
  METHOD get_email.
    DATA l_adrnr TYPE lfa1-adrnr.

    SELECT SINGLE adrnr FROM kna1
      INTO l_adrnr
     WHERE kunnr = kunnr.
    IF sy-subrc = 0.
      IF flgdefault = 'X' AND remark IS INITIAL AND lista IS INITIAL.
        email = zcl_ap_direcciones=>get_email( adrnr = l_adrnr ).
      ELSE.
        email = zcl_ap_direcciones=>get_email( adrnr = l_adrnr remark = remark lista = lista ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_emails.
    DATA l_adrnr TYPE lfa1-adrnr.

    SELECT SINGLE adrnr FROM kna1
      INTO l_adrnr
     WHERE kunnr = kunnr.
    IF sy-subrc = 0.
      emails = zcl_ap_direcciones=>get_emails( adrnr = l_adrnr remark = remark ).
    ENDIF.
  ENDMETHOD.
  METHOD get_emails_lista.
    DATA: i_emails TYPE isu_adsmtp_tab,
          l_email  TYPE adsmtp.

    CLEAR email.
    i_emails = get_emails( kunnr = kunnr remark = remark ).

    LOOP AT i_emails INTO l_email.
      IF email IS INITIAL.
        email = l_email-smtp_addr.
      ELSE.
        CONCATENATE email ',' l_email-smtp_addr INTO email.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_kna1.
    CLEAR kna1.

    READ TABLE i_kna1 INTO kna1 WITH KEY kunnr = kunnr BINARY SEARCH.
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM kna1
        INTO kna1
       WHERE kunnr = kunnr.
      IF sy-subrc = 0.
        APPEND kna1 TO i_kna1.
        SORT i_kna1.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_nombre.
    CLEAR name1.
    SELECT SINGLE name1 FROM kna1
      INTO name1
     WHERE kunnr = kunnr.
  ENDMETHOD.
  METHOD get_nombre_kna1.
    DATA kna1 TYPE kna1.

    kna1 = get_kna1( kunnr ).
    name1 = kna1-name1.
  ENDMETHOD.
  METHOD get_texto_string.
    string = zcl_ap_textos=>get_texto_string( id = id object = 'KNA1' name = kunnr spras = spras ).
  ENDMETHOD.
  METHOD get_url_por_titulo_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    l_clave = kunnr.
    url = zcl_ap_gos=>get_url_por_titulo_st( tipo   = 'KNA1'
                                          clave  = l_clave
                                          titulo = titulo ).
  ENDMETHOD.
  METHOD insertar_url_gos_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    l_clave = kunnr.
    zcl_ap_gos=>insertar_url_gos_st( tipo   = 'KNA1'
                                  clave  = l_clave
                                  titulo = titulo
                                  url    = url ).
  ENDMETHOD.
  METHOD set_texto_string.

    zcl_ap_textos=>save_texto_string( id = id object = 'KNA1' name = kunnr spras = spras string = string ).

  ENDMETHOD.
  METHOD urls_gos_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    l_clave = kunnr.
    tabla = zcl_ap_gos=>urls_gos_st( tipo = 'KNA1' clave = l_clave ).
  ENDMETHOD.
  METHOD visualizar_cliente_st.
    SET PARAMETER ID 'KUN' FIELD kunnr.
    CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN.
  ENDMETHOD.
