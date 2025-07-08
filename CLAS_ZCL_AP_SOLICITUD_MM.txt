class ZCL_AP_SOLICITUD_MM definition
  public
  create public .

public section.

  constants C_OBJECTCLAS type CDHDR-OBJECTCLAS value 'EINKBELEG' ##NO_TEXT.
  constants C_HIST_FACTURA type BEWTP value 'Q' ##NO_TEXT.
  constants C_HIST_ENTREGA type BEWTP value 'E' ##NO_TEXT.
  constants C_OBJETO type SRGBTBREL-TYPEID_A value 'BUS2105' ##NO_TEXT.
  constants C_HIST_SALIDAS type BEWTP value 'U' ##NO_TEXT.

  class-methods URLS_GOS_ST
    importing
      !EBELN type EKKO-EBELN
    returning
      value(TABLA) type ZTAB_URL_GOS .
  class-methods INSERTAR_URL_GOS_ST
    importing
      !URL type ANY
      !TITULO type ANY
      !BANFN type ANY .
  class-methods VISUALIZAR_PEDIDO_ST
    importing
      !EBELN type EKKO-EBELN
      !EBELP type EKPO-EBELP optional .
  class-methods GET_URL_POR_TITULO_ST
    importing
      !EBELN type EKKO-EBELN
      !TITULO type STRING
    returning
      value(URL) type STRING .
  class-methods VISUALIZAR
    importing
      !BANFN type BANFN .
  class-methods GET_TEXTO
    importing
      !BANFN type BANFN
      !BNFPO type BNFPO
      !ID type STXH-TDID
      !SPRAS type STXH-TDSPRAS optional
    returning
      value(LINEAS) type OIJ_TLINE .
  class-methods SAVE_TEXTO
    importing
      !BANFN type BANFN
      !BNFPO type BNFPO optional
      !ID type STXH-TDID
      !SPRAS type STXH-TDSPRAS optional
      !LINEAS type OIJ_TLINE .
  class-methods ATTA_GOS_ST
    importing
      !EBELN type EBELN
    returning
      value(TABLA) type ZTAB_URL_GOS .
  class-methods BORRAR_SOLICITUD
    importing
      !BANFN type BANFN
      !BNFPO type BNFPO optional
      !DELETE_IND type ELOEK default 'X'
      !CLOSED type EBAKZ default ''
      !COMMIT type ABAP_BOOL default 'X'
    returning
      value(MESSAGE) type BAPI_MSG .
protected section.
private section.

  data I_POHEADER type WRF_POHF_BAPIMEPOHEADER_TTY .
  data POHEADER type BAPIMEPOHEADER .
  data I_POITEM type WRF_POHF_BAPIMEPOITEM_TTY .
  data POITEM type BAPIMEPOITEM .
  data I_POHEADERX type WRF_POHF_BAPIMEPOHEADERX_TTY .
  data POHEADERX type BAPIMEPOHEADERX .
  data I_POITEMX type WRF_POHF_BAPIMEPOITEMX_TTY .
  data POITEMX type BAPIMEPOITEMX .
  data I_ERRORES type WTY_RETURN_TAB .
endclass. "ZCL_AP_SOLICITUD_MM definition
class ZCL_AP_SOLICITUD_MM implementation.
method ATTA_GOS_ST.

 data l_clave type srgbtbrel-instid_a.

 l_clave = ebeln.
 tabla = zcl_ap_gos=>atta_gos_st( tipo = c_objeto clave = l_clave ).

endmethod.
METHOD borrar_solicitud.
  DATA: i_items  TYPE TABLE OF bapieband,
        i_return TYPE TABLE OF bapireturn,
        l_item   TYPE bapieband,
        l_return TYPE bapireturn.

  CLEAR l_item.
  l_item-delete_ind = delete_ind.
  l_item-closed     = closed.
  IF bnfpo IS INITIAL.
    SELECT bnfpo FROM eban
      INTO l_item-preq_item
     WHERE banfn = banfn.
      APPEND l_item TO i_items.
    ENDSELECT.
  ELSE.
    l_item-preq_item = bnfpo.
    APPEND l_item TO i_items.
  ENDIF.

  CALL FUNCTION 'BAPI_REQUISITION_DELETE'
    EXPORTING
      number                      = banfn
    TABLES
      requisition_items_to_delete = i_items
      return                      = i_return.

  READ TABLE i_return INTO l_return WITH KEY type = 'E'. "#EC CI_STDSEQ
  IF sy-subrc = 0.
    message = l_return-message.
  ELSE.
    IF commit = 'X'.
      zcl_ap_dev=>commit( ).
    ENDIF.
  ENDIF.

ENDMETHOD.
method GET_TEXTO.
  DATA l_name TYPE stxh-tdname.

  CONCATENATE banfn bnfpo INTO l_name.
  lineas = zcl_ap_textos=>get_texto( id     = id
                                  name   = l_name
                                  spras  = spras
                                  object = 'EBAN' ).

endmethod.
method GET_URL_POR_TITULO_ST.
 DATA: l_clave TYPE srgbtbrel-instid_a.

 l_clave = ebeln.
 url = zcl_ap_gos=>get_url_por_titulo_st( tipo   = 'BUS2012'
                                       clave  = l_clave
                                       titulo = titulo ).

endmethod.
METHOD insertar_url_gos_st.
  DATA: l_clave  TYPE srgbtbrel-instid_a,
        l_titulo TYPE string,
        l_url    TYPE string.

  IF NOT banfn IS INITIAL.
    l_clave = banfn.
  ELSE.
    EXIT.
  ENDIF.
  l_url = url.
  l_titulo = titulo.
  zcl_ap_gos=>insertar_url_gos_st( tipo   = c_objeto
                                  clave  = l_clave
                                  titulo = l_titulo
                                  url    = l_url ).

ENDMETHOD.
method SAVE_TEXTO.
  DATA l_name TYPE stxh-tdname.

  IF bnfpo IS INITIAL.
    l_name = banfn.
    zcl_ap_textos=>save_texto( id     = id
                               name   = l_name
                               spras  = spras
                               object = 'EBANH'
                               lineas = lineas ).
  ELSE.
    CONCATENATE banfn bnfpo INTO l_name.
    zcl_ap_textos=>save_texto( id     = id
                            name   = l_name
                            spras  = spras
                            object = 'EBAN'
                            lineas = lineas ).
  ENDIF.

endmethod.
method URLS_GOS_ST.
 DATA l_clave TYPE srgbtbrel-instid_a.

 l_clave = ebeln.
 tabla = zcl_ap_gos=>urls_gos_st( tipo = 'BUS2012' clave = l_clave ).

endmethod.
METHOD visualizar.

*  SET PARAMETER ID 'BAN' FIELD banfn.
*  CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.

  CALL FUNCTION 'ME_DISPLAY_OBJECT_RQ'
    EXPORTING
      key = banfn.

ENDMETHOD.
method VISUALIZAR_PEDIDO_ST.

 CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
   EXPORTING
     i_ebeln              = ebeln
     i_ebelp              = ebelp
     i_enjoy              = 'X'
   EXCEPTIONS
     not_found            = 1
     no_authority         = 2
     invalid_call         = 3
     preview_not_possible = 4
     OTHERS               = 5.

endmethod.
