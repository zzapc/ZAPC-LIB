
class ZCL_AP_PROXY definition
  public
  create public .

public section.

  methods GET_PAYLOAD
    importing
      !MSGID type SXMSMKEY-MSGID
      !PID type SXMSMKEY-PID default ''
      !SHOW type ABAP_BOOL default ''
      !VERSION type NUMC3 default '000'
    exporting
      !MESSAGE type BAPI_MSG
      value(PAYLOAD) type XSTRING
      !PAYLOAD_STRING type STRING .
  methods DATOS_FROM_PAYLOAD
    importing
      !PAYLOAD type STRING
    exporting
      !DATOS type ANY
      !MESSAGE type BAPI_MSG .
  class-methods GET_MSGGUID
    returning
      value(MSGGUID) type SXMSPEGUID .
protected section.
private section.
endclass. "ZCL_AP_PROXY definition
class ZCL_AP_PROXY implementation.
  METHOD datos_from_payload.

    DATA(xml) = payload.
    DATA(ini) = zcl_ap_regexp=>buscar_patron( string = xml patron = '<ns0:([^>]+)>' ).
    LOOP AT ini ASSIGNING FIELD-SYMBOL(<ini>).
      REPLACE ALL OCCURRENCES OF <ini> IN xml WITH '<?xml version="1.0" encoding="utf-16"?><asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0"><asx:values><DATOS>'.
    ENDLOOP.

    DATA(fin) = zcl_ap_regexp=>buscar_patron( string = xml patron = '</ns0:([^>]+)>'  ).
    LOOP AT fin ASSIGNING FIELD-SYMBOL(<fin>).
      REPLACE ALL OCCURRENCES OF <fin> IN xml WITH '</DATOS></asx:values></asx:abap>'.
    ENDLOOP.

    TRY.
        CALL TRANSFORMATION id
          SOURCE XML xml
          RESULT datos = datos.
      CATCH cx_root INTO DATA(o_root).
        message = o_root->get_text( ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_msgguid.
    DATA l_msgguid TYPE sxmspeguid.

    CLEAR msgguid.
    TRY.
        ASSIGN ('(SAPLSXMS_MAIN)G_MSGGUID') TO FIELD-SYMBOL(<msgguid>).
        IF <msgguid> IS ASSIGNED.
          msgguid = <msgguid>.
        ELSE.
* El parámetro se recupera mediante enhancement en clase CL_XMS_MESSAGE_XMB - GET_MESSAGE_ID
          GET PARAMETER ID 'ZMSGGUID' FIELD DATA(l_id).
          SET PARAMETER ID 'ZMSGGUID' FIELD ''.
          msgguid = l_id.
        ENDIF.
      CATCH cx_root INTO DATA(o_root).
    ENDTRY.

  ENDMETHOD.
  METHOD get_payload.
    DATA: im_msgkey TYPE sxmsmkey.

    CLEAR: message, payload, payload_string.

    im_msgkey-msgid = msgid.
    IF NOT pid IS INITIAL.
      im_msgkey-pid = pid.
    ELSE.
      SELECT pid FROM sxmspmast
        INTO im_msgkey-pid
        UP TO 1 ROWS
       WHERE msgguid = msgid
       ORDER BY exetimest DESCENDING.
      ENDSELECT.
      IF sy-subrc <> 0.
        message = 'No existe ese ID'.
        RETURN.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'SXMB_GET_MESSAGE_PAYLOAD'
      EXPORTING
        im_msgkey      = im_msgkey
        im_archive     = ''
        im_version     = version
      IMPORTING
        ex_msg_bytes   = payload
      EXCEPTIONS
        not_authorized = 1
        no_message     = 2
        internal_error = 3
        no_payload     = 4
        OTHERS         = 5.
    IF sy-subrc <> 0.
      message = 'Error recuperando payload'.
      RETURN.
    ENDIF.

    IF payload_string IS SUPPLIED.
      CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
        EXPORTING
          im_xstring = payload
*         IM_ENCODING = 'UTF-8'
        IMPORTING
          ex_string  = payload_string.
    ENDIF.

    IF show = 'X'.
      TRY.
          CALL FUNCTION 'DISPLAY_XML_STRING'
            EXPORTING
              xml_string      = payload
*             TITLE           =
*             STARTING_X      = 5
*             STARTING_Y      = 5
            EXCEPTIONS
              no_xml_document = 1
              OTHERS          = 2.
          IF sy-subrc NE 0.
            message = 'XML inválido'.
          ENDIF.
        CATCH cx_root INTO DATA(o_root).
          message = o_root->get_text( ).
      ENDTRY.
    ENDIF.


  ENDMETHOD.
