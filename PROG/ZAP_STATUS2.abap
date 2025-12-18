*&---------------------------------------------------------------------*
*& Report ZAP_STATUS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zap_status2.

TABLES sscrfields .

DATA: buttons    TYPE zcl_ap_alv=>t_buttons,
      buttons_v1 TYPE zcl_ap_alv=>t_buttons,
      buttons_v2 TYPE zcl_ap_alv=>t_buttons.
DATA version TYPE string VALUE '20231004a'.

START-OF-SELECTION.

SET TITLEBAR '100'.

FORM set_boton USING pe_buttons.
  buttons = pe_buttons.
ENDFORM.

FORM clear_botones.
  CLEAR buttons.
ENDFORM.


FORM get_boton CHANGING ps_buttons.
  ps_buttons = buttons.
ENDFORM.

FORM guardar_botones_v1.
  buttons_v1 = buttons.
ENDFORM.

FORM restaurar_botones_v1.
  buttons = buttons_v1.
ENDFORM.


FORM guardar_botones_v2.
  buttons_v2 = buttons.
ENDFORM.

FORM restaurar_botones_v2.
  buttons = buttons_v2.
ENDFORM.


FORM set_sscrfields USING pe_sscrfields.
  sscrfields = pe_sscrfields.
ENDFORM.

FORM add_button USING button text icon qinfo.
  FIELD-SYMBOLS <bt> TYPE any.

  DATA: but     TYPE smp_dyntxt.

  CHECK button IS NOT INITIAL.

  IF text IS INITIAL AND icon IS INITIAL.
    RETURN.
  ENDIF.

  but-icon_id = icon.
  but-icon_text = text.
  but-text      = text.
  but-quickinfo = qinfo.

  ASSIGN COMPONENT button OF STRUCTURE buttons TO <bt>.
  IF <bt> IS ASSIGNED.
    <bt> = but.
  ENDIF.

ENDFORM.

FORM remove_button USING button.
  FIELD-SYMBOLS <bt> TYPE any.

  ASSIGN COMPONENT button OF STRUCTURE buttons TO <bt>.
  IF <bt> IS ASSIGNED.
    CLEAR <bt>.
  ENDIF.

ENDFORM.


FORM get_button USING button CHANGING but TYPE smp_dyntxt.
  FIELD-SYMBOLS <bt> TYPE any.

  ASSIGN COMPONENT button OF STRUCTURE buttons TO <bt>.
  IF <bt> IS ASSIGNED.
    but = <bt>.
  ENDIF.

ENDFORM.
