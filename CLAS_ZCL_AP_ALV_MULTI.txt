class ZCL_AP_ALV_MULTI definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_hide,
        fieldname TYPE lvc_fname,
      END OF ty_hide .
  types:
    tt_hide TYPE HASHED TABLE OF ty_hide WITH UNIQUE KEY fieldname .
  types:
    BEGIN OF ty_text,
        fieldname TYPE lvc_fname,
        text      TYPE string,
      END OF ty_text .
  types:
    tt_text TYPE HASHED TABLE OF ty_text WITH UNIQUE KEY fieldname .
  types:
    BEGIN OF ty_alvs,
        title     TYPE lvc_title,
        handle    TYPE slis_handl,
        repid     TYPE sy-repid,
        it_hide   TYPE tt_hide,
        it_text   TYPE tt_text,
        table     TYPE REF TO data,
        falv      TYPE REF TO zcl_ap_alv_grid,
        is_showed TYPE abap_bool,
        index     TYPE i,
        o_event   TYPE REF TO zcl_ap_alv_grid_eventos,
      END OF ty_alvs .
  types:
    tt_alvs TYPE STANDARD TABLE OF ty_alvs .

  data MV_POPUP type ABAP_BOOL .
  data MV_MODE type CHAR1 .
  data MV_TITLE type LVC_TITLE .
  data MT_ALVS type TT_ALVS .
  data HORIZONTAL type CHAR1 value 'H' ##NO_TEXT.
  data VERTICAL type CHAR1 value 'H' ##NO_TEXT.
  data CB_BEFORE_SHOW_ALV type SLIS_FORMNAME value 'FRM_BEFORE_SHOW_ALV' ##NO_TEXT.
  constants:
    BEGIN OF events,
        before_show_alv TYPE slis_formname VALUE 'BEFORE_SHOW_ALV',
      END OF events .
  data MT_BUTTONS type SMP_TXTTAB .
  data O_PROG type ref to ZCL_AP_DEV .

  methods CONSTRUCTOR
    importing
      value(IV_POPUP) type ABAP_BOOL optional
      value(IV_TITLE) type LVC_TITLE optional
      value(IV_CB_BEFORE_SHOW_ALV) type NA_RONAM optional
      value(IV_MODE) type CHAR1 default 'H'
      !IT_EVENTS type SLIS_T_EVENT optional
      !IT_ALVS type TT_ALVS
      !IT_BUTTONS type SMP_TXTTAB optional
      value(O_PROG) type ref to ZCL_AP_DEV optional .
  methods DISPLAY
    importing
      value(IV_START_ROW) type I optional
      value(IV_START_COLUMN) type I optional
      value(IV_END_ROW) type I optional
      value(IV_END_COLUMN) type I optional .
  methods ON_BEFORE_SHOW_ALV
    importing
      !IS_ALVS type TY_ALVS .

  PRIVATE SECTION.
    DATA mt_events TYPE slis_t_event.
endclass. "ZCL_AP_ALV_MULTI definition
class ZCL_AP_ALV_MULTI implementation.
  METHOD constructor.
    mt_alvs = it_alvs.
    mv_popup = iv_popup.
    mv_mode = iv_mode.
    mv_title = iv_title.
    mt_events = it_events.
    mt_buttons = it_buttons.
    me->o_prog = o_prog.
    IF iv_cb_before_show_alv IS SUPPLIED.
      cb_before_show_alv = iv_cb_before_show_alv.
    ELSEIF it_events IS SUPPLIED.
      READ TABLE it_events INTO DATA(ls_events) WITH KEY name = events-before_show_alv.
      IF sy-subrc = 0.
        cb_before_show_alv = ls_events-form.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD display.
    IF     mv_popup         = abap_true AND iv_start_row IS INITIAL
       AND iv_start_column IS INITIAL
       AND iv_end_row      IS INITIAL
       AND iv_end_column   IS INITIAL.

      iv_start_row = 1.
      iv_start_column = 1.
      iv_end_row = 20.
      iv_end_column = 150.
    ENDIF.
    CALL FUNCTION 'Z_AP_ALV_MULTI_DISPLAY'
      EXPORTING
        io_alv          = me
        iv_popup        = mv_popup
        iv_title        = mv_title
        iv_mode         = mv_mode
        it_events       = mt_events
        iv_start_row    = iv_start_row
        iv_start_column = iv_start_column
        iv_end_row      = iv_end_row
        iv_end_column   = iv_end_column.
  ENDMETHOD.
  METHOD on_before_show_alv.
    PERFORM (cb_before_show_alv) IN PROGRAM (sy-cprog) IF FOUND USING is_alvs.
  ENDMETHOD.
