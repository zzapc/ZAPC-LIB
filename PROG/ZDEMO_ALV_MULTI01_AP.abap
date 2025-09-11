*&---------------------------------------------------------------------*
*& Report ZDEMO_ALV_MULTI
*&---------------------------------------------------------------------*
*& Horizontal TAB Layout
*&---------------------------------------------------------------------*
REPORT zdemo_alv_multi01_ap.

START-OF-SELECTION.
  SELECT * FROM usr21 INTO TABLE @DATA(lt_usr21) UP TO 100 ROWS.
  SELECT * FROM adrc INTO TABLE @DATA(lt_adrc) UP TO 100 ROWS.
  SELECT * FROM kna1 INTO TABLE @DATA(lt_kna1) UP TO 100 ROWS.
  SELECT * FROM knb1 INTO TABLE @DATA(lt_knb1) UP TO 100 ROWS.
  NEW zcl_ap_alv_multi( iv_mode = 'H'
    iv_title = 'MULTI ALV DEMO01'
    it_buttons =  VALUE #(  ( obj_code = 'F01' text = 'Prueba' icon_id = icon_execute_object quickinfo = 'Quick' )  )
    it_alvs = VALUE #(
    ( title = 'USR21' table = REF #( lt_usr21 ) )
*    ( title = 'ADRC' table = REF #( lt_adrc ) )
*    ( title = 'KNA1' table = REF #( lt_kna1 ) )
*    ( title = 'KNB1' table = REF #( lt_knb1 ) )
    ) )->display( ).
