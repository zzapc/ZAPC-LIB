*&---------------------------------------------------------------------*
*& Report ZDOWN_SAPUI5
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdown_sapui5.

TABLES o2appl.

SELECT-OPTIONS: s_name FOR o2appl-applname.
PARAMETERS: p_dir TYPE string OBLIGATORY DEFAULT 'C:\temp\sapui5\'.

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  IF NOT p_dir IS INITIAL.
    DATA(l_long) = strlen( p_dir ) - 1.
    IF p_dir+l_long(1) = '\'.
      p_dir = p_dir(l_long).
    ENDIF.
  ENDIF.

START-OF-SELECTION.

  DATA i_bsp TYPE TABLE OF o2appl WITH HEADER LINE.

  SELECT * FROM o2appl
    INTO TABLE i_bsp
    WHERE applname  IN s_name
      AND applname LIKE 'Z%'
      AND applclas LIKE '%UI5%'
   ORDER BY applname.

  DATA: l_dir    TYPE string, l_rc TYPE i, l_string TYPE string.

  LOOP AT i_bsp.
*    SET PARAMETER ID 'ZDIR_UI5' FIELD ''.
*    IF NOT p_dir IS INITIAL.
*      data(l_long) = strlen( p_dir ) - 1.
*      if p_dir+l_long(1) = '\'.
*        CONCATENATE p_dir i_bsp-applname into l_dir.
*      else.
*        CONCATENATE p_dir i_bsp-applname into l_dir SEPARATED BY '\'.
*      endif.
*
*      CLEAR l_rc.
*      data l_existe type abap_bool.
*    CALL METHOD cl_gui_frontend_services=>directory_exist
*      EXPORTING
*        directory            = l_dir
*      RECEIVING
*        result               = l_existe
*      EXCEPTIONS
*        cntl_error           = 1
*        error_no_gui         = 2
*        wrong_parameter      = 3
*        not_supported_by_gui = 4
*        OTHERS               = 5.
*
*      IF l_existe is initial.
*        l_string = l_dir.
*        cl_gui_frontend_services=>directory_create( EXPORTING directory = l_string CHANGING rc = l_rc ).
*      ENDIF.
*      IF l_rc = 0.
*        data l_dirc type text255.
*        l_dirc = l_dir.
*        SET PARAMETER ID 'ZDIR_UI5' FIELD l_dirc.
*      ENDIF.
*    ENDIF.
*    SUBMIT /ui5/ui5_repository_load




    SUBMIT zui5_repository_load
      AND RETURN
      WITH ui5rep = i_bsp-applname
      WITH director = p_dir
      WITH upload = ''
      WITH download = 'X'.

*  METHOD select_directory.
*  DATA L_DIR TYPE TEXT255.
*    GET PARAMETER ID 'ZDIR_UI5' FIELD L_DIR.
*    IF NOT L_DIR IS INITIAL.
*    rv_directory = L_DIR.
*    EXIT.
*    ENDIF.
    SET PARAMETER ID 'ZDIR_UI5' FIELD ''.
  ENDLOOP.
