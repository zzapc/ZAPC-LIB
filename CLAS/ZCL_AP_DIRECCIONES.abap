TYPE-POOLS: szadr.
class ZCL_AP_DIRECCIONES definition
  public
  create public .

public section.

  class-methods GET_EMAIL
    importing
      !ADRNR type ADRNR
      !REMARK type ANY default ''
      !LISTA type ABAP_BOOL default ''
    returning
      value(EMAIL) type STRING .
  class-methods SET_EMAIL
    importing
      !ADRNR type KNA1-ADRNR
      !EMAIL type STRING
      !REMARK type ANY default '#?#'
      !MOD_EMAIL_SI_EXISTE_OTRO type ABAP_BOOL default 'X' .
  class-methods GET_EMAILS
    importing
      !ADRNR type KNA1-ADRNR
      !REMARK type ANY default ''
    returning
      value(EMAILS) type ISU_ADSMTP_TAB .
  class-methods GET_ADDR1_COMPLETE
    importing
      !ADRNR type KNA1-ADRNR optional
      !HANDLE type ANY optional
    preferred parameter ADRNR
    returning
      value(ADDR1_COMPLETE) type SZADR_ADDR1_COMPLETE .
  class-methods GET_DIRECCION
    importing
      !ADRNR type ADRNR optional
      !WERKS type WERKS_D optional
      !HANDLE type ANY optional
      !LGORT type LGORT_D optional
    preferred parameter ADRNR
    returning
      value(DIRECCION) type SZADR_ADDR1_LINE .
  class-methods SET_DIRECCION
    importing
      !ADRNR type KNA1-ADRNR optional
      !DIRECCION type ADDR1_DATA
      !HANDLE type ANY optional
      !COMMIT type ABAP_BOOL default 'X'
    exporting
      !DATA_CHANGED type ABAP_BOOL
    changing
      !I_ERRORES type ADERRORTAB optional .
  class-methods GET_DIRECCION_STRING
    importing
      !ADDRESS_TYPE type SZAD_FIELD-ADDR_TYPE default '1'
      !ADRNR type ADRC-ADDRNUMBER optional
    preferred parameter ADRNR
    returning
      value(DIRECCION) type SZAD_FIELD-ADDR_SHORT .
  class-methods GET_DIRECCION2
    importing
      !ADRNR type ADRNR optional
      !WERKS type WERKS_D optional
      !HANDLE type ANY optional
      !LGORT type LGORT_D optional
    preferred parameter ADRNR
    returning
      value(DIRECCION) type ADDR1_DATA .
  class-methods GET_NOMBRE_PAIS
    importing
      !LAND1 type ANY
      !SPRAS type ANY default SY-LANGU
    returning
      value(LANDX) type T005T-LANDX .
  class-methods GET_DIRECCION_STRINGF
    importing
      !ADRNR type ADRNR optional
      !WERKS type WERKS_D optional
      !HANDLE type ANY optional
      !LGORT type LGORT_D optional
      !SPRAS type SY-LANGU default SY-LANGU
      !MOSTRAR_PAIS type ABAP_BOOL default 'X'
      !MOSTRAR_CP type ABAP_BOOL default 'X'
      !NIF type ANY default ''
      !TITULO_NIF type ANY default 'NIF'
      !NIF_TRAS_NOMBRE type ANY default ''
      !CODIGO type ANY default ''
      !TITULO_CODIGO type ANY default ''
      !FORMATO_NOMBRE type ANY default ''
    preferred parameter ADRNR
    returning
      value(STRING) type STRING .
  class-methods GET_NOMBRE_PROVINCIA
    importing
      !LAND1 type ANY
      !REGIO type ANY
      !SPRAS type ANY default SY-LANGU
    returning
      value(BEZEI) type T005U-BEZEI .
  class-methods POPUP_EMAILS
    importing
      !ADRNR type ADRNR
      !OPERACION type ANY default 'MAINTAIN' .
protected section.
private section.
endclass. "ZCL_AP_DIRECCIONES definition
class ZCL_AP_DIRECCIONES implementation.
method GET_ADDR1_COMPLETE.
  DATA: l_address_selection TYPE addr1_sel,
        l_handle type ADDR1_SEL-ADDRHANDLE.

  clear addr1_complete.

  l_address_selection-addrnumber = adrnr.
  l_handle = handle.
  CALL FUNCTION 'ADDR_GET_COMPLETE'
    EXPORTING
      addrnumber        = l_address_selection-addrnumber
      ADDRHANDLE        = l_handle
    IMPORTING
      addr1_complete    = addr1_complete
    EXCEPTIONS
      parameter_error   = 1
      address_not_exist = 2
      internal_error    = 3
      OTHERS            = 4.


endmethod.
METHOD get_direccion.
  DATA: addr1_complete TYPE szadr_addr1_complete.
  DATA: adsmtp_line TYPE szadr_adsmtp_line,
        addr1_line  TYPE szadr_addr1_line,
        l_adrnr TYPE adrnr.
  CLEAR direccion.

  l_adrnr = adrnr.
  IF adrnr IS INITIAL.
    IF NOT lgort IS INITIAL.
      SELECT SINGLE adrnr FROM twlad
        INTO l_adrnr
       WHERE werks = werks
         AND lgort = lgort.
    ELSEIF NOT werks IS INITIAL.
      SELECT SINGLE adrnr FROM t001w
        INTO l_adrnr
       WHERE werks = werks.
    ENDIF.
  ENDIF.

  CHECK NOT l_adrnr IS INITIAL OR NOT handle IS INITIAL.

  addr1_complete = get_addr1_complete( adrnr = l_adrnr handle = handle ).

  IF sy-subrc = 0.
    READ TABLE addr1_complete-addr1_tab INDEX 1 INTO addr1_line.
    IF sy-subrc = 0.
      direccion = addr1_line.
    ENDIF.
  ENDIF.

ENDMETHOD.
METHOD get_direccion2.
  DATA: addr1_line  TYPE szadr_addr1_line.

  addr1_line = zcl_ap_direcciones=>get_direccion( adrnr     = adrnr
                                                  werks     = werks
                                                  handle    = handle
                                                  lgort     = lgort
                                                ).

  direccion = addr1_line-data.

ENDMETHOD.
method GET_DIRECCION_STRING.

  CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
    EXPORTING
*   ADRSWA_IN                            =
*   ADDRESS_1                            =
*   ADDRESS_2                            =
*   ADDRESS_3                            =
      ADDRESS_TYPE                         = address_type
      ADDRESS_NUMBER                       = adrnr
*   ADDRESS_HANDLE                       = ' '
*   PERSON_NUMBER                        = ' '
*   PERSON_HANDLE                        = ' '
*   SENDER_COUNTRY                       = ' '
*   RECEIVER_LANGUAGE                    = ' '
*   NUMBER_OF_LINES                      = 10
*   STREET_HAS_PRIORITY                  = ' '
*   LINE_PRIORITY                        = ' '
*   COUNTRY_NAME_IN_RECEIVER_LANGU       = ' '
*   LANGUAGE_FOR_COUNTRY_NAME            = ' '
*   NO_UPPER_CASE_FOR_CITY               = ' '
*   IV_NATION                            = ' '
*   IV_NATION_SPACE                      = ' '
*   IV_PERSON_ABOVE_ORGANIZATION         = ' '
*   IS_BUPA_TIME_DEPENDENCY              = ' '
*   IV_LANGU_CREA                        = ' '
   IMPORTING
*   ADRSWA_OUT                           =
*   ADDRESS_PRINTFORM                    =
*   ADDRESS_SHORT_FORM                   =
*   ADDRESS_SHORT_FORM_S                 =
*   ADDRESS_DATA_CARRIER                 =
*   ADDRESS_DATA_CARRIER_0               =
*   NUMBER_OF_USED_LINES                 =
*   NAME_IS_EMPTY                        =
*   ADDRESS_NOT_FOUND                    =
*   ADDRESS_PRINTFORM_TABLE              =
      ADDRESS_SHORT_FORM_WO_NAME           = direccion
*   EV_NATION                            =
            .

endmethod.
METHOD get_direccion_stringf.
  DATA: addr1_line TYPE szadr_addr1_line,
        l_string   TYPE string,
        l_string2  TYPE string,
        l_string3  TYPE string,
        l_string4  TYPE string,
        l_string5  TYPE string,
        aux2(2).


  addr1_line = zcl_ap_direcciones=>get_direccion( adrnr     = adrnr
                                                  werks     = werks
                                                  handle    = handle
                                                  lgort     = lgort
                                                ).

  IF NOT addr1_line-data IS INITIAL.
    IF formato_nombre NE 'SIN_NOMBRE'.
      CONCATENATE addr1_line-data-name1 addr1_line-data-name2 INTO string SEPARATED BY space.

      IF NOT formato_nombre IS INITIAL.
        CONCATENATE '<' formato_nombre '>' string '</' formato_nombre '>' INTO string.
      ENDIF.
    ENDIF.

    IF nif_tras_nombre = 'X' AND nif NE ''.
      CONCATENATE titulo_nif nif INTO l_string SEPARATED BY space.
      CONCATENATE string cl_abap_char_utilities=>cr_lf l_string INTO string.
    ENDIF.

    IF NOT addr1_line-data-street IS INITIAL.
      CONCATENATE string cl_abap_char_utilities=>cr_lf addr1_line-data-street ` ` addr1_line-data-house_num1 INTO string.
    ENDIF.
    IF mostrar_cp = 'X'.
      IF NOT addr1_line-data-po_box IS INITIAL OR NOT addr1_line-data-post_code2 IS INITIAL.
        CASE spras.
          WHEN 'S'.
            l_string = 'Apartado'(apt).
          WHEN 'E'.
            l_string = 'Post Office Box'(pob).
        ENDCASE.
        CONCATENATE l_string addr1_line-data-po_box addr1_line-data-post_code2 addr1_line-data-pcode2_ext INTO l_string SEPARATED BY space.
        CONCATENATE string cl_abap_char_utilities=>cr_lf l_string INTO string.
      ENDIF.
    ENDIF.
    IF NOT addr1_line-data-post_code1 IS INITIAL OR NOT addr1_line-data-city1 IS INITIAL OR NOT addr1_line-data-region IS INITIAL.
      IF addr1_line-data-post_code1 IS INITIAL.
        l_string = addr1_line-data-city1.
      ELSE.
        CONCATENATE addr1_line-data-post_code1 addr1_line-data-city1 INTO l_string SEPARATED BY space.
      ENDIF.
      IF NOT addr1_line-data-region IS INITIAL AND NOT addr1_line-data-country IS INITIAL.
        l_string2 = get_nombre_provincia( land1 = addr1_line-data-country regio = addr1_line-data-region ).
        l_string4 = l_string2.
        TRANSLATE l_string4 TO UPPER CASE.
        zcl_ap_string=>quitar_caracteres_extranos( CHANGING string = l_string4 ).

        l_string5 = addr1_line-data-city1.
        TRANSLATE l_string5 TO UPPER CASE.
        zcl_ap_string=>quitar_caracteres_extranos( CHANGING string = l_string5 ).
        IF l_string4 NE l_string5.
          CONCATENATE '(' l_string2 ')' INTO l_string2.
          IF NOT l_string CS l_string2.
            l_string3 = l_string2.
            TRANSLATE l_string3 TO UPPER CASE.
            IF NOT l_string CS l_string3.
              CONCATENATE l_string l_string2 INTO l_string SEPARATED BY space.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      CONCATENATE string cl_abap_char_utilities=>cr_lf l_string INTO string.
    ENDIF.
    IF NOT addr1_line-data-country IS INITIAL AND mostrar_pais NE ''.
      l_string = get_nombre_pais( addr1_line-data-country ).
      IF mostrar_pais = 'M'.
        TRANSLATE l_string TO UPPER CASE.
      ENDIF.
      CONCATENATE string cl_abap_char_utilities=>cr_lf l_string INTO string.
    ENDIF.
  ENDIF.

  IF nif_tras_nombre = '' AND nif NE ''.
    CONCATENATE titulo_nif nif INTO l_string SEPARATED BY space.
    CONCATENATE string cl_abap_char_utilities=>cr_lf l_string INTO string.
  ENDIF.


  IF codigo NE ''.
    __concat2 l_string titulo_codigo codigo.
    CONCATENATE string cl_abap_char_utilities=>cr_lf l_string INTO string.
  ENDIF.

  aux2 = string.
  IF aux2 = cl_abap_char_utilities=>cr_lf.
    string = string+2.
  ENDIF.


ENDMETHOD.
METHOD get_email.
  DATA: emails  TYPE isu_adsmtp_tab,
        l_email TYPE adsmtp.

  CLEAR email.

  emails = get_emails( adrnr = adrnr remark = remark ).

  IF lista IS INITIAL.
    READ TABLE emails INTO l_email WITH KEY flgdefault = 'X'.
    IF sy-subrc = 0.
      email = l_email-smtp_addr.
    ELSE.
      READ TABLE emails INTO l_email index 1.
      IF sy-subrc = 0.
        email = l_email-smtp_addr.
      ENDIF.
    ENDIF.
  ELSE.
    LOOP AT emails INTO l_email.
      __add_lista email l_email-smtp_addr.
    ENDLOOP.
  ENDIF.


ENDMETHOD.
METHOD get_emails.
  DATA: addr1_complete TYPE szadr_addr1_complete.
  DATA: adsmtp_line TYPE szadr_adsmtp_line,
        addr1_line  TYPE szadr_addr1_line.

  CLEAR emails.

  addr1_complete = get_addr1_complete( adrnr ).

  IF sy-subrc = 0.
    LOOP AT addr1_complete-addr1_tab INTO addr1_line.
      LOOP AT addr1_complete-adsmtp_tab INTO adsmtp_line.
        IF remark IS INITIAL OR remark = adsmtp_line-adsmtp-remark.
          APPEND adsmtp_line-adsmtp TO emails.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

ENDMETHOD.
method GET_NOMBRE_PAIS.

  SELECT single landx FROM  t005t
    into landx
   WHERE spras  = spras
     AND land1  = land1.

endmethod.
METHOD get_nombre_provincia.

  SELECT SINGLE bezei FROM  t005u
    INTO bezei
   WHERE spras  = spras
     AND land1  = land1
     AND bland  = regio.

ENDMETHOD.
METHOD popup_emails.
    DATA: iv_timestamp     TYPE  ad_srctime,
          data_has_changed,
          dialog_mode      TYPE szad_field-maint_mode.

    dialog_mode = operacion.
    CONCATENATE sy-datum sy-uzeit INTO iv_timestamp.
    CALL FUNCTION 'ADDR_COMM_DIALOG_INTERNET'
      EXPORTING
        address_number              = adrnr
        address_handle              = ''
        dialog_mode                 = dialog_mode
        address_object_type         = '1'
        iv_time_dependent_comm_data = ''
        iv_valid_from               = ''
        iv_valid_to                 = ''
        iv_timestamp                = iv_timestamp
        iv_suggested_valid_from     = ''
        iv_suggested_valid_to       = ''
      IMPORTING
        data_has_changed            = data_has_changed
      EXCEPTIONS
        parameter_error             = 01
        no_authority                = 02
        internal_error              = 03.
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE 'Error en parametros'(epa) TYPE 'E'.
        WHEN 2.
          MESSAGE 'No tiene autorización a emails'(aut) TYPE 'E'.
        WHEN 3.
          MESSAGE 'Error interno'(ein) TYPE 'E'.
        WHEN 4.
          MESSAGE 'Error accediendo a email'(eae) TYPE 'E'.
      ENDCASE.
    ELSE.
      IF data_has_changed = 'X'.
        CALL FUNCTION 'ADDR_MEMORY_SAVE'
          EXPORTING
            execute_in_update_task = ''
          EXCEPTIONS
            address_number_missing = 1
            person_number_missing  = 2
            internal_error         = 3
            database_error         = 4
            reference_missing      = 5
            OTHERS                 = 6.
      ENDIF.
    ENDIF.

  ENDMETHOD.
method SET_DIRECCION.
  DATA: addr1_complete TYPE szadr_addr1_complete.
  DATA: addr1_line  TYPE szadr_addr1_line,
        l_adrnr TYPE adrnr,
        l_handle TYPE szad_field-handle.

  CHECK NOT adrnr IS INITIAL OR NOT handle IS INITIAL.

  l_handle = handle.
  CALL FUNCTION 'ADDR_UPDATE'
       EXPORTING
            address_data        = direccion
            address_handle      = l_handle
            address_number      = adrnr
*         DATE_FROM           = '00010101'
*         LANGUAGE            = SY-LANGU
*          nation              = land1
*         CHECK_EMPTY_ADDRESS = 'X'
*         CHECK_ADDRESS       = 'X'
    IMPORTING
*         ADDRESS_DATA        =
*          returncode          =
          data_has_changed    = data_changed
    TABLES
         error_table         = i_errores
      EXCEPTIONS
           address_not_exist   = 1
           parameter_error     = 2
           version_not_exist   = 3
           internal_error      = 4
           OTHERS              = 5.


  IF data_changed = 'X'.
    IF commit = 'X'.
      READ TABLE i_errores TRANSPORTING NO FIELDS WITH KEY msg_type = 'E'.
      IF sy-subrc NE 0.
        CALL FUNCTION 'ADDR_MEMORY_SAVE'
*    EXPORTING
*         EXECUTE_IN_UPDATE_TASK = ' '
            EXCEPTIONS
                 address_number_missing = 1
                 person_number_missing  = 2
                 internal_error         = 3
                 database_error         = 4
                 reference_missing      = 5
                 OTHERS                 = 6.
      ENDIF.
    ENDIF.
  ENDIF.

endmethod.
METHOD set_email.
  DATA: addr1_complete TYPE szadr_addr1_complete.
  DATA: adsmtp_line TYPE szadr_adsmtp_line,
        addr1_line  TYPE szadr_addr1_line.

  addr1_complete = get_addr1_complete( adrnr ).

**** EMAIL
  DATA: xadsmtp  TYPE TABLE OF adsmtp,
        l_adsmtp TYPE adsmtp.

  REFRESH xadsmtp.
  LOOP AT addr1_complete-adsmtp_tab INTO adsmtp_line.
    l_adsmtp = adsmtp_line-adsmtp.
    APPEND l_adsmtp TO xadsmtp.
  ENDLOOP.

  READ TABLE xadsmtp INTO l_adsmtp INDEX 1.
  IF sy-subrc NE 0.
    l_adsmtp-smtp_addr = email.
    l_adsmtp-flgdefault = 'X'.
    l_adsmtp-updateflag = 'I'.
    IF remark NE '#?#'.
      l_adsmtp-remark = remark.
    ENDIF.
    APPEND l_adsmtp TO xadsmtp.
  ELSE.
    READ TABLE xadsmtp INTO l_adsmtp WITH KEY smtp_addr = email.
    IF sy-subrc = 0.
      IF remark NE '#?#'.
        l_adsmtp-remark = remark.
      ENDIF.
      l_adsmtp-updateflag = 'U'.
      MODIFY xadsmtp FROM l_adsmtp INDEX 1.
    ELSE.
      IF mod_email_si_existe_otro IS INITIAL.
        CLEAR l_adsmtp.
        l_adsmtp-smtp_addr = email.
        l_adsmtp-updateflag = 'I'.
        IF remark NE '#?#'.
          l_adsmtp-remark = remark.
        ENDIF.
        APPEND l_adsmtp TO xadsmtp.
      ELSE.
        l_adsmtp-smtp_addr = email.
        IF remark NE '#?#'.
          l_adsmtp-remark = remark.
        ENDIF.
        l_adsmtp-updateflag = 'U'.
        MODIFY xadsmtp FROM l_adsmtp INDEX 1.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ADDR_COMM_MAINTAIN'
    EXPORTING
      address_handle           = addr1_complete-addrhandle
      address_number           = addr1_complete-addrnumber
      table_type               = 'ADSMTP'
      substitute_all_comm_data = 'X'
    TABLES
      comm_table               = xadsmtp
    EXCEPTIONS
      parameter_error          = 1
      address_not_exist        = 2
      internal_error           = 3
      OTHERS                   = 4.

  IF sy-subrc = 0.
    CALL FUNCTION 'ADDR_MEMORY_SAVE'
*    EXPORTING
*         EXECUTE_IN_UPDATE_TASK = ' '
      EXCEPTIONS
        address_number_missing = 1
        person_number_missing  = 2
        internal_error         = 3
        database_error         = 4
        reference_missing      = 5
        OTHERS                 = 6.

  ENDIF.


ENDMETHOD.
