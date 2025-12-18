CLASS zcl_ap_cliente DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS urls_gos_st
      IMPORTING kunnr        TYPE kunnr
      RETURNING VALUE(tabla) TYPE ztab_url_gos.

    CLASS-METHODS insertar_url_gos_st
      IMPORTING kunnr  TYPE kunnr
                url    TYPE string
                titulo TYPE string.

    CLASS-METHODS visualizar_cliente_st
      IMPORTING kunnr TYPE kunnr.

    CLASS-METHODS get_url_por_titulo_st
      IMPORTING kunnr      TYPE kunnr
                titulo     TYPE string
      RETURNING VALUE(url) TYPE string.

    CLASS-METHODS get_emails
      IMPORTING kunnr         TYPE kunnr
                remark        TYPE any DEFAULT ''
      RETURNING VALUE(emails) TYPE isu_adsmtp_tab.

    CLASS-METHODS get_email
      IMPORTING kunnr        TYPE kunnr
                lista        TYPE abap_bool DEFAULT ''
                remark       TYPE any       DEFAULT ''
                flgdefault   TYPE abap_bool DEFAULT ''
      RETURNING VALUE(email) TYPE ad_smtpadr.

    CLASS-METHODS get_nombre
      IMPORTING kunnr        TYPE kunnr
      RETURNING VALUE(name1) TYPE name1.

    CLASS-METHODS get_direccion
      IMPORTING kunnr            TYPE kunnr OPTIONAL
                adrnr            TYPE adrnr OPTIONAL
      RETURNING VALUE(direccion) TYPE addr1_data.

    METHODS get_kna1
      IMPORTING kunnr       TYPE kunnr
      RETURNING VALUE(kna1) TYPE kna1.

    METHODS get_nombre_kna1
      IMPORTING kunnr        TYPE kunnr
      RETURNING VALUE(name1) TYPE kna1-name1.

    CLASS-METHODS get_emails_lista
      IMPORTING kunnr        TYPE kunnr
                remark       TYPE any DEFAULT ''
      RETURNING VALUE(email) TYPE string.

    CLASS-METHODS get_texto_string
      IMPORTING kunnr         TYPE kunnr
                !id           TYPE stxh-tdid
                spras         TYPE spras DEFAULT sy-langu
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS set_texto_string
      IMPORTING kunnr   TYPE kunnr
                !id     TYPE stxh-tdid
                spras   TYPE spras DEFAULT ''
                !string TYPE string.

    CLASS-METHODS borrar_interlocutor
      IMPORTING kunnr          TYPE kunnr
                vkorg          TYPE vkorg
                vtweg          TYPE vtweg
                spart          TYPE spart
                parvw          TYPE parvw
                parza          TYPE knvp-parza OPTIONAL
                lifnr          TYPE lifnr      OPTIONAL
                !commit        TYPE abap_bool  DEFAULT 'X'
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS modificar_interlocutor
      IMPORTING kunnr          TYPE kunnr
                vkorg          TYPE vkorg
                vtweg          TYPE vtweg
                spart          TYPE spart
                parvw          TYPE parvw
                parza          TYPE knvp-parza OPTIONAL
                lifnr_old      TYPE lifnr      OPTIONAL
                lifnr_new      TYPE lifnr      OPTIONAL
                !commit        TYPE abap_bool  DEFAULT 'X'
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS crear_interlocutor
      IMPORTING kunnr          TYPE kunnr
                vkorg          TYPE vkorg
                vtweg          TYPE vtweg
                spart          TYPE spart
                parvw          TYPE parvw
                lifnr_new      TYPE lifnr OPTIONAL
                !commit        TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(message) TYPE bapi_msg.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA i_kna1 TYPE cif_kna1_tab.
endclass. "ZCL_AP_CLIENTE definition
class ZCL_AP_CLIENTE implementation.
  METHOD borrar_interlocutor.
    DATA: r_lifnr TYPE RANGE OF lifnr,
          r_parza TYPE RANGE OF knvp-parza,
          yknvp   TYPE TABLE OF fknvp,
          xknvp   TYPE TABLE OF fknvp.

    IF NOT lifnr IS INITIAL.
      r_lifnr = VALUE #( ( sign = 'I' option = 'EQ' low = lifnr ) ).
    ENDIF.
    IF NOT parzA IS INITIAL.
      r_parzA = VALUE #( ( sign = 'I' option = 'EQ' low = parzA ) ).
    ENDIF.

    SELECT * FROM knvp                        "#EC CI_ALL_FIELDS_NEEDED
     INTO CORRESPONDING FIELDS OF TABLE yknvp
    WHERE kunnr  = kunnr
      AND vkorg  = vkorg
      AND vtweg  = vtweg
      AND spart  = spart
      AND parvw  = parvw
      AND parza IN r_parza
      AND lifnr IN r_lifnr
     order BY PRIMARY KEY.
    IF sy-subrc <> 0.
      message = |No existe el interlocutor { parvw } { lifnr } para el cliente { kunnr }|.
      RETURN.
    ENDIF.


    SELECT SINGLE * FROM kna1  "#EC CI_ALL_FIELDS_NEEDED
      INTO @DATA(l_kna1)
     WHERE kunnr = @kunnr.
*
    SELECT SINGLE * FROM knvv   "#EC CI_ALL_FIELDS_NEEDED
      INTO @DATA(l_knvv)
     WHERE kunnr = @kunnr
       AND vkorg = @vkorg
       AND vtweg = @vtweg
       AND spart = @spart.

    CALL FUNCTION 'SD_CUSTOMER_MAINTAIN_ALL'
      EXPORTING
        i_kna1                  = l_kna1
        i_knvv                  = l_knvv
        pi_postflag             = 'X'
      TABLES
        t_xknvp                 = xknvp
        t_yknvp                 = yknvp
      EXCEPTIONS
        client_error            = 1
        kna1_incomplete         = 2
        knb1_incomplete         = 3
        knb5_incomplete         = 4
        knvv_incomplete         = 5
        kunnr_not_unique        = 6
        sales_area_not_unique   = 7
        sales_area_not_valid    = 8
        insert_update_conflict  = 9
        number_assignment_error = 10
        number_not_in_range     = 11
        number_range_not_extern = 12
        number_range_not_intern = 13
        account_group_not_valid = 14
        parnr_invalid           = 15
        bank_address_invalid    = 16
        tax_data_not_valid      = 17
        no_authority            = 18
        company_code_not_unique = 19
        dunning_data_not_valid  = 20
        knb1_reference_invalid  = 21
        cam_error               = 22
        OTHERS                  = 23.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ELSE.
      IF commit = 'X'.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD crear_interlocutor.
    DATA: r_lifnr TYPE RANGE OF lifnr,
          yknvp   TYPE TABLE OF fknvp,
          l_knvp  TYPE fknvp,
          xknvp   TYPE TABLE OF fknvp.

    IF NOT lifnr_new IS INITIAL.
      r_lifnr = VALUE #( ( sign = 'I' option = 'EQ' low = lifnr_new ) ).
    ENDIF.

    SELECT * FROM knvp                        "#EC CI_ALL_FIELDS_NEEDED
     INTO CORRESPONDING FIELDS OF TABLE yknvp
    WHERE kunnr  = kunnr
      AND vkorg  = vkorg
      AND vtweg  = vtweg
      AND spart  = spart
      AND parvw  = parvw
      AND lifnr IN r_lifnr
     ORDER BY PRIMARY KEY.
    IF sy-subrc = 0.
      message = |Ya existe el interlocutor { parvw } { lifnr_new } para el cliente { kunnr }|.
      RETURN.
    ENDIF.

    l_knvp-kunnr = kunnr.
    l_knvp-vkorg = vkorg.
    l_knvp-vtweg = vtweg.
    l_knvp-spart = spart.
    l_knvp-parvw = parvw.
    IF NOT lifnr_new IS INITIAL.
      l_knvp-lifnr = lifnr_new.
    ENDIF.
    l_knvp-kz = 'I'.
    APPEND l_knvp TO xknvp.

*
    SELECT SINGLE * FROM kna1        "#EC CI_ALL_FIELDS_NEEDED
      INTO @DATA(l_kna1)
     WHERE kunnr = @kunnr.
*
    SELECT SINGLE * FROM knvv         "#EC CI_ALL_FIELDS_NEEDED
      INTO @DATA(l_knvv)
     WHERE kunnr = @kunnr
       AND vkorg = @vkorg
       AND vtweg = @vtweg
       AND spart = @spart.

    CALL FUNCTION 'SD_CUSTOMER_MAINTAIN_ALL'
      EXPORTING
        i_kna1                  = l_kna1
        i_knvv                  = l_knvv
        pi_postflag             = 'X'
      TABLES
        t_xknvp                 = xknvp
        t_yknvp                 = yknvp
      EXCEPTIONS
        client_error            = 1
        kna1_incomplete         = 2
        knb1_incomplete         = 3
        knb5_incomplete         = 4
        knvv_incomplete         = 5
        kunnr_not_unique        = 6
        sales_area_not_unique   = 7
        sales_area_not_valid    = 8
        insert_update_conflict  = 9
        number_assignment_error = 10
        number_not_in_range     = 11
        number_range_not_extern = 12
        number_range_not_intern = 13
        account_group_not_valid = 14
        parnr_invalid           = 15
        bank_address_invalid    = 16
        tax_data_not_valid      = 17
        no_authority            = 18
        company_code_not_unique = 19
        dunning_data_not_valid  = 20
        knb1_reference_invalid  = 21
        cam_error               = 22
        OTHERS                  = 23.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ELSE.
      IF commit = 'X'.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDMETHOD.
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

    LOOP AT i_emails ASSIGNING FIELD-SYMBOL(<email>).
      IF email IS INITIAL.
        email = <email>-smtp_addr.
      ELSE.
        CONCATENATE email ',' <email>-smtp_addr INTO email.
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
  METHOD modificar_interlocutor.
    DATA: r_lifnr TYPE RANGE OF lifnr,
          r_parza TYPE RANGE OF knvp-parza,
          yknvp   TYPE TABLE OF fknvp,
          xknvp   TYPE TABLE OF fknvp.

    IF NOT lifnr_old IS INITIAL.
      r_lifnr = VALUE #( ( sign = 'I' option = 'EQ' low = lifnr_old ) ).
    ENDIF.
    IF NOT parzA IS INITIAL.
      r_parzA = VALUE #( ( sign = 'I' option = 'EQ' low = parzA ) ).
    ENDIF.

    SELECT * FROM knvp                        "#EC CI_ALL_FIELDS_NEEDED
     INTO CORRESPONDING FIELDS OF TABLE yknvp
    WHERE kunnr  = kunnr
      AND vkorg  = vkorg
      AND vtweg  = vtweg
      AND spart  = spart
      AND parvw  = parvw
      AND parza IN r_parza
      AND lifnr IN r_lifnr
     ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      message = |No existe el interlocutor { parvw } { lifnr_old } para el cliente { kunnr }|.
      RETURN.
    ENDIF.

    READ TABLE yknvp INTO DATA(l_knvp) INDEX 1.
    l_knvp-kz = 'D'.
    APPEND l_knvp TO xknvp.
    IF NOT lifnr_new IS INITIAL.
      l_knvp-lifnr = lifnr_new.
    ENDIF.
    l_knvp-kz = 'I'.
    APPEND l_knvp TO xknvp.

*
    SELECT SINGLE * FROM kna1       "#EC CI_ALL_FIELDS_NEEDED
      INTO @DATA(l_kna1)
     WHERE kunnr = @kunnr.
*
    SELECT SINGLE * FROM knvv        "#EC CI_ALL_FIELDS_NEEDED
      INTO @DATA(l_knvv)
     WHERE kunnr = @kunnr
       AND vkorg = @vkorg
       AND vtweg = @vtweg
       AND spart = @spart.

    CALL FUNCTION 'SD_CUSTOMER_MAINTAIN_ALL'
      EXPORTING
        i_kna1                  = l_kna1
        i_knvv                  = l_knvv
        pi_postflag             = 'X'
      TABLES
        t_xknvp                 = xknvp
        t_yknvp                 = yknvp
      EXCEPTIONS
        client_error            = 1
        kna1_incomplete         = 2
        knb1_incomplete         = 3
        knb5_incomplete         = 4
        knvv_incomplete         = 5
        kunnr_not_unique        = 6
        sales_area_not_unique   = 7
        sales_area_not_valid    = 8
        insert_update_conflict  = 9
        number_assignment_error = 10
        number_not_in_range     = 11
        number_range_not_extern = 12
        number_range_not_intern = 13
        account_group_not_valid = 14
        parnr_invalid           = 15
        bank_address_invalid    = 16
        tax_data_not_valid      = 17
        no_authority            = 18
        company_code_not_unique = 19
        dunning_data_not_valid  = 20
        knb1_reference_invalid  = 21
        cam_error               = 22
        OTHERS                  = 23.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ELSE.
      IF commit = 'X'.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
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
