CLASS zcl_ap_equipo DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_objeto TYPE srgbtbrel-typeid_a VALUE '' ##NO_TEXT.
    CONSTANTS c_objectclas TYPE cdobjectcl VALUE '' ##NO_TEXT.

    CLASS-METHODS urls_gos_st
      IMPORTING
        !equnr       TYPE equi-equnr
      RETURNING
        VALUE(tabla) TYPE ztab_url_gos .
    CLASS-METHODS insertar_url_gos_st
      IMPORTING
        !equnr  TYPE equi-equnr
        !url    TYPE string
        !titulo TYPE string .
    CLASS-METHODS get_url_por_titulo_st
      IMPORTING
        !equnr     TYPE equi-equnr
        !titulo    TYPE string
      RETURNING
        VALUE(url) TYPE string .
    CLASS-METHODS visualizar_equipo_st
      IMPORTING
        !equnr TYPE equi-equnr .
    CLASS-METHODS get_texto_string
      IMPORTING
        !equnr        TYPE equnr
        !id           TYPE stxh-tdid DEFAULT 'LTXT'
        !spras        TYPE spras DEFAULT ''
      RETURNING
        VALUE(string) TYPE string .
    CLASS-METHODS visualizar
      IMPORTING
        !equnr TYPE any .
    CLASS-METHODS get_valor_clas
      IMPORTING
        !caract      TYPE any
        !equnr       TYPE any
      RETURNING
        VALUE(valor) TYPE atwrt .
    CLASS-METHODS get_valor_clas_num
      IMPORTING
        !caract      TYPE any
        !equnr       TYPE any
      RETURNING
        VALUE(valor) TYPE mengv13 .
  PROTECTED SECTION.
  PRIVATE SECTION.
endclass. "ZCL_AP_EQUIPO definition
class ZCL_AP_EQUIPO implementation.
  METHOD get_texto_string.

    string = zcl_ap_textos=>get_texto_string( id = id object = 'EQUI' name = equnr spras = spras ).

  ENDMETHOD.
  METHOD get_url_por_titulo_st.
    DATA: l_clave TYPE srgbtbrel-instid_a.

    l_clave = equnr.
    url = zcl_ap_gos=>get_url_por_titulo_st( tipo   = 'EQUI'
                                          clave  = l_clave
                                          titulo = titulo ).

  ENDMETHOD.
  METHOD get_valor_clas.
    DATA l_atinn TYPE atinn.

    l_atinn = zcl_ap_clasificacion=>get_caract_interno( caract ).

    SELECT atwrt FROM inob JOIN ausp ON inob~cuobj = ausp~objek
      INTO valor
      UP TO 1 ROWS
     WHERE inob~klart = '002'
       AND obtab = 'EQUI'
       AND inob~objek = equnr
       AND atinn = l_atinn
     ORDER BY inob~objek.
    ENDSELECT.

  ENDMETHOD.
  METHOD get_valor_clas_num.
    DATA: l_atinn TYPE atinn,
          l_atflv TYPE ausp-atflv,
          l_equnr TYPE equnr.

    l_atinn = zcl_ap_clasificacion=>get_caract_interno( caract ).
    l_equnr = equnr.
    __poner_ceros l_equnr.

    SELECT atflv FROM inob JOIN ausp ON inob~cuobj = ausp~objek
      INTO l_atflv
      UP TO 1 ROWS
     WHERE inob~klart = '002'
       AND obtab = 'EQUI'
       AND inob~objek = l_equnr
       AND atinn = l_atinn
      ORDER BY inob~objek.
    ENDSELECT.
    TRY.
        valor = l_atflv.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.
  METHOD insertar_url_gos_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    l_clave = equnr.
    zcl_ap_gos=>insertar_url_gos_st( tipo   = 'EQUI'
                                  clave  = l_clave
                                  titulo = titulo
                                  url    = url ).

  ENDMETHOD.
  METHOD urls_gos_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    l_clave = equnr.
    tabla = zcl_ap_gos=>urls_gos_st( tipo = 'EQUI' clave = l_clave ).

  ENDMETHOD.
  METHOD visualizar.

    SET PARAMETER ID 'EQN' FIELD equnr.
    CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.

  ENDMETHOD.
  METHOD visualizar_equipo_st.

    SET PARAMETER ID 'EQN' FIELD equnr.
    CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.

  ENDMETHOD.
