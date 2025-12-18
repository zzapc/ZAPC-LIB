TYPES: BEGIN OF T_AUT_USER,
         OBJETO     TYPE UST12-OBJCT,
         PERFIL	    TYPE ZEST_AUT-PERFIL,
         VALOR      TYPE ZEST_AUT-VALOR,
         ACTIVIDAD  TYPE ZEST_AUT-ACTIVIDAD,
       END OF T_AUT_USER.
TYPES TT_AUT_USER TYPE SORTED TABLE OF T_AUT_USER
                       WITH UNIQUE KEY OBJETO PERFIL VALOR.
class ZCL_AP_AUTORIZACION definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !UNAME type SY-UNAME default SY-UNAME .
  class-methods ES_SAPALL
    importing
      !UNAME type SY-UNAME default SY-UNAME
    returning
      value(SI) type ABAP_BOOL .
  class-methods GET_AUT_OBJETO
    importing
      !UNAME type SY-UNAME default SY-UNAME
      !OBJETO type UST12-OBJCT optional
      !CAMPO type ANY optional
      !CAMPO2 type ANY optional
      !VALOR type ANY default '*'
      !ACTIVIDAD type ANY default '*'
    preferred parameter OBJETO
    returning
      value(I_AUT) type ZTAB_AUT .
  methods GET_AUT_OBJ
    importing
      !OBJETO type UST12-OBJCT optional
      !CAMPO type ANY optional
      !VALOR type ANY default '*'
      !ACTIVIDAD type ANY default '*'
    preferred parameter OBJETO
    returning
      value(I_AUT) type ZTAB_AUT .
  class-methods TIENE_ROL
    importing
      !UNAME type SY-UNAME default SY-UNAME
      !ROL type ANY optional
      !FECHA type SY-DATUM default SY-DATUM
    preferred parameter ROL
    returning
      value(SI_TIENE) type ABAP_BOOL .
  class-methods TIENE_PERFIL
    importing
      !UNAME type SY-UNAME default SY-UNAME
      !PERFIL type XUPROFILE
    returning
      value(SI_TIENE) type ABAP_BOOL .
  class-methods ES_USUARIO_SISTEMAS
    importing
      !UNAME type SY-UNAME default SY-UNAME
    returning
      value(SI) type ABAP_BOOL .
  PROTECTED SECTION.

    DATA uname TYPE sy-uname .
    DATA usuario_sapall TYPE abap_bool .
  PRIVATE SECTION.

    DATA i_aut_user TYPE tt_aut_user .
    DATA aut_user TYPE t_aut_user .
endclass. "ZCL_AP_AUTORIZACION definition
class ZCL_AP_AUTORIZACION implementation.
  METHOD constructor.

    me->uname = uname.

    CLEAR: i_aut_user, aut_user.

    usuario_sapall  = es_sapall( uname ).

  ENDMETHOD.
  METHOD es_sapall.
    DATA l_profile TYPE ust04-profile.

    CLEAR si.
    SELECT SINGLE profile FROM ust04
      INTO l_profile
     WHERE bname   = uname
       AND profile = 'SAP_ALL'.
    IF sy-subrc = 0.
      si = 'X'.
    ENDIF.

  ENDMETHOD.
  METHOD es_usuario_sistemas.

    CLEAR si.
    TRY.
        CALL METHOD ('ZCL_USUARIO')=>('ES_USUARIO_SISTEMAS')
          EXPORTING
            uname = uname
          RECEIVING
            si    = si.
      CATCH cx_root INTO DATA(o_root).
        DATA(l_rol_sistemas) = zcl_c=>get_constante( 'ROL_SISTEMAS' ).
        IF NOT l_rol_sistemas IS INITIAL.
          si = tiene_rol( uname = uname
                          rol   = l_rol_sistemas ).
        ELSE.
          si = es_sapall( uname = uname ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.
  METHOD get_aut_obj.
    DATA: l_aut TYPE zest_aut.
    LOOP AT i_aut_user INTO aut_user WHERE objeto = objeto.
      CLEAR l_aut.
      MOVE-CORRESPONDING aut_user TO l_aut.
      APPEND l_aut TO i_aut.
    ENDLOOP.
    IF sy-subrc NE 0.
      i_aut  = get_aut_objeto( uname     = me->uname
                               objeto    = objeto
                               campo     = campo ).
      LOOP AT i_aut INTO l_aut.
        CLEAR aut_user.
        aut_user-objeto = objeto.
        MOVE-CORRESPONDING l_aut TO aut_user.
        INSERT aut_user INTO TABLE i_aut_user.
      ENDLOOP.
    ENDIF.

    IF actividad NE '*'.
      DELETE i_aut WHERE NOT actividad CS actividad
                     AND NOT actividad CS '*'.
    ENDIF.
    IF valor NE '*'.
      DELETE i_aut WHERE NOT valor = valor
                     AND valor NE '*'.
    ENDIF.

  ENDMETHOD.
  METHOD get_aut_objeto.
    DATA: i_usvalues  TYPE TABLE OF usvalues,
          i_usvalues2 TYPE TABLE OF usvalues,
          l_usvalues  TYPE usvalues,
          l_usvalues2 TYPE usvalues,
          l_todo, l_ok, l_mod,
          l_aut       TYPE zest_aut.

    CLEAR i_usvalues.
    CALL FUNCTION 'SUSR_USER_AUTH_FOR_OBJ_GET'
      EXPORTING
        user_name           = uname
        sel_object          = objeto
      TABLES
        values              = i_usvalues
      EXCEPTIONS
        user_name_not_exist = 1
        not_authorized      = 2
        internal_error      = 3
        OTHERS              = 4.

    LOOP AT i_usvalues INTO l_usvalues.
      l_usvalues-auth = l_usvalues-auth(10).
      COLLECT l_usvalues INTO i_usvalues2.
    ENDLOOP.
    SORT i_usvalues2.
    i_usvalues = i_usvalues2.

    LOOP AT i_usvalues INTO l_usvalues WHERE field = campo.
      CLEAR l_aut.
      l_aut-perfil = l_usvalues-auth.
      l_aut-valor  = l_usvalues-von.
      LOOP AT i_usvalues2 INTO l_usvalues2
                          WHERE auth = l_usvalues-auth
                            AND field = 'ACTVT'.
        IF l_aut-actividad IS INITIAL.
          l_aut-actividad = l_usvalues2-von.
        ELSE.
          CONCATENATE l_aut-actividad ',' l_usvalues2-von
                 INTO l_aut-actividad.
        ENDIF.
      ENDLOOP.
      APPEND l_aut TO i_aut.
    ENDLOOP.

    IF actividad NE '*'.
      DELETE i_aut WHERE NOT actividad CS actividad
                     AND NOT actividad CS '*'.
    ENDIF.
    IF valor NE '*'.
      DELETE i_aut WHERE NOT valor = valor
                     AND valor NE '*'.
    ENDIF.

  ENDMETHOD.
  METHOD tiene_perfil.
    DATA l_ust04 TYPE ust04.

    CLEAR si_tiene.

    SELECT SINGLE * FROM ust04
      INTO l_ust04
     WHERE bname   = sy-uname
       AND profile = perfil.
    IF sy-subrc = 0.
      si_tiene = 'X'.
    ENDIF.

  ENDMETHOD.
  METHOD tiene_rol.
    DATA l_agr_users TYPE agr_users.

    CLEAR si_tiene.
    SELECT SINGLE * FROM  agr_users
      INTO l_agr_users
     WHERE agr_name  = rol
       AND uname     = uname
       AND from_dat  <= fecha
       AND to_dat    >= fecha.
    IF sy-subrc = 0.
      si_tiene = 'X'.
    ENDIF.

  ENDMETHOD.
