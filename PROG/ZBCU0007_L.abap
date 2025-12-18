*&---------------------------------------------------------------------*
*& Report ZBCU0007_L
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbcu0007_l.

SELECT-OPTIONS: s_fecha FOR sy-datum NO-EXTENSION OBLIGATORY DEFAULT sy-datum,
                s_hora  FOR sy-uzeit NO-EXTENSION OBLIGATORY.

START-OF-SELECTION.

  zcl_ap_fechas=>rango2fechas( EXPORTING r_fechas = s_fecha[] IMPORTING fecha_desde = DATA(l_fini) fecha_hasta = DATA(l_ffin) ).
  zcl_ap_fechas=>rango2horas( EXPORTING r_horas = s_hora[] IMPORTING hora_desde = DATA(l_hini) hora_hasta = DATA(l_hfin) ).

  DATA(l_cont) = 0.
  WHILE l_fini <= l_ffin.
    WHILE l_hini <= l_hfin.
      IF l_cont > 1000.
        MESSAGE 'Salimos porque se han procesado 1000 interaciones' TYPE 'S'.
        LEAVE PROGRAM.
      ENDIF.
      MESSAGE |Procesando { l_fini DATE = USER } { l_hini TIME = USER }| TYPE 'S'.
      IF l_hini < sy-uzeit.
        IF zcl_ap_temp=>existe_st( clave = 'ZBCU0007_L' subclave = l_fini && l_hini ).
          MESSAGE |Ya procesado| TYPE 'S'.
          COMMIT WORK AND WAIT.
          IF l_hini >= '230000'.
            EXIT.
          ELSE.
            ADD 3600 TO l_hini.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      SUBMIT zbcu0007
        AND RETURN
       WITH rstartti = l_hini
       WITH rreadti = '010000'
       WITH rday = l_fini
       WITH p_act  = 'X'
       WITH p_list = 'N'.
      MESSAGE |Fin proceso { l_fini DATE = USER } { l_hini TIME = USER }| TYPE 'S'.
      COMMIT WORK AND WAIT.
      IF l_hini >= '230000'.
        EXIT.
      ENDIF.
      IF l_hini < sy-uzeit.
        zcl_ap_temp=>set_st( clave = 'ZBCU0007_L' subclave = l_fini && l_hini ).
      ENDIF.
      ADD 3600 TO l_hini.
    ENDWHILE.
    clear l_hini.
    ADD 1 TO l_fini.
  ENDWHILE.
