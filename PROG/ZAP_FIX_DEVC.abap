*&---------------------------------------------------------------------*
*& Report ZAP_FIX_DEVC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zap_fix_devc.

TABLES: tdevc, tadir.

DATA: BEGIN OF i_p OCCURS 0,
        pgmid    TYPE tadir-pgmid,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF i_p.

PARAMETERS: p_sysid LIKE sy-sysid DEFAULT sy-sysid OBLIGATORY,
            p_autor LIKE sy-uname DEFAULT 'APC' OBLIGATORY,
            p_devcl LIKE tdevc-pdevclass OBLIGATORY,
            p_file  LIKE t001-butxt DEFAULT 'D:\P.CSV'.

PARAMETERS: p_ct  RADIOBUTTON GROUP g,
            p_p   RADIOBUTTON GROUP g,
            p_g   RADIOBUTTON GROUP g,
            p_cli RADIOBUTTON GROUP g.

INITIALIZATION.


  CASE sy-sysid.
    WHEN 'EIN'. p_devcl = 'ZEIN'.
    WHEN 'QAS'. p_devcl = 'SAP'.
    WHEN 'GFT'. p_devcl = 'ZGFT'.
    WHEN 'YWD'. p_devcl = 'ZYWD'.
  ENDCASE.

START-OF-SELECTION.

  IF p_ct = 'X'.
    PERFORM corrige_paquetes.
  ELSEIF p_g = 'X'.
    PERFORM graba_fichero_asign.
  ELSEIF p_p = 'X'.
    PERFORM carga_fichero_asign.
  ELSEIF p_cli = 'X'.

    IF sy-sysid = 'QAS'.
      UPDATE tadir
         SET srcsystem = sy-sysid
       WHERE obj_name LIKE 'Z%'
         AND srcsystem IN ('EIN', 'ELO', 'FGV', 'ERD', 'TOR' ).

      UPDATE tdevc
         SET pdevclass = p_devcl
       WHERE devclass LIKE 'Z%'
         AND devclass LIKE 'Y%'
         AND pdevclass IN ('ZEIN', 'ZTOR', 'ZELO', 'ZQAS', 'ZFGV' ).
    ENDIF.
  ENDIF.

FORM corrige_paquetes.

  SELECT devclass FROM tdevc                            "#EC CI_GENBUFF
    INTO TABLE @DATA(i_tdevc)
     WHERE devclass LIKE 'ZAPC%'
       AND devclass NE 'ZAPCMD'.

  LOOP AT i_tdevc ASSIGNING FIELD-SYMBOL(<tdevc>).
    tdevc-devclass = <tdevc>-devclass.
    tdevc-devclass(1) = 'Y'.
    UPDATE tdevc
       SET devclass = tdevc-devclass
   WHERE devclass = <tdevc>-devclass.
  ENDLOOP.

  UPDATE tdevc
     SET parentcl = 'YAPC'
   WHERE parentcl = 'ZAPC'.

  __data_set_vart tadir.
  SELECT obj_name FROM tadir                            "#EC CI_GENBUFF
    INTO CORRESPONDING FIELDS OF TABLE i_tadir
   WHERE pgmid  = 'R3TR'
     AND object = 'DEVC'
     AND obj_name LIKE 'ZAPC%'
     AND obj_name NE 'ZAPCMD'.

  LOOP AT i_tadir ASSIGNING <tadir>.
    tadir-obj_name = <tadir>-obj_name.
    tadir-obj_name(1) = 'Y'.
    UPDATE tadir
       SET obj_name = tadir-obj_name
   WHERE pgmid  = 'R3TR'
     AND object = 'DEVC'
     AND obj_name = <tadir>-obj_name.


    UPDATE tadir
       SET devclass = tadir-obj_name
   WHERE pgmid  = 'R3TR'
     AND obj_name LIKE 'Z%'
     AND devclass = <tadir>-obj_name.

  ENDLOOP.


*  UPDATE tadir
*     SET srcsystem = p_sysid
*         author    = p_autor
*   WHERE pgmid  = 'R3TR'
*     AND object = 'DEVC'
*     AND obj_name LIKE 'YAPC%'.

  UPDATE tdevc
     SET pdevclass = p_devcl
         created_by = p_autor
         changed_by = p_autor
   WHERE devclass LIKE 'YAPC%'.

  UPDATE tadir
     SET srcsystem = p_sysid
         author    = p_autor
   WHERE pgmid  = 'R3TR'
     AND obj_name LIKE 'Z%'
     AND devclass LIKE 'YAPC%'
     AND srcsystem NE p_sysid.

*  UPDATE tadir
*   SET devclass = 'YAPC_BASE'
* WHERE pgmid  = 'R3TR'
*   AND obj_name LIKE 'Z%'
*   AND devclass = 'YAPC'.

ENDFORM.

FORM graba_fichero_asign .

  SELECT * FROM tadir                                   "#EC CI_GENBUFF
    INTO CORRESPONDING FIELDS OF TABLE i_p
   WHERE pgmid  = 'R3TR'
     AND devclass LIKE 'YAPC%'
     AND devclass NE 'YAPCMD'.

  zcl_ap_ficheros=>grabar( EXPORTING fichero = p_file tipo = 'DAT' CHANGING tabla = i_p[] ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CARGA_FICHERO_ASIGN
*&---------------------------------------------------------------------*
FORM carga_fichero_asign .

  zcl_ap_ficheros=>leer( EXPORTING fichero = p_file tipo = 'DAT' CHANGING tabla = i_p[] ).

  LOOP AT i_p.
    SELECT SINGLE * FROM tadir
     WHERE pgmid     = i_p-pgmid
       AND object    = i_p-object
       AND obj_name  = i_p-obj_name.
    IF sy-subrc = 0.
      IF tadir-devclass NE i_p-devclass.
        SELECT SINGLE * FROM tdevc
         WHERE devclass = i_p-devclass.
        IF sy-subrc NE 0.
          SELECT SINGLE * FROM tdevc
           WHERE devclass = 'YAPC_BASE'.
          IF sy-subrc = 0.
            tdevc-devclass = i_p-devclass.
            INSERT tdevc.
          ENDIF.
        ENDIF.

        UPDATE tadir
           SET devclass  = i_p-devclass
         WHERE pgmid     = i_p-pgmid
           AND object    = i_p-object
           AND obj_name  = i_p-obj_name.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
