class ZCL_AP_PEP definition
  public
  create public .

public section.

  constants C_EVENTO type CHAR2 value 'EV' ##NO_TEXT.
  constants C_FERIA type CHAR2 value 'CF' ##NO_TEXT.
  constants C_ESTADO_IMBL type JEST-STAT value 'I0064' ##NO_TEXT.
  constants C_ESTADO_DMBL type JEST-STAT value 'I0065' ##NO_TEXT.
  constants C_ESTADO_LIBS type JEST-STAT value 'E0001' ##NO_TEXT.

  class-methods GET_EXTERNO
    importing
      !PEP type EKKN-PS_PSP_PNR
    returning
      value(TEXTO) type STRING .
  class-methods GET_STATUS
    importing
      !PEP type EKKN-PS_PSP_PNR
    returning
      value(STATUS) type STTXT .
  class-methods GET_INTERNO
    importing
      value(PEP) type ANY
    returning
      value(OBJNR) type EKKN-PS_PSP_PNR .
  class-methods ES_INACTIVO
    importing
      !PEP type EKKN-PS_PSP_PNR
    returning
      value(SI) type ABAP_BOOL .
  class-methods GET_PEP_PADRE
    importing
      !PEP type EKKN-PS_PSP_PNR optional
      !POSID type PRPS-POSID optional
    returning
      value(PADRE) type EKKN-PS_PSP_PNR .
  class-methods GET_PROYECTO_FROM_PEP
    importing
      !PEP type EKKN-PS_PSP_PNR optional
      !POSID type PRPS-POSID optional
    preferred parameter PEP
    returning
      value(PADRE) type EKKN-PS_PSP_PNR .
  class-methods GET_PSPNR
    importing
      !PROYECTO type ANY
    returning
      value(PSPNR) type PROJ-PSPNR .
  class-methods GET_PROJ_INT
    importing
      !PROYECTO type ANY
    returning
      value(PSPNR) type PROJ-PSPNR .
  class-methods GET_PROJ_EXT
    importing
      value(PSPNR) type PROJ-PSPNR
    returning
      value(PROYECTO) type PROJ-PSPID .
  class-methods CONTIENE_STATUS
    importing
      !PEP type EKKN-PS_PSP_PNR
      !STATUS type ANY
      !SPRAS type SY-LANGU default SY-LANGU
      !STATUS_EXCLUIR type ANY default ''
      !STSMA type TJ30T-STSMA default ''
      !USUARIO type ABAP_BOOL default ''
    returning
      value(SI_CONTIENE) type ABAP_BOOL .
  class-methods GET_PROYECTO_FROM_OBJNR
    importing
      !PEP type PRPS-OBJNR optional
    returning
      value(PADRE) type EKKN-PS_PSP_PNR .
  class-methods VISUALIZAR
    importing
      !PEP type ANY optional
      !PROYECTO type ABAP_BOOL default ''
    preferred parameter PEP .
protected section.
private section.
endclass. "ZCL_AP_PEP definition
class ZCL_AP_PEP implementation.
METHOD contiene_status.
    DATA: i_status  TYPE TABLE OF j_txt04,
          l_txt04   TYPE j_txt04,
          l_objnr   TYPE jest-objnr,
          l_istat   TYPE j_status,
          r_status  TYPE RANGE OF j_status,
          lr_status LIKE LINE OF r_status.

    SPLIT status AT ',' INTO TABLE i_status.

    LOOP AT i_status INTO l_txt04.
      IF usuario IS INITIAL.
        SELECT istat FROM tj02t                         "#EC CI_GENBUFF
          INTO l_istat
         WHERE spras = spras
           AND txt04 = l_txt04.

        CLEAR lr_status.
        lr_status-option = 'EQ'.
        lr_status-sign   = 'I'.
        lr_status-low    = l_istat.
        APPEND lr_status TO r_status.
      ENDSELECT.
    ELSE.
      IF NOT stsma IS INITIAL.
        SELECT estat FROM tj30t
          INTO l_istat
          UP TO 1 ROWS
         WHERE stsma = stsma
           AND spras = spras
           AND txt04 = l_txt04
         ORDER BY PRIMARY KEY.
        ENDSELECT.
      ELSE.
        SELECT estat FROM tj30t
          INTO l_istat
          UP TO 1 ROWS
         WHERE stsma = stsma
           AND spras = spras
           AND txt04 = l_txt04
         ORDER BY PRIMARY KEY.
        ENDSELECT.
      ENDIF.
      IF sy-subrc = 0.
        CLEAR lr_status.
        lr_status-option = 'EQ'.
        lr_status-sign   = 'I'.
        lr_status-low    = l_istat.
        APPEND lr_status TO r_status.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF NOT status_excluir IS INITIAL.
    REFRESH i_status.
    SPLIT status_excluir AT ',' INTO TABLE i_status.
    LOOP AT i_status INTO l_txt04.
      IF usuario IS INITIAL.
        SELECT SINGLE istat FROM tj02t                  "#EC CI_GENBUFF
          INTO l_istat
         WHERE spras = spras
           AND txt04 = l_txt04.
      ELSE.
        IF NOT stsma IS INITIAL.
          SELECT SINGLE estat FROM tj30t
            INTO l_istat
           WHERE stsma = stsma
             AND spras = spras
             AND txt04 = l_txt04.
        ELSE.
          SELECT estat FROM tj30t
            INTO l_istat
            UP TO 1 ROWS
           WHERE stsma = stsma
             AND spras = spras
             AND txt04 = l_txt04
           ORDER BY PRIMARY KEY.
          ENDSELECT.
        ENDIF.
      ENDIF.
      IF sy-subrc = 0.
        CLEAR lr_status.
        lr_status-option = 'EQ'.
        lr_status-sign   = 'E'.
        lr_status-low    = l_istat.
        APPEND lr_status TO r_status.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF r_status IS INITIAL.
    EXIT.
  ENDIF.

  CONCATENATE 'PR' pep INTO l_objnr.

  SELECT SINGLE stat FROM  jest
    INTO l_istat
   WHERE objnr  = l_objnr
     AND stat   IN r_status
     AND inact  = ''.
  IF sy-subrc = 0.
    si_contiene = 'X'.
  ENDIF.

ENDMETHOD.
METHOD es_inactivo.
  DATA: l_objnr TYPE jest-objnr,
        l_jest  TYPE jest.

  CONCATENATE 'PR' pep INTO l_objnr.

  CLEAR si.
  SELECT SINGLE objnr FROM jest
    INTO l_jest-objnr
   WHERE objnr =  l_objnr
     AND stat  = c_estado_imbl
     AND inact = ''.
  IF sy-subrc = 0.
    si = 'X'.
  ENDIF.


ENDMETHOD.
METHOD get_externo.

  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
    EXPORTING
      input  = pep
    IMPORTING
      output = texto.


ENDMETHOD.
METHOD get_interno.

*  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
*    EXPORTING
*      input  = pep
*    IMPORTING
*      output = objnr.

  CALL FUNCTION 'CONVERSION_EXIT_KONPR_INPUT'               "#EC EXISTS
    EXPORTING
      input         = pep
    IMPORTING
      output        = objnr
    EXCEPTIONS
      not_found     = 1
      error_message = 999. "Cualquier otra excepción!

ENDMETHOD.
METHOD get_pep_padre.
  DATA l_pep TYPE prps-pspnr.

  IF pep IS INITIAL.
    l_pep = get_interno( posid ).
  ELSE.
    l_pep = pep.
  ENDIF.

  CLEAR padre.
  SELECT SINGLE up FROM prhi
    INTO padre
   WHERE posnr = l_pep.


ENDMETHOD.
METHOD get_proj_ext.

  CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
    EXPORTING
      input  = pspnr
    IMPORTING
      output = proyecto.
*   PSELT         = PSELT

ENDMETHOD.
METHOD get_proj_int.

  CALL FUNCTION 'CONVERSION_EXIT_KONPD_INPUT'
    EXPORTING
      input     = proyecto
    IMPORTING
      output    = pspnr
*     PROJWA    = PROJWA
    EXCEPTIONS
      not_found = 1.

ENDMETHOD.
METHOD get_proyecto_from_objnr.
  IF NOT pep IS INITIAL.
    CLEAR padre.
    SELECT psphi FROM prps                              "#EC CI_NOFIELD
      INTO padre
      UP TO 1 ROWS
     WHERE objnr = pep
     ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDIF.
ENDMETHOD.
METHOD get_proyecto_from_pep.
  DATA l_pep TYPE prps-pspnr.

  IF pep IS INITIAL.
    l_pep = get_interno( posid ).
  ELSE.
    l_pep = pep.
  ENDIF.

  CLEAR padre.
  SELECT SINGLE psphi FROM prps
    INTO padre
   WHERE pspnr = l_pep.

ENDMETHOD.
METHOD get_pspnr.

  CALL FUNCTION 'CONVERSION_EXIT_KONPD_INPUT'
    EXPORTING
      input     = proyecto
    IMPORTING
      output    = pspnr
*     PROJWA    = PROJWA
    EXCEPTIONS
      not_found = 1.

ENDMETHOD.
METHOD get_status.
  DATA l_objnr TYPE jest-objnr.

  CONCATENATE 'PR' pep INTO l_objnr.

  CALL FUNCTION 'STATUS_TEXT_EDIT'
    EXPORTING
      objnr            = l_objnr
      spras            = sy-langu
      flg_user_stat    = 'X'
    IMPORTING
      line             = status
    EXCEPTIONS
      object_not_found = 01.


ENDMETHOD.
METHOD visualizar.
  DATA l_posid TYPE ps_posid.

  WRITE pep TO l_posid.

  IF proyecto = 'X'.
    SET PARAMETER ID 'PSP' FIELD l_posid.
    SET PARAMETER ID 'PRO' FIELD ''.
  ELSE.
    SET PARAMETER ID 'PSP' FIELD ''.
    SET PARAMETER ID 'PRO' FIELD l_posid.
  ENDIF.
  CALL TRANSACTION 'CJ13' AND SKIP FIRST SCREEN.

ENDMETHOD.
