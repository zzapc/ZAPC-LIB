FUNCTION Z_RFC_GET_EMPLEADOS.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  EXPORTING
*"     VALUE(CONTENIDO) TYPE  XSTRING
*"  TABLES
*"      I_EMPLEADOS STRUCTURE  ZHE_EMPLEADOS
*"      R_PERNR STRUCTURE  PERSNO_RANGE OPTIONAL
*"      R_PERSK STRUCTURE  PERSK_RANGE OPTIONAL
*"----------------------------------------------------------------------
  SELECT DISTINCT pernr ename FROM pa0001
        INTO CORRESPONDING FIELDS OF TABLE i_empleados
     WHERE pernr IN r_pernr
       AND persk IN r_persk.

  CALL TRANSFORMATION id
       SOURCE i_empleados = i_empleados
       RESULT XML contenido.

ENDFUNCTION.
