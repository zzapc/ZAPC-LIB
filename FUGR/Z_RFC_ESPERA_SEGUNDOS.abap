FUNCTION Z_RFC_ESPERA_SEGUNDOS.
*"--------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(SEGUNDOS) TYPE  I DEFAULT 1
*"--------------------------------------------------------------------
* Ejemplo de llamada:
*   CALL FUNCTION 'Z_RFC_ESPERA_SEGUNDOS'
*     STARTING NEW TASK 'IF'
*       PERFORMING START_REFRESH ON END OF TASK.
* form START_REFRESH USING TASKNAME.
*   message sy-uzeit type i.
* endform.
* http://www.erpgreat.com/fu014.htm

  DATA: ztime LIKE sy-uzeit.

  GET TIME.

  ztime = sy-uzeit + segundos.

  DO.
    GET TIME.
    IF sy-uzeit >= ztime.
      EXIT.
    ENDIF.
  ENDDO.





ENDFUNCTION.
