FUNCTION z_rfc_get_source.
*"--------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(REPORT) TYPE  RS38M-PROGRAMM
*"  TABLES
*"      I_CONTENT STRUCTURE  SOLI OPTIONAL
*"--------------------------------------------------------------------

  READ REPORT report INTO i_content.

ENDFUNCTION.
