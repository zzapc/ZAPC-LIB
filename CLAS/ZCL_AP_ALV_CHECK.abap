class ZCL_AP_ALV_CHECK definition
  public
  inheriting from ZCL_AP_ALV
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TABLA type STRING default 'I_LISTADO'
      !LIST_DISPLAY type ABAP_BOOL default ''
      !CPROG type SY-CPROG default SY-CPROG
      !OPTIMIZE type ABAP_BOOL default 'X'
      !TOP_OF_PAGE type ABAP_BOOL default ' '
      !END_OF_PAGE type ABAP_BOOL default ' '
      !BOTONES_STANDARD type ABAP_BOOL default 'X'
      !COLOR type LVC_FNAME default ' '
      !STATUS type ANY default ' '
      !SEL type C default 'M'
      !CAMPO_CHECK type FIELDNAME default 'CHECK'
      !LIGHTS type STRING default ''
      !CONTAINER_NAME type C default ''
      !R_CONTAINER type ref to CL_GUI_CONTAINER optional
      !NO_LAYOUT type ABAP_BOOL default ''
      !TOP_OF_PAGE_AUTO type ABAP_BOOL default ''
      !TOP_OF_PAGE_TITULO type ANY default ''
      !LOGO type ANY default ''
      !STATUS_PROG type ANY default ''
      !HANDLE type SLIS_HANDL default ''
      !O_DEV type ref to ZCL_AP_DEV optional
      !RESTRICCION_LAYOUT type INT4 default IF_SALV_C_LAYOUT=>RESTRICT_NONE .
protected section.
private section.
endclass. "ZCL_AP_ALV_CHECK definition
class ZCL_AP_ALV_CHECK implementation.
METHOD constructor.

  CALL METHOD super->constructor
    EXPORTING
      tabla              = tabla
      list_display       = list_display
      cprog              = cprog
      optimize           = optimize
      botones_standard   = botones_standard
      color              = color
      status             = status
      sel                = sel
      campo_check        = campo_check
      lights             = lights
      container_name     = container_name
      r_container        = r_container
      no_layout          = no_layout
      top_of_page_auto   = top_of_page_auto
      top_of_page_titulo = top_of_page_titulo
      logo               = logo
      status_prog        = status_prog
      handle             = handle
      o_dev              = o_dev
      restriccion_layout = restriccion_layout.

ENDMETHOD.
