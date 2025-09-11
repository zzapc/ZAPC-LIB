
class ZCL_AP_BP definition
  public
  final
  create public .

public section.

  class-methods VISUALIZAR
    importing
      !BP type BU_PARTNER
      !ROLE type BUS_ROLES optional .
protected section.
private section.
endclass. "ZCL_AP_BP definition
class ZCL_AP_BP implementation.
  METHOD visualizar.
    DATA:
      lv_request   TYPE REF TO cl_bupa_navigation_request,
      lv_options   TYPE REF TO cl_bupa_dialog_joel_options,
      lv_start_tab TYPE bus_navigation-bupa-sub_header_tab,
      lv_bupr_main TYPE bus_bupr_maintenance.

* set start-up navigation-----------------------------------------------
    lv_start_tab = 'CVIC01'.
    CREATE OBJECT lv_request.

*set partner maintenance
    CALL METHOD lv_request->set_maintenance_id
      EXPORTING
        iv_value = lv_request->gc_maintenance_id_partner.

*set partner number to start with (in case of a guid just use the method
*set_partner_guid)
    CALL METHOD lv_request->set_partner_number( bp ).

*set the partner role to start with
    IF NOT role IS INITIAL.
      CALL METHOD lv_request->set_bupa_partner_role( role ).
    ENDIF.

*set the activity you want the user to start the maintenance with
    CALL METHOD lv_request->set_bupa_activity
      EXPORTING
        iv_value = lv_request->gc_activity_display.

    CALL METHOD lv_request->set_bupa_sub_header_tab
      EXPORTING
        iv_value = lv_start_tab.

*set start-up options---------------------------------------------------
    CREATE OBJECT lv_options.

*start the transaction with an invisible locator
    CALL METHOD lv_options->set_locator_visible( space ).

*don't allow navigations to other partners
    CALL METHOD lv_options->set_navigation_disabled( abap_true ).
    CALL METHOD lv_options->set_bupr_create_not_allowed( abap_true ).

    lv_bupr_main-create_allowed = abap_true.
    lv_bupr_main-change_allowed = abap_true.
    lv_bupr_main-delete_allowed = abap_true.

    CALL METHOD lv_options->set_bupr_maintenance( lv_bupr_main ).

    CALL METHOD lv_options->set_activity_switching_off( space ).

*Call the business partner maintenance----------------------------------
*with those parameters
    CALL METHOD cl_bupa_dialog_joel=>start_with_navigation
      EXPORTING
        iv_request = lv_request
        iv_options = lv_options
      EXCEPTIONS
        OTHERS     = 1.
  ENDMETHOD.
endclass. "ZCL_AP_BP implementation
