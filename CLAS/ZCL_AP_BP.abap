
class ZCL_AP_BP definition
  public
  final
  create public .

public section.

  class-methods VISUALIZAR
    importing
      !BP type BU_PARTNER
      !ROLE type BUS_ROLES optional .
  methods ACTUALIZAR_CREDITO
    importing
      !PARTNER type BU_PARTNER
      !KKBER type KKBER
      !LIMIT_RULE type UKM_LIMIT_RULE optional
      !RISK_CLASS type UKM_RISK_CLASS optional
      !CHECK_RULE type UKM_CHECK_RULE optional
      !CREDIT_LIMIT type UKM_CREDIT_LIMIT optional
    returning
      value(MESSAGE) type BAPI_MSG .
protected section.
private section.
endclass. "ZCL_AP_BP definition
class ZCL_AP_BP implementation.
  METHOD actualizar_credito.

    DATA:    ls_bupa_error TYPE mds_ctrls_error.
    DATA(lo_bupa) = NEW cl_im_mds_bupa_lock( ).
    lo_bupa->if_ex_bupa_lock~lock(
      EXPORTING
        iv_partner = partner
      CHANGING
        cs_error   = ls_bupa_error ).

    IF NOT ls_bupa_error-is_error IS INITIAL.
      message = 'BP bloqueado'.
    ELSE.
      DATA: lo_account       TYPE REF TO cl_ukm_account,
            ls_bp_credit_sgm TYPE ukm_s_bp_cms_sgm,
            ls_ukm_s_bp_cms  TYPE ukm_s_bp_cms.

      "set & fetch Parent data
      DATA(lo_facade) = cl_ukm_facade=>create( i_activity = cl_ukm_cnst_eventing=>bp_maintenance ).
      DATA(lo_bupa_factory) = lo_facade->get_bupa_factory( ).
      DATA(lo_partner) = lo_bupa_factory->get_business_partner( partner ).

      " Fetch & update credit profile data
      lo_partner->get_bp_cms(
        IMPORTING
          es_bp_cms = ls_ukm_s_bp_cms
      ).

      IF NOT limit_rule IS INITIAL AND ls_ukm_s_bp_cms-limit_rule NE limit_rule.
        ls_ukm_s_bp_cms-limit_rule = limit_rule.
        DATA(l_mod) = 'X'.
      ENDIF.
      IF NOT risk_class IS INITIAL AND ls_ukm_s_bp_cms-risk_class NE risk_class.
        ls_ukm_s_bp_cms-risk_class = risk_class.
        l_mod = 'X'.
      ENDIF.

      IF NOT check_rule IS INITIAL AND ls_ukm_s_bp_cms-check_rule NE check_rule.
        ls_ukm_s_bp_cms-check_rule = check_rule.
        l_mod = 'X'.
      ENDIF.
      IF l_mod = 'X'.
        ls_ukm_s_bp_cms-rating_chg_date = sy-datum.
        ls_ukm_s_bp_cms-rating_val_date = sy-datum.
        lo_partner->set_bp_cms(
          EXPORTING
            is_bp_cms = ls_ukm_s_bp_cms
        ).
      ENDIF.

      "Fetch & update credit segment data
      lo_account = lo_bupa_factory->get_credit_account(
        i_partner      = partner
        i_credit_sgmnt = CONV ukm_credit_sgmnt( kkber ) ).

      lo_account->get_bp_cms_sgm(
        IMPORTING
          es_bp_cms_sgm = ls_bp_credit_sgm ).

      IF NOT credit_limit IS INITIAL AND credit_limit NE ls_bp_credit_sgm-credit_limit.
        ls_bp_credit_sgm-credit_limit = credit_limit.
        ls_bp_credit_sgm-limit_chg_date = sy-datum.
        ls_bp_credit_sgm-req_date  = sy-datum.


        lo_account->set_bp_cms_sgm(
          EXPORTING
            is_bp_cms_sgm = ls_bp_credit_sgm
        ).
        l_mod = 'X'.
      ENDIF.

      IF l_mod = 'X'.
        lo_bupa_factory->save_all( i_upd_task = abap_false ). "For Factory methods

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'               "For API Methods
          EXPORTING
            wait = abap_true.
      ENDIF.

      lo_bupa->if_ex_bupa_lock~unlock(
        EXPORTING
          iv_partner = partner
        CHANGING
          cs_error   = ls_bupa_error ).
    ENDIF.
  ENDMETHOD.
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
