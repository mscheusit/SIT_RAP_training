CLASS lsc_zsitc_i_mm_article DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

ENDCLASS.

CLASS lsc_zsitc_i_mm_article IMPLEMENTATION.

  METHOD save_modified.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_Article DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Article RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Article RESULT result.
    METHODS validateArticleNo FOR VALIDATE ON SAVE
      IMPORTING keys FOR Article~validateArticleNo.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Article RESULT result.

    METHODS release FOR MODIFY
      IMPORTING keys FOR ACTION Article~release RESULT result.
    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE Article.

    METHODS earlynumbering_cba_Text FOR NUMBERING
      IMPORTING entities FOR CREATE Article\_Text.

ENDCLASS.

CLASS lhc_Article IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD earlynumbering_create.

    " ensure Article ID is not set yet (idempotent) - must be checked when BO is draft-enabled
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      IF <ls_entity>-ArticleID IS INITIAL.
        " Set Article ID
        TRY.
            <ls_entity>-ArticleID = zcl_mm_i_article=>create_key(  ).
          CATCH cx_uuid_error INTO DATA(lo_error).
            APPEND VALUE #(  %cid = <ls_entity>-%cid
                             %key = <ls_entity>-%key
                             %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                           text     = lo_error->get_text( ) )
                          ) TO reported-article.
            APPEND VALUE #(  %cid = <ls_entity>-%cid
                             %key = <ls_entity>-%key
                          ) TO failed-article.
            CONTINUE.
        ENDTRY.
        ASSERT <ls_entity>-ArticleID IS NOT INITIAL.
        APPEND VALUE #( %cid  = <ls_entity>-%cid
                        %key  = <ls_entity>-%key
                      ) TO mapped-article.
      ELSE.
        APPEND CORRESPONDING #( <ls_entity> ) TO mapped-article.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD earlynumbering_cba_Text.
  ENDMETHOD.

  METHOD validateArticleNo.
  ENDMETHOD.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD release.
  ENDMETHOD.

ENDCLASS.
