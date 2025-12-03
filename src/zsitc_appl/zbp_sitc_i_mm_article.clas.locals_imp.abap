CLASS lhc_articletext DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR ArticleText RESULT result.

ENDCLASS.

CLASS lhc_articletext IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF ZSITC_I_MM_Article IN LOCAL MODE
       ENTITY Article
          FIELDS ( ArticleID Released )
          WITH CORRESPONDING #( keys )
        RESULT DATA(lt_article)
       ENTITY Article BY \_Text
       FIELDS ( ArticleID )
          WITH CORRESPONDING #( keys )
        RESULT DATA(lt_article_texts)
        FAILED failed.

    LOOP AT lt_article ASSIGNING FIELD-SYMBOL(<ls_article>).
      DATA(lv_is_upd_able) = zcl_mm_i_article=>is_update_able( CORRESPONDING #( <ls_article> ) ).
      result = VALUE #( FOR <ls_article_text> IN lt_article_texts USING KEY entity WHERE ( ArticleID = <ls_article>-ArticleID )
       ( %tky = <ls_article_text>-%tky
         %features-%update = COND #( WHEN lv_is_upd_able = abap_true THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled )
         %features-%delete = COND #( WHEN lv_is_upd_able = abap_true THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled ) ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

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
    CONSTANTS gc_x VALUE 'X'.

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

    CHECK zcl_mm_i_article=>gv_eml_bo_bl_enabled = abap_true.

    " ensure Article ID is not set yet (idempotent) - must be checked when BO is draft-enabled
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      IF <ls_entity>-ArticleID IS INITIAL.
        " Set Article ID
        TRY.
            DATA(lv_ArticleID) = zcl_mm_i_article=>create_key(  ).
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
        ASSERT lv_articleid IS NOT INITIAL.
        APPEND CORRESPONDING #( <ls_entity> ) TO mapped-article ASSIGNING FIELD-SYMBOL(<ls_entiy_mapped>).
        <ls_entiy_mapped>-ArticleID = lv_articleid.
        APPEND VALUE #( %cid  = <ls_entiy_mapped>-%cid
                        %key  = <ls_entiy_mapped>-%key
                      ) TO mapped-article.
      ELSE.
        APPEND CORRESPONDING #( <ls_entity> ) TO mapped-article.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD earlynumbering_cba_Text.

    READ ENTITIES OF ZSITC_I_MM_Article IN LOCAL MODE
        ENTITY article BY \_Text
          FROM CORRESPONDING #( entities )
          LINK DATA(lt_text).

    " Loop over all unique TravelIDs
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<ls_text>) GROUP BY <ls_text>-ArticleID.
      LOOP AT <ls_text>-%target ASSIGNING FIELD-SYMBOL(<ls_text_trg>).
        " language is mandatory
        APPEND CORRESPONDING #( <ls_text_trg> ) TO mapped-articletext ASSIGNING FIELD-SYMBOL(<ls_article_text>).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD validateArticleNo.

    CHECK zcl_mm_i_article=>gv_eml_bo_bl_enabled = abap_true.

    READ ENTITIES OF ZSITC_I_MM_Article IN LOCAL MODE
      ENTITY Article
         FIELDS ( ArticleID ArticleType ArticleNo )
         WITH CORRESPONDING #( keys )
       RESULT DATA(lt_articles).
    LOOP AT lt_articles ASSIGNING FIELD-SYMBOL(<ls_article>).
      TRY.
          zcl_mm_i_article=>check_article_no(  VALUE #( ArticleID = <ls_article>-ArticleID
                                                        ArticleType = <ls_article>-ArticleType
                                                        ArticleNo = <ls_article>-ArticleNo )  ).
        CATCH zcx_ca_exception_symsg INTO DATA(lo_error).
          failed-article = VALUE #( BASE failed-article ( %tky = <ls_article>-%tky ) ).
          reported-article = VALUE #(  BASE reported-article ( %tky = <ls_article>-%tky
                                                               %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                                                             text     = lo_error->get_text( ) )
                                                               %element-ArticleNo = if_abap_behv=>mk-on ) ).
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_features.

    CHECK zcl_mm_i_article=>gv_eml_bo_bl_enabled = abap_true.

    READ ENTITIES OF ZSITC_I_MM_Article IN LOCAL MODE
      ENTITY Article
         FIELDS ( ArticleID Released )
         WITH CORRESPONDING #( keys )
       RESULT DATA(lt_articles)
       FAILED failed.

    LOOP AT lt_articles ASSIGNING FIELD-SYMBOL(<ls_article>).
      DATA(lv_is_upd_able) = zcl_mm_i_article=>is_update_able( CORRESPONDING #( <ls_article> ) ).
      result = VALUE #( BASE result (  %tky = <ls_article>-%tky
                                       %features-%action-release = COND #( WHEN lv_is_upd_able = abap_true THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled )
                                       %assoc-_Text = COND #( WHEN lv_is_upd_able = abap_true THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled )
                                       %features-%update = COND #( WHEN lv_is_upd_able = abap_true THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled ) ) ).
    ENDLOOP.

*    result = VALUE #( FOR <ls_article> IN lt_articles
*       ( %tky = <ls_article>-%tky
*         %features-%action-release = COND #( WHEN <ls_article>-Released IS INITIAL THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled )
*         %assoc-_Text = COND #( WHEN <ls_article>-Released IS INITIAL THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled )
*         //%features-%field-ArticleNo = COND #( WHEN <ls_article>-Released IS INITIAL THEN if_abap_behv=>fc-f-unrestricted ELSE if_abap_behv=>fc-f-read_only )
*         //%features-%field-ArticleType = COND #( WHEN <ls_article>-Released IS INITIAL THEN if_abap_behv=>fc-f-unrestricted ELSE if_abap_behv=>fc-f-read_only )
*         //%features-%field-RegularVendor = COND #( WHEN <ls_article>-Released IS INITIAL THEN if_abap_behv=>fc-f-unrestricted ELSE if_abap_behv=>fc-f-read_only )
*         %features-%update = COND #( WHEN <ls_article>-Released IS INITIAL THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled )
*       ) ).

  ENDMETHOD.

  METHOD release.

    MODIFY ENTITIES OF ZSITC_I_MM_Article IN LOCAL MODE
             ENTITY Article
                UPDATE FIELDS ( Released ReleasedDate )
                   WITH VALUE #( FOR key IN keys ( %tky      = key-%tky
                                                   Released = gc_x
                                                   ReleasedDate = COND #( WHEN key-%param-ReleaseDate IS INITIAL THEN cl_abap_context_info=>get_system_date( ) ELSE key-%param-ReleaseDate ) ) ).

    " read changed data for result
    READ ENTITIES OF ZSITC_I_MM_Article IN LOCAL MODE
      ENTITY Article
         ALL FIELDS WITH
         CORRESPONDING #( keys )
       RESULT DATA(lt_article).

    result = VALUE #( FOR <ls_article> IN lt_article ( %tky      = <ls_article>-%tky
                                                       %param    = <ls_article> ) ).

  ENDMETHOD.

ENDCLASS.
