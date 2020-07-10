context("Test variable replacement in HC")

# ------------------------------------------------------------------------------------------------------------#
#
# ------- DEFINE UTILITIES -------
#
# ------------------------------------------------------------------------------------------------------------#


# ------------------------------------------------------------------------------------------------------------#
#
# ------- DEFINE UTILITIES -------
#
# ------------------------------------------------------------------------------------------------------------#

fast_mixl <- "


//LV  = @B_LV_AGE * $age + @B_LV_MALE * $sex + @B_LV_URBAN * $urban + @SIGMA_LV * draw_1 ;
LV = 1;

P_indic_1 = ( exp(0 - @C_1 - 1* LV )/(1 +  exp(0 - @C_1 - 1* LV )))  * $car8_1
          + ( exp(@t2_1 - @C_1 - 1* LV )/(1 +  exp(@t2_1 - @C_1 - 1* LV )) - ( exp(0 - @C_1 - 1* LV ) / (1 +  exp(0 - @C_1 - 1* LV )))) * $car8_2
          + ( exp(@t3_1 - @C_1 - 1* LV )/(1 +  exp(@t3_1 - @C_1 - 1* LV )) - ( exp(@t2_1 - @C_1 - 1* LV ) / (1 +  exp(@t2_1 - @C_1 - 1* LV )))) * $car8_3
          + (1- exp(@t3_1 - @C_1 - 1* LV )/(1 +  exp(@t3_1 - @C_1 - 1* LV ))) * $car8_4 ;

P_indic_2 = ( exp(0 - @C_2 - @THETA_2* LV )/(1 +  exp(0 - @C_2 - @THETA_2* LV )))  * $car13_1
          + ( exp(@t2_2 - @C_2- @THETA_2* LV )/(1 +  exp(@t2_2 - @C_2 - @THETA_2* LV )) - ( exp(0 - @C_2  - @THETA_2* LV ) / (1 +  exp(0 - @C_2 - @THETA_2* LV )))) * $car13_2
          + ( exp(@t3_2 - @C_2- @THETA_2* LV )/(1 +  exp(@t3_2 - @C_2 - @THETA_2* LV )) - ( exp(@t2_2 - @C_2 - @THETA_2* LV ) / (1 +  exp(@t2_2 - @C_2 - @THETA_2* LV )))) * $car13_3
          + (1- exp(@t3_2 - @C_2- @THETA_2* LV )/(1 +  exp(@t3_2 - @C_2 - @THETA_2* LV ))) * $car13_4 ;

P_indic_3 = ( exp(0 - @C_3 - @THETA_3* LV )/(1 +  exp(0 - @C_3 - @THETA_3* LV )))  * $car21_1
          + ( exp(@t2_3 - @C_3 - @THETA_3* LV )/(1 +  exp(@t2_3 - @C_3 - @THETA_3* LV )) - ( exp(0 - @C_3 - @THETA_3* LV ) / (1 +  exp(0 - @C_3 - @THETA_3* LV )))) * $car21_2
          + ( exp(@t3_3 - @C_3 - @THETA_3* LV )/(1 +  exp(@t3_3 - @C_3 - @THETA_3* LV )) - ( exp(@t2_3 - @C_3 - @THETA_3* LV ) / (1 +  exp(@t2_3 - @C_3 - @THETA_3* LV )))) * $car21_3
          + (1- exp(@t3_3 - @C_3 - @THETA_3* LV )/(1 +  exp(@t3_3 - @C_3 - @THETA_3* LV ))) * $car21_4 ;

P_indic_4 = ( exp(0 - @C_4 - @THETA_4* LV )/(1 +  exp(0 - @C_4 - @THETA_4* LV )))  * $car23_1
          + ( exp(@t2_4 - @C_4 - @THETA_4* LV )/(1 +  exp(@t2_4 - @C_4 - @THETA_4* LV )) - ( exp(0 - @C_4 - @THETA_4* LV ) / (1 +  exp(0 - @C_4 - @THETA_4* LV )))) * $car23_2
          + ( exp(@t3_4 - @C_4 - @THETA_4* LV )/(1 +  exp(@t3_4 - @C_4 - @THETA_4* LV )) - ( exp(@t2_4 - @C_4 - @THETA_4* LV ) / (1 +  exp(@t2_4 - @C_4 - @THETA_4* LV )))) * $car23_3
          + (1- exp(@t3_4 - @C_4  - @THETA_4* LV )/(1 +  exp(@t3_4 - @C_4 - @THETA_4* LV ))) * $car23_4 ;

P_indic_5 = ( exp(0 - @C_5 - @THETA_5* LV )/(1 +  exp(0 - @C_5 - @THETA_5* LV )))  * $pt3_1
          + ( exp(@t2_5 - @C_5 - @THETA_5* LV )/(1 +  exp(@t2_5 - @C_5 - @THETA_5* LV )) - ( exp(0 - @C_5 - @THETA_5* LV ) / (1 +  exp(0 - @C_5 - @THETA_5* LV )))) * $pt3_2
          + ( exp(@t3_5 - @C_5 - @THETA_5* LV )/(1 +  exp(@t3_5 - @C_5 - @THETA_5* LV )) - ( exp(@t2_5 - @C_5 - @THETA_5* LV ) / (1 +  exp(@t2_5 - @C_5 - @THETA_5* LV )))) * $pt3_3
          + (1- exp(@t3_5 - @C_5 - @THETA_5* LV )/(1 +  exp(@t3_5 - @C_5 - @THETA_5* LV ))) * $pt3_4 ;


B_COST_RND = -exp( @B_COST + draw_2 * @SIGMA_COST ) * pow( $distie, @LAMBDA_COST) ;


B_TT_W_RND  = ( @B_TT_W + @B_AGE_TT_W*$age + @B_URBAN_TT_W*$urban ) ;

B_TT_B_RND  = ( @B_TT_B + @B_AGE_TT_B*$age + @B_URBAN_TT_B*$urban ) ;

B_TT_C_RND = ( @B_TT_C + @B_AGE_TT_C*$age + @B_URBAN_TT_C*$urban ) ;

B_TT_PT_RND = ( @B_TT_PT + @B_AGE_TT_PT*$age + @B_URBAN_TT_PT*$urban ) ;

B_TT_CS_RND = ( @B_TT_CS + @B_AGE_TT_CS*$age + @B_URBAN_TT_CS*$urban ) ;

B_TT_CP_RND = ( @B_TT_CP + @B_AGE_TT_CP*$age + @B_URBAN_TT_CP*$urban ) ;


ASC_W_RNP  = @ASC_W + @A_AGE_W*$age + @A_URBAN_W*$urban + @A_MALE_W*$sex + draw_3 * @SIGMA_W + @LV_W *  LV   ;

ASC_B_RNP  = @ASC_B + @A_AGE_B*$age + @A_URBAN_B*$urban + @A_MALE_B*$sex+ draw_4 * @SIGMA_B + @LV_B *  LV  ;

ASC_C_RNP  = @ASC_C + @A_AGE_C*$age + @A_URBAN_C*$urban + @A_MALE_C*$sex + draw_5 * @SIGMA_C + @LV_C *  LV  ;

ASC_PT_RNP =      0 + draw_6 * @SIGMA_PT ;

ASC_CS_RNP = @ASC_CS + @A_AGE_CS*$age + @A_URBAN_CS*$urban + @A_MALE_CS*$sex + draw_7 * @SIGMA_CS + @LV_CS *  LV  ;

ASC_CP_RNP = @ASC_CP + @A_AGE_CP*$age + @A_URBAN_CP*$urban + @A_MALE_CP*$sex + draw_8 * @SIGMA_CP + @LV_CP *  LV  ;



U_1 =  1 * (ASC_W_RNP + B_COST_RND * B_TT_W_RND * $tt_w_rp / 60 ) ;

U_2 =  1 * (ASC_B_RNP + B_COST_RND * B_TT_B_RND * $tt_b_rp / 60 ) ;

U_3 =  1 * (ASC_C_RNP + B_COST_RND * ( B_TT_C_RND * $tt_c_rp / 60 + $tc_c_rp )  ) ;

U_4 =  1 * (ASC_PT_RNP + B_COST_RND * ( B_TT_PT_RND * $tt_pt_rp / 60 +
                        @B_TRNS_PT * $trns_pt_rp + @B_ACC_PT * $acc_pt_rp / 60 + @B_FREQ_PT * $freq_pt_rp / 60 + $tc_pt_rp_a1 )  ) ;


U_5 =  ASC_W_RNP  + B_COST_RND * ( B_TT_W_RND * $tt_bf_mc / 60 ) ;

U_6 =  ASC_B_RNP  + B_COST_RND * ( B_TT_B_RND * $tt_v_mc / 60 ) ;

U_7 =  ASC_CP_RNP + B_COST_RND * ( B_TT_CP_RND * $tt_cp_mc / 60 +
                    @B_ACC_CS_CP * $acc_cp_mc / 60 + @B_RISK_CP * $r_cp_mc + $c_cp_mc )  ;

U_8 =  ASC_CS_RNP + B_COST_RND * ( B_TT_CS_RND * $tt_cs_mc / 60 + 
                    @B_ACC_CS_CP * $acc_cs_mc / 60 + $c_cs_mc  )  ;

U_9 =  ASC_PT_RNP + B_COST_RND * ( B_TT_PT_RND * $tt_pt_mc / 60 +
                    @B_TRNS_PT * $tr_pt_mc + @B_ACC_PT * $acc_pt_mc / 60 + @B_FREQ_PT * $s_pt_mc / 60 + $c_pt_mc )  ;


U_10 =  @S_RCC *  ( B_COST_RND * ( B_TT_CS_RND * $tt_a1_rcc / 60 + 
                                  @B_Q_CS * $q_a1_rcc / 60 + @B_ACC_CS_CP * $acc_a1_rcc / 60 + $c_a1_rcc )  ) ;

U_11 =  @S_RCC *  ( B_COST_RND * ( B_TT_CS_RND * $tt_a2_rcc / 60 +
                                  @B_Q_CS * $q_a2_rcc / 60 + @B_ACC_CS_CP * $acc_a2_rcc / 60 + $c_a2_rcc )  ) ;

U_12 =  @S_RCC *  ( B_COST_RND * ( B_TT_CS_RND * $tt_a3_rcc / 60 + 
                                  @B_Q_CS * $q_a3_rcc / 60 + @B_ACC_CS_CP * $acc_a3_rcc / 60 + $c_a3_rcc )  ) ;



U_13 =  @S_RCPT *  ( B_COST_RND * ( B_TT_PT_RND * $tt_a1_rcpt / 60 + 
                                   @B_ACC_PT * $acc_a1_rcpt / 60 + @B_TRNS_PT * $tr_a1_rcpt + @B_FREQ_PT * $f_a1_rcpt / 60 + $c_a1_rcpt )  ) ;

U_14 =  @S_RCPT *  ( B_COST_RND * ( B_TT_PT_RND * $tt_a2_rcpt / 60 + 
                                   @B_ACC_PT * $acc_a2_rcpt / 60 + @B_TRNS_PT * $tr_a2_rcpt + @B_FREQ_PT * $f_a2_rcpt / 60 + $c_a2_rcpt )  ) ;

U_15 =  @S_RCPT *  ( B_COST_RND * ( B_TT_PT_RND * $tt_a3_rcpt / 60 + 
                                   @B_ACC_PT * $acc_a3_rcpt / 60 + @B_TRNS_PT * $tr_a3_rcpt + @B_FREQ_PT * $f_a3_rcpt / 60 + $c_a3_rcpt )  ) ;


"

e1 <- extract_variables(fast_mixl)

test_that("declarations are put into a template", {
  print(e1$data_cols)
  expect_true(T)
  
})


