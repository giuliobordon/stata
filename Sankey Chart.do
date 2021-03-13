gen i_bfr= pc_market_inc
gen i_aft= pc_disp_inc_all
xtile q_bfr= i_bfr, n(4)
xtile q_aft= i_aft, n(4)
table q_bfr q_aft [pw=relwt], format(%9.0f)
/*
BF1[11187]AF1
BF1[440]AF2
BF1[0]AF3
BF1[0]AF4
BF2[590]AF1
BF2[12162]AF2
BF2[353]AF3
BF2[0]AF4
BF3[0]AF1
BF3[349]AF2
BF3[13634]AF3
BF3[256]AF4
BF4[0]AF1
BF4[0]AF2
BF4[195]AF3
BF4[16464]AF4
*/



table pov_pc_market_inc pov_pc_disp_inc_all [pw=relwt], format(%9.0f)
/*
Non-Poor_Bf [48443] Non-Poor_Af
Poor_Bf [2729] Non-Poor_Af
Non-Poor_Bf [0] Poor_Af
Poor_Bf [4458] Poor_Af
*/
*    http://sankeymatic.com/build/