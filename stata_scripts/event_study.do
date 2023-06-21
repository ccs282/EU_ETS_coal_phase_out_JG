
    ** Event time

		if test_specific_date == "yes" { // the first block of the if condition is usually the event study performed for one single event
			capture drop event_date
			gen event_date = .
			summ date if date == date_specific
			if r(N) == 0 {
				tab date if date > date_specific, matrow(mat_temp)
				replace event_date = 1 if date == mat_temp[1, 1]
				summ date if event_date == 1
				scalar date_specific = r(mean)
			}
			else {
				replace event_date = 1 if date == date_specific 
			}
		}

		else { // the else condition is the main option for an event study for multiple events
			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
							capture drop event_date_`x'_`y'`i'
							gen event_date_`x'_`y'`i' = .
							replace event_date_`x'_`y'`i' = 1 if date == `x'_`y'`i'_d
						}
					}
				}
			}
		}

	** Event window

		if test_specific_date == "yes" {
			capture drop ew
			gen ew = .
			summ trading_date if event_date == 1
			replace ew = 1 if (trading_date >= r(mean) - event_length_pre) & (trading_date <= r(mean) + event_length_post)
		}

		else {
			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
							capture drop ew_`x'_`y'`i'
							gen ew_`x'_`y'`i' = .
							summ trading_date if event_date_`x'_`y'`i' == 1
							replace ew_`x'_`y'`i' = 1 if (trading_date >= r(mean) - event_length_pre) & (trading_date <= r(mean) + event_length_post)
						}
					}
				}
			}
		}

	** Estimation window
			
		if test_specific_date == "yes" {
			capture drop est_win
			gen est_win = .
			summ trading_date if event_date == 1
			replace est_win = 1 if (trading_date >= r(mean) - event_length_pre - est_length) & (trading_date < r(mean) - event_length_pre) & (date >= earliest_date)
		}

		else {
			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
							capture drop est_win_`x'_`y'`i'
							gen est_win_`x'_`y'`i' = .
							summ trading_date if event_date_`x'_`y'`i' == 1
							replace est_win_`x'_`y'`i' = 1 if (trading_date >= r(mean) - event_length_pre - est_length) & (trading_date < r(mean) - event_length_pre) & (date >= earliest_date)
						}
					}
				}
			}
		}

	** Deviation from Event Day

		if test_specific_date == "yes" {
			capture drop deviation
			gen deviation = .
			summ trading_date if event_date == 1, meanonly
			local pr = -(est_length + event_length_pre)
			local ps = event_length_post
			forvalues k = `pr'(1)`ps' {
				replace deviation = `k' if trading_date == r(mean) + `k' 
			}
		}

		else {
			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
							capture drop deviation_`x'_`y'`i'
							gen deviation_`x'_`y'`i' = .
							summ trading_date if event_date_`x'_`y'`i' == 1, meanonly
							local pr = -(est_length + event_length_pre)
							local ps = event_length_post
							forvalues k = `pr'(1)`ps' {
								replace deviation_`x'_`y'`i' = `k' if trading_date == r(mean) + `k' 
							}
						}
					}
				}
			}
		}


if price == "yes" { // default price event study
	** Normal returns
		if test_specific_date == "yes" {
			capture drop NR

			* Constant mean
			if reg_type == 1 {
				reg ln_return_eua est_win if est_win == 1, robust noconst
				gen NR = e(b)[1, 1]
				scalar df = e(df_r)
				scalar num_par = e(N) - e(df_r)
			}

			* Zero mean
			else if reg_type == 2 {
				gen NR = 0
				reg ln_return_eua est_win if est_win == 1, robust noconst
				scalar df = e(N)
				scalar num_par = 0

			}

			else if reg_type == 3 { // log returns for all variables; MAIN MODEL
				reg ln_return_eua L.ln_return_eua $ln_return_explanatory if est_win == 1, robust
				scalar df = e(df_r)
				scalar num_par = e(N) - e(df_r)

				predict NR if est_win == 1

				summ trading_date if event_date == 1
				capture drop tempv 
				gen tempv = ln_return_eua if trading_date < (r(mean) - event_length_pre) // create a temporary variable for the recursive estimation (bc. of the lagged dependent variable)

				reg tempv L.tempv $ln_return_explanatory if est_win == 1, robust
	
				local ew_length = event_length_post + event_length_pre + 1
				forvalues i = 1(1)`ew_length' {
					summ trading_date if event_date == 1
					predict NR_`i' if trading_date == (r(mean) - event_length_pre -1 + `i')
					replace tempv = NR_`i' if trading_date == (r(mean) - event_length_pre -1 + `i')
					replace NR = NR_`i' if trading_date == (r(mean) - event_length_pre -1 + `i')
				}

				drop NR_* tempv
			}

			* Koch et al. (2016) variables model
			else if reg_type == 5 { // difference log returns for coal & gas; otherwise log returns
				reg ln_return_eua L.ln_return_eua $D_ln_return_explanatory_2 if est_win == 1, robust
				scalar df = e(df_r)
				scalar num_par = e(N) - e(df_r)

				predict NR if est_win == 1

				summ trading_date if event_date == 1
				capture drop tempv 
				gen tempv = ln_return_eua if trading_date < (r(mean) - event_length_pre) // create a temporary variable for the recursive estimation (bc. of the lagged dependent variable)

				reg tempv L.tempv $D_ln_return_explanatory_2 if est_win == 1, robust
	
				local ew_length = event_length_post + event_length_pre + 1
				forvalues i = 1(1)`ew_length' {
					summ trading_date if event_date == 1
					predict NR_`i' if trading_date == (r(mean) - event_length_pre -1 + `i')
					replace tempv = NR_`i' if trading_date == (r(mean) - event_length_pre -1 + `i')
					replace NR = NR_`i' if trading_date == (r(mean) - event_length_pre -1 + `i')
				}
				drop NR_* tempv
			}


			order NR, after(ln_return_eua) 
		}

		else{

			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
							capture drop NR_`x'_`y'`i'

							* Constant Mean
							if reg_type == 1 {
								reg ln_return_eua est_win_`x'_`y'`i' if est_win_`x'_`y'`i' == 1, robust noconst
								gen NR_`x'_`y'`i' = e(b)[1, 1]
								scalar df_`x'_`y'`i' = e(df_r)
								scalar num_par_`x'_`y'`i' = e(N) - e(df_r)
							}

							* Zero Mean
							else if reg_type == 2 {
								gen NR_`x'_`y'`i' = 0
								reg ln_return_eua est_win_`x'_`y'`i' if est_win_`x'_`y'`i' == 1, robust noconst
								scalar df_`x'_`y'`i' = e(N)
								scalar num_par_`x'_`y'`i' = 0
							}


							else if reg_type == 3 { // log returns for all variables; MAIN MODEL
								reg ln_return_eua L.ln_return_eua $ln_return_explanatory if est_win_`x'_`y'`i' == 1, robust // corresponds to equation 3 in the paper
								scalar df_`x'_`y'`i' = e(df_r)
								scalar num_par_`x'_`y'`i' = e(N) - e(df_r)

								predict NR_`x'_`y'`i' if est_win_`x'_`y'`i' == 1

								summ trading_date if event_date_`x'_`y'`i' == 1
								capture drop tempv = . 
								gen tempv = ln_return_eua if trading_date < (r(mean) - event_length_pre) // create a temporary variable for the recursive estimation (bc. of the lagged dependent variable)

								reg tempv L.tempv $ln_return_explanatory if est_win_`x'_`y'`i' == 1, robust
					
								local ew_length = event_length_post + event_length_pre + 1
								forvalues j = 1(1)`ew_length' {
									summ trading_date if event_date_`x'_`y'`i' == 1
									predict NR_`x'_`y'`i'_`j' if trading_date == (r(mean) - event_length_pre -1 + `j')
									replace tempv = NR_`x'_`y'`i'_`j' if trading_date == (r(mean) - event_length_pre -1 + `j')
									replace NR_`x'_`y'`i' = NR_`x'_`y'`i'_`j' if trading_date == (r(mean) - event_length_pre -1 + `j')
								}

								drop NR_`x'_`y'`i'_* tempv
							}

							* Koch et al. (2016) variables model

							else if reg_type == 5 { // difference log returns for coal & gas; otherwise log returns
								reg ln_return_eua L.ln_return_eua $D_ln_return_explanatory_2 if est_win_`x'_`y'`i' == 1, robust
								scalar df_`x'_`y'`i' = e(df_r)
								scalar num_par_`x'_`y'`i' = e(N) - e(df_r)

								predict NR_`x'_`y'`i' if est_win_`x'_`y'`i' == 1

								summ trading_date if event_date_`x'_`y'`i' == 1
								capture drop tempv = . 
								gen tempv = ln_return_eua if trading_date < (r(mean) - event_length_pre) // create a temporary variable for the recursive estimation (bc. of the lagged dependent variable)

								reg tempv L.tempv $D_ln_return_explanatory_2 if est_win_`x'_`y'`i' == 1, robust
					
								local ew_length = event_length_post + event_length_pre + 1
								forvalues j = 1(1)`ew_length' {
									summ trading_date if event_date_`x'_`y'`i' == 1
									predict NR_`x'_`y'`i'_`j' if trading_date == (r(mean) - event_length_pre -1 + `j')
									replace tempv = NR_`x'_`y'`i'_`j' if trading_date == (r(mean) - event_length_pre -1 + `j')
									replace NR_`x'_`y'`i' = NR_`x'_`y'`i'_`j' if trading_date == (r(mean) - event_length_pre -1 + `j')
								}

								drop NR_`x'_`y'`i'_* tempv
							}
						}
					}
				}
			}
		}

	** Abnormal returns
		if test_specific_date == "yes" {
			capture drop AR
			gen AR = ln_return_eua - NR
			order AR, after(NR)
		}

		else {
			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
							capture drop AR_`x'_`y'`i'
							gen AR_`x'_`y'`i' = ln_return_eua - NR_`x'_`y'`i' // corresponds to equation 2 in the paper
						}
					}
				}
			}
		}
}

if volume == "yes" {
		** Normal returns (volume)
		if test_specific_date == "yes" {
			capture drop NR
			
		}

		else{
			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
							capture drop NR_`x'_`y'`i'
								summ month if event_date_`x'_`y'`i' == 1, meanonly
								if r(mean) == 9 | r(mean) == 10 | r(mean) == 11 { // use December roll-over for events in September-November to avoid significance bc of different data used (there is a strong threshold effect in the data when rolling over to the next maturity)
									reg ln_eua_vol_mon2 est_win_`x'_`y'`i' if est_win_`x'_`y'`i' == 1, robust noconst
									gen NR_`x'_`y'`i' = e(b)[1, 1]
									scalar df_`x'_`y'`i' = e(df_r)
									scalar num_par_`x'_`y'`i' = e(N) - e(df_r)
								}

								else { // use September roll-over otherwise
									reg ln_eua_vol_mon1 est_win_`x'_`y'`i' if est_win_`x'_`y'`i' == 1, robust noconst
									gen NR_`x'_`y'`i' = e(b)[1, 1]
									scalar df_`x'_`y'`i' = e(df_r)
									scalar num_par_`x'_`y'`i' = e(N) - e(df_r)
								}
						}
					}
				}
			}
		}

	** Abnormal returns (volume)
		if test_specific_date == "yes" { // volume
			capture drop AR
			gen AR = ln_trad_vol - NR
			order AR, after(NR)
		}

		else { // (volume)
			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
							capture drop AR_`x'_`y'`i'
							summ month if event_date_`x'_`y'`i' == 1, meanonly
							if r(mean) == 9 | r(mean) == 10 | r(mean) == 11 { 
								gen AR_`x'_`y'`i' = ln_eua_vol_mon2 - NR_`x'_`y'`i'

							}

							else { 
								gen AR_`x'_`y'`i' = ln_eua_vol_mon1 - NR_`x'_`y'`i'
							}
						}
					}
				}
			}
		}
}

	** Cumulative abnormal returns [summed AR]

		if test_specific_date == "yes" {
			capture drop CAR*
			
			* Event window
				egen CARa = total(AR) if ew == 1
				summ CARa, meanonly
				scalar CAR_ew = r(mean)
				
			* Pre-event
				egen CARb = total(AR) if ew == 1 & date < date_specific
				summ CARb, meanonly
				scalar CAR_pre = r(mean)

			* Post-event
				egen CARc = total(AR) if ew == 1 & date > date_specific
				summ CARc, meanonly
				scalar CAR_post = r(mean)

			* Event Day
				egen CARd = total(AR) if event_date == 1
				summ CARd, meanonly
				scalar AR_event = r(mean)
			
			capture drop CAR*

			* Every single day within the event window (not cumulative)
				tab date if ew == 1, matrow(matrix_)

				global pre = event_length_pre
				global post = event_length_post

				forvalues t = -$pre(1)$post {
					capture drop CAR*
					local nom = `t' + event_length_pre + 1
					egen CAR_temp = total(AR) if date == matrix_[`nom', 1]
					summ CAR_temp, meanonly
					scalar AR_d`nom' = `r(mean)'
				}

				capture drop CAR*

			* CAR each day (rolling sum of AR)
				gen CAR = .
				replace CAR = sum(AR) if ew == 1

				forvalues t = -$pre(1)$post {
					local nom = `t' + event_length_pre + 1
					summ CAR if deviation == `t', meanonly
					scalar CAR`nom' = r(mean)
				}
		}

		else {

			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx { // corresponds to equation 4
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
							* Event window
								egen CARa = total(AR_`x'_`y'`i') if ew_`x'_`y'`i' == 1
								summ CARa, meanonly
								scalar CAR_ew_`x'_`y'`i' = r(mean)
		
							* Pre-event
								summ date if event_date_`x'_`y'`i' == 1, meanonly
								egen CARb = total(AR_`x'_`y'`i') if ew_`x'_`y'`i' == 1 & date < `r(mean)'
								summ CARb, meanonly
								scalar CAR_pre_`x'_`y'`i' = r(mean)

							* Post-event
								summ date if event_date_`x'_`y'`i' == 1, meanonly
								egen CARc = total(AR_`x'_`y'`i') if ew_`x'_`y'`i' == 1 & date > `r(mean)'
								summ CARc, meanonly
								scalar CAR_post_`x'_`y'`i' = r(mean)

							* Event Day
								egen CARd = total(AR_`x'_`y'`i') if event_date_`x'_`y'`i' == 1
								summ CARd, meanonly
								scalar AR_event_`x'_`y'`i' = r(mean)

							capture drop CAR*

							* Every single day within the event window
								tab date if ew_`x'_`y'`i' == 1, matrow(mat_`x'_`y'`i')

								global pre = event_length_pre
								global post = event_length_post

								forvalues t = -$pre(1)$post {
									capture drop CAR_t*
									local nom = `t' + event_length_pre + 1
									egen CAR_temp = total(AR_`x'_`y'`i') if date == mat_`x'_`y'`i'[`nom', 1]
									summ CAR_temp, meanonly
									scalar AR_d`nom'_`x'_`y'`i' = `r(mean)'
								}

								capture drop CAR_t*

							* CAR each day (rolling sum of AR)
								gen CAR_`x'_`y'`i' = .
								replace CAR_`x'_`y'`i' = sum(AR_`x'_`y'`i') if ew_`x'_`y'`i' == 1

								forvalues t = -$pre(1)$post {
									local nom = `t' + event_length_pre + 1
									summ CAR_`x'_`y'`i' if deviation_`x'_`y'`i' == `t', meanonly
									scalar CAR`nom'_`x'_`y'`i' = r(mean)
								}
						}
					}
				}
			}
		}

	** Average CAR across dates and countries

		if test_specific_date != "yes" { // corresponds to equation 7

			scalar No = 0
			
			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_s
						if _rc == 0 {
							if `x'_`y'`i'_s == 1 {
								scalar No = No + `x'_`y'`i'_s
							}
						}
					}
				}
			}

            * Pre-event
			    capture drop v_*

				foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
					foreach y in main alt new rev follow leak canc parl nuc {
						forvalues i = 1(1)10 {
							capture confirm scalar `x'_`y'`i'_d
							if _rc == 0 {
								capture drop v_CAR_pre_`x'_`y'`i'
                            	if weighted == "yes" {
									gen v_CAR_pre_`x'_`y'`i' = CAR_pre_`x'_`y'`i' * `x'_`y'`i'_w
								}
								else {
									gen v_CAR_pre_`x'_`y'`i' = CAR_pre_`x'_`y'`i'
								}
							}
						}
					}
				}

                if weighted == "yes" {
					egen v_CAR_pre_avg = rowtotal(v_CAR*)
				}
				else {
					egen v_CAR_pre_avg = rowmean(v_CAR*)
				}
                scalar CAR_pre_avg = v_CAR_pre_avg[1]
                capture drop v_*
                    
			* Post-event

				foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
					foreach y in main alt new rev follow leak canc parl nuc {
						forvalues i = 1(1)10 {
							capture confirm scalar `x'_`y'`i'_d
							if _rc == 0 {
								capture drop v_CAR_post_`x'_`y'`i'
								if weighted == "yes" {
									gen v_CAR_post_`x'_`y'`i' = CAR_post_`x'_`y'`i' * `x'_`y'`i'_w
								}
								else {
									gen v_CAR_post_`x'_`y'`i' = CAR_post_`x'_`y'`i'
								}
							}
						}
					}
				}

				if weighted == "yes" {
					egen v_CAR_post_avg = rowtotal(v_CAR*)
				}
				else {
					egen v_CAR_post_avg = rowmean(v_CAR*)
				}
                scalar CAR_post_avg = v_CAR_post_avg[1]
                capture drop v_*
                    
			* Event Day

				foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
					foreach y in main alt new rev follow leak canc parl nuc {
						forvalues i = 1(1)10 {
							capture confirm scalar `x'_`y'`i'_d
							if _rc == 0 {
								capture drop v_AR_event_`x'_`y'`i'
								if weighted == "yes" {
									gen v_AR_event_`x'_`y'`i' = AR_event_`x'_`y'`i' * `x'_`y'`i'_w
								}
								else {
									gen v_AR_event_`x'_`y'`i' = AR_event_`x'_`y'`i'
								}
							}
						}
					}
				}

				if weighted == "yes" {
					egen v_AR_event_avg = rowtotal(v_AR*)
				}
				else {
					egen v_AR_event_avg = rowmean(v_AR*)
				}
                scalar AR_event_avg = v_AR_event_avg[1]
                capture drop v_*

            * Event window

				foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
					foreach y in main alt new rev follow leak canc parl nuc {
						forvalues i = 1(1)10 {
							capture confirm scalar `x'_`y'`i'_d
							if _rc == 0 {
								capture drop v_CAR_ew_`x'_`y'`i'
								if weighted == "yes" {
									gen v_CAR_ew_`x'_`y'`i' = CAR_ew_`x'_`y'`i' * `x'_`y'`i'_w
								}
								else {
									gen v_CAR_ew_`x'_`y'`i' = CAR_ew_`x'_`y'`i'
								}
							}
						}
					}
				}

				if weighted == "yes" {
					egen v_CAR_ew_avg = rowtotal(v_CAR*)
				}
				else {
					egen v_CAR_ew_avg = rowmean(v_CAR*)
				}
                scalar CAR_ew_avg = v_CAR_ew_avg[1]
                capture drop v_*

			* Every single day within the event window (not cumulative; individual days)

				foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
					foreach y in main alt new rev follow leak canc parl nuc {
						forvalues i = 1(1)10 {
							capture confirm scalar `x'_`y'`i'_d
							if _rc == 0 {
								forvalues t = -$pre(1)$post {
									local nom = `t' + event_length_pre + 1
									capture drop v_AR_d`nom'_`x'_`y'`i'
									if weighted == "yes" {
										gen v_AR_d`nom'_`x'_`y'`i' = AR_d`nom'_`x'_`y'`i' * `x'_`y'`i'_w
									}
									else {
										gen v_AR_d`nom'_`x'_`y'`i' = AR_d`nom'_`x'_`y'`i'
									}
								}
							}
						}
					}
				}

				forvalues t = -$pre(1)$post {
					local nom = `t' + event_length_pre + 1
					if weighted == "yes" {
						egen v_AR_d`nom'_avg = rowtotal(v_AR_d`nom'*)
					}
					else {
						egen v_AR_d`nom'_avg = rowmean(v_AR_d`nom'*)
					}
                	scalar AR_d`nom'_avg = v_AR_d`nom'_avg[1]
				}
				
				capture drop v_*

			* Rolling cumulative average

				foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
					foreach y in main alt new rev follow leak canc parl nuc {
						forvalues i = 1(1)10 {
							capture confirm scalar `x'_`y'`i'_d
							if _rc == 0 {
								forvalues t = -$pre(1)$post {
									local nom = `t' + event_length_pre + 1
									//capture drop v_CAR_`x'_`y'`i'
									capture gen v_CAR_`x'_`y'`i' = .
									if weighted == "yes" {
										replace v_CAR_`x'_`y'`i' = CAR`nom'_`x'_`y'`i' * `x'_`y'`i'_w if _n == `nom'
									}
									else {
										replace v_CAR_`x'_`y'`i' = CAR`nom'_`x'_`y'`i' if _n == `nom'
									}
								}
							}
						}
					}
				}
				
				if weighted == "yes" {
					egen v_CAR_avg = rowtotal(v_CAR*)
				}
				else {
					egen v_CAR_avg = rowmean(v_CAR*)
				}

				forvalues t = -$pre(1)$post {
					local nom = `t' + event_length_pre + 1
                	scalar CAR_d`nom'_avg = v_CAR_avg[`nom']
				}				
				
				capture drop v_*
		}