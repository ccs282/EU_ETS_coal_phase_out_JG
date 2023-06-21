	
    if price == "yes" {

    ** Variance & SD AR (estimation win)
		if test_specific_date == "yes" {
        	summ ln_return_eua if est_win == 1
            capture drop AR_squared
            capture drop TSS
            gen AR_squared = .
            replace AR_squared = AR^2 if est_win == 1
            egen TSS = total(AR_squared) if est_win == 1
            summ TSS
            scalar TSS_aux = r(mean)
            summ trading_date if est_win == 1
            scalar var_AR = (1/(r(max)-r(min)+1-num_par))*TSS_aux
            scalar SD_AR = sqrt(var_AR)
            capture drop AR_squared TSS 
		}

		else {
			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
	                    	summ ln_return_eua if est_win_`x'_`y'`i' == 1
                            capture drop AR_squared
                            capture drop TSS
                            gen AR_squared = .
                            replace AR_squared = AR_`x'_`y'`i'^2 if est_win_`x'_`y'`i' == 1
                            egen TSS = total(AR_squared) if est_win_`x'_`y'`i' == 1
                            summ TSS
                            scalar TSS_aux = r(mean)
                            summ trading_date if est_win_`x'_`y'`i' == 1
                            scalar var_AR_`x'_`y'`i' = (1/(r(max)-r(min)+1-num_par_`x'_`y'`i'))*TSS_aux
                            scalar SD_AR_`x'_`y'`i' = sqrt(var_AR_`x'_`y'`i')
                            capture drop AR_squared TSS
                            scalar drop TSS_aux 
						}
					}
				}
			}
        }
    }

    if volume == "yes" {

            ** Variance & SD AR (estimation win) // volume
		if test_specific_date == "yes" { // (volume)
            capture drop AR_squared
            capture drop TSS
            gen AR_squared = .
            replace AR_squared = AR^2 if est_win == 1
            egen TSS = total(AR_squared) if est_win == 1
            summ TSS
            scalar TSS_aux = r(mean)
            summ trading_date if est_win == 1
            scalar var_AR = (1/(r(max)-r(min)+1-num_par))*TSS_aux
            scalar SD_AR = sqrt(var_AR)
            capture drop AR_squared TSS 
		}

		else { // (volume)
			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
                            capture drop AR_squared
                            capture drop TSS
                            gen AR_squared = .
                            replace AR_squared = AR_`x'_`y'`i'^2 if est_win_`x'_`y'`i' == 1
                            egen TSS = total(AR_squared) if est_win_`x'_`y'`i' == 1
                            summ TSS
                            scalar TSS_aux = r(mean)
                            summ trading_date if est_win_`x'_`y'`i' == 1
                            scalar var_AR_`x'_`y'`i' = (1/(r(max)-r(min)+1-num_par_`x'_`y'`i'))*TSS_aux
                            scalar SD_AR_`x'_`y'`i' = sqrt(var_AR_`x'_`y'`i')
                            capture drop AR_squared TSS
                            scalar drop TSS_aux 
						}
					}
				}
			}
        }


    }

	** Variance & SD CAR (event window)
		if test_specific_date == "yes" {
            * Full Event window
                scalar var_CAR_ew = (event_length_pre + event_length_post + 1)*var_AR
                scalar SD_CAR_ew = sqrt(var_CAR_ew)

            * Pre-event
                scalar var_CAR_pre = event_length_pre*var_AR
                scalar SD_CAR_pre = sqrt(var_CAR_pre)

            * Post-event
                scalar var_CAR_post = event_length_post*var_AR
                scalar SD_CAR_post = sqrt(var_CAR_post)

            * Event Day
                scalar var_AR_event = var_AR
                scalar SD_AR_event = sqrt(var_AR_event)
		}

        else {
			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx { // corresponds to equation 6
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
	                        * Full Event window
                                scalar var_CAR_ew_`x'_`y'`i' = (event_length_pre + event_length_post + 1)*var_AR_`x'_`y'`i'
                                scalar SD_CAR_ew_`x'_`y'`i' = sqrt(var_CAR_ew_`x'_`y'`i')

                            * Pre-event
                                scalar var_CAR_pre_`x'_`y'`i' = event_length_pre*var_AR_`x'_`y'`i'
                                scalar SD_CAR_pre_`x'_`y'`i' = sqrt(var_CAR_pre_`x'_`y'`i')

                            * Post-event 
                                scalar var_CAR_post_`x'_`y'`i' = event_length_post*var_AR_`x'_`y'`i'
                                scalar SD_CAR_post_`x'_`y'`i' = sqrt(var_CAR_post_`x'_`y'`i')

                            * Event Day
                                scalar var_AR_event_`x'_`y'`i' = var_AR_`x'_`y'`i'
                                scalar SD_AR_event_`x'_`y'`i' = sqrt(var_AR_event_`x'_`y'`i')
						}
					}
				}
			}
        }

    ** Variance & SD average CAR (event window; across different dates)
		
		if test_specific_date != "yes" { // corresponds to equation 9
            * Pre-event
                capture drop v_*

                foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
                    foreach y in main alt new rev follow leak canc parl nuc {
                        forvalues i = 1(1)10 {
                            capture confirm scalar `x'_`y'`i'_d
                            if _rc == 0 {
                                if weighted == "yes" {
                                    gen v_var_CAR_pre_`x'_`y'`i' = var_CAR_pre_`x'_`y'`i' * (`x'_`y'`i'_w)^2
                                }
                                else {
                                    gen v_var_CAR_pre_`x'_`y'`i' = var_CAR_pre_`x'_`y'`i'
                                }
                            }
                        }
                    }
                }

                egen v_var_CAR_pre_sum = rowtotal(v_*)

                if weighted == "yes" {
                    scalar var_CAR_pre_avg = v_var_CAR_pre_sum[1]
                }
                else {
                    scalar var_CAR_pre_avg = v_var_CAR_pre_sum[1]/No^2
                }

                scalar SD_CAR_pre_avg = sqrt(var_CAR_pre_avg)

                capture drop v_*

			* Post-event
                foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
                    foreach y in main alt new rev follow leak canc parl nuc {
                        forvalues i = 1(1)10 {
                            capture confirm scalar `x'_`y'`i'_d
                            if _rc == 0 {
                                if weighted == "yes" {
                                    gen v_var_CAR_post_`x'_`y'`i' = var_CAR_post_`x'_`y'`i' * (`x'_`y'`i'_w)^2
                                }
                                else {
                                    gen v_var_CAR_post_`x'_`y'`i' = var_CAR_post_`x'_`y'`i'
                                }
                            }
                        }
                    }
                }

                egen v_var_CAR_post_sum = rowtotal(v_*)

                if weighted == "yes" {
                    scalar var_CAR_post_avg = v_var_CAR_post_sum[1]
                }
                else {
                    scalar var_CAR_post_avg = v_var_CAR_post_sum[1]/No^2
                }
                
                scalar SD_CAR_post_avg = sqrt(var_CAR_post_avg)

                capture drop v_*

			* Event Day
                foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
                    foreach y in main alt new rev follow leak canc parl nuc {
                        forvalues i = 1(1)10 {
                            capture confirm scalar `x'_`y'`i'_d
                            if _rc == 0 {
                                if weighted == "yes" {
                                    gen v_var_AR_event_`x'_`y'`i' = var_AR_event_`x'_`y'`i' * (`x'_`y'`i'_w)^2
                                }
                                else {
                                    gen v_var_AR_event_`x'_`y'`i' = var_AR_event_`x'_`y'`i'
                                }
                            }
                        }
                    }
                }

                egen v_var_AR_event_sum = rowtotal(v_*)

                if weighted == "yes" {
                    scalar var_AR_event_avg = v_var_AR_event_sum[1]
                }
                else {
                    scalar var_AR_event_avg = v_var_AR_event_sum[1]/No^2
                }

                scalar SD_AR_event_avg = sqrt(var_AR_event_avg)

                capture drop v_*

            * Event window

                foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
                    foreach y in main alt new rev follow leak canc parl nuc {
                        forvalues i = 1(1)10 {
                            capture confirm scalar `x'_`y'`i'_d
                            if _rc == 0 {
                                if weighted == "yes" {
                                    gen v_var_CAR_ew_`x'_`y'`i' = var_CAR_ew_`x'_`y'`i' * (`x'_`y'`i'_w)^2
                                }
                                else {
                                    gen v_var_CAR_ew_`x'_`y'`i' = var_CAR_ew_`x'_`y'`i'
                                }
                            }
                        }
                    }
                }

                egen v_var_CAR_ew_sum = rowtotal(v_*)

                if weighted == "yes" {
                    scalar var_CAR_ew_avg = v_var_CAR_ew_sum[1]
                }
                else {
                    scalar var_CAR_ew_avg = v_var_CAR_ew_sum[1]/No^2
                }
                
                scalar SD_CAR_ew_avg = sqrt(var_CAR_ew_avg)

                capture drop v_*
		}
		
    if test_specific_date == "yes" {
            * Pre-event
                scalar t_pre = CAR_pre/SD_CAR_pre
                scalar p_pre = ttail(df ,abs(t_pre))*2

            * Event day
                scalar t_event = AR_event/SD_AR_event
                scalar p_event = ttail(df ,abs(t_event))*2

            * Post-event
                scalar t_post = CAR_post/SD_CAR_post
                scalar p_post = ttail(df ,abs(t_post))*2
                
            * Full Event window
                scalar t_ew = CAR_ew/SD_CAR_ew
                scalar p_ew = ttail(df ,abs(t_ew))*2

            * Individual days within event window

				local pre = event_length_pre
				local post = event_length_post

                forvalues t = -`pre'(1)`post' {
                    local nom = `t' + event_length_pre + 1
                    scalar t_d`nom' = AR_d`nom'/SD_AR_event
                    scalar p_d`nom' = ttail(df ,abs(t_d`nom'))*2
				}
            
            * Estimation window
                capture drop t_stat
                gen t_stat = AR/SD_AR_event if est_win == 1
                
                capture drop p_value
                gen p_value = ttail(df ,abs(t_stat))*2 if est_win == 1

                count if p_value <= 0.05 & p_value != .
                capture drop est_win_sig = .
                gen est_win_sig = r(N)/est_length if _n == 1
		}

        else {


			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx { // corresponds to equation 5
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
	                        * Pre-event
                                scalar t_pre_`x'_`y'`i' = CAR_pre_`x'_`y'`i'/SD_CAR_pre_`x'_`y'`i'
                                scalar p_pre_`x'_`y'`i' = ttail(df_`x'_`y'`i' ,abs(t_pre_`x'_`y'`i'))*2

                            * Event day
                                scalar t_event_`x'_`y'`i' = AR_event_`x'_`y'`i'/SD_AR_event_`x'_`y'`i'
                                scalar p_event_`x'_`y'`i' = ttail(df_`x'_`y'`i' ,abs(t_event_`x'_`y'`i'))*2

                            * Post-event
                                scalar t_post_`x'_`y'`i' = CAR_post_`x'_`y'`i'/SD_CAR_post_`x'_`y'`i'
                                scalar p_post_`x'_`y'`i' = ttail(df_`x'_`y'`i' ,abs(t_post_`x'_`y'`i'))*2
                                
                            * Full Event window
                                scalar t_ew_`x'_`y'`i' = CAR_ew_`x'_`y'`i'/SD_CAR_ew_`x'_`y'`i'
                                scalar p_ew_`x'_`y'`i' = ttail(df_`x'_`y'`i' ,abs(t_ew_`x'_`y'`i'))*2
                            
                            * Individual days within event window

                                local pre = event_length_pre
                                local post = event_length_post

                                forvalues t = -`pre'(1)`post' {
                                    local nom = `t' + event_length_pre + 1
                                    scalar t_d`nom'_`x'_`y'`i' = AR_d`nom'_`x'_`y'`i'/SD_AR_event_`x'_`y'`i'
                                    scalar p_d`nom'_`x'_`y'`i' = ttail(df_`x'_`y'`i' ,abs(t_d`nom'_`x'_`y'`i'))*2
                                }
                            * Estimation window
                                capture drop t_stat_`x'_`y'`i'
                                gen t_stat_`x'_`y'`i' = AR_`x'_`y'`i'/SD_AR_event_`x'_`y'`i' if est_win_`x'_`y'`i' == 1
                                
                                capture drop p_value_`x'_`y'`i'
                                gen p_value_`x'_`y'`i' = ttail(df_`x'_`y'`i' ,abs(t_stat_`x'_`y'`i'))*2 if est_win_`x'_`y'`i' == 1

                                count if p_value_`x'_`y'`i' <= 0.05 & p_value_`x'_`y'`i' != .
                                capture drop est_win_sig_`x'_`y'`i' = .
                                gen est_win_sig_`x'_`y'`i' = r(N)/est_length if _n == 1

						}
					}
				}
			}

            capture drop est_win_sig_avg
            egen est_win_sig_avg = rowmean(est_win_sig_*)

            * Average CAR

            scalar df_avg = 0

			foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
				foreach y in main alt new rev follow leak canc parl nuc {
					forvalues i = 1(1)10 {
						capture confirm scalar `x'_`y'`i'_d
						if _rc == 0 {
							scalar df_avg = df_avg + df_`x'_`y'`i'
						}
					}
				}
			}

            scalar df_avg = df_avg / No 

                // corresponds to equation 8
                * Pre-event
                    scalar t_pre_avg = CAR_pre_avg/SD_CAR_pre_avg
                    scalar p_pre_avg = ttail(df_avg ,abs(t_pre_avg))*2

                * Event day
                    scalar t_event_avg = AR_event_avg/SD_AR_event_avg
                    scalar p_event_avg = ttail(df_avg ,abs(t_event_avg))*2

                * Post-event
                    scalar t_post_avg = CAR_post_avg/SD_CAR_post_avg
                    scalar p_post_avg = ttail(df_avg ,abs(t_post_avg))*2
                            
                * Full Event window
                    scalar t_ew_avg = CAR_ew_avg/SD_CAR_ew_avg
                    scalar p_ew_avg = ttail(df_avg ,abs(t_ew_avg))*2

                * Individual days within event window
                    forvalues t = -`pre'(1)`post' {
                        local nom = `t' + event_length_pre + 1
                        scalar t_d`nom'_avg = AR_d`nom'_avg/SD_AR_event_avg
                        scalar p_d`nom'_avg = ttail(df_avg ,abs(t_d`nom'_avg))*2
				    }
        }