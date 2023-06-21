
*** explanatory variables 
	
	global explanatory oil coal gas elec gsci vix stoxx diff_baa_aaa ecb_spot_3m

	save data, replace

	foreach var of global explanatory { // create log returns for explanatory variables
		drop if `var' == .
		capture drop ln_return_`var'
		gen ln_return_`var' = ln(`var'[_n] / `var'[_n - 1]) if _n > 1
		save `var', replace
		use `var', clear
		keep ln_return_`var' date
		save, replace
		use data, clear
		mmerge date using `var'
		drop _merge
		save data, replace
		erase "`var'.dta"
	}


	global ln_return_explanatory ln_return_oil ln_return_coal ln_return_gas ln_return_elec ln_return_gsci ln_return_vix ln_return_stoxx ln_return_diff_baa_aaa ln_return_ecb_spot_3m

	save data, replace

	foreach var of global ln_return_explanatory { // create differenced log returns
		drop if `var' == .
		capture drop D_`var'
		gen D_`var' = `var'[_n] - `var'[_n - 1] if _n > 1
		save `var', replace
		use `var', clear
		keep D_`var' date
		save, replace
		use data, clear
		mmerge date using `var'
		drop _merge
		save data, replace
		erase "`var'.dta"
	}

    global D_ln_return_explanatory_2 ln_return_oil ln_return_elec ln_return_gsci ln_return_vix ln_return_stoxx ln_return_diff_baa_aaa ln_return_ecb_spot_3m D_ln_return_gas D_ln_return_coal // variables used for alternative specification using differenced log returns for coal and gas

	capture drop aaa baa
	
*** explained/dependent variable

	capture drop eua eua_vol1 eua_vol2

	gen eua = .
	gen eua1 = .
	gen eua_vol1 = .
	gen eua_vol2 = .

	forvalues i = 7(1)22 { // generate eua variable; rolling over to next year's maturity as late as possible (mid-December)
		if `i' < 10 {
			replace eua = eua0`i' if eua == . & eua0`i' != .
		}
		else {
			replace eua = eua`i' if eua == . & eua`i' != .
		}
	}

	order eua, after(date)

	** for volume analysis


		forvalues i = 7(1)22 { // generate eua volume variable 1; rolling over to next year's maturity at the end of September
			if `i' < 10 {
				replace eua_vol1 = eua200`i'_vol1 if eua_vol1 == . & eua200`i'_vol1 != .
			}
			else {
				replace eua_vol1 = eua20`i'_vol1 if eua_vol1 == . & eua20`i'_vol1 != .
			}
		}

		forvalues i = 7(1)22 { // generate eua volume variable 2; rolling over to next year's maturity as late as possible (mid-December)
			if `i' < 10 {
				replace eua_vol2 = eua200`i'_vol2 if eua_vol2 == . & eua200`i'_vol2 != .
			}
			else {
				replace eua_vol2 = eua20`i'_vol2 if eua_vol2 == . & eua20`i'_vol2 != .
			}
		}


		forvalues i = 7(1)22 { // generate eua variable; rolling over to next year's maturity at the end of September; used for volume analysis (not main analysis)
			if `i' < 10 {
				tab date if eua200`i'_vol1 != ., matrow(temp`i')
				quietly levelsof date if eua200`i'_vol1 != .
				local no_dates_`i' = r(r)
				local cut_off_date_`i' = temp`i'[`no_dates_`i'', 1]
				replace eua1 = eua0`i' if eua1 == . & eua0`i' != . & date <= `cut_off_date_`i''
			}
			else {
				tab date if eua20`i'_vol1 != ., matrow(temp`i')
				quietly levelsof date if eua20`i'_vol1 != .
				local no_dates_`i' = r(r)
				local cut_off_date_`i' = temp`i'[`no_dates_`i'', 1]
				replace eua1 = eua`i' if eua1 == . & eua`i' != . & date <= `cut_off_date_`i''
			}
		}


		* volume monetised

			capture drop eua_vol_mon1 // based on the variables rolling over at the end of September
			gen eua_vol_mon1 = .
			replace eua_vol_mon1 = eua_vol1*1000*eua1

			capture drop ln_eua_vol_mon1
			gen ln_eua_vol_mon1 = .
			replace ln_eua_vol_mon1 = ln(eua_vol_mon1)

			capture drop eua_vol_mon2 // based on the variables rolling over in December
			gen eua_vol_mon2 = .
			replace eua_vol_mon2 = eua_vol2*1000*eua

			capture drop ln_eua_vol_mon2
			gen ln_eua_vol_mon2 = .
			replace ln_eua_vol_mon2 = ln(eua_vol_mon2)

	save data, replace

	global dependent eua

	foreach var of global dependent { // log returns eua and eua volume
		drop if `var' == .
		capture drop ln_return_`var'
		gen ln_return_`var' = ln(`var'[_n] / `var'[_n - 1]) if _n > 1
		save `var', replace
		use `var', clear
		keep ln_return_`var' date
		save, replace
		use data, clear
		mmerge date using `var'
		drop _merge
		save data, replace
		erase "`var'.dta"
	}
	
	forvalues i = 7(1)19 { // drop yearly eua futures
		if `i' < 10 {
			capture drop eua0`i'*
		}
		else {
			capture drop eua`i'*
		}
	}
	capture drop eua20 eua21 eua22
	capture drop eua20*

	order ln_return_eua, after(eua)

	** drop all observations that have a missing value for one of the explanatory variables or the dependent variable
		drop if ln_return_oil == .| ln_return_coal == .| ln_return_gas == .| ln_return_elec == .| ln_return_gsci == .| ln_return_vix == .| ln_return_stoxx == .| ln_return_diff_baa_aaa == .| ln_return_ecb_spot_3m == .| ln_return_eua == .

*** Prep time series

	drop if date <= 20080314 // same as Koch et al. (2016)

	capture drop year month day stata_date
	gen year = int(date/10000) 
	gen month = int((date-year*10000)/100) 
	gen day = int((date-year*10000-month*100)) 
	gen stata_date = mdy(month,day,year)
	order stata_date, after(date)
	format stata_date  %td

	capture drop trading_date
	gen trading_date = 1
	replace trading_date = trading_date[_n-1] + 1 if _n != 1 // there are missing dates (weekends etc.)

	tsset trading_date, d

	order eua, after(stata_date)