
save data, replace
import delimited "phase_outs.csv", clear asdouble

if weighted == "yes" {
	scalar emissions_total = 0
}

foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx { // extract event dates from CSV
	foreach y in main alt new rev follow leak canc parl nuc {
		forvalues i = 1(1)10 {
				capture confirm variable `x'_`y'`i'
				if _rc == 0 {
					scalar `x'_`y'`i'_s = `x'_`y'`i'[2]
					if `x'_`y'`i'_s == 1 {
						scalar `x'_`y'`i'_d = `x'_`y'`i'[1]
						if weighted == "yes" { // get emissions data at phase-out date
							scalar `x'_`y'`i'_e = `x'_`y'`i'[3]
							if `x'_`y'`i'_e == . {
								scalar drop `x'_`y'`i'_d // drop dates/events for which weights will not be feasible
								scalar drop `x'_`y'`i'_e // drop NA emissions scalars
								scalar drop `x'_`y'`i'_s
							}
							else {
								scalar emissions_total = emissions_total + `x'_`y'`i'_e
							}
						}
					}
				}
		}
	}
}

use data, clear

	** if phase-out date is not a trading date, choose the next trading date:
				foreach x in bg cz dk fi de el hu it nl pl pt ro sk si es uk xx {
					foreach y in main alt new rev follow leak canc parl nuc {
						forvalues i = 1(1)10 {
							capture confirm scalar `x'_`y'`i'_d
							if _rc == 0 {
								* use date that is also in the main dataset
								summ date if date == `x'_`y'`i'_d
								if r(N) == 0 {
									tab date if date > `x'_`y'`i'_d, matrow(mat_temp)
									scalar `x'_`y'`i'_d = mat_temp[1, 1]
								}
								* create weights
								if weighted == "yes" {
									scalar `x'_`y'`i'_w = `x'_`y'`i'_e/emissions_total
								}
							}
						}
					}
				}

