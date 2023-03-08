Function Abs_Be, ene, PF60 = PF60

;+
; NAME:
;		ABS_BE
; VERSION:
;		8.72
; PURPOSE:
;		Calculates the absorption coefficient of commercial grade Be, taking
;		into account impurities.
; CATEGORY:
;		Experimental utility.
; CALLING SEQUENCE:
;		Result = Result = ABS_BE( ENE [,/PF60)
; INPUTS:
; 	ENE
; 		Vector or scalar, photon energy in keV.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/PF60
; 		Switch.  If set, the calculation is for a PF60 grade Be.  The default is
; 		S-200FH or PS-200 (same impurities).
; OUTPUTS:
;		Returns the value(s) of the absorption coefficient of commercial grade
;		Be, including quoted values of impurities, for all input energy values.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		Not reliable below 1 keV.
; PROCEDURE:
;		Straightforward, using ABS_COEFF from SRUFF_LIB.
; MODIFICATION HISTORY:
;		Created 15-NOV-2020 by Mati Meron.
;		Documented 25-DEC-2020 by Mati Meron.
;		Updated 25-FEB-2021 by Mati Meron.
;-

	on_error, 1

	if keyword_set(pf60) then begin
		ele = ['be', 'al','b',   'cd',  'ca','c', 'cr', 'co',  'cu',  'fe',$
			'pb', 'li',  'mg', 'mn','mo', 'ni','n', 'o', 'si','ag']
		wei = [99.29,0.05,0.0003,0.0002,0.01,0.06,0.01,0.001,0.01,0.08,$
			0.002,0.0003,0.049,0.01,0.002,0.02,0.03,0.51,0.04,0.001]
	endif else begin
		ele = ['be',  'al',  'c', 'fe', 'mg',  'o', 'si']
		wei = [99.04, 0.10, 0.15, 0.13, 0.08, 0.96, 0.06]
	endelse
	res = Abs_coeff(ene,ele=ele,wei=wei,den=1.848)

	return, res
end