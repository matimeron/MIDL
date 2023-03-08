Function ICflux_old, inp, ene, sensitivity = sen, inverse = inv

;+
; NAME:
;		ICFLUX
; VERSION:
;		8.331
; PURPOSE:
;		Translates ion chamber readings to photon flux.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = ICFLUX ( ICR, ENE, [SENSITIVITY = SEN])
; INPUTS:
;	INP															|	One, at most
;		Either the ion chamber reading (as processed by the		|	of these two
;		scaler) or, when /REVERSE is set, photon flux in ph/s.	|	may be a
;	ENE															|	vector.
;		Photon energy, in keV.									|
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SENSITIVITY
;		Amplifier sensitivity, in units of microamp/volt.  Default value is 1.
;	/INVERSE
;		Switch.  If set, the function assums flux input and calculates ion
;		chamber reading.  Default is ion chamber reading input and flux result.
; OUTPUTS:
;		Returns the calculated photon flux or, if /INVERSE is set, the 
;		calculated ion chamber reading.  If one of the inputs (whether INP
;		or ENE) is a vector, the format of the result is same as said vector.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block ICHAMDAT, stores relevant spline coefficients.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Follows the derivation by Tim Graber.  Calls DEFAULT, FPU_FIX,
;		SPLIN_COEFFS, SPLIN_EVAL and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JUL-2006 by Mati Meron.
;		Modified 1-AUG-2011 by Mati Meron.  Internal changes.
;		Modified 15-OCT-2014 by Mati Meron.  Added keyword INVERSE.
;-

	common ichamdat, iexs, stab
	on_error, 1

	rho = 0.001165
	enedat = [ 1.000,  1.500,  2.000,   3.000,   5.000,  10.000, $
			15.000, 20.000, 30.000,  50.000, 100.000]
	muedat = [ 3306.,  1080.,  475.5,   144.7,   30.86,   3.545, $
			0.9715, 0.3867, 0.1099, 0.03217, 0.02231]

	coef = 2.19e6
	len = 6.

	if Type(iexs) eq 0 then begin
		stab = Splin_coeffs(alog(enedat),alog(rho*muedat),/seg)
		iexs = 1
	endif

	if n_elements(inp) gt 1 and n_elements(ene) gt 1 $
	then message, 'At least one of the inputs must be a scalar!'
	wsen = Default(sen,1.,/dtyp)

	mue = exp(Splin_eval(alog(ene),stab))
	mult = coef*wsen/(ene*(1 - exp(-len*mue)))
	
	if keyword_set(inv) then res = inp/mult else res = inp*mult

	return, FPU_fix(res)
end