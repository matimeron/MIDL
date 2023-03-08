Function LS_gamma, freq, order= ord, period= per, density= den, viscosity= vis,$
	no_correction = noc

;+
; NAME:
;		LS_GAMMA
; VERSION:
;		8.214
; PURPOSE:
;		Calculates surface tension.
; CATEGORY:
;		LS calculations.
; CALLING SEQUENCE:
;		Result = LS_GAMMA( FREQ, ORDER = ORD, [,optional keywords])
; INPUTS:
; 	FREQ
; 		Integer scalar or array, the value[s] of measured LS frequency.  Can be
; 		given either in Hz or in kHz.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	ORDER
; 		Integer scalar, mandatory
; 	PERIOD
; 		The grating period, in meters or microns.  Default is 255 microns.
; 	DENSITY
; 		The liquid density, in kg/m^3 or gr/cm^3.  Default is water density at
; 		20 degrees C.
; 	VISCOSITY
; 		Kinematic viscosity of the liquid, in meter^2/sec.  Default is water
; 		kinematic viscosity at 20C.
; 	/NO_CORRECTION
; 		Switch.  If set, the viscosity correction is ignored.  For testing
; 		purposes only.
; OUTPUTS:
;		Returns the calculated value(s) of surface tension, in same format as
;		FREQ.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None.
; PROCEDURE:
;		Direct calculation from theoretical formula.  Calls DEFAULT, FPU_FIX,
;		REAL_MM and POLSOLVE, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-NOV-2013 by Mati Meron.
;-

	on_error, 1

	if max(freq) lt 1e3 then wfreq = 1e3*freq else wfreq = freq
	wper = Default(per,255e-6,/dtyp)
	if wper gt 1 then wper = 1e-6*wper 
	wden = Default(den,997.8,/dtyp)
	if wden le 1e2 then wden = 1e3*wden
	wvis = Default(vis,0.96e-6,/dtyp)

	k = 2*!pi*ord/wper
	c = sqrt(2*wvis*k^2)
	omg = 2*!pi*wfreq
	n = n_elements(omg)
	proot = fltarr(n)
	if keyword_set(noc) then proot = sqrt(omg) $
	else for i= 0, n-1 do proot[i]= max(Real_mm(Polsolve([-c^3/2,-omg[i],0,1])))
	res = wden*proot^4/k^3
	if n eq 1 then res = res[0]

	return, FPU_fix(res)
end