Function Wrin_ecoeff, nroot

;+
; NAME:
;		WRIN_ECOEFF
; VERSION:
;		8.3
; PURPOSE:
;		Calculating wrinkle energy multiplier.
; CATEGORY:
;		Wrinkles function.
; CALLING SEQUENCE:
;		Result = WRIN_ECOEFF( NROOT)
; INPUTS:
;	NROOT
;		The serial number of a root of the secular equation.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the calculated value of the energy coefficient, defined as
;		(/int((y'')^2 + (k*l)^4*/int(y^2))/(2*(k*L)^2*/int((y')^2))
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		Block WRINSTUFF, containes previous calculation results.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Root must be defined
; PROCEDURE:
;		Based on the results of "Wrinkle Math".  Calls EROMBERG and FPU_FIX
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JUN-2008 by Mati Meron.
;		Modified 5-May-2014 by Mati Meron.  Changed from EROMBERG to ROMBERG,
;		following the upgrade of ROMBERG.
;-

	common wrinstuff, palp, pkl, proots
	on_error, 1
	deps = 1e-12

	nmx = n_elements(proots) - 1
	if nmx gt 0 then begin
		if nroot le 0 or nroot gt nmx then message, $
		'Currently allowed range is 1 - ' + string(nmx,form='(i0)')
	endif else message, 'Common block not defined!'

	int0 = Romberg($
	'wrin_fun',[0d,1d],deps,try=3,/ver,nro=nroot,der=0,/squa,/prev,_extra=_e)
	int1 = Romberg($
	'wrin_fun',[0d,1d],deps,try=3,/ver,nro=nroot,der=1,/squa,/prev,_extra=_e)
	int2 = Romberg($
	'wrin_fun',[0d,1d],deps,try=3,/ver,nro=nroot,der=2,/squa,/prev,_extra=_e)

	res = (pkl^4*int0 + int2)/(2*pkl^2*int1)

	return, FPU_fix(res)
end