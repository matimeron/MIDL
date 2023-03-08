Function LS_spfun, omega, tau, y= y, alp= alp, bet= bet, del= del, xspec= xsp

;+
; NAME:
;		LS_SPFUN
; VERSION:
;		8.15
; PURPOSE:
; 		Calculates the power spectrum for light scattering off liquid surface
; 		waves.
; CATEGORY:
;		LS calculations.
; CALLING SEQUENCE:
;		Result = LS_SPFUN( OMEGA, TAU, [, keywords])
; INPUTS:
;	OMEGA
;		Numeric vector containing the scattering frequency shift.
; OPTIONAL INPUT PARAMETERS:
;	TAU
;		Numeric scalar, time constant.  See Light Scattering by Liquid Surfaces
;		p. 171 (LSLS).  Defaults to 1.
;		
;		Note:  OMEGA can be given as OMEGA*TAU, in which case TAU isn't needed.
; KEYWORD PARAMETERS:
; 	Y
; 		See LSLS.
; 	ALP
; 		Ditto.
; 	BET
; 		Ditto.
; 	DEL
; 		Ditto.
; 	/XSPEC
; 		Switch.  Specifies that the power spectrum of scattering of longitudinal
; 		waves (Px) is to be calculated.  Default is calculatin scattering of 
; 		transverse waves (Pz).
; OUTPUTS:
;		Returns the reduced power spectrum, in same format as OMEGA.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Calculates the Im{}/(omega*tau), see LSLS p. 171.  Calls CAST, DEFAULT,
; 		FPU_FIX and IMAGINARY_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-APR-2012 by Mati Meron.
;-

	on_error, 1

	z = omega*Default(tau,1.,/dtyp)
	s = complex(0,-z)
	wy = Cast(y,4)
	a = Default(alp,1.,/dtyp)
	b = Default(bet,0.,/dtyp)
	d = Default(del,0.,/dtyp)

	sq = sqrt(1 + 2*s)
	xnum = s^2*sq + (wy + d*s)*(sq - 1)
	znum = s^2 + (a*wy + b*s)*(sq - 1)
	den = s^2*((1 + s)^2 + wy - sq) + (a*wy + b*s)*xnum + d*s^3

	res = s
	zer = where(s eq 0,nzer,comp=nozer,ncomp=nnozer)
	if keyword_set(xsp) then begin
		if nzer gt 0 then res[zer] = complex(0,(1. + b)/(a*wy)^2)
		if nnozer gt 0 then res[nozer] = xnum[nozer]/((den*z)[nozer])
	endif else begin
		if nzer gt 0 then res[zer] = complex(0,(1. + d)/wy^2)
		if nnozer gt 0 then res[nozer] = znum[nozer]/((den*z)[nozer])
	endelse

	return, FPU_fix(Imaginary_mm(res))
end