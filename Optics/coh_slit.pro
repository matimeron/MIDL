Function Coh_slit, psi, sig

;+
; NAME:
;		COH_SLIT
; VERSION:
;		4.3
; PURPOSE:
;		Calculating Frauenhoffer patterns for a slit with a finite coherence
;		length beam.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = COH_SLIT( X [, SIG ])
; INPUTS:
;	X
;		Numeric scalar or array, representing value(s) of k*d*theta, where
;		k = 2*pi/lambda, d is the slit width and theta is the angle.
; OPTIONAL INPUT PARAMETERS:
;	SIG
;		Sigma (i.e. square root of second moment) of the relative width
;		distribution.  Defaults to 0.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the resulting diffraction pattern, in same format as X.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Calculation based on GWO.  Details elsewhere.
;		Calls CAST, CONVOL_MM, DEFAULT, FPU_FIX, SP_BESELJ and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-APR-2003 by Mati Meron.
;-

	on_error, 1

	typ = Type(psi) > Type(sig) > 4
	stp = Cast(sqrt(2*!dpi),typ,typ)
	npsi = n_elements(psi)
	wsig = Default(sig,0.) > Toler(typ=typ)
	if npsi gt 1 then dx = (max(psi,min=min) - min)/npsi $
	else dx = wsig*stp
	f = Sp_beselj(psi/2,0)^2
	g = FPU_fix(exp(-psi^2/(2*wsig^2))*((dx/(wsig*stp)) < 1))
	res = Convol_mm(f,g)

	return, res
end