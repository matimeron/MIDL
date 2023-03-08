Function XR_eta, qz, temp = tmp, gamma = gam, absolute = abt

;+
; NAME:
;		XR_ETA
; VERSION:
;		7.09
; PURPOSE:
;		Calculating the "Eta" factor for capillary wave scattering.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = XR_ETA (QZ, TEMP = TMP, GAMMA = GAM [, /ABSOLUTE)
; INPUTS:
;	QZ
;		Q_z value(s), numeric, otherwise arbitary.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	TEMP
;		Temperature, in degrees Centigrade, unless ABSOLUTE is set.  Scalar.
;	GAMMA
;		Surface tension, in mN/m.  Scalar.
;	/ABSOLUTE
;		Switch, if set the temperature is taken in degrees K.
; OUTPUTS:
;		Returns the value(s) of Eta corresponding to the inputs.  The output
;		format follows this of QZ.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, from definition.  Calls FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 15-AUG-2008 by Mati Meron.
;-

	on_error, 1
	mult = 1e23
	kb = 1.3806504e-023
	dtof = 273.15

	if keyword_set(abt) then tof = 0. else tof = dtof
	res = mult*kb*(tmp+tof)*qz^2/(2*!pi*gam)

	return, FPU_fix(res)
end