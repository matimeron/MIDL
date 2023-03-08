Function Iso_incomp, area, spline = spc, show = sho 

;+
; NAME:
;		ISO_INCOMP
; VERSION:
;		8.07
; PURPOSE:
;		Evaluates the inverse compressibility for pre-read isotherm data.
; CATEGORY:
;		Isotherm data processing.
; CALLING SEQUENCE:
;		Result = ISO_INCOMP( [AREA,] SPLINE = SPC [, /SHOW)
; INPUTS:
;	AREA
;		Optional numeric vector, reprenting the area values for which the 
;		evaluation is to  be performed.  Optional, if not given the values
;		contained in the spline data (see below) are used.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SPLINE
;		The spline coefficients array that was generated by ISO_READ (see there)
;		while reading the data.
; 	/SHOW
; 		Switch.  If set, the calculated result is displayed to the screen.
; OUTPUTS:
;		Returns a 2-column array where the first column contains the area 
;		values and the second contains minus the derivative of the isotherm 
;		pressure with respect to area, calculated from the spline fit to the 
;		isotherm data. 
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward spline evaluation Calls PEAK_SHOW from SPEC.  Calls
;		DEFAULT and SPLIN_EVAL, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JUN-2011 by Mati Meron.
;		Rewritten 15-JUN-2011 by Mati Meron.
;-

	on_error, 1

	are = Default(area,reform(spc[*,0]))
	pre = -Splin_eval(are,spc,/der)
	res = transpose([[are],[pre]])

	if keyword_set(sho) then Peak_show, res

	return, res
end