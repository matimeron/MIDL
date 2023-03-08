Function Trungauss, x, sig, width = wid

;+
; NAME:
;		TRUNGAUSS
; VERSION:
;		8.72
; PURPOSE:
;		Calculates Truncated Gaussian values.  This is gaussian distribution
;		truncated to a finite width a renormalized.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = TRUNGAUSS (X, SIG, WIDTH = WID)
; INPUTS:
; 	X
; 		Numeric, arbitrary.
; 	SIGMA
; 		Numeric scalar.  The sigma value of the distribution.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 	WIDTH
; 		The truncation width.  Numeric scalar.  If not given, assumed infinite.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None other than those specified for inputs.
; PROCEDURE:
;		Straightforward, from definition. Call CALCTYPE, CAST and IGAMMA_MM
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 15-MAY-2018 by Mati Meron.
;		Documented 25-DEC-2020 by Mati Meron.
;-

	on_error, 1

	typ = Calctype(x,sig,wid,def=4)
	wx = Cast(x,5)
	if n_elements(sig) eq 1 then wsig = sig[0] $
	else message, 'Sig must be a scalar!'
	if keyword_set(wid) then begin
		if n_elements(wid) eq 1 then wwid = wid[0] $
		else message, 'Width must be a scalar!'
		res = 0*wx
		dum = where(abs(x) le abs(wwid/2.),ndum)
		if ndum gt 0 then res[dum] = exp(-wx[dum]^2/(2*wsig^2))/$
			(sqrt(2*!dpi*wsig^2)*Igamma_mm(wwid^2/(8d*wsig^2),.5d))
	endif else res = exp(-wx^2/(2*wsig^2))/sqrt(2*!dpi*wsig^2)

	return, Cast(res,typ,typ,/fix)
end