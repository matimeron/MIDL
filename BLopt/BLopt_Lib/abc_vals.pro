Function ABC_vals, abc, check = chk

;+
; NAME:
;		ABC_VALS
; VERSION:
;		8.714
; PURPOSE:
;		Returns the sigma values of the distribution in an {ABC} structure.
; CATEGORY:
;		Optics ABC-formalism calculations.
; CALLING SEQUENCE:
;		Result = ABC_VALS ( ABC, {, /CHECK})
; INPUTS:
;	ABC
;		An {ABC} type structure.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/CHECK
; 		Switch, performs consistency check.
; OUTPUTS:
;		Returns a 4-element vector containing, in order:
;		[SIGMA_X, SIGMA_Y, SIGMA_ANGX, SIGMA_ANGY]
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward extraction from the data in ABC.
;		If CHECK is set, prints tot he screen the value of the determinant of
;		the A matrix in ABC, as well as 1/(product of all sigma values)^2.
;		These two values should be equal.  Calls DIAGOVEC, STREQ and SVD_INVERT,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUNE-2017 by Mati Meron.
;		Documented 5-JUL-2019 by Mati Meron.
;-

	on_error, 1

	if Streq(tag_names(abc,/str),'abc') then begin
		fir = Diagovec(abc.amat)
		sec = Diagovec(SVD_invert(abc.amat))
		res = sqrt([1/fir[0:1],sec[2:3]])
		if keyword_set(chk) then print, determ(abc.amat), product(1/res^2)
	endif else message, 'Primary input must be an ABC structure!'

	return, res
end