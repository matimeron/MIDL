Function ABC_sler, abc, sler = sle, clean = cle

;+
; NAME:
;		ABC_SLER
; VERSION:
;		8.714
; PURPOSE:
;		Applies a "slope error" correction to an ABC-type distribution.
; CATEGORY:
;		Optics ABC-formalism calculations.
; CALLING SEQUENCE:
;		Result = ABC_SLER ( ABC, {, keywords})
; INPUTS:
;	ABC
;		An {ABC} type structure.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	SLER
; 		Two element vector, containing slope errors in the X and Y directions.
; 		May be given in miliradians or microradians (corrected to miliradians
; 		internally).
; 	/CLEAN
; 		Switch.  Specifying "cleaning up" the result.  See ABC_CLEAN for details
; OUTPUTS:
;		Returns an ABC structure, with values transformed due to slope error
;		corrections.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Following the precepts from the "OPT_REL" write-up.  Calls ABC_CLEAN.
;		Calls DEFAULT, STREQ and SVD_INVERT, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUNE-2017 by Mati Meron.
;		Documented 5-JUL-2019 by Mati Meron.
;-

	on_error, 1

	if Streq(tag_names(abc,/str),'abc') then begin
		res = abc
		wsle = Default(sle,[0d,0d],/dtyp)
		if n_elements(wsle) eq 2 then begin
			if max(wsle) ge 0.01 then sfac = 1d-3 else sfac = 1d
			wsle = sfac*wsle
		endif else message, 'SLER needs two elements, [horizontal,vertical]!'
		fdet = determ(res.amat)
		tmat = SVD_invert(res.amat,/ref)
		tmat[[2,3],[2,3]] = tmat[[2,3],[2,3]] + (2*wsle)^2
		res.amat = SVD_invert(tmat,/ref)
		sdet = determ(res.amat)
		res.amp = res.amp*sqrt(sdet/fdet)
		if keyword_set(cle) then res = ABC_clean(res)
	endif else message, 'Primary input must be an ABC structure!'

	return, res
end