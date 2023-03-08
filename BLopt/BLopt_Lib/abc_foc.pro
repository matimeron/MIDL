Function ABC_foc, abc, focl = foc, clean = cle

;+
; NAME:
;		ABC_FOC
; VERSION:
;		8.714
; PURPOSE:
;		Performs focusing transformation on an ABC-type distribution.
; CATEGORY:
;		Optics ABC-formalism calculations.
; CALLING SEQUENCE:
;		Result = ABC_FOC ( ABC, {, keywords})
; INPUTS:
;	ABC
;		An {ABC} type structure.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	FOCL
; 		Scalar, focal length (normally in meters).
; 	/CLEAN
; 		Switch.  Specifying "cleaning up" the result.  See ABC_CLEAN for details
; OUTPUTS:
;		Returns an ABC structure, with values transformed thru focusing by
;		optics with focal length given by FOCL.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Following the focusing procedure described in the "Beam-Crystal
;		Interaction" write-up.  Calls ABC_CLEAN.  Calls DEFAULT, DIAGOARR, SIGN,
;		STREQ and TOLER, from MIDL.
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUNE-2017 by Mati Meron.
;		Documented 5-JUL-2019 by Mati Meron.
;-

	on_error, 1

	if Streq(tag_names(abc,/str),'abc') then begin
		res = abc
		wfoc = Default(foc,[0d,0d],/dtyp)
		if n_elements(wfoc) eq 2 then begin
			eps = Toler(/doub)
			inf = 1d/(abs(wfoc) > eps)*Sign(wfoc)
		endif else message, 'FOCL needs two elements, [horizontal,vertical]!'
		tmat = Diagoarr(replicate(1d,4))
		tmat[[0,1],[2,3]] = inf
		res.amat = transpose(tmat)##res.amat##tmat
		res.bvc0 = transpose(tmat)##res.bvc0
		res.bvc1 = transpose(tmat)##res.bvc1
		if keyword_set(cle) then res = ABC_clean(res)
	endif else message, 'Primary input must be an ABC structure!'

	return, res
end