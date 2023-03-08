Function Mat_refl, v, clean = cle

;+
; NAME:
;		MAT_REFL
; VERSION:
;		8.72
; PURPOSE:
;		Generates a reflection matrix.
; CATEGORY:
;		Math Utility.
; CALLING SEQUENCE:
;		Result = MAT_REFL ( V))
; INPUTS:
;	U
;		Vector, mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/CLEAN
;		Switch.  If set, values close to limit of significance are set to 0.
; OUTPUTS:
; 		Generates a matrix representation of a reflection in direction normal
; 		to V.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls CALCTYPE, CAST, MAT_IDENT, TOLER, VINP and VOUP,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JAN-2021 by Mati Meron.
;		Modified 25-APR-2021 by Mati Meron.  Added keyword CLEAN.
;-

	on_error, 1

	typ = Calctype(v,0.)
	n = Cast(n_elements(v),typ)
	res = Mat_ident(n) - 2*Voup(v,v)/Vinp(v,v)
	if n_elements(res) eq 1 then res = res[0]

	if keyword_set(cle) then begin
		dum = where(abs(res) lt 4*Toler(/doub),ndum)
		if ndum gt 0 then res[dum] = 0
	endif

	return, res
end