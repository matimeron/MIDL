Function Mat_rot2, first = fir, second = sec, clean = cle

;+
; NAME:
;		MAT_ROT2
; VERSION:
;		8.72
; PURPOSE:
;		Generates a 3D rotation matrix based on two vectors.
; CATEGORY:
;		Math Utility.
; CALLING SEQUENCE:
;		Result = MAT_ROT2 ( FIRST = FIR, SECOND = SEC [, /CLEAN)
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FIRST
;		3D numerical vector, mandatory.
;	SECOND
;		3D numerical vector, mandatory.
;	/CLEAN
;		Switch.  If set, values close to limit of significance are set to 0.
; OUTPUTS:
; 		Generates the rotation matrix required to rotate a vector in the
; 		direction of FIRST to the direction of SECOND.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Both FIRST and SECOND must be 3D numeric vectors.
; PROCEDURE:
;		Straightforward.  Calls CALCTYPE, CAST, MAT_IDENT, MAT_ROT, TOLER,
;		VINP and VNORM, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JUN-2021 by Mati Meron.
;-

	on_error, 1

	typ = Calctype(fir,sec,0.)
	wfir = Cast(fir,5)
	wsec = Cast(sec,5)
	wfir = wfir/Vnorm(wfir)
	wsec = wsec/Vnorm(sec)
	ax = crossp(wfir,wsec)
	ang = asin(Vnorm(ax))
	if Vinp(wfir,wsec) lt 0 then ang = !dpi - ang
	if ang eq 0 then res = Mat_ident(3d) $
	else res = Mat_rot(ang,axis=ax)

	if keyword_set(cle) then begin
		dum = where(abs(res) lt 2*Toler(typ=typ),ndum)
		if ndum gt 0 then res[dum] = 0
	endif

	return, Cast(res,typ,typ,/fix)
end