Function Mat_proj, u, v, complementary = com, clean = cle

;+
; NAME:
;		MAT_PROJ
; VERSION:
;		8.72
; PURPOSE:
;		Generates a projection or skewed projection matrix.
; CATEGORY:
;		Math Utility.
; CALLING SEQUENCE:
;		Result = MAT_PROJ ( U [, V] [, /COMPLEMENTARY])
; INPUTS:
;	U
;		Vector, mandatory.
; OPTIONAL INPUT PARAMETERS:
;	V
;		Vector.  If given, must be same length as U.
; KEYWORD PARAMETERS:
;	/COMPLEMENTARY
;		Switch.  If set, the complementary operator (i.e. (I - Projection) is
;		returned.
;	/CLEAN
;		Switch.  If set, values close to limit of significance are set to 0.
; OUTPUTS:
; 		If a single input is given, returns a projection operator, i.e. the
; 		outer product of U with itself, divided by the inner (scalar) product of
; 		U with itself, or, if /COMPLEMENTARY is set, returns an identity matrix
; 		minus said product.
; 		If two inputs are given, returns a skewed projection operator, i.e. the
; 		outer product of U and V, divided by the inner (scalar) product of
; 		U and V, or, if /COMPLEMENTARY is set, returns an identity matrix
; 		minus said product.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than U and V (if V given) must be of same length.
; PROCEDURE:
;		Straightforward.  Calls CALCTYPE, CAST, CODIMS, MAT_IDENT, TOLER, VINP
;		and VOUP, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JAN-2021 by Mati Meron.
;		Modified 25-APR-2021 by Mati Meron.  Added keyword CLEAN.
;-

	on_error, 1

	case n_params() of
		0	:	message, 'Missing input(s)
		1	:	v = u
		2	:
		else:	message, 'Too many inputs'	
	endcase

	if Codims(u,v,dim=n) then begin
		res = Voup(u,v)/Vinp(u,v)
		if keyword_set(com) then begin
			typ = Calctype(u,v,0.)
			n = Cast(n,typ)
			res = Mat_ident(n) - res
		endif
	endif else message, 'Dimensional mismatch!'

	if n_elements(res) eq 1 then res = res[0]
	if keyword_set(cle) then begin
		dum = where(abs(res) lt 4*Toler(/doub),ndum)
		if ndum gt 0 then res[dum] = 0
	endif

	return, res
end