Function Mat_rot, ang, axis= axs, degrees = deg, radians = rad, clean = cle

;+
; NAME:
;		MAT_ROT
; VERSION:
;		8.72
; PURPOSE:
;		Generates a 3D rotation matrix.
; CATEGORY:
;		Math Utility.
; CALLING SEQUENCE:
;		Result = MAT_ROT ( ANG, AXIS = AXS, [,/DEGREES] [,/RADIANS], [,/CLEAN])
; INPUTS:
;	ANG
;		Scalar, rotation angle.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	AXIS
;		3D numerical vector, the axis of rotation.  Mandatory.
;	/DEGREES											 	| At most one of
;		Switch.  If set, the angle is assumed in degrees.	| these two may be
;	/RADIANS												| set. Default is
;		Switch.  If set, the angle is assumed in radians.	| RADIANS.
;	/CLEAN
;		Switch.  If set, values close to limit of significance are set to 0.
; OUTPUTS:
; 		Generates a matrix representation of a rotation by angle ANG around
; 		axis AXS.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than AXIS must be a 3D vector.
; PROCEDURE:
;		Straightforward.  Calls CALCTYPE, CAST, MAT_IDENT, MAT_PROJ, ONE_OF,
;		TOLER and VNORM, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JAN-2021 by Mati Meron.
;		Modified 25-APR-2021 by Mati Meron.  Added keyword CLEAN.
;-

	on_error, 1

	typ = Calctype(ang,axs,0.)
	if One_of(deg,rad) eq 0 then amult = !dpi/180 else amult = 1d
	if n_elements(axs) eq 3 then begin
		wang = amult*ang
		waxs = Cast(axs,5)
		waxs = waxs/Vnorm(waxs)
		sx = [[0,0,0],[0,0,-1],[0,1,0]]
		sy = [[0,0,1],[0,0,0],[-1,0,0]]
		sz = [[0,-1,0],[1,0,0],[0,0,0]]

		imat = Mat_ident(3d)
		sum = waxs[0]*sx + waxs[1]*sy + waxs[2]*sz
		res = imat + sin(wang)*sum - (1-cos(wang))*Mat_proj(waxs,/comp)
	endif else message, 'Only 3D vectors allowed!'

	if keyword_set(cle) then begin
		dum = where(abs(res) lt 2*Toler(typ=typ),ndum)
		if ndum gt 0 then res[dum] = 0
	endif

	return, Cast(res,typ,typ,/fix)
end