Function ABC_refm, abc, tet, chi, nvec=nvc, ref=ref, radians=rad, degrees=deg, $
	clean = cle, tmat = tmt

;+
; NAME:
;		ABC_REFM
; VERSION:
;		8.714
; PURPOSE:
;		Performs mirror reflection transformation on an {ABC} structure.
; CATEGORY:
;		Optics ABC-formalism calculations.
; CALLING SEQUENCE:
;		Result = ABC_REFM ( ABC, {, TET, CHI} { keywords})
; INPUTS:
;	ABC
;		An {ABC} type structure.
; OPTIONAL INPUT PARAMETERS:
;	TET
;		The angle between the beam and the mirror's surface, i.e. the angle
;		between the beam and a normal to the mirror's surface, minus !pi/2.
;	CHI
;		The rotation angle of the projection of the mirror's normal on the X-Y
;		plane (which is normal to the beam's direction) relative to the X-axis. 
; KEYWORD PARAMETERS:
; 	NVEC
; 		3-element vector, the normal vector of the mirror.
; 		Note:	If NVEC is provided, TET and CHI are not needed and, if given,
; 				are ignored.
; 	REF
; 		Scalar, the reflectivity coefficient of the mirror.  Default value is 1.
; 	/RADIANS												|
; 		Switch.  If set and the angles TET, CHI are			| At most one of
; 		provided, radians units are assumed.				| these two may be
; 	/DEGREES												| set.  If none is,
; 		Switch.  If set and the angles TET, CHI are			| the default is
; 		provided, degrees units are assumed.				| DEGREES.
; 	/CLEAN
; 		Switch.  Specifying "cleaning up" the result.  See ABC_CLEAN for details
; 	TMAT
; 		Optional output, see below.
; OUTPUTS:
;		Returns the input ABC structure, transformed to the post-reflection
;		coordinates.
; OPTIONAL OUTPUT PARAMETERS:
;	TMAT
;		Returns the matrix used in the transformation to the new coordinates.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Following a reflection it is advantageous to transform the beam's
;		distribution to a new coordinate system, centered on the reflected
;		beam's direction.  The procedure used here is described in detail in the
;		"Transformation of Transverse Coordinates upon Reflection" writeup.
;		Calls ABC_CLEAN.  Calls CAST, DEFAULT, DIAGOARR, ISNUM, ONE_OF, STREQ
;		and VNORM, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUNE-2017 by Mati Meron.
;		Documented 5-JUL-2019 by Mati Meron.
;-

	on_error, 1

	if Streq(tag_names(abc,/str),'abc') then begin
		res = abc
		if Isnum(nvc) then begin
			if n_params() gt 1 then message, 'TET, CHI inputs ignored!', /con
			wnvc = Cast(nvc,5)
		endif else begin
			if (One_of(deg,rad) > 0) then amult = 1d else amult = !dpi/180
			wtet = amult*tet
			wchi = amult*chi
			wnvc = [cos(wtet)*[cos(wchi),sin(wchi)],-sin(wtet)]
		endelse

		smt = (tmt = Diagoarr([1d,1d,1d]) - 2*transpose(wnvc)##wnvc)
		tem = crossp([0,1d,0],tmt[*,2])
		tmt[*,0] = tem/Vnorm(tem)
		tmt[*,1] = crossp(tmt[*,2],tmt[*,0])
		rmt = (tmt##smt)[0:1,0:1]
		tmat = dblarr(4,4)
		tmat[0:1,0:1] = rmt
		tmat[2:3,2:3] = rmt

		res.amp = Default(ref,1)*res.amp
		res.amat = transpose(tmat)##res.amat##tmat
		res.bvc0 = transpose(tmat)##res.bvc0
		res.bvc1 = transpose(tmat)##res.bvc1

		if keyword_set(cle) then res = ABC_clean(res)
	endif else message, 'Primary input must be an ABC structure!'

	return, res
end