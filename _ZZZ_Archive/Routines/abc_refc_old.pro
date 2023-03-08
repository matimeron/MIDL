Function ABC_refc_old, abc, tet, chi, nvec = nvc, eta = eta, ref = ref, $
	hor_pol= hor, ver_pol= ver, radians= rad, degrees= deg, clean= cle, tmat=tmt

;+
; NAME:
;		ABC_REFC
; VERSION:
;		8.714
; PURPOSE:
;		Performs crystal reflection transformation on an {ABC} structure.
; CATEGORY:
;		Optics ABC-formalism calculations.
; CALLING SEQUENCE:
;		Result = ABC_REFC ( ABC, {, TET, CHI} { keywords})
; INPUTS:
;	ABC
;		An {ABC} type structure.
; OPTIONAL INPUT PARAMETERS:
;	TET
;		The angle between the beam and the crystal's surface, i.e. the angle
;		between the beam and a normal to the mirror's surface, minus !pi/2.
;	CHI
;		The rotation angle of the projection of the crystal's normal on the X-Y
;		plane (which is normal to the beam's direction) relative to the X-axis.
; KEYWORD PARAMETERS:
; 	NVEC
; 		3-element vector, the normal vector of the crystal.
; 		Note:	If NVEC is provided, TET and CHI are not needed and, if given,
; 				are ignored.
; 	ETA
; 		Scalar, the Delta_E/E value of the crystal.  No defaults.
; 	REF
; 		Scalar, the reflectivity coefficient of the crystal. Default value is 1.
; 	/HOR_POL										|
; 		Switch.  Specifies horizontal polarization.	| At most one of these two
; 	/VER_POL										| may be set.  If none is,
; 		Switch.  Specifies vertical polarization.	| the default is HORIZONTAL.
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
;		Returns the input ABC structure, modified to account for the crystal
;		bandwidth and polarization factor and transformed to the post-reflection
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
; 		The distribution is adjusted for the crystal's bandwidth and the
; 		polarization factor of the reflectivity.  Furthermore, following a
; 		reflection it is advantageous to transform the beam's distribution to a
; 		new coordinate system, centered on the reflected beam's direction.  The
; 		procedure used here is described in detail in the "Transformation of
; 		Transverse Coordinates upon Reflection" writeup.
;		Calls ABC_CLEAN and ABC_REFM.  Calls CAST, DEFAULT, ISNUM, ONE_OF and
;		STREQ, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUNE-2017 by Mati Meron.
;		Documented 5-JUL-2019 by Mati Meron.
;-

	on_error, 1
	mult = 1d3

	if Streq(tag_names(abc,/str),'abc') then begin
		res = abc
		if Isnum(nvc) then begin
			if n_params() gt 1 then message, 'TET, CHI inputs ignored!', /con
			wnvc = Cast(nvc,5)
			wtet = asin(-wnvc[2])
			wchi = atan(wnvc[1],wnvc[0])
			wmvc = [cos(wchi),sin(wchi)]
		endif else begin
			if (One_of(deg,rad) > 0) then amult = 1d else amult = !dpi/180
			wtet = amult*tet
			wchi = amult*chi
			wmvc = [cos(wchi),sin(wchi)]
			wnvc = [cos(wtet)*wmvc,-sin(wtet)]
		endelse

		pfac = abs(cos(2*wtet))
		if (One_of(hor,ver) > 0) then wchi = wchi + !dpi/2
		weta = eta*(pfac*cos(wchi)^2 + sin(wchi)^2)
		refq = Default(ref,1)
		gv = (1 - refq)/(2*sqrt(refq))
		if pfac gt 0 then begin
			gvp = gv/pfac
			refp = (sqrt(1 + gvp^2) - gvp)^2
		endif else refp = 0
		wref = refp*cos(wchi)^2 + refq*sin(wchi)^2
		res.amp = wref*res.amp

		dar = mult*weta*tan(wtet)
		res.amat[2:3,2:3]= res.amat[2:3,2:3]+ 2*!dpi/dar^2*transpose(wmvc)##wmvc
		res.bvc1[2:3] = res.bvc1[2:3] - 2*!dpi/(dar*weta)*wmvc
		res.con2 = res.con2 + 2*!dpi/weta^2

		res = ABC_refm(res,nvec=wnvc,tmat=tmt)
		if keyword_set(cle) then res = ABC_clean(res)
	endif else message, 'Primary input must be an ABC structure!'

	return, res
end