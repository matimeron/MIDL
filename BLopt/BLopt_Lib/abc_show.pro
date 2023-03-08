Pro ABC_show, abc, radial = rad, angular = ang, recenter = rec, $
	hrange = hrn, vrange = vrn, normalize = nrm, radians = rdn, degrees = deg, $
	project = prj, hor_ang = han, ver_ang = van, cumulative = cum, _extra= _e

;+
; NAME:
;		ABC_SHOW
; VERSION:
;		8.714
; PURPOSE:
;		Displays the distribution in ABC, properly integrated, as a 2D image.
; CATEGORY:
;		Optics ABC-formalism processing.
; CALLING SEQUENCE:
;		ABC_SHOW, ABC, { keywords}
; INPUTS:
;	ABC
;		An {ABC} type structure.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 	/RADIAL												| At most one of these
; 		Switch.  Specifies radial distribution image.	| two may be set.  If
; 	/ANGULAR											| none is, default is
; 		Switch.  Specifies angular distribution image.	| RADIAL.
; 	/RECENTER
; 		Switch.  Specifying recentering the image.
; 	HRANGE
; 		Specifies the horizontal range of the display.  If given as 2-element
; 		vector, the horizontal range is between MIN(HRANGE) and MAX(HRANGE).
; 		if given as scalar, the range is 0.5*ABS(HRANGE)*[-1,1].  If not
; 		provided, range is generated internally from the data.
; 	VRANGE
; 		Same as HRANGE for vertical range.
; 	/NORMALIZE
; 		Switch.  If set, displayed data is normalized to maximum of 1.
; 	/RADIANS												|
; 		Switch.  If set and PROJECT (see below) is set		| At most one of
; 		radians units are assumed for angles.				| these two may be
; 	/DEGREES												| set.  If none is,
; 		Switch.  If set and PROJECT (see below) is set		| the default is
; 		degree units are assumed for angles.				| DEGREES.
; 	/PROJECT
; 		Switch.  If set, the displayed image is projected to other plane than
; 		the default X-Y plane.
; 		Note:	PROJECT is only valid for displaying radial distributions.  For
; 		angular distributions it is quietly ignored (if set).
; 	HOR_ANG
; 		Scalar value, the inplane transformation angle for the projection plane.
; 	VER_ANG
; 		Scalar value, the tilt angle of the projection plane.
; 	/CUMULATIVE
; 		Switch.  If set, the image is saved in a common block and for as long as
; 		CUMULATIVE remains set, all subsequent images are added together.
; 	_EXTRA
; 		System keyword used to transfer additional keywords to imbedded
; 		routines.  Not to be used directly.
; OUTPUTS:
;		None other than the displayed image.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block ABC_CUM.  Contains:
;			CFL	-	flag signifyig the CUMULATIVE is operating.
;			IMG -	Last saved image, i.e. 2D array.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Evaluation using the definitions of the ABC distribution (see "ABC Beam
;		Calculations" and geometric trnsformations, followed by image display.
;		Calls ABC_INT.  Calls DEFAULT, DISPLAY_MM, FPU_FIX, MAKE_GRID, ONE_OF,
;		SVD_INVERT and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUNE-2017 by Mati Meron.
;		Documented 5-JUL-2019 by Mati Meron.
;-

	common ABC_cum, cfl, img
	on_error, 1
	eps = Toler()

	cfl = 0 > Default(cum,0,/dtyp) < 2
	if cfl eq 0 then img = [] else img = Default(img,0)

	tit = ['Radial Distribution', 'Angular Distribution']
	xtit = ['X (mm)', "X' (mr)"]
	ytit = ['Y (mm)', "Y' (mr)"]

	whi = One_of(rad,ang) > 0
	rafl = [0,0]
	rafl[whi] = 1
	sdat = ABC_int(abc,rad=rafl[1],ang=rafl[0])

	if keyword_set(nrm) then amp = 1 else amp = sdat.amp
	off = dblarr(2)
	a = sdat.amat
	b = sdat.bvc
	c = sdat.con

	if keyword_set(prj) and not whi then begin
		if (One_of(deg,rdn) > 0) then amult = 1d else amult = !dpi/180
		whan = amult*han
		wvan = amult*van
		tmat = [[cos(whan),sin(whan)],[sin(wvan)*[-sin(whan),cos(whan)]]]
		a = transpose(tmat)##a##tmat
		b = transpose(tmat)##b
		c = c
	endif

	if keyword_set(rec) then begin
		ainv = SVD_invert(a)
		off = -ainv##transpose(b)
		c = c - b##ainv##transpose(b)/2d
		b = 0*b
	endif

	traa = trace(a)
	teps = traa*sqrt(eps)
	len = sqrt(traa/(determ(a + teps) > teps))
	drn = 4*[-len,len]
	whrn = Default(hrn,drn,/dtyp)
	if n_elements(whrn) eq 1 then whrn = abs(whrn)*[-0.5,0.5]
	wvrn = Default(vrn,drn,/dtyp)
	if n_elements(wvrn) eq 1 then wvrn = abs(wvrn)*[-0.5,0.5]

	xy = Make_grid([[whrn + off[0]],[wvrn + off[1]]],256)
	x = reform(xy[0,*,*])
	y = reform(xy[1,*,*])
	siz = size(xy)
	siz[1] = siz[1] + 1
	glob = make_array(siz=siz)
	glob[0:1,*,*] = xy
	arg = (a[0,0]*x^2 + (a[0,1]+a[1,0])*x*y + a[1,1]*y^2)/2 + b[0]*x + b[1]*y +c
	glob[2,*,*] = amp*exp(-arg)

	if cfl ne 0 then glob[2,*,*] = glob[2,*,*] + img
	Display_mm, glob, /auz, tit= tit[whi], xtit= xtit[whi], ytit= ytit[whi], $
	 _extra = _e
	dum = Fpu_fix(glob)
	if cfl eq 2 then begin
		cfl = 0
		img = []
	endif else if cfl eq 1 then img = reform(glob[2,*,*])

	return
end