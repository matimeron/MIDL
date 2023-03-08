Function Consec, npo, size = siz, new = new, focal = foc, keep = kep, $
	radius = rad, angle = ang, excent = exc, rotation = phi, center = cnt, $
	degrees = deg, count = con, complement = com, ncomplement = ncm

;+
; NAME:
;		CONSEC
; VERSION:
;		4.0
; PURPOSE:
;		Selects the inside (and, optionally, outside) of either
;		1)	a region of an array enclosed within a conic section (circle,
;			ellipse, parabola or hiperbola) or between two conic sections.
;		or
;		2)  a region of an array contained within the angle between two rays.
;
;		or a combination of both.
; CATEGORY:
;		Array processing.
; CALLING SEQUENCE:
;		Result = CONSEC([NPO] [,SIZE = SIZ] [,keywords])
; INPUTS:
;	NPO
;		Scalar or 2-element vector.  Not needed if SIZ is provided, mandatory
;		otherwise.  If NPO is a scalar then CONSEC assumes a square NPOxNPO
;		array, else it assumes an NPO[0]xNPO[1] array.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SIZE
;		Accepts input in the form of the output of the IDL SIZE command and
;		uses it to form the array.  Mandatory, unless NPO is provided.  If
;		both SIZE and NPO are provided, NPO is ignored.
;	/NEW
;		Switch.  Forces reevaluation of the common block.  Resets parameters
;		to:
;			CENTER		- 	Geometric center of the array
;			EXCENTRICITY -	0
;			ROTATION 	-	0
;			RADIAL RANGE -	[0 - practical infinity]
;			ANGLE RANGE	-	Full, [0, 2*pi]
;
;		Also resets the "data saved" KMOD flag in the common block, to 0.  All
;		these can be overriden using keywords.
;	/FOCAL
;		Switch.  If set, the conical sections are evaluated in the "focal
;		point" representation, i.e. the provided center is taken to be a focal
;		point.  In the default mode the center is taken to be the symmetry
;		center of the shape.
;
;		Important:
;			1)  /FOCAL is not "sticky" and if in followup calls CONSEC is
;				called without /FOCAL, the representation reverts to standard.
;			2)	Any switch between "focal" and "symmetric" modes carries an
;				implied /NEW (see above) with the subsequent resetting of
;				all variables.
;	/KEEP
;		Switch.  If set, data to be used with other routines is saved in the
;		common block.
;		Note:  Setting /KEEP prevents CONSEC from reusing the previous RADIUS
;		and ANGLE values (see below).  Instead, the initialization default
;		values are substituted for RADIUS/ANGLE if not provided.
;	RADIUS
;		The radius of conic section.  May be given as scalar or as a 2-element
;		vector.  In the second case the annular region between RADIUS_min and
;		RADIUS_max is selected.  If not given, the radius used on the previous
;		call is reused (unless /KEEP is set).  Initialized (on first call) to
;		[0,infinity].
;	ANGLE
;		The angles of the rays enclosing the angular section, relative to the
;		x axis.  Given in radians unless /DEGREES is set.  May be given as a
;		scalar or as a 2-element vector.  In the first case, the section
;		between the x-axis and the ray defined by the angle is used.  If not
;		given, the angles from the previous call are reused (unless /KEEP is
;		set).  Initialized (on first call) to [0, 2*pi].
;	EXCENT
;		The excentricity of the conic section.
;			0	-    circle.  This is the default value.
;			< 1	-    ellipse.
;			1	-    parabola.
;			> 1	-    hiperbola.
;		If not given, the value from the previous call is used.  Initialized
;		to 0.
;		Note that the EXCENT = 1, parabola case, can only be represented in
;		the focal mode.
;	ROTATION
;		The rotation angle of the main axis of the conic section, relative to
;		horizontal.  Given in radians, unless the keyword DEGREES is set.  If
;		not given, the value from the previous call is used.  Initialized to 0.
;	CENTER
;		Optional.  Location of the center of the conic section.  If given,
;		must be a 2-element vector.  If not provided, the location from the
;		previous call is used.  Initialized to the middle of each dimension.
;	/DEGREES
;		Switch.  If set, input angles (when given) are taken to be in degrees.
;		Default is radians.
;	COUNT
;		Optional output, see below.
;	COMPLEMENT
;		Optional output, see below.
;	NCOMPLEMENT
;		Optional output, see below.
; OUTPUTS:
;		Returns the indices of the array locations which fulfill the
;		appropriate "inside" condition.  If no such indices exist, returns -1.
; OPTIONAL OUTPUT PARAMETERS:
;	COUNT
;		The name of the variable to receive the number of points fulfilling the
;		"inside" condition.  Same as the keyword COUNT in the WHERE function.
;	COMPLEMENT
;		The name of the variable to receive to complement of the indices set
;		returned by the function (i.e. indices corresponding to the outside of
;		the selected region).  Same as the keyword COMPLEMENT in WHERE.
;	NCOMPLEMENT
;		The name of the variable to receive the number of points fulfilling the
;		"outside" condition.  Same as the keyword NCOMPLEMENT in WHERE.
; COMMON BLOCKS:
;		BARBARIAN.  Includes:
;
;			NFL	-	"New" flag, setting it to TRUE forces reevaluation.
;			FFL	-	"Focal" flag, TRUE in "focal" mode, FALSE otherwise.
;			KMOD -	Specifies the type of region saved with the last /KEEP.
;					Possible values are:
;											0	- 	None.
;											1`	- 	Radial range.
;											2	-	Angular range.
;											3	-	Radial and angular range.
;			WDIM -	Dimensions of current/last array.
;			WEXC -	Exccentricity value.
;			WPHI - 	Rotation angle (in radians).
;			WCNT -	Center coordinates (2-element vector).
;			WRAD -	RADIUS values (2-element vector in [min,max] form)
;			WANG -	ANGLE values (2-element vector in [min,max] form), radians.
;			UARR -	A 2-D array of same dimensions as specified by SIZE,
;					containing the U-coordinates (rotated and translated X).
;			VARR -	A 2-D array of same dimensions as specified by SIZE,
;					containing the V-coordinates (rotated and translated Y).
;			SQU	-	A 2-D array containing U^2.
;			SQUV - 	A 2-D array containing U^2 + V^2.
;			KEPT -	A 2-D byte array.  Set to 1b at all the locations returned
;					by the last CONSEC call using /KEEP.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Works only with 2D arrays.
; PROCEDURE:
;		Straightforward.  Generates arrays for the X and Y coordinates
;		corresponding to the original array and used them to check for the
;		*inside the conical section* and *inside the angular sector*
;		conditions, with the provided radii and angles.
;		Calls ARREQ, CAST, DEFAULT, HOW_MANY and TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 5-NOV-1999 by Mati Meron.
;		Upgraded 20-FEB-2001 by Mati Meron.  New keywords and
;		capabilities added.
;-

	common barbarian, nfl, ffl, kmod, wdim, wexc, wphi, wcnt, wrad, wang, $
		uarr, varr, squ, squv, kept

	on_error, 1
	sinf = machar()
	drad = [0.,sqrt(sinf.xmax)/2]
	dang = [0.,2*!pi]
	if keyword_set(deg) then amul = !dtor else amul = 1.

	if n_elements(siz) gt 0 then begin
		if siz[0] eq 2 then dim = siz[1:2] else $
		message, 'Only two dimensions allowed!'
	endif else begin
		ntem = n_elements(npo)
		case ntem of
			0	:	message, 'Missing size information!'
			1	:	dim = Cast([npo,npo],3,3)
			2	:	dim = Cast(npo,3,3)
			else:	message, 'Only two dimensions allowed!'
		endcase
	endelse

	if keyword_set(foc) then tffl = 255b else tffl = 0b
	nfl = (Type(nfl) eq 0) or (tffl ne Default(ffl,tffl)) or $
		(not Arreq(dim,wdim)) or byte(keyword_set(new))
	ffl = tffl

	ncnt = n_elements(cnt)
	if nfl then begin
		wdim = dim
		wexc = 0.
		wphi = 0.
		wcnt = 0.5*(wdim-1)
		wrad = drad
		wang = dang
		uarr = (findgen(wdim[0],wdim[1]) mod wdim[0]) - wcnt[0]
		varr = (transpose(findgen(wdim[1],wdim[0])) mod wdim[1]) - wcnt[1]
		if ncnt eq 0 then begin
			squ = uarr^2
			squv = squ + varr^2
		endif
		kept = make_array(wdim[0],wdim[1],val=1b)
		kmod = 0
		nfl = not nfl
	endif

	wexc = Default(exc,wexc,/dtyp)

	phifl = 0
	if n_elements(phi) ne 0 then begin
		tphi = amul*phi
		if tphi ne wphi and wexc ne 0 then begin
			dphi = tphi - wphi
			tem = cos(dphi)*uarr + sin(dphi)*varr
			varr = -sin(dphi)*uarr + cos(dphi)*varr
			uarr = temporary(tem)
			if ncnt eq 0 then squ = uarr^2 else phifl = 1
			wphi = tphi
		endif
	endif

	if ncnt eq 2 then begin
		if not Arreq(cnt,wcnt) then begin
			dcnt = wcnt - cnt
			uarr = uarr + cos(wphi)*dcnt[0] + sin(wphi)*dcnt[1]
			varr = varr - sin(wphi)*dcnt[0] + cos(wphi)*dcnt[1]
			squ = uarr^2
			squv = squ + varr^2
			wcnt = cnt
		endif else if phifl then squ = uarr^2
	endif else if ncnt ne 0 then message, '2 coordinates needed for center!'

	if keyword_set(kep) then begin
		kept = make_array(wdim[0],wdim[1],val=1b)
		wrad = drad
		wang = dang
	endif

	hdum = How_many(first=rad,second=ang,third=foc,which=whi,/nozero)
	if whi and 1 then begin
		wrad = [min(([rad > drad[0], 0.])[0:1],max=max),max < drad[1]]
		if Arreq(wrad,drad) then whi = whi-1
	endif
	if (whi and 2)/2 then begin
		wang = amul*[min(([ang, 0.])[0:1],max=max),max]
		if (wang[1] - wang[0]) ge (2*!pi) then begin
			wang = dang
			whi = whi-2
		endif
	endif

	if not Arreq(wrad,drad) then begin
		if wexc ne 0 then begin
			if ffl then begin
				tem = wexc*uarr
				bin = $
				squv ge (tem+wrad[0])^2 and squv lt (temporary(tem)+wrad[1])^2
			endif else begin
				srad = [min((1 - wexc^2)*wrad^2,max=max),max]
				tem = wexc^2*squ
				bin = $
				squv ge (tem+srad[0]) and squv lt (temporary(tem)+srad[1])
			endelse
		endif else bin = squv ge wrad[0]^2 and squv lt wrad[1]^2
	endif else bin = make_array(wdim[0],wdim[1],val=1b)

	if not Arreq(wang,dang) then begin
		psi = wang - wphi
		if (psi[1] - psi[0]) lt !pi then begin
			bin = bin and $
			(((-sin(psi[0])*uarr + cos(psi[0])*varr) ge 0) and $
			((-sin(psi[1])*uarr + cos(psi[1])*varr) lt 0))
		endif else begin
			bin = bin and $
			(((-sin(psi[0])*uarr + cos(psi[0])*varr) ge 0) or $
			((-sin(psi[1])*uarr + cos(psi[1])*varr) lt 0))
		endelse
	endif

	res = where(bin, con, complement= com, ncomplement = ncm)
	if keyword_set(kep) then begin
		kmod = whi
		if ncm gt 0 then kept(com) = 0b
	endif

	return, res
end
