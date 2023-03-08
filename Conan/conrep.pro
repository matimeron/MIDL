Function Conrep, focal = foc, radius = rad, angle = ang, $
	degrees = deg, count = con, complement = com, ncomplement = ncm

;+
; NAME:
;		CONREP
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
;		To be used in conjuction with CONSEC only.
; CATEGORY:
;		Array processing.
; CALLING SEQUENCE:
;		Result = CONREP([keywords])
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/FOCAL
;		Switch.  If set, the conical sections are evaluated in the "focal
;		point" representation, i.e. the provided center is taken to be a focal
;		point.  In the default mode the center is taken to be the symmetry
;		center of the shape.
;		The setting of /FOCAL must correspond to the setting used during the
;		call to CONSEC which generated the KEPT array, else an error results.
;	RADIUS
;		The radius of conic section.  May be given as scalar or as a 2-element
;		vector.  In the second case the annular region between RADIUS_min and
;		RADIUS_max is selected.  If not given, a [0,effective infinity] range
;		is used.
;	ANGLE
;		The angles of the rays enclosing the angular section, relative to the
;		x axis.  Given in radians unless /DEGREES is set.  May be given as a
;		scalar or as a 2-element vector.  In the first case, the section
;		between the x-axis and the ray defined by the angle is used.  If not
;		given, the full angular range (0,2*pi) is assumed.
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
;		BARBARIAN.  See CONSEC for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Works only with 2D arrays.  Can be used *only* after a call to CONSEC
;		with the keyword /KEEP has been made.
; PROCEDURE:
;		Works in conjuction with CONSEC (see details there).  A call to CONSEC
;		using the keyword /KEEP must precede calls to CONREP.  The purpose
;		of CONREP is to capitalize on the geometry defined by CONSEC,
;		supplying complementary data.  Therefore, out of the two variables
;		RADIUS and ANGLE only this which didn't appear in the defining (i.e.
;		using /KEEP) call to CONSEC may be used by CONREP.  If CONSEC used
;		both RADIUS and ANGLE then CONREP can only be called with no input
;		variables.
;		Also, the setting of /FOCAL maust be the same as in the defining call
;		to CONSEC.
;		Calls ARREQ and HOW_MANY from MIDL.
; MODIFICATION HISTORY:
;		Created 20-FEB-2001 by Mati Meron.
;-

	common barbarian, nfl, ffl, kmod, wdim, wexc, wphi, wcnt, wrad, wang, $
		uarr, varr, squ, squv, kept

	on_error, 1
	if keyword_set(deg) then amul = !dtor else amul = 1.

	hdum = How_many(first=rad,second=ang,third=foc,which=whi,/nozero)

	if (((whi + kmod) mod 8) gt 3) or ((whi eq 1) and (kmod eq 1)) then $
	message, 'Incompatibility with CONSEC data!'

	sinf = machar()
	drad = [0.,sqrt(sinf.xmax)/2]
	dang = [0.,2*!pi]
	bin = kept

	if whi and 1 then begin
		trad = [min(([rad > drad[0], 0.])[0:1],max=max),max < drad[1]]
		if not Arreq(trad,drad) then begin
			if wexc ne 0 then begin
				if (whi and 4)/4 then begin
					tem = wexc*uarr
					bin = bin and squv ge (tem+trad[0])^2 and $
						squv lt (temporary(tem)+trad[1])^2
				endif else begin
					srad = [min((1 - wexc^2)*trad^2,max=max),max]
					tem = wexc^2*squ
					bin = bin and squv ge (tem+srad[0]) and $
						squv lt (temporary(tem)+srad[1])
				endelse
			endif else bin = bin and squv ge trad[0]^2 and squv lt trad[1]^2
		endif
	endif

	if (whi and 2)/2 then begin
		tang = amul*[min(([ang, 0.])[0:1],max=max),max]
		tang = tang[0] + ((tang - tang[0]) mod (2*!pi))
		if not Arreq(tang,dang) then begin
			psi = tang - wphi
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
	endif

	res = where(bin, con, complement= com, ncomplement = ncm)

	return, res
end
