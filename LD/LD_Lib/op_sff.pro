Pro Op_sff, rad, ppos = pps, range = ran, _extra = _e

;+
; NAME:
;		OP_SFF
; VERSION:
;		8.42
; PURPOSE:
;		Adding form factor data to a GID image.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = OP_SFF (RAD, PPOS = POS, RANGE  = RAN [, _EXTRA = _E])
; INPUTS:
;	RAD
;		Numeric scalar, the radius of a spherical scatterer, in Angstrem.
;		Alternatively can be given as 2 element vector.  This is taken to
;		indicate a rotational (around the vertical axis) ellipsoid scatterer
;		with RAD[0], RAD[1] being the horizontal and vertical radii,
;		respectively.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	PPOS
;		A 4-element integer vector containing the "plot position" (see
;		!P.POSITION) of the image in device units.
;	RANGE
;		A 4-element floating vector containing the X and Y coordinate ranges 
;		of the image in same format as PPOS, i.e. [X_min, Y_min, X_max, Y_max].
;
;		Note:	PPOS and RANGE are provided as outputs by the routine DISPLAY_MM
;				which is used to generate the image.  See there for details.
;	_EXTRA
;		A formal keyword used to pass keywords acceptable by imbedded routines.
;		Not to be used directly.
; OUTPUTS:
;		None, other than graphic output to existing image.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls ELLIPSE_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 10-OCT-2010 by Mati Meron.  Functionally similar to the old
;		routine TRACE_SFF, but completely different internally.
;		Modified 10-JUN-2015 by Mati Meron.  Enabled 2-element radii for
;		elipsoidal scatterers.
;-

	on_error, 1

	wrad = ([rad,rad])[0:1]
	if min(wrad) gt 0 then begin
		if n_elements(pps) ne 4 or n_elements(ran) ne 4 then message, $
		'Bad position and/or range inputs!'
		qrmax = sqrt(total((wrad*ran[2:3])^2))
		nmax = max(floor(((qrmax + sqrt(qrmax^2 + 8))/!pi - 1)/2))
		if nmax gt 0 then begin
			plot, ran[[0,2]], ran[[1,3]], pos=pps, /dev, xsty=13, ysty=13, $
			/noerase, /nodata
			nn = 1 + lindgen(nmax)
			arn = (nn + 0.5)*!pi
			qnn = (1./wrad)#(arn - 2/(arn + sqrt(arn^2-8./3)))
			for i = 0, nmax-1 do Ellipse_mm, cent=[0,0], rad=qnn[*,i], $
			noclip=0 ,color = !pcol.white, _extra = _e
		endif
	endif

	return
end