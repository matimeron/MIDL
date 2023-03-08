Pro Stripes, periods, ratio, rand_space= rans, rand_width= ranw, gaussian= gau,$
	xlims = xls, ylims = yls, edge = edg, colors = cls, background = bck, $
	rotate = rot, degrees = deg, _extra = _e

;+
; NAME:
;		STRIPES
; VERSION:
;		8.15
; PURPOSE:
;		Draws a striped rectangle between the limits specified by XLIMS and 
;		YLIMS, possibly rotated.  The drawing is done in the currently
;		defined plot area.
; CATEGORY:
;		General Graphics.
; CALLING SEQUENCE:
;		STRIPES, PERIODS, RATIO, XLIMS = XLS, YLIMS = YLS [, optional keywords]
; INPUTS:
;	PERIODS
;		Integer scalar, number of periods (i.e. foreground-background stripe
;		pairs.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;	RATIO
;		Floating scalar, foreground to background width ratio within a period.
;		Defaults to 0.5.
; KEYWORD PARAMETERS:
;	RAND_SPACE
;		Floating scalar, specifies magnitude of random variation of the stripes'
;		locations.  Defaults to 0 (i.e. no variation).
;	RAND_WIDTH
;		Floating scalar, specifies magnitude of random variation of the stripes'
;		widths.  Defaults to 0 (i.e. no variation).
;	/GAUSSIAN
;		Switch.  If set, the random variations specified above follow a Gaussian
;		distribution.  Default is uniform distribution.
;	XLIMS
;		2 dimensional vector, format [xmin,xmax], mandatory.  Specifies initial
;		X-boundaries of the stripe pattern.
;	YLIMS
;		2 dimensional vector, format [xmin,xmax], mandatory.  Specifies initial
;		Y-boundaries of the stripe pattern.
;	/EDGE
;		Switch.  If set, one extra foreground color stripe is added at the end 
;		of the pattern so that both edges are same color.
;	COLORS
;		Integer scalar or vector containing a list of true type color codes.  If
;		more than one entry is present, the colors of the stripes are cycled.
;		If not given, the system color !P.COLOR is used.
;	BACKGROUND
;		Integer scalar, specifying a background color.  If not given, 
;		!P.BACKGROUND is used.
;	ROTATE
;		Optional.  Angle of rotation in the mathematical positive direction.
;		Assumed in radians, unless DEGREES is set.  For more details see the 
;		routine RECTAN.
;	/DEGREES
;		Switch.  Specifies that the rotation angle is given in degrees.
;	_EXTRA
;		A formal keyword used to pass keywords to embedded routines.  Not to be
;		used directly.
;		
;		Note:	All RECTAN keywords can be used.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Only restrictions related to RECTAN keywords.
; PROCEDURE:
;		Straightforward, through multiple calls to RECTAN, in MIDL.  Also calls
;		DEFAULT, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JAN-1995 by Mati Meron.
;		Checked for operation under Windows, 30-JAN-2001, by Mati Meron.
;		Slightly modified 15-MAR-2012 by Mati Meron.  Added keyword BACKGROUND.
;-

	on_error, 1

	ratio = Default(ratio,0.5,/dtype) < 1.
	rans = Default(rans,0.,/dtype)
	ranw = Default(ranw,0.,/dtype)
	nper = periods + keyword_set(edg)
	cls = Default(cls,!p.color,/dtype)
	ncls = n_elements(cls)

	if rans gt 0 or ranw gt 0 then begin
		alp = (1. - ratio)/(4*(1+ratio))
		if keyword_set(gau) then begin
			spat = alp*rans*randomn(se,nper)
			wpat = alp*ranw*randomn(se,nper)
		endif else begin
			spat = alp*rans*(2*randomu(se,nper) - 1)
			wpat = alp*ranw*(2*randomu(se,nper) - 1)
		endelse
	endif else begin
		spat = fltarr(nper)
		wpat = fltarr(nper)
	endelse

	if yls(0) gt yls(1) then yls = reverse(yls)
	hei = (yls(1) - yls(0))/float(periods + ratio*keyword_set(edg))
	heib = ratio*hei
	rot_cent = 0.5*[total(xls),total(yls)]

	if n_elements(bck) eq 1 then Rectan, xlims = xls, ylims = yls, /fill, $
	color= bck, rotate= rot, rotation_center= rot_cent, degrees= deg, _extra= _e
	for i = 0l, nper - 1 do Rectan, /fill, col = cls[i mod ncls], xlims = xls, $
	ylims = yls(0) + (i + spat(i))*hei + [0,heib*(1 + wpat(i))], $
	rotate = rot, rotation_center = rot_cent, degrees = deg, _extra = _e

	return
end