Pro Circart, niter, tet, rlim, degrees= deg, cycle= cyc, true= tru, _extra= _e

;+
; NAME:
;		CIRCART
; PURPOSE:
;		Generates iterated circular drawings.
; CATEGORY:
;		Graphic Art.
; CALLING SEQUENCE:
;		CIRCART, NITER, TET, RLIM [, keywords]
; INPUTS:
;	NITER
;		Number of circles drawn.
;	TET
;		Coordinate rotation per step.
;	RLIM
;		Final radius, must be < 1 (initial radius is 1.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/DEGREES
;		Switch, specifies that TET is given in degrees (default is radians).
;	CYCLE
;		If specified and greater than 1, consecutive line segments are drawn
;		using consecutive colors from the color table, within color # range of
;		[0, CYCLE - 1].  Ignored for B&W plots.
;	TRUE
;		A list of color indices for True_Color displays.
;	_EXTRA
;		A formal keyword used to pass all plotting keywords.  Not to be used
;		directly.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Generates the circle shape and proceeds to shrink and rotate it.  Uses
;		DEFAULT, PLVAR_KEEP, BOX, COO_CONV and SHAPE_TRANS from MIDL.
; MODIFICATION HISTORY:
;		Created 15-DEC-1991 by Mati Meron.
;		Modified 30-APR-2001 by Mati Meron.  Added keyword TRUE.
;-

	on_error, 1
	rlim = Default(rlim,0.01,/dtype)
    if rlim ge 1 then message, 'Final radius must be <1 !'

	tet = Default(tet,0)
	if keyword_set(deg) then tet = !dtor*tet
	ntr = n_elements(tru)
	if ntr gt 0 then begin
		col = tru
		cyc = Default(cyc,ntr,/dtyp) < ntr
	endif else begin
		col = lindgen(!d.table_size)
		cyc = 1 > Default(cyc,1,hi=3) < !d.table_size
	endelse
	rad = 1.
	rat = rlim^(1./niter)
	off = [0,0]
	rotan = 0

	Plvar_keep, act = 'save'
	!x.margin = [3,3]
	!y.margin = [2,2]
	Box, [-1,1], [-1,1], truasp = 'xcen'
	dmult = max([Coo_conv(1,ax= 'x',to ='Dev'),Coo_conv(1,ax= 'y',to= 'Dev')])
	npoints = 4096
	for i = 0, niter do begin
		nenpoints = 4*(1 + fix(!pi/4*sqrt(dmult*rad)))
		if nenpoints lt npoints then begin
			npoints = nenpoints
			tem = 2*!pi/npoints*[indgen(npoints),0]
			circ = transpose([[cos(tem)], [sin(tem)]])
		endif
		plots, Shape_trans(circ,0,rad,off), thick= 2*rad, $
		color=col[i mod cyc], _extra = _e
		off = rad*(1 - rat)*[cos(rotan),sin(rotan)] + off
		rad = rat*rad
		rotan = rotan + tet
	endfor
	Plvar_keep, act = 'rest'

	return
end
