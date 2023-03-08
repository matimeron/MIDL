Pro Polyart, nver, nstep, niter, wiggle= wig, cycle= cyc, true= tru, _extra = _e

;+
; NAME:
;		POLYART
; PURPOSE:
;		Generates iterated polygonial drawings.
; CATEGORY:
;		Graphic Art.
; CALLING SEQUENCE:
;		POLYART, NVER, NSTEP, NITER [, keywords]
; INPUTS:
;	NVER
;		Number of vertices.  Must be >= 3.
;	NSTEP
;		Number of rotation steps, where rotation step is defined as the
;		smallest angle of symmetry rotation for the polygon.
;	NITER
;		Number of polygons drawn.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	WIGGLE
;		Introduces a wiggle in the lines.  Given as (at most) three element
;		vector.  First element gives the number of periods, second (if given)
;		the amplitude, third the optional phase shift.
;	CYCLE
;		If specified and greater than 1, consecutive line segments are drawn
;		using consecutive colors from the color table, within color # range of
;		[0, CYCLE - 1].  Ignored for B&W plots.
;	TRUE
;		A list of color indices for True_Color displays.
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
;		Generates the Polygon shape and proceeds to shrink and rotate it.  Uses
;		DEFAULT, PLVAR_KEEP, BOX, SHAPE_TRANS and WIGGLINE from MIDL.
; MODIFICATION HISTORY:
;		Created 15-DEC-1991 by Mati Meron.
;		Modified 30-APR-2001 by Mati Meron.  Added keywords WIGGLE and TRUE.
;-

	on_error, 1
	if (nstep eq 0 or niter eq 0 or nver lt 3) then $
	message, 'Variables out of range!'

	polang = 2*!pi/nver
	polind = [indgen(nver),0] + 0.5
	polyshap = transpose([[cos(polang*polind)],[sin(polang*polind)]])
	if n_elements(wig) ne 0 then begin
		wpar = [wig,1,0]
		poly = temporary(polyshap)
		polyshap = poly[*,0]
		for i = 1l, nver do begin
			Wiggline, from = poly[*,i-1], to = poly[*,i], /exact, $
			per = wpar[0], amp = wpar[1], pha = wpar[2], /no_show, shap = tem
			polyshap = [[polyshap],[tem[*,1:*]]]
		endfor
	endif
	tet = -nstep*polang/niter
	mag = cos(polang/2)/cos(polang/2 - abs(tet))
	yspan = sin(polang*(0.5 + floor((nver-1)/4)))
	ntr = n_elements(tru)
	if ntr gt 0 then begin
		col = tru
		cyc = Default(cyc,ntr,/dtyp) < ntr
	endif else begin
		col = lindgen(!d.table_size)
		cyc = 1 > Default(cyc,1,hi=3) < !d.table_size
	endelse

	Plvar_keep, act = 'save'
	!x.margin = [3,3]
	!y.margin = [2,2]
	Box, [-1,1], [-yspan,yspan], truasp = 'xcen'
	for i = 0l, niter do begin
		plots, Shape_trans(polyshap, -tet*i, mag^i), thick = 2*mag^i, $
		color = col[i mod cyc], _extra = _e
	endfor
	Plvar_keep, act = 'rest'

	return
end