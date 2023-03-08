Pro Elliart, rat, nstep, niter, cycle = cyc, true = tru, _extra = _e

;+
; NAME:
;		ELLIART
; PURPOSE:
;		Generates iterated ellipsoidal drawings.
; CATEGORY:
;		Graphic Art.
; CALLING SEQUENCE:
;		ELLIART, NITER, TET, RLIM [, CYCLE = CYC]
; INPUTS:
;	RAT
;		Ratio of minor to major axis.
;	NSTEP
;		Number of 180 degrees (symmetry) turns.
;	NITER
;		Number of ellipses drawn.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
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
;		Generates the ellipse shape and proceeds to shrink and rotate it.  Uses
;		DEFAULT, PLVAR_KEEP, BOX, COO_CONV and SHAPE_TRANS from MIDL.
; MODIFICATION HISTORY:
;		Created 15-DEC-1991 by Mati Meron.
;		Modified 30-APR-2001 by Mati Meron.  Added keyword TRUE.
;-

	on_error, 1
	if (nstep eq 0 or niter le 0) then $
	message, 'Variables out of range!'

	ntr = n_elements(tru)
	if ntr gt 0 then begin
		col = tru
		cyc = Default(cyc,ntr,/dtyp) < ntr
	endif else begin
		col = lindgen(!d.table_size)
		cyc = 1 > Default(cyc,1,hi=3) < !d.table_size
	endelse
	tet = !pi*sign(1-rat)*nstep/niter
	fac = sin(tet)*(1 - rat^2)
	mag = 2.*rat/(fac + sqrt((2*rat)^2 + fac^2))
	yspan = rat > float(!d.y_vsize)/!d.x_vsize

	Plvar_keep, act = 'save'
	!x.margin = [3,3]
	!y.margin = [2,2]
	Box, [-1,1], [-yspan,yspan], truasp = 'xcen'
	dmult = max([Coo_conv(1,ax= 'x',to ='Dev'),Coo_conv(rat,ax= 'y',to= 'Dev')])
	npoints = 4096
	for i = 0, niter do begin
		nenpoints = 4*(1 + fix(!pi/4*sqrt(dmult*mag^i)))
		if nenpoints lt npoints then begin
			npoints = nenpoints
			tem = 2*!pi/npoints*[indgen(npoints),0]
			ellishap = transpose([[cos(tem)],[rat*sin(tem)]])
		endif
		plots, Shape_trans(ellishap, tet*i, mag^i), thick = 2*mag^i, $
		color = col[i mod cyc], _extra = _e
	endfor
	Plvar_keep, act = 'rest'

	return
end
