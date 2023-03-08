Pro Liss, mx, my, npoints, funs = funs, params = pars, phase = pha, $
	cycle = cyc, roundit = ron, nolin = nol, true = tru, _extra = _e

;+
; NAME:
;		LISS
; PURPOSE:
;		Generates Lissajou curves.
; CATEGORY:
;		Graphic Art.
; CALLING SEQUENCE:
;		LISS, MX, MY [, NPOINTS] [, keywords]
; INPUTS:
;	MX, MY
;		Relative frequencies for the X and Y directions.
; OPTIONAL INPUT PARAMETERS:
;	NPOINTS
;		Number of points in the plot.  Defaults to 1024.
; KEYWORD PARAMETERS:
;	FUNS
;		Accepts a character vector of length 2, and uses the entries as the
;		names of the generating functions.  Defaults to ['cos','sin'].  If only
;		one name is provided, same function is used for the X and Y dimensions.
;	PARAMS
;		Accepts a vector of length 2 which includes optional parameters for the
;		generating functions (one parameter per function).  If PARAMS includes
;		only one entry, same parameter is provided for both functions.
;	/PHASE
;		Switch.  If set, changes the way in which the phase shifts are computed.
;	CYCLE
;		If specified and greater than one, consecutive line segments are drawn
;		using consecutive colors from the color table, within color # range of
;		[0, CYCLE - 1].  Ignored for B&W plots.
;	/ROUNDIT
;		Switch.  Applies a square --> circle transformation.
;	/NOLIN
;		Switch.  If specified, only points are drawn, skipping the joining
;		lines.
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
;		Straightforward.  Uses DEFAULT, STRMATCH_MM, BOX and PLVAR_KEEP from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 15-DEC-1991 by Mati Meron.
;		Major upgrade on 15-FEB-1994 by Mati Meron.  Combined previous LISS and
;		Gliss into a single module.
;		Modified 30-APR-2001 by Mati Meron.  Added keyword TRUE.
;-

	on_error, 1
	np = Default(npoints,1024l,/dtype)
	funs = Default(funs,['cos','sin'],/strict)
	if n_elements(funs) eq 1 then funs = [funs,funs]
	if Strmatch_mm('cos',funs) ge 0 or Strmatch_mm('sin',funs) ge 0 then $
	step = !pi/np else step = 1./np
	if n_elements(pars) ne 0 then begin
		pfl = 1
		if n_elements(pars) eq 1 then pars = [pars,pars]
	endif else pfl = 0
	ntr = n_elements(tru)
	if ntr gt 0 then begin
		col = tru
		cyc = Default(cyc,ntr,/dtyp) < ntr
	endif else begin
		col = lindgen(!d.table_size)
		cyc = 1 > Default(cyc,1,hi=3) < !d.table_size
	endelse
	xfl = fix(Streq(!d.name,'X',1))
	if keyword_set(pha) then begin
		dum = mx*lindgen(np + 1) mod np
		hdum = where(dum ge (np + 1)/2)
		dum(hdum) = dum(hdum) - np
		x = 2*step*dum
		dum = my*lindgen(np + 1) mod np
		hdum = where(dum ge (np + 1)/2)
		dum[hdum] = dum[hdum] - np
		y = 2*step*dum
	endif else begin
		x = step*(2*(mx*lindgen(np + 1) mod np) - np)
		y = step*(2*(my*lindgen(np + 1) mod np) - np)
	endelse

	if pfl then begin
		fx = call_function(funs[0],x,pars[0])
		fy = call_function(funs[1],y,pars[1])
	endif else begin
		fx = call_function(funs[0],x)
		fy = call_function(funs[1],y)
	endelse
	fig = transpose([[fx/max(abs(fx))],[fy/max(abs(fy))]])
	if keyword_set(ron) then fig = Squpeg(fig)

	if not keyword_set(nol) then begin
		sval = 0
		mag = 1.0
	endif else begin
		short = !pi/6*indgen(13)
		sym = 0.25*transpose([[cos(short)],[sin(short)]])
		usersym , sym, /fill
		sval = 8
		mag = 1.01
	end

	Plvar_keep, action = 'save'
	!x.margin = [5,5]
	!y.margin = [2,2]
	Box, [-1,1], mag*[-1,1], truasp = 'xcen'
	for i = 1l, np do plots, fig[*,i-1:i], psym = sval, $
	color = col[xfl + (i mod (cyc - xfl))], _extra = _e
	Plvar_keep, action = 'restore'

	return
end