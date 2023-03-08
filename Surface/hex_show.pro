Pro Hex_show, ord, xlim = xl, ylim = yl

;+
; NAME:
;		HEX_SHOW
; VERSION:
;		8.0
; PURPOSE:
;		Displays the arrangement of "diffracting lines" for a given order of 2-D
;		hexagonal lattice.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		HEX_SHOW, ORD, XLIM = XL, YLIM = YL
; INPUTS:
;	ORD
;		The diffraction order(s) for which a display is wanted.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	XLIM
;		Two element vector, the X-range of the display, in units of single
;		scatterer's size.  Default is [-6,6]
;	YLIM
;		Same as XLIM, for the Y-range.  Default is [-5,5].
; OUTPUTS:
;		None other than a graphic display.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Draws a set of hexagonally arranged circular scatterers, then draws the
;		diffracting lines corresponding to the specific order requested.
;		Calculates line parameters from the order's definition.  Calls HEXORD.
;		Also calls BOX, CIRCLE_MM, DEFAULT, GCD, ISNUM, MAKE_GRID, PLVAR_KEEP 
;		and SHAPE_TRANS from MIDL.
; MODIFICATION HISTORY:
;		Created 25-AUG-2003 by Mati Meron.
;		Modified 10-NOV-2007 by Mati Meron.  Internal changes.
;		Modified 25-AUG-2010 by Mati meron.  Replaced CIRCLE with CIRCLE_MM.
;-

	on_error, 1

	acb = sqrt(3)/2

	a = [1.,0.]
	b = [1./2,acb]
	ap = [1.,-1./(2*acb)]
	bp = [0,1/acb]

	xlim = Default(xl,6)*[-1,1]
	ylim = Default(yl,5)*[-1,1]

	if Isnum(ord) then begin

		Plvar_keep, act = 'save'
		!x.margin = [5,5]
		!y.margin = [4,4]

		nzfl = ord ne 0
		qsq = Hexord(ord,ind=ind)
		rvec = ind[0]*ap + ind[1]*bp
		tvec = ind[1]*a - ind[0]*b
		if nzfl then spac = GCD(ind[0],ind[1])*acb^2/qsq
		sing = strcompress('[' + strjoin(reform(ind),', ') + ']')
		tit = strcompress('order # '+ string(ord)+ ' ; indices = '+ sing) +'!c'

		Box, xlim, ylim, tru = 'xcen', /update, tit = tit

		wxl = max(xlim)
		wyl = max(ylim)
		span = sqrt(wxl^2 + wyl^2)

		xg = ceil((b[1]*wxl + b[0]*wyl)/b[1])
		yg = ceil(wyl/b[1])
		carr = Make_grid([[-xg,xg],[yg,-yg]],[1,-1],/step,dimvec = dim)
		clen = dim[0]*dim[1]
		x = reform(carr[0,*,*] + b[0]*carr[1,*,*],clen)
		y = reform(b[1]*carr[1,*,*],clen)
		xy = transpose([[x],[y]])

		Circle_mm, cent = [0,0], rad = 0.5, shape=circ, thick=4
		for i = 0l, clen-1 do plots, $
		Shape_trans(circ,0,1,xy[*,i]), noclip=0, thi=2

		if nzfl then begin
			glin = span*[[-tvec],[tvec]]
			gper = ceil(span/spac)
			for i = -gper, gper do plots, $
			Shape_trans(glin,0,1,i*spac*rvec), noclip=0, thi=3
		endif

		Plvar_keep, act = 'rest'

	endif else message, 'What indices'

	return
end