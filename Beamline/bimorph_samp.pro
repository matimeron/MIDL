Pro Bimorph_samp, npo, angle = ang, limit = lim 

;+
; NAME:
;		BIMORPH_SAMP
; VERSION:
;		8.12
; PURPOSE:
;		Finds best sampling distance for bimorph mirror scan. 
; CATEGORY:
;		15ID Bimorph mirror specific.
; CALLING SEQUENCE:
;		BIMORPH_SAMP, NPO, ANGLE = ANG [, LIMIT = LIM]
; INPUTS:
;	NPO
;		Integer scalar, number of sampling points.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	ANGLE
; 		Mirror angle in mr.  Mandatory
; 	LIMIT
; 		Numeric scalar, specifies vertical scan limit (the scan is between
; 		[-LIMIT, LIMIT].  If not given, optimal value is calculated internally.
; OUTPUTS:
; 		Screen output only.  Two parts:
; 			1 -	Displays a plot of the sampling points distribution and (inset)
; 				of minimal beam-boundary distance as a function of LIMIT.
; 			2 -	Print to the screen the scan limit, scan step on mirror and 
; 				minimal beam boundary distance.
;
;		Note:	If LIMIT is given, the parameters printed correspond to this
;				limit, else they correspond to the optimal limit, yielding
;				maximum minimal distance.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		BIMORPH_DAT.  See BIMORPH_INIT for details.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Attempts to maximize the minimal beam-boundary distance while keeping
; 		the average number of sample points per segment as close to constant as
; 		feasible.  Calls BIMORPH_INIT.  Calls DEFAULT, INSET_MM, LEGEND_MM and 
; 		MAKE_GRID, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-NOV-2011 by Mati Meron.
;-

	common bimorph_dat, bexs, nseg, modln, bords, cwei, bmconst
	on_error, 1
	Bimorph_init

	if !d.window lt 0 then window, 0
	cwin = !d.window

	wseg = nseg/2
	modl = modln[wseg:*]
	bord = bords[wseg:*]

	wang = sin(1e-3*ang)
	nh = npo/2.
	nav = nh/wseg
	nlo = floor(nav)
	nhi = ceil(nav)
	if nlo eq nhi then nlo = (nhi - 1) > 1
	if nlo eq nhi then message, 'Nothing to refine!'
	lh = bord[-2]*(nh - 0.5)/(nh - [nlo-0.5,nhi+0.5])
	nch = round(2e3*(lh[1] - lh[0])/nh)
	chlim = Make_grid(lh,nch)
	check = fltarr(nch)

	for j = 0, nch-1 do begin
		sampo = Make_grid(chlim[j]*[-1,1],npo)
		fom = min(modl)
		for i = 0, wseg do fom = fom < min(abs(sampo-bord[i]))
		check[j] = fom
	endfor
	dum = max(check,loc)
	lim = Default(lim,wang*chlim[loc])
	sampo = Make_grid(lim*[-1,1],npo)/wang

	window, 0
	plot, [min(bord,max=max),max], [-0.5,1], /nodata, ystyle=1, $
	tit = 'Optimal sampling with ' + string(npo,form='(i0," points;  ")') + $
	'Scan limit = ' + string(lim,form='(f5.3,"mm")'), $
	xtit= 'Location on mirror (mm)'
	plots, bord, 0, psym=8,thi=2, col = !pcol.red
	plots, sampo, 0, psym= 1,thi= 2, col =!pcol.green, noclip= 0
	Legend_mm, text = ['Segment boundaries','Sampling locations'], loc = 'lr', $
	sym=[8,1], col = [!pcol.red,!pcol.green]
	Inset_mm, wang*chlim, check, loc = 'ul', xtit = 'Scan limit (mm)', $
	ytit = 'Minimal beam-boundary distance (mm)'

	fom = min(modl)
	for i = 0, wseg do fom = fom < min(abs(sampo - bord[i]))
	print
	print, '	Scan limit (vertical)		=  ', lim, form = '(a,f8.3)'
	print, '	Scan step on mirror		=  ',2*lim/wang/(npo-1),form='(a,f8.3)'
	print, '	Minimal beam-boundary distance	=  ', fom, form = '(a,f8.3)'
	print

	wset, cwin
	return
end