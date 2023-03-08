Pro Analyzer, eran, len, show_range = shran, crystal = crs, index = ind, $
	radians = rad, degrees = deg, miscut = mis,  symmetric = sym, $
	gap = gap, blade = bld, center_off = cof, beam_off = bof

;+
; NAME:
;		ANALYZER
; VERSION:
;		4.3
; PURPOSE:
;		Calculating parameters for spatial acceptance optimized channel-cut
;		monochromator (as well as results of deviations).
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = ANA_FUN (E [, LEN], keywords)
; INPUTS:
;	ERAN
;		A two element vector providing the optimization energy range (in keV).
; OPTIONAL INPUT PARAMETERS:
;	LEN
;		Total length of crystal (arbitrary units).  Default value is 1.
; KEYWORD PARAMETERS:
;	SHOW_RANGE
;		A two element vector specifying plot range.  Defaults to ERAN.
;	CRYSTAL
;		Character scalar translating to a name of recognized crystal.
;		Currently the recognized list includes : Diamond, Silicon, Germanium.
;		Only the first two letters are needed.
;	INDEX
;		The reflection index, given as 3 element vector.  Example: [1,1,1].
;	/RADIANS
;		Switch.  If set, the miscut angle is given in radians.
;	/DEGREES
;		Switch.  If set, tthe miscut angle is given in degrees.
;
;		Comment:	Either RADIANS or DEGREES (but not both) may be set.  If
;					neither is set, the default is DEGREES.
;	MISCUT
;		Magnitude of the miscut angle.  Default is zero.
;	/SYMMETRIC
;		Switch.  If set, limits the reflection to the part symmetric around
;		the centerline of the beam.
;	GAP
;		Width of the gap (channel) of the monochromator.  If not given, the
;		optimal value is used for the second curve (see PROCEDURE).
;	BLADE
;		Length of the blade (vane) of the monochromator.  If not given, the
;		optimal value is used for the second curve (see PROCEDURE).
;	CENTER_OFF
;		The offset of the center of rotation from the optimal position.  See
;		"Transverse Acceptance of a Channel-cut Crystal".  Default is zero.
;	BEAM_OFF
;		The transverse offset of the centerline of the beam from the optimal
;		location (passing through the center of rotation).  Default is zero.
; OUTPUTS:
;		None
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Calculation following the writeup "Transverse Acceptance of a
;		Channel-cut Crystal".  Displayes two curves, one of the acceptance
;		obtained using optimized parameters, the other for the acceptance
;		obtained with the actual parameters.  Calls DEFAULT, HOW_MANY and
;		MAKE_GRID from MIDL.  Also calls ANA_FUN and OPT_VALS.
; MODIFICATION HISTORY:
;		Created 30-APR-2002 by Mati Meron.
;-

	on_error, 1

	oacp = Opt_vals(len,energy=eran,crys=crs,ind=ind,/rad,$
			blade=obld,gap=ogap,anr=tran)
	pran = Default(shran,eran,lo=4)
	if n_elements(pran) ne 2 then message, 'Range needs two elements!'
	if pran[1] lt pran[0] then pran = reverse(pran)
	penr = Make_grid(pran,0.01 < (pran[1] - pran[0])/100,/step)

	oatot = Ana_fun(penr,len,opt=eran,crys=crs,ind=ind)
	plot, penr, oatot
	plots, eran, [oacp,oacp], psym = 1, thick=2

	modfl= (How_many(first=gap,second=bld,third=cof,fourth=bof,fifth=mis) gt 0)
	if modfl then begin
		wgap = Default(gap,ogap,/dtyp)
		wbld = Default(bld,obld,/dtyp)
		wcof = Default(cof,0.,/dtyp)
		wbof = Default(bof,0.,/dtyp)
		if (One_of(deg,rad) > 0) eq 0 then amult = 1 else amult = !radeg
		wmis = Default(mis,0.,/dtyp)*amult
		watot = Ana_fun(penr,len,opt=eran,crys=crs,ind=ind,rad=rad,deg=deg,$
				mis=mis,gap=gap,blade=bld,cent=cof,beam=bof,sym=sym)
		oplot, penr, watot, line=2
	endif

	print
	print, 'Full length  = ', len, form = '(a,f8.3)'
	print
	print, 'Optimization energy range = ', eran[0], eran[1], $
	form = '(a,"[",f7.3,",",f7.3," ]")'
	print, 'Optimization angle range  = ', !radeg*tran[0], !radeg*tran[1], $
	form = '(a,"[",f7.3,",",f7.3," ]")'
	print
	print, 'Channel gap, optimal  = ', ogap, form = '(a,f7.3)'
	if modfl then print, 'Channel gap, used     = ', wgap, form = '(a,f7.3)'
	print, 'Blade length, optimal = ', obld, form = '(a,f8.3)'
	if modfl then print, 'Blade length, used    = ', wbld, form = '(a,f8.3)'
	if modfl then begin
		print
		print, 'Center offset = ', wcof, form = '(a,f7.3)'
		print, 'Beam offset   = ', wbof, form = '(a,f7.3)'
		print, 'Miscut angle  = ', wmis, form = '(a,f7.3)'
	endif
	print
	print, 'Min. acceptance, optimal   = ', oacp, form = '(a,f7.3)'
	print

	return
end