Function Bimorph_fcalc, cur, energy= ene, angle= ang, ivolt= ivl, mir_loc= mlc,$
	flat= flt, collimate= col, focus_to= foc, show= sho, shape= shp, _extra= _e

;+
; NAME:
;		BIMORPH_FCALC
; VERSION:
;		8.72
; PURPOSE:
;		Calculating voltages for a bimorph mirror.
; CATEGORY:
;		Bimorph mirror specific.
; CALLING SEQUENCE:
;		Result = BIMORPH_FCALC( CUR, keywords)
; INPUTS:
;	CUR
;		A [2,N} or [3,N] array representing the current mirror slope profile.
;		Similar to the BASE input of BIMORPH_MCALC but does not need to be 
;		measured with no voltages being applied.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ENERGY
;		Beam energy, in keV.  If not given, defaults to 10 keV. Has minor, but 
;		non-zero influence on the results. 
;	ANGLE
;		Mirror angle, in mr.
;	MIR_LOC
;		Numeric scalar location of the mirror(s) measured in meters, from the 
;		source.  For two mirrors the midpoint should be used.
;	IVOLT
;		Numeric vector containing the bimorph voltages corresponding to CUR.
;		If not given, the output of BIMORPH_FCALC contains voltage increments 
;		only, not the full voltages required.
;	/FLAT													|
;		Switch.  Specifies that a flat mirror is required.	| One and only one
;	/COLLIMATE												| of these keywords 
;		Switch.  Specifies that a collimating mirror is		| must be used.
;		required.											|
;	FOCUS_TO												|
;		Numeric scalar, the required location of the focus.	|
;	/SHOW
;		Switch.  If set, the calculated corrected mirror slope errors, before
;		and after the voltage correction, are displayed.
;	/SHAPE
;		Switch.  If set and if SHOW is set, the shape errors are displayed as
;		well.
;	_EXTRA
;		Formal keyword used to pass keywords to imbedded routines.  Not to be 
;		used directly.
; OUTPUTS:
;		Returns the set of voltages required to bring the bimorph mirror to the
;		required shape.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		BIMORPH_DAT.  See BIMORPH_INIT for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Calculates the required local curvature corrections by fitting the 
; 		difference between theoretical and measured slope profile to the 
; 		function QCHAIN (see in MIDL_LIB).  Converts curvature corrections to
; 		voltage corrections using an empirical voltage-curvature factor.  If
; 		needed, corrects the resulting voltages to assure that their differences
; 		are within allowed limits (this correction is only active when IVOLT is
; 		present.
; 		Calls UND_BEAMSIZE, BIMORPH_CORV, BIMORPH_INIT, QCHAIN_FIT and 
; 		QCHAIN_FUN.  Calls ARREQ, DEFAULT, INTEG, LINFIT_MM, ONE_OF and POLEVAL,
; 		FROM MIDL.
; MODIFICATION HISTORY:
;		Created 1-DEC-2011 by Mati Meron.
;		Modified 30-JUL-2012 by Mati Meron.  Internal changes.
;		Modified 1-JAN-2013 by Mati Meron.  Replaced dependence on the obsolete
;		BEAM_SIZE with UND_BEAMSIZE.
;		Modified 20-DEC-2020 by Mati Meron.  Internal changes.
;-

	common bimorph_dat, bexs, nseg, modln, bords, cwei, bmconst
	on_error, 1
	Bimorph_init

	hstet = sin(1e-3*ang)/2
	mlc = Default(mlc,32.5)

	whi = One_of(flt,col,foc,val=val)
	case whi of
		0	:	irad = 0
		1	:	irad = hstet/mlc
		2	:	irad = hstet/mlc + hstet/(foc-mlc)
		else:	message, 'Missing focusing type info!'
	endcase
	irad = 1e3*irad

	zmir = reform(cur[0,*])
	cslp = reform(cur[1,*])
	zc = (bords[0:-2] + bords[1:-1])/2
	coe = Linfit_mm(zmir,cslp)
	msk = lonarr(nseg)
	for i = 0l, nseg-1 do begin
		dum = where(zmir ge bords[i] and zmir lt bords[i+1], ndum)
		msk[i] = ndum < 1
	endfor
	dum = where(msk eq 0, ndum)
	if ndum gt 0 then begin
		for i = 0l, ndum-1 do begin
			zmir = [zmir,zc[dum[i]]]
			cslp = [cslp,Poleval(zc[dum[i]],coe)]
		endfor
		s = sort(zmir)
		zmir = zmir[s]
		cslp = cslp[s]
	endif
	rslp = irad*zmir
	dslp = rslp - cslp

	vsig = UND_beamsize(ene,dist=mlc,/ver,/def,_extra=_e)
	wei = exp(-(2*hstet*zmir/vsig)^2/2)
	c_ini = replicate(irad-coe[1],nseg)
	curv = Qchain_fit(zmir,dslp,gval=bords,/der,c_ini=c_ini,mask=msk,wei=wei)
	dvolt = curv/bmconst

	cvfl = 0
	case n_elements(ivl) of
		0	:	volt = dvolt
		nseg:	begin
					volt = Bimorph_corv(ivl + dvolt, stat=sta)
					if sta ne 1 then begin
						ccurv = (volt - ivl)*bmconst
						cvfl = 1
						print
						if sta/2 then print, "	Adjusted voltages" $
						else print, "	Can't fully correct voltages"
					endif
				end
		else:	message, 'Wrong initial voltage input!
	endcase

	if keyword_set(sho) then begin
		cwin = !d.window
		window, 0
		eslp = Qchain_fun(curv,zmir,gval=bords)
		if cvfl then ceslp= Qchain_fun(ccurv,zmir,gval=bords) else ceslp= eslp
		yrn = max(abs([-dslp,eslp-dslp,ceslp-dslp]))
		plot, zmir, -dslp, tit= 'Slope Errors', xtit= 'mm', ytit= '!7l!xrad', $
		/nodata, yran= [-1,1]*yrn, _extra = _e
		oplot, 2*zmir, 0*zmir, line=2
		oplot, zmir, -dslp, col = !pcol.red, thi = 2
		oplot, zmir, eslp - dslp, col = !pcol.green, thi = 2, line = 1
		oplot, zmir, ceslp - dslp, col = !pcol.green, thi = 2
		if keyword_set(shp) then begin
			window, 1
			csher = 1e-3*Integ(zmir,-dslp)
			esher = 1e-3*Integ(zmir,eslp-dslp)
			if cvfl then cesher = 1e-3*Integ(zmir,ceslp-dslp) else cesher=esher
			yrn = max(abs([csher,esher,cesher]))
			plot, zmir, csher, tit= 'Shape Errors', xtit= 'mm', ytit= '!7l!xm',$
			/nodata, yran= [-1,1]*yrn, _extra = _e
			oplot, 2*zmir, 0*zmir, line=2
			oplot, zmir, csher, col = !pcol.red, thi = 2
			oplot, zmir, esher ,col = !pcol.green, thi = 2, line = 1
			oplot, zmir, cesher, col = !pcol.green, thi = 2
		endif
		curerr = sqrt(total(dslp^2*wei)/total(wei))
		corerr = sqrt(total((dslp-eslp)^2*wei)/total(wei))
		print
		print, '	Current rms slope error 	  = ', string(curerr)
		print, '	Corrected rms slope error, ideal  = ', string(corerr)
		if cvfl then begin
			ccorerr = sqrt(total((dslp-ceslp)^2*wei)/total(wei))	
			print,'	Corrected rms slope error, v-lim. = ', string(ccorerr)
		endif
		print
		if cwin ge 0 then wset, cwin
	endif

	return, float(round(volt))
end