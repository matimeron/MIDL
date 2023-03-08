Function Scan_inter_old, scan, ival, sxlog = xlg, sylog = ylg, spline = spl

;+
; NAME:
;		SCAN_INTER
; VERSION:
;		8.23
; PURPOSE:
;		Interpolates within a scan
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_INTER ( SCAN, IVAL [keywords])
; INPUTS:
;	SCAN
;		A scan, i.e. a [3,n] array.
;	IVAL
;		Interpolation value(s) the X values(s) for which the corresponding Y
;		and error values need to be found.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/SXLOG
;		Switch.  If set, the interpolation is linear in LOG(X) instead of X.
;	/SYLOG
;		Switch.  If set, the interpolation is linear in LOG(Y) instead of Y.
;	/SPLINE
;		Switch.  If explicitly set to 0, linear interpolation is used.  The 
;		default is spline interpolation.
; OUTPUTS:
;		Returns a result in the standard scan format, i.e. a [3,n] array with
;		the three columns containing X values (the provided IVAL), the
;		interpolated Y values and the interpolated Y errors, respectively.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.  Note, however, that when /XLOG and/or /YLOG is stipulated, the
;		data must be such as allows the calculation of logarithms.
; PROCEDURE:
;		Straightforward spline interpolation or, if SPLINE is set to zero, 
;		linear interpolation.  Uses SCAN_SORT.  Also calls DEFAULT, FPU_FIX, 
;		JOIN_XY, SPLIN_COEFFS, SPLIN_EVAL and SPLIT_XY from MIDL.
; MODIFICATION HISTORY:
;		Created 10-SEP-2002 by Mati Meron.
;		Modified 15-APR-2004 by Mati Meron.  Internal data format changes.
;		Modified 25-DEC-2013 by Mati Meron.  Added SPLINE keyword to facilitate
;		spline interpolation option.
;		Modified 5-FEB-2014 by Mati Meron.  Made spline interpolation the 
;		default option.
;-

	on_error, 1

	xlfl = keyword_set(xlg)
	ylfl = keyword_set(ylg)
	ni = n_elements(ival)
	if ni eq 0 then message, 'Nothing to interpolate!'
	sscan = Scan_sort(scan)

	if xlfl then begin
		xit = alog(ival)
		sscan[0,*] = alog(sscan[0,*])
	endif else xit = ival

	if ylfl then begin
		sscan[2,*] = sscan[2,*]/sscan[1,*]
		sscan[1,*] = alog(sscan[1,*])
	endif

	ns = Split_xy(sscan,x=x,y=y,z=ser,/keep)
	if ns lt 2 then message, 'At least two data points needed!'

	if Default(spl,1,/dtyp) ne 0 then begin
		yspl = Splin_coeffs(x,y)
		espl = Splin_coeffs(x,ser)
		res = Join_xy(xit,Splin_eval(xit,yspl),Splin_eval(xit,espl))
	endif else begin
		lo = value_locate(x,xit)
		hi = lo + (xit gt x[lo])
	
		dum = where(lo lt 0, ndum)
		if ndum gt 0 then begin
			lo[dum] = 0
			hi[dum] = 1
		endif
	
		dum = where(hi eq ns, ndum)
		if ndum gt 0 then begin
			lo[dum] = ns - 2
			hi[dum] = ns - 1
		endif
	
		res = reform(sscan[*,lo],3,ni)
		dum = where(hi gt lo,ndum)
		if ndum gt 0 then begin
			dif = x[hi[dum]] - x[lo[dum]]
			fir = (x[hi[dum]] - xit[dum])/dif
			sec = (xit[dum] - x[lo[dum]])/dif
			res[0,dum] = xit[dum]
			res[1,dum] = fir*y[lo[dum]] + sec*y[hi[dum]]
			res[2,dum] = sqrt((fir*ser[lo[dum]])^2 + (sec*ser[hi[dum]])^2)
		endif
	endelse

	if xlfl then res[0,*] = exp(res[0,*])
	if ylfl then begin
		res[1,*] = exp(res[1,*])
		res[2,*] = res[1,*]*res[2,*]
	endif

	return, FPU_fix(res)
end