Function CRL_check, snum, columns = col, low_drop = ldr, high_drop = hdr, $
	peak = pea, show = sho, quiet = qui, fwhm = fwhm, correct = cor, $
	error = err, energy = ene, hslit = hsl, _extra = _e

;+
; NAME:
;		CRL_CHECK
; VERSION:
;		8.72
; PURPOSE:
;		Analyzes a Beam-Size scan (typically for a CRL produced beam).
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = CRL_CHECK ( SNUM, [ keywords])
; INPUTS:
; 	SNUM
; 		Scan number.  Scalar, mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COLUMNS
;		Either an integer vector or a character vector with entries, specifying
;		the data columns to be used.  See details in the SCAN_READ routine.
;		The default values for COLUMNS are ['an_sam_h','monc','monp'].
;	LOW_DROP
;		Integer scalar, specifying the number of data points to be dropped from
;		the beginning of the scan.
;	HIGH_DROP
;		Same as LOW_DROP, for data points at the end of the scan.
;	/PEAK
;		Switch.  If set, the data is (numerically) differentiated and fitted to
;		a Gaussian peak.  The default operation is fitting to a Gaussian edge,
;		i.e. Error function.
;	/SHOW
;		Switch.  If set, the results of the fitting are shown on the screen.
;		These include a plot of the fit and values of the fit parameters.
;	/QUIET
;		Switch, set by default, to supress it one needs to set QUIET= 0.  When
;		set and /SHOW is set, the plot of the fitting is shown but printing
;		data to the screen is suppressed. 
;	/FWHM
;		Switch.  If set, FWHM value for the fit is returned, instead of the
;		default Sigma value.
;	/CORRECT
;		Switch.  If set and /PEAK is set, the result is corrected for the
;		broadening caused by using finite differences as approximation for
;		differentiation.
;	ERROR
;		Optional output, see below.
;	ENERGY
;		Optional output, see below.
;	HSLIT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.  Provides access to the SPEC_FILE_INFO and PEAK_FIT
;		keywords.
; OUTPUTS:
;		Returns the Sigma value (or FWHM value if /FWHM is set) of the analyzed
;		data.
; OPTIONAL OUTPUT PARAMETERS:
;	ERROR
;		Returns the calculated error of the output.
; 	ENERGY
; 		Returns the energy for the analyzed scan.
; 	HSLIT
; 		Returns the horizontal slit (S0) size for the analyzed scan.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.  However,if the data doesn't represent an edge scan, nonsensical
;		results may be returned.
; PROCEDURE:
; 		Analyzes the data using the PEAK_FIT function from BLIN, either in the
; 		/EDGE mode or in the standard mode if /PEAK is set.  Calls SCAN_DER,
; 		SCAN_PAR_READ, SCAN_READ, SCAN_SCALE and SPEC_FILE_INFO, from SPEC.
; 		Also calls DEFAULT, FLTROUND, ISNUM, SDEP and STRPARSE_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JAN-2020 by Mati Meron.
;		Modifed 20-MAR-2020 by Mati Meron.  Added keyword HSLIT.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop

	on_error, 1

	Spec_file_info, _extra = _e

	dcol = ['an_sam_h','monc','monp']
	wcol = Default(col,dcol)
	mult = 1e3
	if keyword_set(fwhm) then mult = mult*sqrt(alog(256))

	dat = Scan_read(snum,col=wcol)
	if Isnum(ldr,/int) then dat = dat[*,(ldr>0):*]
	if Isnum(hdr,/int) then dat = dat[*,0:-1-(hdr>0)]

	shofl = Default(sho,0)
	if shofl then quifl = Default(qui,1)
	if keyword_set(pea) then begin
		dat = Scan_scale(Scan_der(dat),-1)
		tit = 'Peak fit!c'
		edg = 0
	endif else begin
		tit = 'Edge fit!c'
		edg = 1
	endelse

	dum = Strparse_mm(fildat.name,Sdep(/ds),lis)
	tit = tit + strjoin(lis[-2:-1],Sdep(/ds)) + $
	' ; Scan # ' + string(snum,form='(i0)')
	res = Peak_fit(dat,edge=edg,back=2,error=err,sho=shofl,qui=quifl,$
	xtit='height',ymar=[4,4],charsiz=1.5,charthi=1.5,tit=tit,den=4,_extra=_e)

	res = res[5]
	err = err[5]

	if keyword_set(cor) and keyword_set(pea) then begin
		xvals = dat[0,*]
		dx = (max(xvals,min=min) - min)/(n_elements(xvals) - 1)
		res = sqrt(res^2 - dx^2/12)
	endif

	res = mult*res
	if err le 1e30 then err = mult*err else err = !values.f_infinity
	if arg_present(ene) then ene = (Scan_par_read(snum,'en'))[0]
	if arg_present(hsl) then hsl = $
	Fltround(total((fildat.scan[snum].sl_val[*,0])[2:3]),dec=2)

	return, res
end