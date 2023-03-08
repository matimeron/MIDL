Pro CRL_check_batch,  low_drop= ldr, high_drop= hdr, peak= pea, $
	show = sho, quiet= qui, fwhm = fwhm, correct = cor, wait = wai, $
	list = lis, energy = ene, hslit = hsl, sigma = sig, error = err, _extra = _e

;+
; NAME:
;		CRL_CHECK_BATCH
; VERSION:
;		8.72
; PURPOSE:
;		Analyzes all Beam-Size scans in the current SPEC file.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = CRL_CHECK_BATCH ([ keywords])
; INPUTS:
; 		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	LOW_DROP
;		Integer scalar, specifying the number of data points to be dropped from
;		the beginning of all the scans.
;	HIGH_DROP
;		Same as LOW_DROP, for data points at the end of the scans.
;	/PEAK
;		Switch.  If set, the data is (numerically) differentiated and fitted to
;		Gaussian peaks.  The default operation is fitting to Gaussian edges,
;		i.e. Error functions.
;	/SHOW
;		Switch.  If set, the results of the fitting are shown on the screen.
;		These include plots of the fits and a table of the results, including
;		scan numbers, energy, width values and width errors.
;	/QUIET
;		Switch, set by defaul.  if explicitly set to 0 and /SHOW is set, all the
;		fit parameters for all the scans are printed to the screen.
;	/FWHM
;		Switch.  If set, FWHM values for widths are returned, instead of the
;		default Sigma values.
;	/CORRECT
;		Switch.  If set and /PEAK is set, the results is corrected for the
;		broadening caused by using finite differences as approximation for
;		differentiation.
;	/WAIT
;		Switch.  If set, the routine waits for user input ("any key") to advance
;		to the next scan.  Alternatively, if WAIT is given as negative value, 
;		the routine waits for the time specified by the absolute value of WAIT
;		before processing the next scan.
;
;		Note:	In the positive WAIT mode, pressing "Q" forces exit of the
;		routine.
;	LIST
;		Optional output, see below.
;	ENERGY
;		Optional output, see below.
;	HSLIT
;		Optional output, see below.
;	SIGMA
;		Optional output, see below.
;	ERROR
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.  Provides access to the CRL_FIND and TABULATE keywords.
; OUTPUTS:
;		Screen output when /SHOW is set, and additional output through optional
;		output keywords.
; OPTIONAL OUTPUT PARAMETERS:
; 	LIST
;		List of the CRL test scans (i.e. Beam-size scans) numbers present in
;		the currently loaded SPEC file, as a character array. If nothing is
;		found, returns a null string, or !NULL if /NULL is set.
; 	ENERGY
; 		Returns the energies of the analyzed scans.  Floating array.
; 	HSLIT
; 		Returns the horizontal slit (S0) sizes for the analyzed scans.  Floating
; 		array.
; 	SIGMA
; 		Returns a list of the Sigma values (or FWHM values if /FWHM is set) of
; 		the analyzed scans.  Floating array.
; 	ERROR
; 		Returns a list of the Sigma values (or FWHM values) statistical errors.
; 		Floating array.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Uses CRL_FIND to locate the scans, and CRL_CHECK to analyze them.
; 		Calls DEFAULT, STREQ, TABULATE and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JAN-2020 by Mati Meron.
;		Modifed 20-MAR-2020 by Mati Meron.  Added keyword HSLIT.
;-

	on_error, 1

	lis = CRL_find(ene=ene,hslit=hsl,count=cnt,/null,_extra=_e)
	dum = (Wherinstruct('new',_e))[0]
	if dum ge 0 then _e.(dum) = 0
	sig = (err = !null)
	shofl = keyword_set(sho)
	if shofl then wai = Default(wai,-1) else wai = 0
	if wai gt 0 then print, string([13b,9b,9b]) + $
	'Hit "Q" to exit, any other key to continue'

	if cnt gt 0 then begin
		sig = (err = fltarr(cnt))
		for i = 0, cnt-1 do begin
			sig[i] = CRL_check(lis[i],low=ldr,high=hdr, $
			peak=pea,show=shofl,fwhm=fwhm,correct=cor,error=serr,_extra=_e)
			err[i] = serr
			if wai gt 0 then begin
				dum = (dum = get_kbrd())
				if Streq(dum,'q',1) then break
			endif else if wai le 0 then wait, abs(wai)
		endfor
		if shofl then begin
			print
			if keyword_set(fwhm) $
			then head = ['Scan #', 'Energy', 'Slit', 'FWHM', 'FWHM_err'] $
			else head = ['Scan #', 'Energy', 'Slit', 'Sigma', 'Sigma_err']
			form=['a4','f7.3','f6.3','f6.3','f5.3']
			Tabulate, lis, ene, hsl, sig, err, form = form,head = head, $
			tit = 'Beam Size Data'
		endif
	endif else sig = (err = !null)

	return
end