Pro G_l3, scans, centers= cen, peak= pek, sort= sor, force_zero= fzr, _extra= _e

;+
; NAME:
;		G_L3
; VERSION:
;		8.07
; PURPOSE:
;		Calculating the G_L3 parameter for the surface spectrometer.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		G_L3, SCANS [, keywords]
; INPUTS:
;	SCANS
;		A list of scans to be used in the calculation.  Can be given either as
;		a numeric array, for example [43,46,47,48,49,55], or as a character
;		string (in which case range designation can be used).  The array above
;		represented as a string, will be '43,46-49,55'
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	CENTERS
;		Numeric array, a list of peak centers for the scans provided.  By
;		default the center values are read from the SPEC file.  The external
;		list, when given, replaces the file values.
;	/PEAK
;		Switch.  If set and if CENTERS is not provided, the values read from 
;		the SPEC file are those of PEAK_LOC, instead of the default CENTER.
;	/SORT
;		Switch.  If set, scans are sorted in ascending value of the angle
;		ALPHA.
;	/FORCE_ZERO
;		Switch.  If set, the linear fit is forced to pass through the origin.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to
;		SPEC_FILE_INFO.  Not to be used directly.
; OUTPUTS:
;		None other than screen printout of the fit parameters and a plot of the
;		fit.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the scans on the list must in fact exist.
; PROCEDURE:
;		Linear fit of measured center values to angles.  Calls SCAN_FIELD_READ,
;		SCAN_LIST_VER, SCAN_PAR_READ and SPEC_FILE_INFO from SPEC.  Also calls
;		ARREQ, LINFIT_MM, POLEVAL and TABULATE from MIDL.
; MODIFICATION HISTORY:
;		Created 1-JUL-2002 by Mati Meron.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 20-JUL-2011 by Mati Meron.  Added keyword PEAK.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	nlis = Scan_list_ver(scans,list=slis,flag=sfl,_extra=_e)
	if nlis gt 0 and sfl then begin
		Spec_file_info, _extra = _e
		check = Arreq([slis gt 0 and slis le fildat.nscan],replicate(1b,nlis))
		if fildat.status and check then begin
			dum = where(fildat.scan[slis].stat, ndum)
			if nlis ne ndum then message, 'Data is missing!'
			par = Scan_par_read(slis,['In_Rot','Out_H'])
			alp = reform(par[*,0])
			ohn = reform(par[*,1])
			if n_elements(cen) eq 0 then $
			if keyword_set(pek)then ohc = Scan_field_read(slis,'PEAK_LOC') $
			else ohc = Scan_field_read(slis,'CEN') $
			else if n_elements(cen) eq nlis then ohc = cen $
			else message, 'Wrong number of centers!'
			dum = where(alp eq 0, ndum)
			if ndum gt 0 then ohc[dum] = ohn[dum]

			if keyword_set(sor) then begin
				s = sort(alp)
				alp = alp[s]
				ohn = ohn[s]
				ohc = ohc[s]
				slis = slis[s]
			endif

			talp = tan(!dtor*alp)
			nmc = (ohn - ohc)
			print
			Tabulate, fix(slis), alp, ohn, ohc, nmc, tit = 'Old values', $
			head=['#','alpha','Oh Nom','Oh Cen','N - C']

			fzfl = keyword_set(fzr)
			ncof = Linfit_mm(-talp,ohc,ord=1-fzfl,fac=fzfl,err=nerr)
			cohn = Poleval(-talp,ncof)*(-talp)^fzfl
			cnmc = (cohn - ohc)
			print
			Tabulate, fix(slis), alp, cohn, ohc, cnmc, tit = 'New values', $
			head=['#','alpha','Cor. Oh Nom','Oh Cen','Cor. N - C']

			print
			print
			g_l = Scan_field_read(slis,'g_l',/const,cfl=cfl)
			if cfl[2] and cfl[3] then print, $
			form='("		old (g_l2 - g_l3)  = ",f9.3)', g_l[2] - g_l[3]
			print, form='("		new (g_l2 - g_l3)  = ",f9.3,"  (",f6.3,")")', $
			ncof[1-fzfl], nerr[1-fzfl]
			if not fzfl then print, $
			form='("		offset    		 = ",f9.3,"  (",f6.3,")")', $
			ncof[0], nerr[0]

			if (!d.flags and 256)/256 then begin
				if !d.window eq (-1) then wset
				wshow
			endif

			plot, alp, cohn - ohc, psym=-4
			oplot, alp, 0*alp, line=2
		endif else message, 'Bad or missing file!'
	endif else message, 'Scan numbers?'

	return
end