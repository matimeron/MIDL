Pro Patch_z, s_0, s_1,  s_2,  s_3,  s_4,  s_5,  s_6,  s_7, $
			s_8, s_9, s_10, s_11, s_12, s_13, s_14, s_15, $
			s_16, s_17, s_18, s_19, s_20, s_21, s_22, s_23, $
			s_24, s_25, s_26, s_27, s_28, s_29, s_30, s_31, $
	columns = col, signal = sig, normval = nrm, firnorm = fnr, maxnorm = mnr, $
	force = frc, tau = tau, checkfac = chf, title = tit, $
	result = res, factors = mfac, _extra= _e

;+
; NAME:
;		PATCH_Z
; VERSION:
;		7.15
; PURPOSE:
;		Patches together Q_Z (reflectivity) scans.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PATCH_Z, S_0 [, S_1, ....] [,keywords]
; INPUTS:
;	S_0, S_1, ....
;		Scans or scan lists.  Maximal currently acceptable number of items is
;		32, but it can be increased if needed.  Each S_i can be of any form
;		acceptable by SCAN_LIST_VER (see there).  Note, however, the following:
;
;		1)	If S_i translates to a single scan number, it is used as is.
;		2)	If S_i translates to a group of scan numbers (for example, '8-10')
;			then it is assumed that one of the scans in the group is the signal
;			with the rest being background.  In such case, background
;			subtraction is performed and the group yields a single (background
;			subtracted) scan.
;		3)  If S_i resolves to a "signed" group of scan numbers, i.e. a group
;			of scan numbers some of which are preceded by minus sign, then the
;			"positive scans" are taken as signal and the negative as background.
;			Again, the group yields a single (background subtracted) scan.
;
;		Regarding which of the scans is taken as signal (in the absence of
;		plus-minus signs), see keyword SIGNAL.
;
;		Note:	The ordering of S_0, ... S_N is arbitrary.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COLUMNS
;		An integer vector with 2 or 3 entries, specifying which of the data
;		columns present in the scan should be used.  The first two entries are
;		the numbers of the X and Y columns (either counting from 0 up, from
;		left, or from -1 down, from right).  If a third entry is provided, it
;		is used as the number of a normalization column.
;
;		The default values are [2,-1,-2]
;	SIGNAL
;		Integer, specifies the number (counting from 0 up) of the "signal" scan
;		within scan groups.  The default value is 1, which means the second scan
;		(the middle one in a 3-scan group).  SIGNAL is ignored for "signed
;		groups".
;	NORMVAL
;		Normalization value for the curve.  If not supplied, no normalization
;		is performed.
;	/FIRNORM												|	At most one of
;		Switch.  If set *and* NORMVAL is given, the 		|	these keywords
;		normalization sets the first Y value to NORMVAL.	|	may be set.  If
;	/MAXNORM												|	none is, the
;		Switch.  If set *and* NORMVAL is given, the 		|	default is
;		normalization sets the maximal Y value to NORMVAL.	|	FIRNORM.
;	/FORCE
;		Switch.  If set, forces junction even when there is no overlap between
;		some scans.  This involves extrapolation which greatly increases
;		uncertainties, so it should be used with care.
;	TAU
;		The time constant for the calculation of pileup correction.  If zero or
;		not given, no correction is performed.
;	/CHECKFAC
;		Switch.  If set, PATCH_Z displays (in addition to the standard plot) a
;		ylog plot of 1/FACTOR (see below) versus then number of absorbers used.
;		Plot should appear linear, nonlinearity may indicate signal loss.
;	TITLE
;		Character constant or variable, the plot title.  Optional.
;	RESULT
;		Optional output, see below.
;	FACTORS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to
;		SPEC_FILE_INFO and SCAN_SHOW.  Not to be used directly.
;
;		Note:	The keyword /OUTPUT will be passed to SCAN_SHOW and generate
;				printer output.  The keywords /PNG, /JPG, /BMP will be passed
;				to WIMG_MM.
; OUTPUTS:
;		Standard output is graphics only, a plot of reflectivity as a function
;		of Q_z.  Additional outputs are provided through the output parameters.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the patched data in the standard [3,*] form:
;
;		Column 0	:	QZ
;		Column 1	:	Reflectivity.
;		Column 2	:	Squared errors.
;	FACTOR
;		A vector of length N (the number of scans present) returning the
;		multiplicative factors of the matches.  The i-th value is the factor by
;		which scans #i is multiplied relative to scan #0.  The 0-th value is
;		always 1.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the scans on the list must in fact exist.
; PROCEDURE:
;		Reads the scans, subtracts backgrounds as needed, and joins them
;		together.  Calls SCAN_CLEAN, SCAN_FIELD_READ, SCAN_JOIN, SCAN_LC,
;		SCAN_LIST_VER, SCAN_ORDER, SCAN_PATCH_SHOW and SPEC_FILE_INFO, from
;		SPEC.  Also calls ARREQ, DEFAULT and ONE_OF, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-AUG-2003 by Mati Meron.
;		Modified 1-DEC-2003 by Mati Meron.  Extensive internal changes.
;		Modified 15-FEB-2004 by Mati Meron.  Added keywords OUTPUT, PNG and
;		CHANGE.
;		Modified 1-MAR-2004 by Mati Meron.  Added optional pileup correction.
;		Modified 20-APR-2004 by Mati Meron.  Data format changes.
;       Modified 16-JUL-2005 by Mark Kittisopikul.  Expanded the number of
;       parameters to 32 from 16.
;		Modified 7-MAR-2005 by Stephen Danauskas to  allow for a user specified
;		title.
;		Modified 20-NOV-2007 by Mati Meron.  Removed the keywords OUTPUT and PNG
;		which are now provided through SCAN_PATCH_SHOW.  Enabled graphics file
;		saving in JPG and BMP formats (in addition to PNG).
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	snams =  strcompress('s_' + sindgen(32),/remove)

	col = Default(col,[2,-1,-2],/dtyp)
	sig = Default(sig,1,/dtyp)
	n = n_params()

	if n gt 0 then begin
		Spec_file_info, _extra = _e
		rlis = lonarr(n)
		for i = 0l, n-1 do begin
			idum = execute('l = Scan_list_ver(' + snams[i] + ',lis=lis)')
			if l gt 0 then begin
				pos = where(lis gt 0) > 0
				rlis[i] = ((lis[pos])[Scan_order(lis[pos],col=col)])[0]
			endif else message, 'Input errors!'
		endfor
		j = Scan_order(rlis,col=col)
		tarr = strarr(n)
		mfac = replicate(1.,n)
		for i = 0l, n-1 do begin
			idum = execute('l = Scan_list_ver(' + snams[j[i]] + ',lis=lis)')
			if l gt 1 then begin
				if Arreq(lis,abs(lis)) or Arreq(-lis,abs(lis)) then begin
					sml = sig mod l
					lis = -abs(lis)
					lis[sml] = -lis[sml]
				endif
			endif else lis = abs(lis)
			tarr[i] = strjoin(string(lis),',')
			if l gt 1 then tarr[i] = ' [' + tarr[i] + ']'
			next = Scan_clean(lis,col=col,/inter,tau=tau)
			if i gt 0 then begin
				res = Scan_join(res,next,force=frc,fact=fac,_extra=_e)
				mfac[i] = fac[1]
			endif else res = next
		endfor
		if n_elements(nrm) eq 1 then begin
			whi = One_of(fnr,mnr) > 0
			if whi then norx = max(res[1,*]) else norx = res[1,0]
			res = Scan_lc(res,coef=nrm/norx)
		endif
	endif else message, 'Patch what?!'

	if n_elements(tit) eq 0 then begin
		tarr = strcompress(tarr)
		tlen = strlen(tarr)
		llen = 64
		clen = 3 + tlen[0]
		tit = 's#:' + tarr[0]
		for i = 1l, n-1 do begin
			if clen + tlen[i] le llen then begin
				tit = tit + ',' + tarr[i]
				clen = clen + tlen[i] + 1
			endif else begin
				tit = tit + '!c   ' + tarr[i]
				clen = tlen[i] + 3
			endelse
		endfor
		tit = fildat.name + '!c' + tit
	endif
	xtit = 'Q!dz!n'

	Scan_patch_show, res, /ylog, tit= tit, xtit= xtit, ymar= [4,6], _extra= _e

	if keyword_set(chf) then begin
		window, 1
		plot, Scan_field_read(rlis,'nabso'), 1/mfac, /ylog, psym = -6, $
		xtit = '# of absorbers', ytit = '1/factor'
		wset, 0
	endif

	return
end