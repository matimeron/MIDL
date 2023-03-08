Pro Patch_y, snum0, snum1, snum2, columns = col, normval = nrm, $
	force = frc, tau = tau, title = tit, result= res, factors= mfac, _extra= _e

;+
; NAME:
;		PATCH_Y
; VERSION:
;		7.15
; PURPOSE:
;		Patches together Q_Y (rocking) scans.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PATCH_Y, SNUM0 [, SNUM1 [, SNUM2]] [,keywords]
; INPUTS:
;	SNUM0
;		Scan list can be of any list form (i.e. scan numbers, not actual scans)
;		acceptable by SCAN_LIST_VER (see there).
; OPTIONAL INPUT PARAMETERS:
;	SNUM1, SNUM2
;		Same as SNUM0.
; KEYWORD PARAMETERS:
;	COLUMNS
;		An integer vector with 2 or 3 entries, specifying which of the data
;		columns present in the scan should be used.  The first two entries are
;		the numbers of the X and Y columns (either counting from 0 up, from
;		left, or from -1 down, from right).  If a third entry is provided, it
;		is used as the number of a normalization column.
;
;		The default values are [1,-1,-2]
;	NORMVAL
;		Normalization value, the value of the highest point on the intensity
;		curve.  No defaults.
;	/FORCE
;		Switch.  If set, forces junction even when there is no overlap between
;		some scans.  This involves extrapolation which greatly increases
;		uncertainties, so it should be used with care.
;	TAU
;		The time constant for the calculation of pileup correction.  If zero or
;		not given, no correction is performed.
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
;		Standard output is graphics only, a plot of "rocking intensity" as a
;		function of Qy .  Additional outputs are provided through the output
;		parameters.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the patched data in the standard [3,*] form:
;
;		Column 0	:	Q_y
;		Column 1	:	Intensity.
;		Column 2	:	Squared errors.
;	FACTOR
;		An array of size (N,S) where N is the number of scans per group and S
;		is the number of scan groups (3 at most).  Each row of FACTOR is a
;		vector of length N (the number of scans present) returning the
;		multiplicative factors of the matches for the corresponding group.  The
;		 i-th value is the factor by which scan #i is multiplied relative to
;		scan #0.  The 0-th value in each column is always 1.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The scans on the list must exist and, unless /FORCE is set, each scan
;		should have at least one-point overlap with some other scan.
; PROCEDURE:
;		Reads the scans, and joins them together.  Calls SCAN_CLEAN, SCAN_LC,
;		SCAN_PATCH_GEN and SCAN_PATCH_SHOW, from SPEC.  Also calls DEFAULT and
;		WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-AUG-2003 by Mati Meron.
;		Modified 15-FEB-2004 by Mati Meron.  Added keywords OUTPUT, PNG and
;		CHANGE.
;		Modified 1-MAR-2004 by Mati Meron.  Added optional pileup correction.
;		Modified 15-JUN-2004 by Mati Meron.  Internal changes only.
;		Modified 25-OCT-2007 by Mati Meron.  Internal changes only.
;		Modified 20-NOV-2007 by Mati Meron.  Removed the keywords OUTPUT and PNG
;		which are now provided through SCAN_PATCH_SHOW.  Enabled graphics file
;		saving in JPG and BMP formats (in addition to PNG).
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	col = Default(col,[1,-1,-2],/dtyp)

	np = n_params()-1
	if np ge 0 then begin
		res0 = Scan_patch_gen(snum0, /sylog, $
		col=col,forc=frc,tau=tau,nsc=n0,sli=s0,fac=f0,_extra=_e)
		tat = strcompress('s#: [' + strjoin(string(s0),',') +']')
		dum = (Wherinstruct('new',_e))[0]
		if dum ge 0 then _e.(dum) = 0
		if np ge 1 then begin
			res1 = Scan_patch_gen(snum1, /sylog, $
			col=col,forc=frc,tau=tau,nsc=n1,sli=s1,fac=f1,_extra=_e)
			if n1 ne n0 then message, 'Scan numbers mismatch!'
			tat = tat + '!c' + strcompress('b#: ['+strjoin(string(s1),',')+']')
			if np ge 2 then begin
				if np eq 2 then begin
					res2 = Scan_patch_gen(snum2, /sylog, $
					col=col,forc=frc,tau=tau,nsc=n2,sli=s2,fac=f2,_extra=_e)
					if n2 ne n1 then message, 'Scan numbers mismatch!'
					tat = tat + strcompress(' , ['+strjoin(string(s2),',')+']')
					res = Scan_clean(res0,res1,res2,/inter)
					mfac = [[f0],[f1],[f2]]
				endif else message, 'Too many inputs!'
			endif else begin
				res = Scan_clean(res0,res1,/inter)
				mfac = [[f0],[f1]]
			endelse
		endif else begin
			res = res0
			mfac = [f0]
		endelse
		tit = Default(tit,fildat.name + '!c' + tat)
		xtit = 'Q!dy!n'
	endif else message, 'Patch what?'

	if n_elements(nrm) eq 1 then res = Scan_lc(res,coef=nrm/max(res[1,*]))
	Scan_patch_show, res, tit= tit, xtit= xtit, ymar= [4,6], _extra= _e

	return
end