Pro Patch_xy, snum, columns = col, normval = nrm, firnorm = fnr, maxnorm = mnr,$
	force = frc, tau = tau, dth = dth, title = tit, $
	result = res, factors = mfac, _extra= _e

;+
; NAME:
;		PATCH_XY
; VERSION:
;		7.15
; PURPOSE:
;		Patches together Q_XY (inplane) scans.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PATCH_XY, SNUM [,keywords]
; INPUTS:
;	SNUM
;		Scan list can be of any list form (i.e. scan numbers, not actual scans)
;		acceptable by SCAN_LIST_VER (see there).
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
;		The default values are [0,-1,-2]
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
;	/DTH
;		Switch.  If set, the first data column contains Dth (detector_theta)
;		values.  Default is Q_xy values.
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
;		Standard output is graphics only, a plot of inplane diffraction as a
;		function of Q_xy (or Dth if /DTH is set).  Additional outputs are
;		provided through the output parameters.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the patched data in the standard [3,*] form:
;
;		Column 0	:	Q_xy (or Dth if /DTH is set)
;		Column 1	:	Inplane intensity.
;		Column 2	:	Squared errors.
;	FACTOR
;		A vector of length N (the number of scans present) returning the
;		multiplicative factors of the matches.  The i-th value is the factor by
;		which scan #i is multiplied to match with scan #(i-1).  The 0-th value
;		is always 1.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The scans on the list must exist and, unless /FORCE is set, each scan
;		should have at least one-point overlap with some other scan.
; PROCEDURE:
;		Reads the scans, orders them and joins them together.  Calls QXY.
;		Calls SCAN_FIELD_READ, SCAN_LC, SCAN_PATCH_GEN and SCAN_PATCH_SHOW,
;		from SPEC.  Also calls DEFAULT, ONE_OF and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-AUG-2003 by Mati Meron.
;		Modified 25-NOV-2003 by Mati Meron.  Replaced keyword QOUT by DTH.
;		Added keywords FIRNORM and MAXNORM.  Also, added the capacity to input
;		scans in any order.
;		Modified 15-FEB-2004 by Mati Meron.  Added keywords OUTPUT, PNG and
;		CHANGE.
;		Modified 1-MAR-2004 by Mati Meron.  Added optional pileup correction.
;		Modified 25-OCT-2007 by Mati Meron.  Internal changes.
;		Modified 20-NOV-2007 by Mati Meron.  Removed the keywords OUTPUT and PNG
;		which are now provided through SCAN_PATCH_SHOW.  Enabled graphics file
;		saving in JPG and BMP formats (in addition to PNG).
;		Modified 15-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	del = 1e-3
	col = Default(col,[0,-1,-2],/dtyp)

	res = Scan_patch_gen(snum,col=col,forc=frc,tau=tau,nsc=l,slis=slis,$
		fact=mfac,_extra=_e)
	if n_elements(nrm) eq 1 then begin
		whi = One_of(fnr,mnr) > 0
		if whi then norx = max(res[1,*]) else norx = res[1,0]
		res = Scan_lc(res,coef=nrm/norx)
	endif

	lam = Scan_field_read(slis,'lambda',/const,cfl=lfl)
	if not lfl or lam eq 0 then message, 'lambda not constant!'
	angs = Scan_field_read(slis,'angs',/const,tol=del,cfl=cfl)
	if not cfl[0] then message, 'Alpha not constant!' $
	else if not cfl[1] then message, 'Beta not constant!'

	if not keyword_set(dth) then begin
		res[0,*] = Qxy(angs[0],angs[1],lam=lam[0],dth=res[0,*])
		xtit = 'Q!dxy!n'
	endif else xtit = 'dth'

	if n_elements(tit) eq 0 then begin
		tit = ''
		ll = 12 < l
		cur = 0l
		repeat begin
			if cur eq 0 then pre = 's#:	' else pre = '!c	'
			tit = tit + pre + $
			strjoin(strcompress(string(slis[cur:((cur+ll)<l)-1])),',')
			cur = cur + ll
		endrep until cur ge l
		tit = fildat.name + '!c' + tit
	endif

	qps = ((Wherinstruct('xlo',_e))[0] ge 0)
	Scan_patch_show, res, qpos = qps, tit= tit, xtit= xtit, _extra= _e

	return
end