Pro LD_patch_z, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
	chan_range = chr, bin = bin, normval = nrm, firnorm = fnr, maxnorm = mnr, $
	force = frc,  title = tit, result = res, factors = mfac, _extra = _e

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
;		List(s) of linear detector scans, provided in any form that is
;		acceptable (individually) by LD_READ.  The lists will be combined into
;		a single list, internally
;
;		Note:	The ordering of S_0, ... S_N is arbitrary.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	CHAN_RANGE
;		The range of channels to use in the readout.  Default is full range
;		present in the data.
;	BIN
;		Specifies the bin size to be applied to each scan.  Default is 1, i.e.
;		no binning.  Any size provided is always rounded downward to the nearest
;		power of 2 (but the resulting binsize is never smaller than 1).
;		Note:	When both CHAN_RANGE and BIN are present, the CHAN_RANGE values
;				are taken as pre-binned values.
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
;	TITLE
;		Character constant or variable, the plot title.  Optional.
;	RESULT
;		Optional output, see below.
;	FACTORS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to imbedded
;		routines.  Not to be used directly.
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
;		The patching is performed by LD_PATCH_GEN, see there for details.
;		Calls SCAN_LC and SCAN_PATCH_SHOW, from SPEC and LD_PATCH_GEN from LD.
;		Also calls DEFAULT and ONE_OF, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-OCT-2007 by Mati Meron.
;		Modified 20-NOV-2007 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	res = LD_patch_gen(s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
		orientation= 'ver', chan_range= chr, bin= bin, force= frc, $
		nscan= nsc, order= sord, slist= slis, factors= mfac, _extra= _e)

	if n_elements(nrm) eq 1 then begin
		whi = One_of(fnr,mnr) > 0
		if whi then norx = max(res[1,*]) else norx = res[1,0]
		res = Scan_lc(res,coef=nrm/norx)
	endif

	dtit = fildat.name + '!CScan #: ' + strcompress(strjoin(string(slis),', '))
	tit = Default(tit,dtit)
	xtit = 'Q!dz!n'

	Scan_patch_show, res, title= tit, xtitle= xtit, /ylog, _extra= _e

	return
end