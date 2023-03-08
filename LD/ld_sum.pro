Pro LD_sum, sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, $
	orientation = ori, chan_range = chr, bin = bin, norm = nrm, $
	angles = ang, signed = sgn, average = ave, result = res, _extra = _e
;+
; NAME:
;		LD_SUM
; VERSION:
;		7.15
; PURPOSE:
;		Summing (or averaging) LD scans.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		LD_SUM, SL_0, ..., [,keywords]
; INPUTS:
;	SL_0, SL_1, SL_2, SL_3, SL_4, SL_5, SL_6, SL_7
;		List(s) of linear detector scans, provided in any form that is
;		acceptable (individually) by LD_READ.  The lists will be combined into
;		a single list, internally
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ORIENTATION
;		Character string input specifying LD orientation.  Two possible inputs,
;		"horizontal" and "vertical" (first letter suffices).  Default is
;		"vertical".
;	CHAN_RANGE
;		The range of channels to use in the readout.  Default is full range
;		present in the data.
;	BIN
;		Specifies the bin size to be applied to each scan.  Default is 1, i.e.
;		no binning.  Any size provided is always rounded downward to the nearest
;		power of 2 (but the resulting binsize is never smaller than 1).
;		Note:	When both CHAN_RANGE and BIN are present, the CHAN_RANGE values
;				are taken as pre-binned values.
;	/NORM
;		Specifies data normalization.  If NORM is set (with nonzero numeric
;		value) the data is normalized to MONC.  This can be overriden by
;		giving NORM as character value in which case this value specifies
;		that column to be used for normalization.
;	/ANGLES
;		Switch.  If set the data is read as a function of the angles DTHETA
;		(for horizontal) or BETA (for vertical), instead of Q_xy and Q_z.
;	/SIGNED
;		Switch.  If set and the orientation is "horizontal" and /ANGLES is not
;		set, then the Q_xy values are multiplied by SIGN(DTH).  Default is not
;		signed.
;	/AVERAGE
;		Switch.  If set, a weighted (by experimental errors) average is
;		performed instead of a sum.
;	/SHOW
;		Switch.  If set, the result is plotted to the screen (or sent to a
;		printer or PNG file, using SCAN_SHOW keywords.
;	RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  Not to be used directly.
; OUTPUTS:
;		Returns the result in the standard scan form, i.e. a [3,N_POINTS] array
;		where the first column contains the Q_z (or BETA) values for vertical
;		orientation, Q_XY (or DTH) for horizontal one, the second column
;		contains the summed (or averaged) data and the third the data errors.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the data being displayed to the screen, in the standard scan
;		form, i.e. a [3,N_POINTS] array where the first column contains the Q_z
;		(or BETA) values for vertical orientation, Q_XY (or DTH) for horizontal
;		one, the second column contains the summed (or averaged) data and the
;		third the data errors.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward summing or averaging.  Calls SCAN_LD_SUM and SCAN_SHOW.  Calls LIMG_COO_CONV.  Also
;		Calls DEFAULT, and STRMATCH_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 20-OCT-2007 by Mati Meron.
;		Modified 15-NOV-2007 by Mati Meron.  Internal streamlining.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	res = Scan_LD_sum(sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, lis=sls, $
		ori=ori,chan=chr,bin=bin,norm=nrm,ang=ang,sign=sgn,ave=ave,_extra=_e)

	xtits = ['Q!dxy!n','Dth','Q!dz!n','Beta']
	wori = abs(Strmatch_mm(Default(ori,'',/dtyp),['hor','ver'],1,/nosub))
	xtit = xtits[keyword_set(ang) + 2*wori]
	tit= fildat.name + '!CScan #:' + strcompress(strjoin(string(sls),', '))
	tem = !y.margin
	!y.margin = [4,4]
	Scan_show, res, xtit = xtit, ytit = 'Counts', tit = tit, _extra = _e
	!y.margin = tem

	return
end