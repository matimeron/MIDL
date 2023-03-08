Function Peak_region, x, y, ser, sort = sor, background = bck, brange = bran, $
	sigfactor = sgf, ampfactor = amf, show = sho, _extra = _e

;+
; NAME:
;		PEAK_REGION
; VERSION:
;		4.9
; PURPOSE:
;       Finding an appropriate integration region for a peak.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_REGION ( [X,] Y [, keywords])
; INPUTS:
;	X
;		Numeric, A vector (scalar is considered to be a vector of length 1), an
;		[2,*] array or a [3,*] array.
; OPTIONAL INPUT PARAMETERS:
;	Y
;		Numeric, acceptable only when X is a vector, in which case Y has to be
;		a vector of same length.
;	SER
;		Numeric, same restrictions as for Y.  Taken as the values of the
;		statistical y-errors.  Not needed in PEAK_REGION, provided for
;		consistency only.
;
;		Note:	If only one input is provided then:
;				1)	If it is a vector, it is taken to be Y and X is generated
;					internally with a spacing of 1.  SER is taken as 0.
;				2)	If it is a [2,*] array, it is split into X and Y vectors.
;					SER is taken as 0.
;				3)	If it is a [3,*] array, it is split into X, Y and SER.
; KEYWORD PARAMETERS:
;	/SORT
;		Switch.  If set, data is sorted in ascending order of the x-values.
;	BACKGROUND
;		Provides background value(s) to be subtracted from the peak.  May be
;		given either as scalar or a vector of same length as the data.
;	BRANGE
;		The number of channels on each side of the peak to be used for
;		background estimate generation.  May be given as scalar (in which case
;		the number of channel specified is used on both sides of the peak) or
;		a 2-element vector (in which case the first value is used for the low
;		end and the second value for the high one.
;		Note that, since PEAK_REGION uses the whole data, BRANGE always
;		defaults to a single channel on each end.
;		Note!:		Either BACKGROUND or BRANGE but *not* both may be given.
;					If neither is provided, a zero background is assumed.
;
;	SIGFACTOR
;		Specifies an inclusion (in the range) cutoff in terms of number of
;		standard deviations on each side of the center. Must be a non-negative
;		value.
;	AMPFACTOR
;		Specifies an inclusion (in the range) cutoff in terms of fraction of
;		maximal amplitude.  Thus, if AMPFACTOR of 0.1 is specified, points
;		on both sides of the maximum up to (but not including) the *first*
;		points dropping below 0.1*Max are included.
;	/SHOW
;		Switch.  If set, plot of the original spectrum, and the selected part
;		is shown.
;	_EXTRA
;		A formal keyword used to pass plotting keywords.  Not to be used
;		directly.
; OUTPUTS:
;		Returns a vector of indices of the data elements within the range.
;		This vector can be used as RANGE in the other PEAK routines.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		At least 3 points are needed for a meaningful definition of a peak.
; PROCEDURE:
;		Straightforward.  Calls PEAK_CENT, PEAK_SHOW, PEAK_SIG and PEAK_STRIP.
;		Also calls DIF, ONE_OF, SORPURGE and SPLIT_XY from MIDL.
; MODIFICATION HISTORY:
;		Created 25-OCT-2002 by Mati Meron.
;-

	on_error, 1

	what = One_of(sgf,amf)
	if what eq -1 then message, 'Select by width or amplitude?'
	sxy = Peak_strip(x,y,ser,sort=sor,back=bck,bran=bran)
	nxy = Split_xy(sxy,x=wx,y=wy,/keep)

	if what eq 0 then begin
		if sgf lt 0 then message, 'Sig_factor must be non-negative'
		cent = Peak_cent(sxy)
		sig = Peak_sig(sxy)
		res = where(abs(wx - cent) le sgf*sig,nres)
		if nres eq 0 then dum = min(abs(wx-cent),res)
		res = [res]
	endif else begin
		if amf lt 0 or amf gt 1 then message, $
		'Amp_factor must be between 0 and 1'
		peak = max(wy,ploc)
		lres = where(wx le wx[ploc] and wy ge amf*peak, nl)
		hres = where(wx ge wx[ploc] and wy ge amf*peak, nh)
		ldif = Dif(lres,/forw,/clip)
		hdif = Dif(hres,/back,/clip)
		ldum = where(ldif gt 1,nldum)
		hdum = where(hdif gt 1,nhdum)
		if nldum gt 0 then lres = lres[ldum[nldum-1]+1:nl-1]
		if nhdum gt 0 then hres = hres[0:hdum[0]-1]
		res = [lres,hres]
		res = res[Sorpurge(res)]
	endelse

	if keyword_set(sho) then begin
		Peak_show, sxy, shift = dsh, _extra = _e
		Peak_show, sxy, ran = res, shift = dsh, thick= 2, col= !pcol.red, /over
	endif

	return, res
end