Function Peak_nei, x, y, ser, sort = sor, background = bck, brange = bran, $
	maximum= max, minimum= min, npoints= npo, show= sho, neran= ner, _extra= _e

;+
; NAME:
;		PEAK_NEI
; VERSION:
;		8.08
; PURPOSE:
;       Selecting neighborhood of a peak.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_NEI ( [X,] Y [, keywords])
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
;		statistical y-errors.  Not needed in PEAK_NEI, provided for
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
;	/MAXIMUM												| Only one of these
;		Switch.  If set, the maximum is selected as center.	| two may be set. If
;	/MINIMUM												| non is, MAXIMUM is
;		Switch.  If set, the maximum is selected as center.	| the default.
;	NPOINTS
;		Number of points required in the selected region, symetrically on both
;		sides of the peak (or dip).  The number given is rounded up to the 
;		nearest odd number, if needed.  Minimal (and default) value is 3.
;	/SHOW
;		Switch.  If set, plot of the original spectrum, and the selected part
;		is shown.
;	NERAN
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass plotting keywords.  Not to be used
;		directly.
; OUTPUTS:
;		Returns a vector of indices of the data elements within the range.
;		This vector can be used as RANGE in the other PEAK routines.
; OPTIONAL OUTPUT PARAMETERS:
;	NERAN
;		Returns the number of elements of the result (i.e. N_ELEMENTS(result)).
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		At least 3 points are needed for a meaningful definition of a peak.
; PROCEDURE:
;		Straightforward.  Calls PEAK_SHOW and PEAK_STRIP.
;		Also calls DEFAULT, MAKE_RANGE, ONE_OF and SPLIT_XY from MIDL.
; MODIFICATION HISTORY:
;		Created 25-AUG-2011 by Mati Meron.
;-

	on_error, 1

	sxy = Peak_strip(x,y,ser,sort=sor,back=bck,bran=bran)
	nxy = Split_xy(sxy,x=wx,y=wy,/keep)
	if nxy lt 3 then message, 'Not enough points, cannot proceed!'
	hpo = (Default(npo,3,/dtyp) < nxy)/2 > 1

	whi = One_of(max,min) > 0
	if whi then dum = min(wy,cen) else dum = max(wy,cen)
	lim = cen + [-hpo,hpo]
	if lim[0] < 0 then begin
		hpo = (hpo + lim[0]) > 1
		lim = cen + [-hpo,hpo]
		if lim[0] < 0 then begin
			cen = cen + 1
			lim = cen + [-hpo,hpo]
		endif
	endif
	if lim[1] gt nxy-1 then begin
		hpo = (hpo + (nxy-1) - lim[1]) > 1
		lim = cen + [-hpo,hpo]
		if lim[1] gt nxy-1 then begin
			cen = cen - 1
			lim = cen + [-hpo,hpo]
		endif
	endif

	res = Make_range(lim,neran=ner)
	if keyword_set(sho) then begin
		Peak_show, sxy, shift = dsh, _extra = _e
		Peak_show, sxy, ran = res, shift = dsh, thick= 2, col= !pcol.red, /over
	endif

	return, res
end