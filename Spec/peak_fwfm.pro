Function Peak_fwfm, x, y, fraction = frc, range = ran, xrange = xran, $
	background = bck, brange = bran, sigfactor = sgf, ampfactor = amf, $
	relative = rel, show = sho, _extra = _e

;+
; NAME:
;		PEAK_FWHM
; VERSION:
;		4.3
; PURPOSE:
;       Finding the FWHM of a peak.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_FWHM ( [X,] Y [, keywords])
; INPUTS:
;	X
;		Numeric, Either a vector (scalar is considered to be a vector of
;		length 1) or an [2,*] array.
; OPTIONAL INPUT PARAMETERS:
;	Y
;		Numeric, acceptable only when X is a vector, in which case Y has to be
;		a vector of same length.
;
;		Note:	If only one input is provided then, if it is a [2,*] array, it
;				is split into X and Y vectors.  If it is a vector then it is
;				taken to be Y.  In such case X is generated internally as a
;				vector with spacing of 1, unless DELTA is explicitly provided.
; KEYWORD PARAMETERS:
;	RANGE
;		Specifies the range of indices of the region of interest.  Typically
;		provided as a two-element vector (like [30,80] to signify using
;		Y[30:80] as the ROI) but other forms are possible.  See MAKE_RANGE in
;		MIDL for a full listing of the possibilities.
;	XRANGE
;		Same as RANGE, only specified in X-units instead of indices.  Only
;		matters if X is provided and the spacing of X values differs from 1.
;
;		Note:	Either RANGE or XRANGE, but not both, may be given.  If neither
;				is given, RANGE defaults to the full range of indices present.
;	BACKGROUND
;		Provides background value(s) to be subtracted from the peak.  May be
;		given either as scalar or a vector of same length as the ROI specified
;		by RANGE.
;	BRANGE
;		The number of channels on each side of the peak to be used for
;		background estimate generation.  May be given as scalar (in which case
;		the number of channel specified is used on both sides of the peak) or
;		a 2-element vector (in which case the first value is used for the low
;		end and the second value for the high one.
;
;		Example:	Assume that RAN was given as [30,80] and BRAN as [3,5].
;					In this case the background on the low side of the peak
;					will be estimated as the average of the 3 values, Y[28],
;					Y[29] and Y[30] and the value will be attributed to the
;					midpoint, X[29].  Similarly on the high end the background
;					will be estimated as the average of the 5 values, Y[80],...
;					Y[84] and attributed to the midpoint, X[82].  The
;					background is then calculated using linear interpolation
;					between these two points.
;
;		Note!:		Either BACKGROUND or BRANGE but *not* both may be given.
;					If neither is provided, a zero background is assumed.
;	SIGFACTOR
;		Specifies an inclusion (in the range) cutoff in terms of number of
;		standard deviations on each side of the center. Must be a non-negative
;		value.
;	AMPFACTOR
;		Specifies an inclusion (in the range) cutoff in terms of fraction of
;		maximal amplitude.  Thus, if AMPFACTOR of 0.1 is specified, points
;		on both sides of the maximum up to (but not including) the *first*
;		points dropping below 0.1*Max are included.
;	/RELATIVE
;		Switch.  If set, the half-maximum level is calculated relative to the
;		minimal value present of the spectrum, else it is calculated relative
;		to zero.
;	/SHOW
;		Switch.  If set, plot of the original spectrum, the selected part and
;		the backround is shown.
;	_EXTRA
;		A formal keyword used to pass plotting keywords.  Not to be used
;		directly.
; OUTPUTS:
;		Returns the FWHM value for a peak.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		At least 3 points are needed for a meaningful definition of a peak.
; PROCEDURE:
;		Straightforward.  Calls PEAK_STRIP to affect background subtraction.
;		Also calls SPLIN_COEFFS, SPLINROOT and SPLIT_XY from MIDL.
; MODIFICATION HISTORY:
;		Created 30-OCT-2002 by Mati Meron.
;-

	on_error, 1

	sxy = Peak_strip(x,y,ran=ran,xran=xran,back=bck,bran=bran,$
					sig=sgf,amp=amf,show=sho,_extra=_e)
	nxy = Split_xy(sxy,x=wx,y=wy,/keep)

	if frc lt 0 or frc gt 1 then message, 'Fraction must be between 0 and 1''
	xmax = max(wx,min=xmin)
	ymax = max(wy,ploc,min=ymin)
	xpeak= wx[ploc]
	if keyword_set(rel) then frac = frc*(ymax-ymin) else frac = frc*ymax
	spc = Splin_coeffs(wx,wy-frac)
	lrt = Splinroot(spc,[xmin,xpeak],stat=lst)
	hrt = Splinroot(spc,[xpeak,xmax],stat=hst)
	lvl = where(lst,nl)
	hvl = where(hst,nh)
	if nl ne 0 and nh ne 0 then begin
		lrt = lrt[lvl]
		hrt = hrt[hvl]
		res = hrt[0] - lrt[nl-1]
	endif else res = xmax-xmin

	return, res
end