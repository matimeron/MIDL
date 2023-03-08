Function Peak_count, x, y, ser, error = err,ret_range = ret, _extra = _e

;+
; NAME:
;		PEAK_COUNT
; VERSION:
;		4.9
; PURPOSE:
;       Summing up peak counts, with possible background subtraction.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_COUNT( X [, Y] [, SER] [, keywords])
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
;		statistical y-errors.
;
;		Note 1:	If only one input is provided then:
;				1)	If it is a vector, it is taken to be Y and X is generated
;					internally with a spacing of 1.  SER is taken as 0.
;				2)	If it is a [2,*] array, it is split into X and Y vectors.
;					SER is taken as 0.
;				3)	If it is a [3,*] array, it is split into X, Y and SER.
; KEYWORD PARAMETERS:
;	ERROR
;		Optional output, see below.
;	RET_RANGE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to PEAK_STRIP, as well as
;		plotting keywords.  Not to be used directly.
;
;		The following keywords are accepted by PEAK_STRIP:
;
;		/SORT	:	Forces sorting of data.
;		/ROI	:	Allows interactive region of interest selection.	| only
;		RANGE	:	Indices range for region of interest.				| 1 out
;		XRANGE	:	x-values range for region of interest.				| of
;		SIGFACTOR:	Range definition in terms of standard deviations.	| these
;		AMPFACTOR:	Range definition in terms of fraction of amplitude.	|
;		BACKGROUND:	Background value(s).							| only 1 out
;		BRANGE	:	Range of channels for background estimation.	| of these
;		/SHOW	:	Forces display of spectrum and selected part.
; OUTPUTS:
;		Returns the sum of counts in the peak.
; OPTIONAL OUTPUT PARAMETERS:
;	ERROR
;		Returns the calculated statistical error of the sum.
;	RET_RANGE
;		Returns the boundaries of the range used by PEAK_STRIP (in XRANGE
;		units) as a 2-element vector, [low_limit, high_limit].
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		At least 3 points are needed for a meaningful definition of a peak.
; PROCEDURE:
;		Calls PEAK_STRIP to affect background subtraction.  Also calls SPLIT_XY
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 20-MAY-2004 by Mati Meron.
;		Modified 10-JUN-2004 by Mati Meron.  Added keyword RET_RANGE.
;-

	on_error, 1

	sxy = Peak_strip(x,y,ser,ret=ret,_extra=_e)
	nxy = Split_xy(sxy,x=wx,y=wy,z=wser,inpz=erfl,/keep)
	if erfl then err = sqrt(total(wser^2)) else err = 0.

	return, total(wy)
end