Function Peak_swid, x, y, ser, fwhm = fwhm, error = err, ret_range = ret, $
	_extra = _e

;+
; NAME:
;		PEAK_SWID
; VERSION:
;		7.09
; PURPOSE:
;       Estimating the width of a peak.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_SWID ( X [, Y] [, SER] [, keywords])
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
;		Note:	If only one input is provided then:
;				1)	If it is a vector, it is taken to be Y and X is generated
;					internally with a spacing of 1.  SER is taken as 0.
;				2)	If it is a [2,*] array, it is split into X and Y vectors.
;					SER is taken as 0.
;				3)	If it is a [3,*] array, it is split into X, Y and SER.
; KEYWORD PARAMETERS:
;	/FWHM
;		Switch.  If set, the FWHM (calculated as sqrt(2*log(2)/pi)*"full_width")
;		is returned.
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
;		Returns the "full width" of the peak, calculated as
;
;			(Integral(Y))^2/integral(Y^2)
;
;		For a gaussian this yields sigma*sqrt(4*pi).
;
;		If the keyword /FWHM is set, returns the full width at half magnitude,
;		calculated as sqrt(2*log(2)/pi)*"full_width".
; OPTIONAL OUTPUT PARAMETERS:
;	ERROR
;		Returns the calculated statistical error of the full-width or the FWHM.
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
;		Calls PEAK_STRIP to affect background subtraction and PEAK_STSUMS.
;		Calls FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JUL-2002 by Mati Meron.
;		Modified 10-SEP-2003 by Mati Meron.  Added statistical error handling.
;		Modified 15-APR-2004 by Mati Meron.  Internal processing changes.
;		Modified 10-JUN-2004 by Mati Meron.  Added keyword RET_RANGE.
;		Modified 5-SEP-2008 by Mati Meron.  Internal changes.
;-

	on_error, 1
	if keyword_set(fwhm) then mult = sqrt(2*alog(2)/!pi) else mult = 1.

	sxy = Peak_strip(x,y,ser,_extra=_e)
	nxy = Peak_stsums(sxy,r=r,y=siv,dx=rov,es=ses,erfl=erfl,/keep)
	if nxy gt 0 then begin
		res = mult*r/total(rov*siv^2)
		if erfl then begin
			tem = 2*res/r*(res/mult*siv - r)
			err = sqrt(total((tem*ses)^2))
		endif else err = 0
	endif else message, 'Bad or missing input!'

	return, FPU_FIX(res)
end