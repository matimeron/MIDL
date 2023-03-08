Function Peak_fwhm, x, y, ser, relative = rel, quiet = qui, $
	error = err, status = sta, ret_range = ret, _extra = _e

;+
; NAME:
;		PEAK_FWHM
; VERSION:
;		7.09
; PURPOSE:
;       Finding the FWHM of a peak.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_FWHM ( X [, Y] [, SER] [, keywords])
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
;	/RELATIVE
;		Switch.  If set, the half-maximum level is calculated relative to the
;		minimal value present of the spectrum, else it is calculated relative
;		to zero.
;	/QUIET
;		Switch.  If set, warning messages for suspect/invalid results are
;		supressed.
;	ERROR
;		Optional output, see below.
;	STATUS
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
;		Returns the FWHM value for a peak.
; OPTIONAL OUTPUT PARAMETERS:
;	ERROR
;		Currently returns a vector of zeroes, provided only for consistency
;		with other PEAK routines.
;	STATUS
;		Returns 1 if a valid result is found, 0 otherwise.
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
;		Straightforward.  Calls PEAK_STRIP to affect background subtraction.
;		Also calls SPLIN_COEFFS, SPLINROOT, SPLIT_XY and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 30-OCT-2002 by Mati Meron.
;		Modified 10-JUN-2004 by Mati Meron.  Added keyword RET_RANGE.
;		Modified 10-DEC-2008 by Mati Meron.  Added keyword STATUS and internal
;		changes.
;		Modified 15-MAR-2009 by Mati Meron.  Added keyword QUIET and internal
;		changes.
;-

	on_error, 1

	eps = 4*Toler()
	sxy = Peak_strip(x,y,ser,ret=ret,_extra=_e)
	nxy = Split_xy(sxy,x=wx,y=wy,/keep)

	xmax = max(wx,min=xmin)
	ymax = max(wy,ploc,min=ymin)
	xpeak= wx[ploc]
	if keyword_set(rel) then half = 0.5*(ymax+ymin) else half = 0.5*ymax
	spc = Splin_coeffs(wx,wy-half)

	if xmin lt xpeak then begin
		loc = where(wx lt xpeak and wy lt half, lind)
		if lind gt 0 then begin
			xmin = wx[loc[lind-1]]
			lrt = Splinroot(spc,[xmin,xpeak],eps,stat=lst,/rel)
			if lst then xmin = lrt
		endif else lst = 0
	endif else lst = 0
	if xmax gt xpeak then begin
		loc = where(wx gt xpeak and wy lt half, lind)
		if lind gt 0 then begin
			xmax = wx[loc[0]]
			hrt = Splinroot(spc,[xpeak,xmax],eps,stat=hst,/rel)
			if hst then xmax = hrt
		endif else hst = 0
	endif else hst = 0

	res = xmax - xmin
	err = 0.*res
	sta = lst and hst
	if not (sta or keyword_set(qui)) $
	then message, 'Warning, result may be invalid!', /con

	return, res
end