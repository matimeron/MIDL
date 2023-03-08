Function Peak_strip, x, y, ser, sort = sor, staterr = ste, $
	roi = roi, range = ran, xrange = xran, sigfactor = sgf, ampfactor = amf, $
	augment = aug, background = bck, brange = bran, positive = pos, $
	ret_range = ret, show = sho, scol = scl, _extra = _e

;+
; NAME:
;		PEAK_STRIP
; VERSION:
;		8.08
; PURPOSE:
;       Stripping background off peak.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_STRIP ( X [, Y] [, SER] [, keywords])
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
;	/SORT
;		Switch.  If set, data is sorted in ascending order of the x-values.
;	/STATERR
;		Switch.  If set and if the data contains no statistical errors, said
;		errors are internally generated, as SQRT(Y).
;	/ROI
;		Switch.  If set, a region of interest is determined interactively,
;		using the mouse.  The keyword /PICK_RANGE of PEAK_ROI may be used here
;		to display a limited area around the region of interest.
;	RANGE
;		Specifies a range of indices for the region of interest.  Typically
;		provided as a two-element vector (like [30,80] to signify using
;		Y[30:80] as the ROI) but other forms are possible.  See RANGE_PROC and
;		MAKE_RANGE in MIDL for a full listing of the possibilities.
;	XRANGE
;		Same as RANGE, only specified in X-units instead of indices.  Only
;		matters if X is provided and the spacing of X values differs from 1.
;	SIGFACTOR
;		Specifies an inclusion (in the range) cutoff in terms of number of
;		standard deviations on each side of the center. Must be a non-negative
;		value.
;	AMPFACTOR
;		Specifies an inclusion (in the range) cutoff in terms of fraction of
;		maximal amplitude.  Thus, if AMPFACTOR of 0.1 is specified, points
;		on both sides of the maximum up to (but not including) the *first*
;		points dropping below 0.1*Max are included.
;
;		Note!:		Only one from {ROI, RANGE, XRANGE, SIGFACTOR, AMPFACTOR}
;					may be given.  If none is provided, the working range
;					defaults to the full range of indices present.
;	/AUGMENT
;		Switch.  If set, and if the aplication of the selection criteria above
;		results in the selected range having less than 3 points, it'll be 
;		replaced by a 3-point interval around the data peak.
;	BACKGROUND
;		Provides background value(s) to be subtracted from the peak.  May be
;		given either as scalar or a vector of same length as the ROI specified
;		by one of the range keywords above.
;	BRANGE
;		The number of channels on each side of the peak to be used for
;		background estimate generation.  May be given as scalar (in which case
;		the number of channels specified is used on both sides of the peak) or
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
;	/POSITIVE
;		Switch.  If set and the date, after background correction, is wholly
;		or partially negative, it is offset upwards to become non-negative.
;	RET_RANGE
;		Optional output, see below.
;	/SHOW
;		Switch.  If set, plot of the original spectrum, the selected part and
;		the background is shown.
;	SCOL
;		A true color value to be used in plotting the selected part.  Optional.
;	_EXTRA
;		A formal keyword used to pass plotting keywords.  Not to be used
;		directly.
; OUTPUTS:
;		Returns a [2,*] or [3,*] array where the first column contains the
;		X-values and the second the Y-values (with background subtracted) of
;		the peak.  The third column, which is provided if the input data
;		includes SER, contains the SER values.
; OPTIONAL OUTPUT PARAMETERS:
;	RET_RANGE
;		Returns the boundaries of the range used by PEAK_STRIP (in XRANGE units)
;		as a 2-element vector, [low_limit, high_limit].
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		At least 3 points are needed for a meaningful definition of a peak.
; PROCEDURE:
;		Straightforward.  Calls PEAK_NEI, PEAK_REGION, PEAK_ROI and PEAK_SHOW.
;		Also calls ARREQ, DEFAULT, JOIN_XY, MAKE_RANGE, ONE_OF, RANGE_PROC and
;		SPLIT_XY from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JUL-2002 by Mati Meron.
;		Modified 25-OCT-2002 by Mati Meron.  Added keyword /SHOW.
;		Modified 30-OCT-2002 by Mati Meron.  Added keywords SIGFACTOR,
;		AMPFACTOR and _EXTRA.
;		Rewritten 10-SEP-2003 by Mati Meron.  Internal modifications only.
;		Modified 20-MAY-2004 by Mati Meron.  Added keywords STATERR and ROI.
;		Modified 10-JUN-2004 by Mati Meron.  Added keyword RET_RANGE.
;		Modified 25-JUN-2004 by Mati Meron.  Added keyword POSITIVE.
;		Modified 10-NOV-2006 by Mati Meron.  Internal changes only.
;		Modified 5-JUN-2009 by Mati Meron.  Internal changes only.
;		Modified 25-AUG-2011 by Mati Meron.  Added keyword AUGMENT.
;-

	on_error, 1

	nxy = Split_xy(x,y,ser,x_ret=wx,y_ret=wy,z_ret=wser,inpz=erfl,/keep)
	if keyword_set(sor) then begin
		s = sort(wx)
		wx = wx[s]
		wy = wy[s]
		if erfl then wser = wser[s]
	endif
	if not erfl and keyword_set(ste) then begin
		wser = sqrt(wy)
		erfl = 1b
	endif
	res = Join_xy(wx,wy,wser)

	wha = One_of(roi,ran,xran,sgf,amf)
	if wha gt 2 then ran = Peak_region(res,back=bck,bran=bran,sig=sgf,amp=amf) $
	else if wha eq 0 then xran = Peak_ROI(res,_extra=_e)

	if nxy ge 3 then begin
		case One_of(ran,xran) of
			-1	:	wran = [0l,nxy-1]
			0	:	wran = Range_proc(ran)
			1	:	begin
						xran = [min(xran),max(xran)]
						wran = where(wx ge xran[0] and wx le xran[1],nran)
						if nran gt 0 then wran = wran[[0,nran-1]] $
						else message, 'Impossible range!'
					end
		endcase

		wran = Make_range(0>wran<(nxy-1),/nozero,neran=nran)
		if nran lt 3 then begin
			if keyword_set(aug) then begin
				message, 'Not enough points, augmenting', /cont
				wran = Peak_nei(res,neran=nran)
			endif else message, 'Not enough points', /cont
		endif
		ret = wx[[min(wran),max(wran)]]

		case One_of(bck,bran) of
			-1	:	wbck = (dwbsq = 0)
			0	:	begin
						wbck = bck
						nbck = n_elements(bck)
						if nbck eq 1 then wbck = replicate(wbck,nran) else $
						if nbck ne nran then message,'Background size mismatch!'
						dwbsq = 0
					end
			1	:	begin
						wbran = bran > 1l
						if n_elements(wbran) eq 1 then wbran = [wbran,wbran]
						lran = Make_range((min(wran) + [1-wbran[0],0])>0)
						hran = Make_range((max(wran) + [0,wbran[1]-1])<(nxy-1))
						lx = (min(lran,max=max) + max)/2.
						hx = (max(hran,min=min) + min)/2.
						ly = total(wy[lran])/n_elements(lran)
						hy = total(wy[hran])/n_elements(hran)
						wbck = (ly*(hx-wran) + hy*(wran-lx))/(hx - lx)
						if erfl then begin
							dlysq = total(wser[lran]^2)/(n_elements(lran))^2
							dhysq = total(wser[hran]^2)/(n_elements(hran))^2
							dwbsq = $
							(dlysq*(hx-wran)^2 + dhysq*(wran-lx)^2)/(hx - lx)^2
						endif
					end
		endcase
	endif else message, 'Bad or missing input!'

	if keyword_set(sho) then begin
		scl = Default(scl,!pcol.red)
		Peak_show, res, _extra = _e
		Peak_show, res, ran= wran, thick= 2, col= scl, /over
		if not Arreq(wbck,0) then $
		oplot, wx[wran], [wbck], thick = 2, line = 2, col = scl
	endif

	res = res[*,[wran]]
	res[1,*] = res[1,*] - wbck
	if erfl then res[2,*] = sqrt(res[2,*]^2 + dwbsq)
	if keyword_set(pos) then begin
		dmin = min(res[1,*],lmin)
		if dmin lt 0 then begin
			res[1,*] = res[1,*] - dmin
			if erfl then res[2,*] = sqrt(res[2,*]^2 + res[2,lmin]^2)
		endif
	endif
	if nran eq 1 then res = reform(res,n_elements(res),1)

	return, res
end