Function Peak_smooth, x, y, ser, width= wid, binco= bnc, savgol= svg, box= box,$
	ret_range = ret, kernel = ker, _extra = _e

;+
; NAME:
;		PEAK_SMOOTH
; VERSION:
;		7.09
; PURPOSE:
;       Smoothing a spectrum.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_SMOOTH ( X [, Y] [, SER] [, keywords])
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
;	WIDTH
;		Smoothing width.  Should be an odd integer, any other input is rounded
;		upwards to the nearest odd integer.  Default value is 1 (i.e. no
;		smoothing).
;		
;		Note:	Due to different kernel profiles, same width value results in
;				different "effective width".  Given width W, the effective
;				width is, approximately:
;				
;				 Binomial coefficients	-	sqrt(!pi*W)
;				 Savitzki-Golay			-	4/9*W
;				 Box					-	W
;	/BINCO
;		Switch.  Specifies a Binomial coefficients smoothing kernel.
;		This is also the default, absent any specification.
;	/SAVGOL
;		Switch.  Specifies Savitzky-Golay smoothing kernel.
;	/BOX
;		Switch.  Specifies Box smoothing kernel.
;	RET_RANGE
;		Optional output, see below.
;	KERNEL
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
;		Returns a smoothed version of the original spectrum, in the standard
;		format of a [2,*] or [3,*] (depending on input) array.  If the input
;		includes statistical errors, statistical errors for the smoothed
;		spectrum are included in the output.
; OPTIONAL OUTPUT PARAMETERS:
;	RET_RANGE
;		Returns the boundaries of the range used by PEAK_SMOOTH (in XRANGE
;		units) as a 2-element vector, [low_limit, high_limit].
;	KERNEL
;		Returns the smoothing kernel being used.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The number of data points cannot be smaller than the width used.
; PROCEDURE:
;		Uses Binomial coefficients (default), Savitzky-Golay or plain box
;		kernel for smoothing.  Assumes equally spaced x coordinates (for non
;		equally spaced ones see the PEAK_SGSMOOTH routine).  Calls PEAK_STRIP.
;		Calls BINCOEF, CALCTYPE, CAST, CONVOL_MM, DEFAULT, JOIN_XY, ONE_OF,
;		SORPURGE and SPLIT_XY, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAY-2008 by Mati Meron.
;		Modified 20-OCT-2008 by Mati Meron.  Internal changes.
;-

	on_error, 1

	sxy = Peak_strip(x,y,ser,ret=ret,_extra=_e)
	nxy = Split_xy(sxy,x=wx,y=wy,z=wser,inpz=erfl,/keep)
	if nxy gt 0 then begin
		m = Default(wid,1l,/dtyp)/2 > 0
		k = dindgen(2*m+1) - m

		s = Sorpurge(wx,net=nl)
		if nl ge 2*m+1 then begin
			wx = wx[s]
			wy = wy[s]
			if erfl then wser = wser[s]
			typ = Calctype(0.,sxy)
		endif else message, 'Data array too short for intended operation!'

		whi = One_of(bnc,svg,box) > 0
		case whi of
			0	:	ker = Bincoef(2d*m,long(k+m))/2d^(2*m)
			1	:	ker = 3d/(2*m-1)/(2*m+1)/(2*m+3)*(3*m^2 + 3*m - 1 - 5*k^2)
			2	:	ker = replicate(1d/(2*m+1),2*m+1)
		endcase
		smy = Convol_mm(wy,ker,/edge_trun)
		if erfl then smser = sqrt(Convol_mm(wser^2,ker^2,/edge_trun)>0)
		res = Join_xy(wx,smy,smser)
	endif else message, 'Bad or missing input!'

	return, Cast(res,typ,typ,/fix)
end