Function Peak_moment, x, y, ser, order = ord, origin = ori, internal = int, $
	error = err

;+
; NAME:
;		PEAK_MOMENT
; VERSION:
;		4.9
; PURPOSE:
;       Calculating data moments.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_MOMENT( [X,] Y [, SER] [, keywords])
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
;	ORDER
;		Numeric scalar, the order of the moment to be calculated.
;	/ORIGIN
;		Switch.  If set, the order is calculated about the origin.  The default
;		is moment about the mean.
;	/INTERNAL
;		Switch.  Specifies "internal calculation", where only the moment and not
;		the moment's error is calculated.
;	ERROR
;		Optional output, see below.
; OUTPUTS:
;		Returns the calculated moment value.
; OPTIONAL OUTPUT PARAMETERS:
;	ERROR
;		Returns the statistical error of the calculated moment.  The calculation
;		requires the values of SER (see above) being provided, directly or
;		indirectly.  If these values are missing, the returned error is zero.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls itself recursively, when needed.  Also calls
;		PEAK_STSUMS.
; MODIFICATION HISTORY:
;		Created 10-SEP-2003 by Mati Meron.
;		Modified 15-APR-2004 by Mati Meron.  Internal processing changes.
;-

	on_error, 1

	nor = round(ord)
	nxy = Peak_stsums(x,y,ser,x=wx,y=siv,dx=rov,es=ses,erfl=erfl,/keep)
	if nxy gt 0 and nor ne 0 then begin
		erfl = erfl and (not keyword_set(int))
		cnfl = not keyword_set(ori)
		if cnfl and nor eq 1 then begin
			res = 0.
			err = 0.
		endif else begin
			if cnfl then wx = wx - Peak_moment(x,y,ser,ord=1,/ori,/int)
			res = total(rov*siv*wx^nor)
			if erfl then begin
				tem = wx^nor - res
				if cnfl then tem=tem-nor*wx*Peak_moment(x,y,ser,ord=nor-1,/int)
				err= sqrt(total((tem*ses)^2))
			endif else err = 0.
		endelse
	endif else begin
		if nxy gt 0 then begin
			res = 1.
			err = 0.
		endif else message, 'Bad or missing input!'
	endelse

	return, res
end