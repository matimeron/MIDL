Function Peak_ROI, x, y, ser, pick_range = prn, _extra = _e

;+
; NAME:
;		PEAK_ROI
; VERSION:
;		4.9
; PURPOSE:
;       Interactive selecting a region of interest.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_ROI ( X [, Y] [, SER] [, keywords])
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
;	/PICK_RANGE
;		Switch.  If set, user is asked to enter a search range in the form of
;		a 2-element vector.
;		Alternatively PICK_RANGE can be provided as 2-element vector in which
;		case it is used as is.
;	_EXTRA
;		A formal keyword used to pass plotting keywords.  Not to be used
;		directly.
; OUTPUTS:
;		Returns a 2-element vector containing the x coordinates of the
;		interactively selected range.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Uses CURSOR to establish region boundaries.  Calls
;		PEAK_SHOW.  Calls TRUCOL from MIDL.
; MODIFICATION HISTORY:
;		Created 20-MAY-2004 by Mati Meron.
;		Modified 5-JUN-2009 by Mati Meron.  Added possible 2-element input for
;		/PICK_RANGE.
;-

	on_error, 1

	Peak_show, x, y, ser, /auto, _extra = _e
	if keyword_set(prn) then begin
		wshow
		if n_elements(prn) ne 2 then begin
			rlo = (rhi = 0.)
			read, rlo, rhi, prompt = 'Enter Lo, Hi: '
		endif else rlo = min(prn,max=rhi)
		Peak_show, x, y, ser, /auto, xrange = [rlo,rhi], _extra = _e
	endif

	print
	print, 'Select first'
	wshow
	cursor, cx, cy, /data, /down
	plots, cx, cy, psym = 1, color = Trucol(0,4,4), thi = 2
	res = cx
	print, 'Select second'
	cursor, cx, cy, /data, /down
	plots, cx, cy, psym = 1, color = Trucol(0,4,4), thi = 2
	res = [res,cx]

	print
	print, 'OK'

	return, res(sort(res))
end