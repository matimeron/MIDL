Function Net_area, hv, ab, relative = rel, half = hlf

;+
; NAME:
;		NET_AREA
; VERSION:
;		5.2
; PURPOSE:
;		Calculates the intersection area of a rectangle and ellipse.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = NET_AREA( HV, AB [,keywords])
; INPUTS:
;	HV
;		A two element numerical vector representing the horizontal and vertical
;		dimensions of the rectangle (or half of those if /HALF is set).
;	AB
;		A two element numerical vector representing the horizontal and vertical
;		axes of the ellipse (or half of those if /HALF is set)
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/RELATIVE
;		Switch.  If set, the ratio of the intersection area to the area of the
;		full rectangle is calculated.
;	/HALF
;		Switch.  If set, HV represents the extensions of the rectangle from its
;		center (half the full dimensions) and same for the ellipse.
;
;		Note:  If /RELATIVE is set, /HALF does not matter.
; OUTPUTS:
;		Returns the net area of the intersection or, if /RELATIVE is set, the
;		ratio of said area to this of the rectangle.
;
;		Note:	For the purpose of the calculation the rectangle is assumed to
;				be no lareger than this needed to fully envelop the ellipse.  A
;				larger rectangle is cut down to this size.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Analytical calculation, straightforward.  Calls CALCTYPE and CAST from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 15-NOV-2005 by Mati Meron.
;-

	on_error, 1

	if n_elements(ab) eq 2 then wab = abs(ab) else message, 'Invalid [a,b]!'
	if n_elements(hv) eq 2 then whv= (abs(hv)<wab) else message,'Invalid [h,v]!'

	rfl = keyword_set(rel)
	if product(wab) gt 0 then begin
		rat = 1d*whv/wab
		if total(rat^2) le 1 then res = 1d else res = $
		(total(rat*sqrt(1-rat^2)) + total(asin(rat)) - !dpi/2)/(2*product(rat))
		if not rfl then res = (1 + 3*keyword_set(hlf))*res*product(whv)
	endif else res = 1d*rfl

	return, Cast(res,4,Calctype(whv,wab))
end