Pro Align_det_arm, xcen, slits, sec_scan = ses, test = test

;+
; NAME:
;		ALIGN_DET_ARM
; VERSION:
;		7.06
; PURPOSE:
;		Calculates the correction parameters for the detector arm (DTH and OTH)
;		Rotation
; CATEGORY:
;		Surface spectrometer software.
; CALLING SEQUENCE:
;		ALIGN_DET_ARM, XCEN, SLITS [, /SEC_SCAN]
; INPUTS:
;	XCEN
;		The center value for the slit scan on the second slit, after the first
;		has been zeroed by DTH scan.
;
;		Important:	The convention of positive_x <-> inboard direction is used.
;	SLITS
;		The distances of both slits from the sample.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/SEC_SCAN
;		Switch.  Set to specify that the slit scan is performed with the slit
;		further away from the detector.  By default, angle scan is performed
;		with this slit while slit scan is performed with the slit closer to the
;		sample.
; OUTPUTS:
;		Prints out the required corrections for OTH and DTH.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		As described in OTH-DTH corrections.
; MODIFICATION HISTORY:
;		Created 10-JUN-2008 by Mati Meron.
;-

	on_error, 1

	ro = 706.8
	psi = !dtor*34.16

	if n_elements(slits) eq 2 then slits = slits(sort(slits)) $
	else message, 'Must have two slit locations'
	if keyword_set(ses) then slits = reverse(slits)

	eps = asin(1.*xcen/(slits[0] - slits[1]))
	gam = -slits[1]*eps/(ro*cos(psi))
	del = -(gam + eps)

	print
	print, '	Rotate OTH by ' + string(!radeg*gam,form= '(f7.4," degrees")')
	print, '	Rotate DTH by ' + string(!radeg*del,form= '(f7.4," degrees")')
	print

	if keyword_set(test) then begin
		nv = [-sin(eps),cos(eps)]
		rov = ro*[-sin(psi),cos(psi)]
		rs = [0.,slits[1]]
		rf = rs - (slits[1]-slits[0])*nv
		rote = [[cos(eps),sin(eps)],[-sin(eps),cos(eps)]]
		rotd = [[cos(del),-sin(del)],[sin(del),cos(del)]]
		print, rote##rf + (rotd-rote)##rov
		print
		print, rote##rs + (rotd-rote)##rov
		print
	endif

	return
end
