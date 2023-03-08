Pro Miscut, phi1 = ph1, phi2 = ph2, h1 = h1, h2 = h2, distance = dis, $
	polite = pol

;+
; NAME:
;		MISCUT
; VERSION:
;		4.3
; PURPOSE:
;		Calculating crystal miscut.
; CATEGORY:
;		Surface spectrometer calculations.
; CALLING SEQUENCE:
;		MISCUT, PHI1 = PH1, PHI2 = PH2
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	PHI1
;		The Bragg reflection angle at chi = 0, theta = 0.  If not provided, the
;		routine querries for this value.
;	PHI2
;		The Bragg Reflection angle at chi = 180, theta = 180.  If not provided
;		the routine querries for this value.
;	H1
;		The height of the beam at the PHI1 setting.  Optional.
;	H2
;		The height of the beam at the PHI2 setting.  Optional.
;	DISTANCE
;		The distance at which H1 and H2 are measured.  Not needed if H1 and H2
;		are not provided.
;	/POLITE
;		Switch.  Adds "Please" to the querries.
; OUTPUTS:
;		None other than screen output.  Prints to the screen the longitudinal
;		and (optionally) the transverse miscut values and the required
;		corrections.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Described in the "Miscut Calculations" writeup.  Calls ISNUM from MIDL.
; MODIFICATION HISTORY:
;		Created 8-30-2002 by Mati Meron.
;-

	on_error, 1
	qui = !quiet
	!quiet = 1

	prompt = 'Gimme ' + ['phi (0) ','phi (180) ', 'Distance ']
	if keyword_set(pol) then prompt = prompt + ', please '

	if not Isnum(ph1) then read, ph1, prompt = prompt[0]
	if not Isnum(ph2) then read, ph2, prompt = prompt[1]

	misl = (ph2 - ph1)/2.
	print
	print, misl, form = '("		Longitudinal miscut = ",f8.5)'
	print
	if misl ne 0 then begin
		print, -misl, $
		form='("	To correct, move phi to ",f8.5," , then reset to zero.")'
		print
	endif
	if Isnum(h1) and Isnum(h2) then begin
		if not Isnum(dis) then begin
			read, dis, prompt = prompt[2]
			print
		endif
		mist = !radeg/2*asin((h1-h2)/(2*dis*sin(!dtor/2*(ph1+ph2))))
		print, mist, form = '("		Transverse miscut = ",f8.5)'
		print
	endif else mist = 0.

	if mist ne 0 then begin
		print, mist, $
		form='("	To correct, move chi to ",f8.5," , then reset to zero.")'
		print
	endif

	bang = !radeg*asin(cos(!dtor*mist)*sin(!dtor/2*(ph1 + ph2)))
	print
	print, bang, form = '("		Bragg angle = ",f8.5)'
	print

	!quiet = qui
	return
end