Pro Align_3_circle, chi, phim = phm, phiz = phz, phip = php, polite = pol

;+
; NAME:
;		ALIGN_3_CIRCLE
; VERSION:
;		4.3
; PURPOSE:
;		Calculates the correction parameters for the 3-circle axis allignment.
; CATEGORY:
;		Surface spectrometer software.
; CALLING SEQUENCE:
;		ALIGN_3_CIRCLE [CHI, PHIM = PHM, PHIZ = PHZ, PHIP = PHP [,/POLITE]]
; INPUTS:
;	CHI
;		The absolute value of the Chi-angle used (measurements are taken at
;		-CHI, 0, CHI).  If not given, the program querries for this value.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	PHIM
;		Accepts the value of Phi (measured by Bragg reflection) corresponding
;		to Chi = -CHI.  If not given, the program querries for this value.
;	PHIZ
;		Accepts the value of Phi (measured by Bragg reflection) corresponding
;		to Chi = 0.  If not given, the program querries for this value.
;	PHIP
;		Accepts the value of Phi (measured by Bragg reflection) corresponding
;		to Chi = +CHI.  If not given, the program querries for this value.
;	/POLITE
;		Switch.  Adds "please" to the querries.
; OUTPUTS:
;		Prints out the vertical (ALPHA) and horizontal (THETA) angle errors of
;		the Chi-axis, and the corrections needed to cancel these errors.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		As described in SURFERR, App. C.  Calls MONTILT.  Also calls ISNUM from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 30-OCT-2002 by Mati Meron.
;-

	on_error, 1
	qui = !quiet
	!quiet = 1

	prompt = 'Gimme ' + ['chi ','phi (-chi) ','phi (0) ','phi (+chi) ']
	if keyword_set(pol) then prompt = prompt + ', please '

	if not Isnum(chi) then read, chi, prompt = prompt[0]
	if not Isnum(phm) then read, phm, prompt = prompt[1]
	if not Isnum(phz) then read, phz, prompt = prompt[2]
	if not Isnum(php) then read, php, prompt = prompt[3]

	wchi = !dtor*abs(chi)
	dalp = (phm - php)/(2*sin(wchi))
	dtet = (phm + php - 2*phz)/(2*(1-cos(wchi)))

	print
	print, dalp, form = '("		d_alpha = ",f8.5)'
	print, dtet, form = '("		d_theta = ",f8.5)'
	print
	print, '				THETA CORRECTION'
	print
	print, -dtet, form='("	Move theta to ",f8.5," , then reset it to zero.")'
	print
	print, '				ALPHA CORRECTION'
	Montilt, -dalp

	!quiet = qui
	return
end
