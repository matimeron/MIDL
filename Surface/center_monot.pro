Pro Center_monot, mt_0 = mtf, mt_180 = mts, polite = pol

;+
; NAME:
;		CENTER_MONOT
; VERSION:
;		4.3
; PURPOSE:
;		Calculates the correction parameters for "MONOT" and "XT".
; CATEGORY:
;		Surface spectrometer software.
; CALLING SEQUENCE:
;		ALLIGN_3_CIRCLE [MT_0 = MTF, MT_180 = MTS [,/POLITE]]
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	MT_0
;		Accepts the position (corresponding to beam center) of MONOT at CHI = 0.
;		If not given, the program querries for the value.
;	MT_180
;		Accepts the position (corresponding to beam center) of MONOT at
;		CHI = 180.  If not given, the program querries for the value.
;	/POLITE
;		Switch.  Adds "please" to the querries.
; OUTPUTS:
;		Prints out new value for MONOT and the correction required for XT.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		As described in log book 2.  Calls ISNUM from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUN-2003 by Mati Meron.
;-

	on_error, 1
	qui = !quiet
	!quiet = 1

	prompt = 'Gimme ' + ['monot_0 ','monot_180 ']
	if keyword_set(pol) then prompt = prompt + ', please '

	if not Isnum(mtf) then read, mtf, prompt = prompt[0]
	if not Isnum(mts) then read, mts, prompt = prompt[1]

	print
	print, (mtf + mts)/2, $
	form = '("	Change monot to = ",f8.5," and reset to zero")'
	print
	print, (mtf - mts)/2, $
	form = '("	Correct xt by   = ",f8.5," and reset to zero")'
	print

	!quiet = qui
	return
end